/*
 Dandelion, a Lisp plugin for Eclipse.
 Copyright (C) 2007 Michael Bohn

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

package de.fh_trier.eclipse.lisp.internal.core.connection.protocol;

import java.io.*;
import java.util.*;
import java.util.regex.*;

import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.core.meta.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Implementierung der {@link IProtocolReader}-Schnittstelle.
 * @author Michael Bohn
 * testcase
 */
public class ProtocolReader 
implements IProtocolReader 
{
	private final BufferedReader fReader;
	private final IProtocolPatternFactory fPatternFactory;
	
	/**
	 * Erstellt einen neuen ProtocolReader fuer die angegebene
	 * Verbindung.
	 * @param connection
	 * @throws ConnectionException
	 */
	public ProtocolReader(final IConnection connection)
	throws ConnectionException
	{
		if (connection == null) {
			throw new NullPointerException("connection must not be null");
		}
		
		this.fPatternFactory = new ProtocolPatternFactory();
		this.fReader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
	}

	/**
	 * @see IProtocolReader#readEvalResult()
	 */
	public IResult readEvalResult() throws ProtocolException 
	{
		String input = doRead();
		Pattern pattern = fPatternFactory.getEvalResultPattern();
		checkMatch(pattern, input);
		
		//invariante: Gültige EvalResult Antwort von Server
		
		IResult result = null;
		String[] parts = split(input);
		if(parts[0].equals(ProtocolConstants.TOKEN_OK)) {
			String resultForm = ProtocolUtilities.decodeBase64(parts[2]);
			List<String> multipleValueList = createMultipleValueList(parts);
			result = Result.instanceOfSuccess(parts[1], resultForm, multipleValueList);
		} else if (parts[0].equals(ProtocolConstants.TOKEN_EVAL_ERROR)) {
			List<IRestart> restarts = createRestarts(parts);
			result = Result.instanceOfEvalError(ProtocolUtilities.decodeBase64(parts[1]), restarts);
		} else { //read-error
			result = Result.instanceOfReadError(ProtocolUtilities.decodeBase64(parts[1]));
		}
		
		return result;
	}

	private List<String> createMultipleValueList(String[] parts) {
		List<String> multipleValues = new ArrayList<String>((parts.length-2));
		
		for(int i=3; i<parts.length; i++) {
			multipleValues.add(ProtocolUtilities.decodeBase64(parts[i]));
		}
		
		return multipleValues;
	}

	private List<IRestart> createRestarts(String[] parts)
	{
		//durch den Regex-check ist sichergestellt das die Restarts
		//in gueltiger Form vorliegen, deshalb keine Ueberpruefung hier
		List<IRestart> restarts = new ArrayList<IRestart>((parts.length-2)/2);
		for(int i=2; i<parts.length; i += 2) {
			String restartSymbol = parts[i];
			String restartDescription = parts[i+1];
			
			IRestart restart = new Restart(restartSymbol, ProtocolUtilities.decodeBase64(restartDescription));
			restarts.add(restart);
		}
		return restarts;
	}
	
	/**
	 * @see IProtocolReader#readSuccess()
	 */
	public void readSuccess() 
	throws ProtocolException 
	{
		checkMatch(fPatternFactory.getSuccessPattern(), doRead());
	}
	
	private String[] split(final String input)
	{
		return fPatternFactory.getWordSplitPattern().split(input);
	}
	
	/**
	 * @see IProtocolReader#readPackageList()
	 */
	public List<String> readPackageList() throws ProtocolException 
	{
		Pattern init = fPatternFactory.getPackageListInitPattern();
		Pattern pack = fPatternFactory.getPackageListElementPattern();
		
		String initString = doRead();
		checkMatch(init, initString);
		
		String[] initParts = split(initString);
		int cnt = Integer.parseInt(initParts[1]);
		List<String> packages = new ArrayList<String>(cnt);
		
		for(int i=0;i<cnt;i++) {
			String packString = doRead();
			checkMatch(pack, packString);
			packages.add(packString);
		}
		
		return packages;
	}
	
	/**
	 * @see IProtocolReader#readFunctionSymbols(String, TMetaType)
	 */
	@SuppressWarnings("Dm")
	public List<IMetaSymbol> readFunctionSymbols(final String pack, final TMetaType type) throws ProtocolException 
	{	
		Pattern init = fPatternFactory.getFunctionListInitPattern();
		Pattern element = fPatternFactory.getFunctionListElementPattern();
		
		String initString = doRead();
		checkMatch(init, initString);
		
		String initParts[] = split(initString);
		if(initParts[0].equals(ProtocolConstants.TOKEN_ERROR)) {
			throw new ProtocolException("ERROR " + ProtocolUtilities.decodeBase64(initParts[1]));
		}
		
		int cnt = Integer.parseInt(initParts[1]);
		List<IMetaSymbol> metaSymbols = new ArrayList<IMetaSymbol>(cnt);
		for(int i=0;i<cnt;i++) {
			String metaString = doRead();
			checkMatch(element, metaString);
			String metaParts[] = split(metaString);
			String fn = metaParts[0];
			String doc = ProtocolUtilities.decodeBase64(metaParts[1]);
			List<String> args = null;
			
			if(doc.equalsIgnoreCase("NIL")) {
				doc = null;
			}
			
			int argLen = metaParts.length - 2;
			if(argLen > 0) {
				args = new ArrayList<String>(argLen);
				for(int z=2;z<metaParts.length;z++) {
					args.add(metaParts[z]);
				}
			}
			
			metaSymbols.add(new FunctionMetaSymbol(pack.toUpperCase(), fn.toUpperCase(), doc, args, type));
 		}
		return metaSymbols;
	}

	private void checkMatch(final Pattern p, final String input)
	throws ProtocolException
	{
		if( input == null ) {
			throwIllegalResponse(null);
		}
		
		Matcher m = p.matcher(input);
		if( !m.matches() ) {
			throwIllegalResponse(input);
		}
	}
	
	private void throwIllegalResponse(final String input)
	throws ProtocolException
	{
		throw new ProtocolException("Unknown/Illegal Response: " + input);
	}
	
	private String doRead()
	throws ProtocolException
	{
		try {
			return fReader.readLine();
		} catch (IOException e) {
			//Error Translation
			throw new ProtocolException("Error while reading", e);
		}
	}
}
