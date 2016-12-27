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

package de.defmacro.dandelion.internal.core.connection;

import java.io.*;
import org.eclipse.core.runtime.jobs.ILock;

/**
 * Standardimplementierung des {@link IEvaluation}-Interface.
 * @author Michael Bohn
 */
public class LispEvaluation 
implements IEvaluation 
{	
	private IConnection fConnection;
	//private BufferedReader fIOReader;
	private PushbackReader fIOReader;
	private boolean fOutputIsRead;
	
	/**
	 * Erstellt ein neues Evaluierungsobjekt fuer die Verbindung.
	 * @param connection
	 * @throws ConnectionException
	 */
	public LispEvaluation(IConnection connection)
	throws ConnectionException
	{
		if( !connection.isConnected() ) {
			throw new ConnectionException("Connection not connected");
		}
		
		this.fConnection = connection;
		this.fIOReader = new PushbackReader(connection.getIOReader(), 2);
	}
	
	/**
	 * @see IEvaluation#evalStart(String, String)
	 */
	public void evalStart(final String pack, final String form)
	throws ConnectionException
	{
		fOutputIsRead = false;
		fConnection.getProtocolWriter().writeEval(pack, form);
	}
	
	/**
	 * @see IEvaluation#evalResult()
	 */
	public IResult evalResult() throws ConnectionException 
	{
		fOutputIsRead = false;  //!!!!! Bei invokeRestart wird nur evalResult aufgerufen
		return fConnection.getProtocolReader().readEvalResult();
	}

	/**
	 * @see IEvaluation#getConnection()
	 */
	public IConnection getConnection()
	{
		return fConnection;
	}
	
	/**
	 * @see IEvaluation#resetOutputState()
	 */
	public void resetOutputState() {
		fOutputIsRead = false; 
		
	}

	/**
	 * @see IEvaluation#nextOutputLine()
	 */
	public String nextOutputLine()
	throws ConnectionException
	{		
		if( fOutputIsRead ) {
			return null;
		}
		
		try {
			StringBuilder buffer = new StringBuilder();
			while(true) {
				int c = fIOReader.read();
				if(c < 0) { 
					return null;
				}
				
				if(c == '\n' && nullLinefeedAhead()) { //ende erreicht
					fOutputIsRead = true;
					if(buffer.toString().trim().equals("")) {
						return null;
					}
					return buffer.toString();
				} else if (c == '\r') {
					int lf = fIOReader.read(); //read geht immer, da zumindest null-linefeed kommt
					if(lf != '\n') {
						fIOReader.unread(lf);
					} /*else {
						System.out.println("discarding lf");
					}*/
					return buffer.toString();
				} else if(c == '\n') {
					return buffer.toString();
				}
				
				buffer.append((char)c);
			}
			
		} catch (IOException e) {
			throw new ConnectionException("Error Reading IO", e);
		}
	} 
	
	private boolean nullLinefeedAhead()
	throws IOException
	{
		char[] nl = new char[2];
		for(int i=0;i<2;i++) {
			int c = fIOReader.read();
			if(c < 0) return true;
			nl[i] = (char)c;
		}
		
		boolean isAhead = false;
		if(nl[0] == 0 && nl[1] == '\n') {
			isAhead = true;
		} else {
			fIOReader.unread(nl);
		}
	
		return isAhead;
	} 

	/**
	 * Das Lock-Objekt ist das Lock-Objekt der Verbindung.
	 * @see IEvaluation#getLock().
	 */
	public ILock getLock() {
		return getConnection().getLock();
	}
}
