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

import java.util.regex.*;

import static de.fh_trier.eclipse.lisp.internal.core.connection.protocol.ProtocolConstants.*;

/**
 * Implementierung des {@link IProtocolPatternFactory}-Interfaces.
 * @author Michael Bohn
 * testcase
 */
public class ProtocolPatternFactory
implements IProtocolPatternFactory
{	
	private static String errorResponse;
	
	private static Pattern evalResultPattern;
	private static Pattern packageListInitPattern;
	private static Pattern packageListElementPattern;
	private static Pattern functionListInitPattern;
	private static Pattern functionListElementPattern;
	private static Pattern successPattern;
	private static Pattern wordSplitPattern;
	
	/**
	 * @see IProtocolPatternFactory#getEvalResultPattern()
	 */
	public Pattern getEvalResultPattern() 
	{
		if(evalResultPattern == null) {
			StringBuilder pattern = new StringBuilder();
			
			//OK response
			pattern.append(group(TOKEN_OK));
			pattern.append(REGEX_BLANK);
			pattern.append(group(REGEX_SYMBOL));
			pattern.append(REGEX_BLANK);
			
			pattern.append('(');
			String form = group(REGEX_BASE64);
			String formMid = group(REGEX_BASE64, REGEX_BLANK);
			
			pattern.append(form); //nur eine Form als Ergebnis (kein Multiple-Value)
			pattern.append('|');
			pattern.append(formMid);
			pattern.append('+');
			pattern.append(form);
			pattern.append(')');
	
			pattern.append('|');
			
			//ODER Error Response
			pattern.append(group(TOKEN_EVAL_ERROR));
			pattern.append(REGEX_BLANK);
			pattern.append(group(REGEX_BASE64)); //<reason>
			
			pattern.append('(');
			pattern.append(group(REGEX_BLANK, REGEX_SYMBOL, REGEX_BLANK, REGEX_BASE64));
			pattern.append("*");
			pattern.append(')');
			
			//ODER Reader Error
			pattern.append('|');
			pattern.append(group(TOKEN_READ_ERROR));
			pattern.append(REGEX_BLANK);
			pattern.append(group(REGEX_BASE64));
			
			evalResultPattern = doCompile(pattern.toString());
		}
		return evalResultPattern;
	}

	private String getErrorResponse()
	{
		if(errorResponse == null) {
			StringBuilder pattern = new StringBuilder();
			pattern.append(group(TOKEN_ERROR));
			pattern.append(REGEX_BLANK);
			pattern.append(group(REGEX_BASE64));
			errorResponse = pattern.toString();
		}
		return errorResponse;
	}
	
	/**
	 * @see IProtocolPatternFactory#getPackageListInitPattern()
	 */
	public Pattern getPackageListInitPattern() {
		if(packageListInitPattern == null) {
			StringBuilder pattern = new StringBuilder();
			
			pattern.append(group(TOKEN_PACKAGELIST));
			pattern.append(REGEX_BLANK);
			pattern.append(REGEX_INTEGER);
			
			packageListInitPattern = doCompile(pattern.toString());
		}
		return packageListInitPattern;
	}
	
	/**
	 * @see IProtocolPatternFactory#getFunctionListElementPattern()
	 */
	public Pattern getPackageListElementPattern() {
		if(packageListElementPattern == null) {
			packageListElementPattern = doCompile(REGEX_SYMBOL);
		}
		return packageListElementPattern;
	}

	/**
	 * @see IProtocolPatternFactory#getFunctionListInitPattern()
	 */
	public Pattern getFunctionListInitPattern() {
		if(functionListInitPattern == null) {
			StringBuilder pattern = new StringBuilder();
			
			pattern.append(group(getErrorResponse()));
			
			pattern.append('|');
			
			pattern.append('(');
			pattern.append(group(TOKEN_FUNCTIONLIST));
			pattern.append(REGEX_BLANK);
			pattern.append(REGEX_INTEGER);
			pattern.append(')');
			
			functionListInitPattern = doCompile(pattern.toString());
		}
		return functionListInitPattern;
	}

	/**
	 * @see IProtocolPatternFactory#getFunctionListElementPattern()
	 */
	public Pattern getFunctionListElementPattern() {
		if(functionListElementPattern == null) {
			StringBuilder pattern = new StringBuilder();
			pattern.append(REGEX_SYMBOL);
			pattern.append(REGEX_BLANK);
			pattern.append(REGEX_BASE64);
			pattern.append(group(REGEX_BLANK, REGEX_SYMBOL));
			pattern.append('*');
			functionListElementPattern = doCompile(pattern.toString());
		}
		return functionListElementPattern;
	}

	/**
	 * @see IProtocolPatternFactory#getSuccessPattern()
	 */
	public Pattern getSuccessPattern() 
	{
		if(successPattern == null) {
			successPattern = doCompile(TOKEN_OK);
		}
		return successPattern;
	}
	
	/**
	 * @see IProtocolPatternFactory#getWordSplitPattern()
	 */
	public Pattern getWordSplitPattern() {
		if(wordSplitPattern == null) {
			wordSplitPattern = Pattern.compile(REGEX_BLANK);
		}
		return wordSplitPattern;
	}

	private String group(String... regexes)
	{
		StringBuilder group = new StringBuilder();
		group.append('(');
		
		for(String regex : regexes) {
			group.append(regex);
		}
		
		group.append(')');
		
		return group.toString();
	}
	
	private Pattern doCompile(String regex)
	{
		return Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
	}
}
