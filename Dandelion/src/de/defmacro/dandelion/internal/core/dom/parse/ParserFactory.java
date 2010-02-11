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

package de.defmacro.dandelion.internal.core.dom.parse;

/**
 * Fuer die Erstellung der einzelnen Typparser verantwortlich.
 * @author Michael Bohn
 */
public class ParserFactory
{	
	private ParserFactory()
	{
		//Instanz ueber Factory-Methode
	}

	public static OrdinaryLambdaListParser getOrdinaryLambdaListParser()
	{
		/*
		if(fLambdaListParser == null) {
			fLambdaListParser = new OrdinaryLambdaListParser();
		}
		
		return fLambdaListParser;*/
		return new OrdinaryLambdaListParser();
	}
	
	public static MacroLambdaListParser getMacroLambdaListParser()
	{
		/*
		if(fMacroLambdaListParser == null) {
			fMacroLambdaListParser = new MacroLambdaListParser();
		}
		return fMacroLambdaListParser;*/
		return new MacroLambdaListParser();
	}
	
	public static InpackageParser getInpackageParser()
	{
		/*
		if(fInpackageParser == null) {
			fInpackageParser = new InpackageParser();
		}
		return fInpackageParser;*/
		return new InpackageParser();
	}
	
	public static DefunParser getDefunParser()
	{
		/*
		if(fDefunParser == null) {
			fDefunParser = new DefunParser();
		}
		return fDefunParser;*/
		return new DefunParser();
	}
	
	public static LambdaParser getLambdaParser()
	{
		/*
		if(fLambdaParser == null) {
			fLambdaParser = new LambdaParser();
		}
		return fLambdaParser; */
		return new LambdaParser();
	}
	
	public static DefmacroParser getDefmacroParser()
	{
		/*
		if( fDefmacroParser == null ) {
			fDefmacroParser = new DefmacroParser();
		}
		return fDefmacroParser;*/
		return new DefmacroParser();
	}
}
