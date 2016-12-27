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

package de.defmacro.dandelion.internal.core.dom;

import java.util.List;

import javax.annotation.CheckForNull;


/**
 * Die Lambda-Liste eines Makros.
 * @author Michael Bohn
 */
public class MacroLambdaList
extends OrdinaryLambdaList 
{
	/**
	 * Die NIL-Lambda Liste.
	 */
	public static final MacroLambdaList NIL = new MacroLambdaList(null, null, null, null, null, false, null, null, null);
	
	private Symbol fWhole;
	private Symbol fEnvironment;
	private Symbol fBody;
	
	public MacroLambdaList(final List<RequiredParameterDefinition> required, final List<ParameterDefinition> optional, 
						   final Symbol rest, final Symbol body, final List<ParameterDefinition> keyword, 
						   final boolean allowOtherKeys, final List<ParameterDefinition> aux,
						   final Symbol whole, final Symbol environment) 
	{
		super(required, optional, rest, keyword, allowOtherKeys, aux);
		this.fWhole = whole;
		this.fEnvironment = environment;
		this.fBody = body;
	}
	
	/**
	 * Liefert das &ampwhole Symbol.
	 * @return whole-Symbol
	 */
	public Symbol getWholeParameter()
	{
		return fWhole;
	}
	
	/**
	 * Liefert das &ampenvironment Symbol.
	 * @return environment Symbol
	 */
	public Symbol getEnvironmentParameter()
	{
		return fEnvironment;
	}

	/**
	 * Liefert das &ampbody Symbol.
	 * @return body-Symbol
	 */
	@CheckForNull
	public Symbol getBodyParameter()
	{
		return fBody;
	}
	
	/**
	 * Test ob &ampwhole-Parameter angegeben.
	 * @return
	 */
	public boolean hasWholeParameter()
	{
		return getWholeParameter() != null;
	}
	
	/**
	 * Test ob &ampenvironment-Parameter angegeben.
	 * @return
	 */
	public boolean hasEnvironmentParameter()
	{
		return getEnvironmentParameter() != null;
	}
	
	/**
	 * Test ob &ampbody-Parameter angegeben.
	 * @return
	 */
	public boolean hasBodyParameter()
	{
		return getBodyParameter() != null;
	}

	/**
	 * Test ob Lambda-Liste NIL.
	 * @return <code>true</code> wenn Lambda-Liste NIL
	 */
	@Override
	public boolean isNil() {
		return super.isNil() && !hasBodyParameter() && !hasWholeParameter() && !hasEnvironmentParameter();
	}
}
