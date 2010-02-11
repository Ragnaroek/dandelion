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

/**
 * Eine Parameter-Definition in einer Lambda-Liste.
 * @author Michael Bohn
 */
public class ParameterDefinition 
{
	private Symbol fParameterSymbol;
	private SExpression   fInitValue;
	private Symbol fSuppliedTestSymbol;
	
	/**
	 * Erstellt eine neue Parameter-Definition.
	 * @param paramSymbol - Das Parameter-Symbol
	 * @param initValue - Der Intialisierungsausdruck.
	 * @param suppliedTestSymbol - das Symbol fuer Test auf uebergabe des Parameters
	 */
	public ParameterDefinition(final Symbol paramSymbol, final SExpression initValue, final Symbol suppliedTestSymbol)
	{
		this.fParameterSymbol = paramSymbol;
		this.fInitValue = initValue;
		this.fSuppliedTestSymbol = suppliedTestSymbol;
	}
	
	/**
	 * Liefert das Parameter-Symbol.
	 * @return
	 */
	public Symbol getParameterSymbol()
	{
		return fParameterSymbol;
	}
	
	/**
	 * Liefert das Intialisierungssymbol oder die Form.
	 * Kann <code>null</code> sein.
	 * @return Init-Form
	 */
	public SExpression getInitValue()
	{
		return fInitValue;
	}
	
	/**
	 * Liefert das Testsymbol.
	 * Kann <code>null</code> sein.
	 * @return
	 */
	public Symbol getSuppliedTestSymbol()
	{
		return fSuppliedTestSymbol;
	}
	
	/**
	 * Test auf vorhandensein Parametersymbol.
	 * @return
	 */
	public boolean hasParameterSymbol()
	{
		return fParameterSymbol != null;
	}
	
	/**
	 * Test auf vorhandensein Init-Form.
	 * @return
	 */
	public boolean hasInitValue()
	{
		return fInitValue != null;
	}
	
	/**
	 * Test auf vorhandensein Test-Symbol.
	 * @return
	 */
	public boolean hasSuppliedTestSymbol()
	{
		return fSuppliedTestSymbol != null;
	}
}
