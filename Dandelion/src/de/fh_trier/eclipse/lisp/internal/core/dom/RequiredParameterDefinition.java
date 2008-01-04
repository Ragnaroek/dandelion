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

package de.fh_trier.eclipse.lisp.internal.core.dom;

/**
 * Die Definition eines Required-Parameter in Lambda-Liste.
 */
public class RequiredParameterDefinition 
{
	private final boolean fDestructuring;
	private final SExpression fDestructuringForm;
	private final Symbol fParameter;
	
	/**
	 * Erstellt einen neuen Destructuring-Required-Parameter.
	 * Nur fuer Macro-Lambda-Liste.
	 * @param destructuring - Destructuring Parameter Liste.
	 */
	public RequiredParameterDefinition(final SExpression destructuring)
	{
		this.fDestructuring = true;
		this.fDestructuringForm = destructuring;
		this.fParameter = null;
	}
	
	/**
	 * Erstellt ein Symbol-Required-Parameter.
	 * @param symbol
	 */
	public RequiredParameterDefinition(final Symbol symbol) {
		this.fDestructuring = false;
		this.fDestructuringForm = null;
		this.fParameter = symbol;
	}
	
	/**
	 * Liefert <code>true</code> wenn Parameter Destructuring-Parameter ist.
	 * @return
	 */
	public boolean isDestructuring() {
		return fDestructuring;
	}
	
	/**
	 * Liefert <code>true</code> wenn normale Parameter Definition.
	 * Kein Destructuring.
	 * @return
	 */
	public boolean isSimpleDefinition()
	{
		return !fDestructuring;
	}
	
	/**
	 * Liefert den Destructuring-Parameter.
	 * Gibt <code>null</code> zurueck wenn nicht Destructuring.
	 * @return
	 */
	public SExpression getDestructuringParameter() {
		return fDestructuringForm;
	}
	
	/**
	 * Liefert das normale Parameter Symbol.
	 * Gibt <code>null</code> zurueck wenn Destructuring.
	 * @return
	 */
	public Symbol getParameterSymbol() {
		return fParameter;
	}
}
