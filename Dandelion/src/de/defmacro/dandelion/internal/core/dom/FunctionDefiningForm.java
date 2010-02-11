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

import java.util.*;

import org.eclipse.jface.text.Position;

/**
 * Basisklasse fuer Ausdruecke die Funktionen definieren.
 * @author Michael Bohn
 */
public abstract class FunctionDefiningForm 
extends DefiningForm 
{
	protected static final List<SExpression> EMPTY_BODY = Collections.unmodifiableList(new ArrayList<SExpression>(0));
	
	public FunctionDefiningForm(final List<SExpression> childs)
	{
		this(childs, null);
	}
	
	public FunctionDefiningForm(final List<SExpression> childs, final Position position) {
		super(childs, position);
	}
	
	/**
	 * @see Case
	 */
	@Override
	public <T> T typeSwitch(final Case<T> c)
	{
		T result = c.typeCase(this);
		if(c.isFallthrough() && result == null) {
			return super.typeSwitch(c);
		}
		return result;
	}
	
	/**
	 * Liefert die Lambda-Liste der Funktion.
	 * @return
	 */
	public abstract OrdinaryLambdaList getLambdaList();
	
	/**
	 * Liefert die Body-Ausdruecke der Funktion.
	 * @return
	 */
	public abstract List<SExpression> getBody();
}
