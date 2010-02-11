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

import org.eclipse.jface.text.Position;

/**
 * Basisklasse fuer Forms.
 * @author Michael Bohn
 */
public class Form
extends SExpression
{
	private Symbol fFunctionSymbol;
	
	public Form(final List<SExpression> childs)
	{
		this(childs, null);
	}
	
	public Form(final List<SExpression> childs, final Position position)
	{
		super(childs, position);
		
		if( childs.isEmpty() ) {
			throw new IllegalArgumentException("A Form needs at least one symbol");
		}
		
		if(childs.get(0).getTyp() != TSExpression.SYMBOL) {
			throw new IllegalArgumentException("First child of a form must be a symbol");
		}
		
		this.fFunctionSymbol = (Symbol)childs.get(0);
		setTyp(TSExpression.FORM);
	}
	
	
	/**
	 * Setzt einen Ausdruck an die erstelle Stelle.
	 * Erstes Child bei Form ist die Position hinter dem Funktionssymbol.
	 * Position 1 in der Child-Liste.
	 */
	@Override
	public void addFirstChild(final SExpression child) {
		addNth(1, child);
	}

	/**
	 * Das Symbol der Form. Erstes Element in Liste.
	 * @return
	 */
	public Symbol getFunctionSymbol() {
		return fFunctionSymbol;
	}

	/**
	 * @see SExpression#accept(ISexpDOMVisitor)
	 */
	@Override
	public void accept(final ISexpDOMVisitor visitor) 
	{
		boolean visitChilds = visitor.visit(this);
		if( !hasChildren() ) return;
		
		if( visitChilds ) {	
			for(int i=1,n=getChildren().size();i<n;i++) {
				SExpression child = getChild(i);
				child.accept(visitor);
			}
		}
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
}
