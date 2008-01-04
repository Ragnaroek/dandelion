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

import java.util.List;

import org.eclipse.jface.text.Position;

/**
 * Klasse fuer defpackage-Ausdruecke.
 * @author Michael Bohn
 */
public class DefpackageForm 
extends DefiningForm
{
	public DefpackageForm(final List<SExpression> childs)
	{
		this(childs, null);
	}
	
	public DefpackageForm(final List<SExpression> childs, final Position position) 
	{
		super(childs, position);
		setTyp(TSExpression.DEFPACKAGE);
	}
	
	/**
	 * Der Paketname.
	 * @see DefiningForm#getDefinedName()
	 */
	@Override
	public Symbol getDefinedName() 
	{
		//TODO Da Defpackage noch nicht geparst wird, versuchen paketname zu extrahieren
		//der Code ist nur Übergangsweise
		
		if(getChildren().size() >= 2) {
			SExpression child = getChild(1);
			if(child instanceof Symbol) {
				return (Symbol)child;
			}
			return new Symbol("UNKNOWN");
		} 
		return new Symbol("UNKNOWN");
	}

	/**
	 * @see SExpression#accept(ISexpDOMVisitor)
	 */
	@Override
	public void accept(ISexpDOMVisitor visitor) 
	{
		boolean visitChilds = visitor.visit(this);
		if( !hasChildren() ) return;
		
		if( visitChilds ) {		
			for(SExpressionNode child : getChildren()) {
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
