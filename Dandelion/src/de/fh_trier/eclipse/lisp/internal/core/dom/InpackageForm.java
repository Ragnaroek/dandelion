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

import java.util.*;
import org.eclipse.jface.text.Position;

/**
 * Ein In-package Ausdruck.
 * @author Michael Bohn
 */
public class InpackageForm 
extends Form 
{	
	private Symbol fPackageSymbol;
	
	public InpackageForm(final List<SExpression> childs, final Symbol packageDesignator)
	{
		this(childs, null, packageDesignator);
	}
	
	public InpackageForm(final List<SExpression> childs, final Position position, final Symbol packageDesignator) 
	{
		super(childs, position);
		setTyp(TSExpression.INPACKAGE);
		
		this.fPackageSymbol = packageDesignator;
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
	
	/**
	 * Das Paket des in-package Ausdrucks.
	 * @return
	 */
	public Symbol getPackage()
	{
		return fPackageSymbol;
	}
	
	/**
	 * Liefert das in-package wenn kein in-package angegeben wurde (CL-USER).
	 * Es wird immer ein neues Objekt zurueckgegeben.
	 * @return Neue in-package Objekt
	 */
	public static InpackageForm defaultPackage() {
		List<SExpression> childs = new ArrayList<SExpression>(2);
		Symbol cluser = new Symbol(Symbol.SYM_CL_USER);
		childs.add(new Symbol(Symbol.SYM_IN_PACKAGE));
		childs.add(cluser);
		return new InpackageForm(childs, new Position(0,0), cluser);
	}
}
