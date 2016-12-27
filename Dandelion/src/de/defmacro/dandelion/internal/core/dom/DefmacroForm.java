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
 * Klasse fuer Makros.
 * @author Michael Bohn
 */
public class DefmacroForm 
extends FunctionDefiningForm 
{
	private Symbol fDefinedName;
	private MacroLambdaList fMacroLambdaList;
	private List<SExpression> fBody;
	
	public DefmacroForm(final List<SExpression> childs, final Symbol definedName, final MacroLambdaList lambdaList, List<SExpression> body)
	{
		this(childs, null, definedName, lambdaList, body);
	}
	
	public DefmacroForm(final List<SExpression> childs, final Position position, final Symbol definedName, final MacroLambdaList lambdaList, List<SExpression> body)
	{
		super(childs, position);
		setTyp(TSExpression.DEFMACRO);
		this.fDefinedName = definedName;
		this.fMacroLambdaList = lambdaList;
		this.fBody = body == null ? EMPTY_BODY : body;
	}
	
	/**
	 * @see DefiningForm#getDefinedName()
	 */
	@Override
	public Symbol getDefinedName() {
		return fDefinedName;
	}

	/**
	 * @see FunctionDefiningForm#getBody()
	 */
	@Override
	public List<SExpression> getBody() {
		return fBody;
	}

	/**
	 * Liefert die Macro-Lambda-List der Markodefinition.
	 */
	@Override
	public MacroLambdaList getLambdaList() {
		return fMacroLambdaList;
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
			for(SExpressionNode child : getBody()) {
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
