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

import org.eclipse.jface.text.Position;

/**
 * Ein Reader-Symbol.
 * @author Michael Bohn
 */
public class ReaderSymbol 
extends Symbol 
{
	/**
	 * Symbolname fuer ein Vector-Reader-Symbol.
	 */
	public static final String VECTOR = "vector";
	
	private char fDispatchCharacter;
	private boolean fExpectsObject;
	private boolean fSymbolAllowed;
	
	public ReaderSymbol(final String symbol, final Position position, final char dispatchChar) 
	{
		super(symbol, position);
		setTyp(TSExpression.READER_SYMBOL);	
		this.fDispatchCharacter = Character.toLowerCase(dispatchChar);
		this.fExpectsObject = computeExpectsSExpression(fDispatchCharacter);
		this.fSymbolAllowed = computeSymbolAllowed(fDispatchCharacter);
	}
	
	/**
	 * @see SExpression#accept(ISexpDOMVisitor)
	 */
	@Override
	public void accept(final ISexpDOMVisitor visitor) {
		boolean visitChilds = visitor.visit(this);
		
		if(visitChilds && this.hasChildren()) {
			for(SExpression child : this.getChildren()) {
				child.accept(visitor);
			}
		}
	}

	private boolean computeExpectsSExpression(final char c)
	{
		if(c == '(' || c == '+' || c == '-' || c == 'a' || c == 'c' || c == 's') {
			return true;
		}
		if(c == '.' || c == '\'') {
			return getSymbolName().length() == 0; //nur wenn #. oder #' (kein symbolname) angegeben wird eine Form erwartet
		}
		return false;
	}
	
	private boolean computeSymbolAllowed(char c)
	{
		return c == '.' || c == '+' || c == '-' || c == '\'';
	}
	
	/**
	 * Liefert den Dispatch-Character des Reader-Symbols.
	 * @return
	 */
	public char getDispatchCharacter()
	{
		return fDispatchCharacter;
	}
	
	/**
	 * Liefert <code>true</code> wenn es sich um ein Reader-Char-Symbol handelt.
	 * @return <code>true</code> wenn Char-Symbol
	 */
	public boolean isCharSymbol()
	{
		return fDispatchCharacter == '\\';
	}
	
	/**
	 * Liefert <code>true</code> wenn diese Reader-Symbol eine S-Expression
	 * erwartet. z.B. fuer #(1 2 3) ; Vector reader symbol
	 * @return <code>true</code> wenn Objekt erwartet
	 */
	public boolean expectsObject()
	{
		return fExpectsObject;
	}
	
	/**
	 * Liefert <code>true</code> wenn auch die Angabe eines Symbols
	 * erlaubt ist.
	 * @return <code>true</code> wenn Symbol erlaubt
	 */
	public boolean symbolAllowed()
	{
		return fSymbolAllowed;
	}
	
	/**
	 * Test ob weiteres Symbol benoetigt wird.
	 * @return <code>true</code> wenn noch nicht alle Symbole vorhanden.
	 */
	public boolean needMore()
	{
		if(getDispatchCharacter() == '+' || getDispatchCharacter() == '-') {
			return checkReadCondition();
		}
		return false;
	}
	
	private boolean checkReadCondition()
	{
		if(!hasChildren()) {
			return true; //min. ein child erwartet
		}
		
		//invariante: min. ein child vorhanden
		
		SExpression sexp = getChild(0);
		if(getSymbolName().length() == 0 && sexp.getTyp() == TSExpression.FORM) {
			String fn = ((Form)sexp).getFunctionSymbol().getSymbolName();
			if( fn.equalsIgnoreCase("or") || fn.equalsIgnoreCase("and") ) {
				return getChildren().size() < 2;
			}
			return false;
		} 
		return false;
		
	}
	/**
	 * Liefert <code>null</code> wenn {@link ReaderSymbol#expectsSExpression()} <code>false</code> liefert.
	 * Ansonsten die zugehoerige S-Expression.
	 * @return Das erste Objekt des Reader-Symbols
	 */
	public SExpression getObject()
	{
		if(!hasChildren()) {
			return null;
		}
		return getChild(0);
	}
	
	/**
	 * Liefert das n-te Objekt des Reader-Symbols.
	 * Gibt <code>null</code> zurueck wenn nicht vorhanden.
	 * @param n - n-tes Objekt liefern
	 * @return
	 */
	public SExpression getObject(final int n)
	{
		if(!hasChildren()) {
			return null;
		}
		
		if(n >= getChildren().size()) {
			return null;
		}
		return getChild(n);
	}
}
