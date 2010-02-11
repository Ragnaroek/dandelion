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

import de.defmacro.dandelion.internal.core.dom.parse.DestructuredSymbol;

import edu.umd.cs.findbugs.annotations.*;

/**
 * Ein Lisp-Symbol.
 * @author Michael Bohn
 */
public class Symbol 
extends SExpression
{
	public static final String SYM_QUOTE = "quote";
	public static final String SYM_BACKQUOTE = "`";
	public static final String SYM_COMMA = ",";
	public static final String SYM_DEFUN = "defun";
	public static final String SYM_DEFMACRO = "defmacro";
	public static final String SYM_DEFPACKAGE = "defpackage";
	public static final String SYM_IN_PACKAGE = "in-package";
	public static final String SYM_LAMBDA = "lambda";
	public static final String SYM_NIL    = "NIL";
	public static final String SYM_T      = "T";
	public static final String SYM_CL_USER = "CL-USER";
	
	private static final String PRIVATE_SEPARATOR =  "::";
	private static final String PUBLIC_SEPARATOR = ":";
	
	private String fSymbolName;
	private String fQualifier;
	private String fQualifiedName;
	private boolean fInterned;
	private boolean fPrivate;
	
	public Symbol(final String symbolName)
	{
		this(symbolName, null);
	}
	
	public Symbol(final String symbolName, final Position position)
	{
		this(symbolName, position, null, true, false);
	}
	
	public Symbol(final Position position, final boolean interned, final DestructuredSymbol parts)
	{
		this(parts.getSymbolName(), position, parts.getQualifier(), interned, parts.isPrivate());
	}
	
	public Symbol(final String symbolName, final Position position, final String qualifier, final boolean interned, final boolean privateSymbol)
	{
		super(null, position);
		setTyp(TSExpression.SYMBOL);
		
		if(symbolName == null) {
			throw new NullPointerException();
		}
		
		if(!interned && qualifier != null) {
			throw new IllegalArgumentException("uninterned symbol must not have a qualifier");
		}
		
		if(privateSymbol && qualifier == null) {
			throw new IllegalArgumentException("private symbol needs a qualifier");
		}
		
		this.fSymbolName = symbolName;
		this.fQualifier = qualifier;
		this.fInterned = interned;
		this.fPrivate = privateSymbol;
		this.fQualifiedName = null;
	}
	
	@Override
	public void accept(final ISexpDOMVisitor visitor) 
	{
		visitor.visit(this);
	}
	
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
	 * Liefert den Namen des Symbols als String.
	 * @return
	 */
	@NonNull
	public String getSymbolName()
	{
		return fSymbolName;
	}
	
	/**
	 * Liefert <code>true</code> wenn symbol geinternt wurde.
	 * @return
	 */
	public boolean isInterned()
	{
		return fInterned;
	}

	/*package*/ void setInterned(final boolean interned)
	{
		this.fInterned = interned;
	}
	
	/**
	 * Liefert <code>true</code> wenn Symbol qualifiziert angegeben wurde.
	 * @return
	 */
	public boolean isQualified()
	{
		return fQualifier != null;
	}
	
	/**
	 * Liefert <code>true</code> wenn Symbol privat angegeben wurde.
	 * @return
	 */
	public boolean isPrivate()
	{
		return fPrivate;
	}
	
	/*package*/ void setPrivate(final boolean priv)
	{
		this.fPrivate = priv;
	}
	
	/**
	 * Test ob Symbol gueltiger StringDesignator.
	 * @return
	 */
	public boolean isStringDesignator()
	{
		TSExpression typ = getTyp();
		if(typ == TSExpression.READER_SYMBOL) {
			ReaderSymbol readerSymbol = (ReaderSymbol)this;
			return readerSymbol.isCharSymbol();
		}
		return typ == TSExpression.STRING_SYMBOL  || typ == TSExpression.KEYWORD_SYMBOL || typ == TSExpression.SYMBOL;
	}	
	
	/**
	 * Liefert den kompletten Symbolname, mit evtl. vorhandenem Qualifier.
	 * @return
	 */
	public String getQualifiedName()
	{   
		//lazy-init
		if(fQualifiedName == null) {
			fQualifiedName = lazyInitQualfiedName();
		}
		
		return fQualifiedName;
	}
	
	private String lazyInitQualfiedName()
	{
		if(fQualifier == null && fInterned) {
			return fSymbolName;
		} 
		
		if(!fInterned) { //ein nicht geinterntes Symbol hat kein Package
			return "#:" + fSymbolName;
		}
		
		return fQualifier + (fPrivate ? PRIVATE_SEPARATOR : PUBLIC_SEPARATOR) + fSymbolName;
	}
	
	/**
	 * Liefert den Qualifier.
	 * <code>null</code> wenn kein Qualifiert angegeben wurde.
	 * @return
	 */
	@Nullable
	public String getQualifier()
	{
		return fQualifier;
	}
	
	/*package*/ void setQualifier(final String qualifier)
	{
		this.fQualifier = qualifier;
	}
}
