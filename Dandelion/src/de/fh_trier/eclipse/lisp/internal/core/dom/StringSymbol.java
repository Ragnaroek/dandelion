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

import org.eclipse.jface.text.Position;

/**
 * Ein String-Symbol.
 * @author Michael Bohn
 */
public class StringSymbol 
extends Symbol
{
	public StringSymbol(final String symbol)
	{
		this(symbol, null);
	}
	
	public StringSymbol(final String symbol, final Position position)
	{
		super(symbol, position);
		setTyp(TSExpression.STRING_SYMBOL);
	}
	
	//besondere Methoden StringSymbol, in Erweiterung
}
