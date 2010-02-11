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

package de.defmacro.dandelion.internal.ui.text;

/**
 * Schnittstelle fuer Visitor eines Lisp-Dokumentes.
 * End-Offset ist Position in Dokument an der der 
 * letzte Char steht!
 * Daher ist z.B. fuer Symbol <code>x</code> start Offset == end Offset.
 */
public interface ISexpressionVisitor
extends IDocumentVisitor
{
	public boolean visitTopLevelSymbolStart(int offset);
	public boolean visitTopLevelSymbolEnd(int offset);
	public boolean visitTopLevelOpen(int offset);
	public boolean visitParenthesisOpen(int offset);
	public boolean visitParenthesisClose(int offset);
	public boolean visitTopLevelClose(int offset);
	public boolean visitQuote(int offset);
	public boolean visitBackquote(int offset);
	public boolean visitComma(int offset);
	public boolean visitSymbolStart(int offset);
	public boolean visitSymbolEnd(int offset);
	public boolean parenthesisMalformation(int atOffset, int balance);
}
