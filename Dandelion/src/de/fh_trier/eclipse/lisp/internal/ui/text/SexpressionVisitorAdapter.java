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

package de.fh_trier.eclipse.lisp.internal.ui.text;

import org.eclipse.jface.text.IDocument;

/**
 * Adapter-Implementierung des {@link ISexpressionVisitor} Interfaces.
 * 
 * @author Michael Bohn
 */
public class SexpressionVisitorAdapter 
implements ISexpressionVisitor 
{
	public void preVisit(IDocument document) {
		/* tue nichts */
	}
	
	public boolean visitParenthesisClose(int offset) {
		return false;
	}

	public boolean visitParenthesisOpen(int offset) {
		return false;
	}

	public boolean visitTopLevelClose(int offset) {
		return false;
	}

	public boolean visitTopLevelOpen(int offset) {
		return false;
	}

	public boolean visitTopLevelSymbolEnd(int offset) {
		return false;
	}

	public boolean visitTopLevelSymbolStart(int offset) {
		return false;
	}

	public boolean visitQuote(int offset) {
		return false;
	}
	
	public boolean visitBackquote(int offset) {
		return false;
	}

	public boolean visitComma(int offset) {
		return false;
	}
	
	public boolean visitSymbolEnd(int offset) {
		return false;
	}

	public boolean visitSymbolStart(int offset) {
		return false;
	}

	public boolean parenthesisMalformation(int atOffset, int balance) {
		return false;
	}
}
