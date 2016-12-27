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

import org.eclipse.jface.text.IDocument;


/**
 * Dieser Visitor testet ob eine Top-Level-Form im
 * Dokument vorhanden ist. Wird die erste Top-Level-Form
 * gefunden bricht die Suche ab. Dieser Visitor
 * kann wiederverwendet werden.
 * @author Michael Bohn
 * testcase = {@link TestSourceUtilities#testValidForm()}
 */
public class TopLevelFormPresentVisitor 
extends SexpressionVisitorAdapter 
{	
	private boolean fFormSeen = false;
	private boolean fMalformed = false;
	private boolean fSymbolQuoted = false;
	private int fLastTLStart = -1;
	
	@Override
	public void preVisit(final IDocument document) 
	{
		fFormSeen = false;
		fMalformed = false;
		fSymbolQuoted = false;
		fLastTLStart = -1;
	}

	@Override
	public boolean visitTopLevelClose(int offset) 
	{
		//quote alleine reicht nicht aus fuer komplette Form
		fFormSeen = !fSymbolQuoted || (offset-fLastTLStart) >= 1;
		return true; //Erste Top-Level-Form besucht, Suche abbrechen.
	}
	
	@Override
	public boolean visitTopLevelOpen(int offset) 
	{
		fLastTLStart = offset;
		return false;
	}

	@Override
	public boolean visitTopLevelSymbolEnd(int offset) 
	{
		fFormSeen = true;
		return true; //Top-Level-Symbol gefunden, ebenfalls Sucbe abbrechen.
	}

	@Override
	public boolean visitQuote(int offset)
	{
		fSymbolQuoted = true;
		return false;
	}

	@Override
	public boolean parenthesisMalformation(int atOffset, int balance) {
		fMalformed = true;
		return true; //abbrechen
	}

	/**
	 * Gibt zurueck ob beim Visit-Vorgang ueberhaupt eine Form
	 * gesehen wurde.
	 * @return
	 */
	public boolean isFormSeen()
	{
		return fFormSeen;
	}
	
	/**
	 * Liefert <code>true</code> wenn Fehler im Quelltext.
	 * @return
	 */
	public boolean isMalformed()
	{
		return fMalformed;
	}
}
