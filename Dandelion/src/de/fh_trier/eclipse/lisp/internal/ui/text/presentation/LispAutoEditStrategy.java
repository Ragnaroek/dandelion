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

package de.fh_trier.eclipse.lisp.internal.ui.text.presentation;

import org.eclipse.jface.text.*;

/**
 * Abstrakte Auto-Edit Strategy fuer Lisp.
 * Sichtbarkeit der {@link LispAutoEditStrategy#copyIndentation(IDocument, DocumentCommand)}-Methode erhoeht.
 * @author Michael Bohn
 *
 */
public abstract class LispAutoEditStrategy
extends DefaultIndentLineAutoEditStrategy
{
	protected void copyIndentation(final IDocument d, final DocumentCommand c)
	{
		// CODE KOPIERT AUS SUPER-KLASSE DA DORT PRIVATE
		// == DefaultIndentLineAutoEditStrategy.autoIndentAfterNewLine(IDocument d, DocumentCommand c)
		
		if (c.offset == -1 || d.getLength() == 0)
			return;

		try {
			// find start of line
			int p= (c.offset == d.getLength() ? c.offset  - 1 : c.offset);
			IRegion info= d.getLineInformationOfOffset(p);
			int start= info.getOffset();

			// find white spaces
			int end= findEndOfWhiteSpace(d, start, c.offset);

			StringBuffer buf= new StringBuffer(c.text);
			if (end > start) {
				// append to input
				buf.append(d.get(start, end - start));
			}

			c.text= buf.toString();

		} catch (BadLocationException excp) {
			// stop work
		}
	}
}
