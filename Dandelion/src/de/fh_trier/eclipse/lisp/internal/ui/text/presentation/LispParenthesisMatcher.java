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

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPartitioningException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.ui.text.*;

/**
 * Sucht die passende Klammer.
 * @author Michael Bohn
 *
 */
/*package*/ class LispParenthesisMatcher 
implements ICharacterPairMatcher
{
	private int anchor = 0;
	
	/**
	 * @see ICharacterPairMatcher#clear()
	 */
	public void clear() 
	{
		this.anchor = 0;
	}

	/**
	 * Unbenutzt.
	 */
	public void dispose() {
		//nichts freizugeben
	}

	/**
	 * @see ICharacterPairMatcher#getAnchor()
	 */
	public int getAnchor() {
		return anchor;
	}

	/**
	 * Passende Position fuer den angegeben offset suchen.
	 * @see ICharacterPairMatcher#match(IDocument, int)
	 */
	public IRegion match(IDocument doc, int i) 
	{	
		char beforeCaret = 0; //char vor Textmarker
		char atCaret = 0;     //char an Textmarker Position
		try {
			if(i-1 >= 0) {
				beforeCaret = doc.getChar(i-1);
			}
			if(i < doc.getLength()) {
				atCaret = doc.getChar(i);
			}

			Boolean direction = SourceUtilities.resolveSearchDirection(beforeCaret, atCaret);
			if(direction == null) return null; //unentscheidbar
			
			if(direction == true) {
				anchor = ICharacterPairMatcher.LEFT;
			} else {
				anchor = ICharacterPairMatcher.RIGHT;
			}
			
			int offset = SourceUtilities.translateOffset(beforeCaret, atCaret, i);
			return SourceUtilities.findMatchingParenthesis(doc, offset, direction);

		} catch (BadLocationException e) {
			LispPluginActivator.logError("broken invariant: Character not available in Document", e);
			return null; //kein Match 
		} catch (BadPartitioningException e) {
			LispPluginActivator.logError("Error while retrieving Partitiontype", e);
			return null; //kein Match
		}
	}
}
