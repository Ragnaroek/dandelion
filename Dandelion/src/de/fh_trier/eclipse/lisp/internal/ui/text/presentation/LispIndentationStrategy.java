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

import de.fh_trier.eclipse.lisp.internal.ui.text.SourceUtilities;
import de.fh_trier.eclipse.lisp.internal.ui.text.partition.LispPartitionConstants;

/**
 * Einrueckungsstrategy fuer Lisp-Quelltext.
 * @author Michael Bohn
 *
 */
public class LispIndentationStrategy 
extends LispAutoEditStrategy
{
	/**
	 * Rueckt den Text bei Zeilenumbruch enstprechend ein.
	 */
	@Override
	public void customizeDocumentCommand(final IDocument document,
			final DocumentCommand command) 
	{		
		if(command.length == 0 && command.text != null 
				&& TextUtilities.endsWith(document.getLegalLineDelimiters(), command.text) != -1) {
			try {
				indentAfterNewline(document, command);
			} catch (BadLocationException e) {
				//no-op
			} catch (BadPartitioningException e) {
				//no-op
			}
		}		
	}

	private void indentAfterNewline(final IDocument document, final DocumentCommand command) 
	throws BadLocationException, BadPartitioningException
	{
		if(command.offset < 0 || document.getLength() == 0) {
			return;
		}
		
		ITypedRegion partition = ((IDocumentExtension3)document).getPartition(LispPartitionConstants.PARTITION_ID, command.offset, false);
		if(partition.getType().equals(LispPartitionConstants.LISP_PARTITION_COMMENT)) {
			copyIndentation(document, command);
		} else {
			int indentChars = SourceUtilities.calculateIndentationWidth(document, command.offset);
	
			StringBuilder alteredText = new StringBuilder();
			alteredText.append(command.text);
			for(int i=0;i<indentChars;i++) {
				alteredText.append(' ');
			}
			
			command.text = alteredText.toString();
		}
	}
}
