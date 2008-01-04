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
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.ui.text.SourceUtilities;

/**
 * Verhalten bei druecken von Tab im Editor.
 * @author Michael Bohn
 *
 */
public class LispTabStrategy 
extends LispAutoEditStrategy
{
	/**
	 * Passt den Editorinhalt bei druecken der Tabulator-Taste an.
	 */
	@Override
	public void customizeDocumentCommand(final IDocument document, final DocumentCommand command) 
	{
		if(command.length == 0 && command.text != null 
				&& command.text.endsWith("\t")) {
			try {
				IRegion line = document.getLineInformationOfOffset(command.offset);
				String lineRest = document.get(command.offset, line.getLength()-(command.offset - line.getOffset())).trim(); //text hinter offset
				String lineText = document.get(line.getOffset(), line.getLength()).trim(); //komplette zeile
				
				//wenn hinter offset nur leerzeichen und zeile nicht komplett leer (bei komplett leerere zeile tab bis zu passender Klammer
				if(lineRest.length() == 0 && lineText.length() != 0) { //normaler tab
					return;
				}
				int indentWidth = SourceUtilities.calculateIndentationWidth(document, line.getOffset());
				command.text = SourceUtilities.appendIndentation(indentWidth, lineText);
				command.offset = line.getOffset();
				command.length = line.getLength();
				command.shiftsCaret = false;
			} catch (BadLocationException e) {
				LispPluginActivator.logBrokenInvariant("auto indent failed", e);
			}
		}
	}
}
