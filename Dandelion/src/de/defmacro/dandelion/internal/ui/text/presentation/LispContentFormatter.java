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

package de.defmacro.dandelion.internal.ui.text.presentation;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.formatter.*;
import org.eclipse.jface.text.source.ISourceViewer;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.ui.text.SourceUtilities;

/**
 * Formatiert einen selektierten Bereich
 * von Lisp-Quelltext.
 * @author Michael Bohn
 *
 */
public class LispContentFormatter 
implements IContentFormatter 
{
	private ISourceViewer fSourceViewer;
	
	/**
	 * Erzeugt einen neuen Formatierer fuer den uebergebenen Viewer.
	 * @param viewer - Viewer fuer den Formatierungen vorgenommen werden sollem
	 */
	public LispContentFormatter(final ISourceViewer viewer)
	{
		if (viewer == null) {
			throw new NullPointerException("viewer must not be null");
		}

		this.fSourceViewer = viewer;
	}
	
	/**
	 * Formatiert den angegebenen Bereich im Dokument.
	 */
	public void format(final IDocument document, final IRegion region) 
	{
		//Keine Selektion -> region = komplette datei
		TextSelection selection = (TextSelection)fSourceViewer.getSelectionProvider().getSelection();
		if(selection.getLength() == 0) {
			return;
		}
		
		try {
			int lines = document.getNumberOfLines(region.getOffset(), region.getLength());
			int startLine = document.getLineOfOffset(region.getOffset());
			int lastLine = startLine+lines-1;
			
			for(int line=startLine;line<=lastLine;line++) {
				IRegion lineInfo = document.getLineInformation(line);
				int lineStart = document.getLineOffset(line);
				int lineLen   = document.getLineLength(line);
				int indentWidth = SourceUtilities.calculateIndentationWidth(document, lineInfo.getOffset());
				
				int lineDelimiterLen = getLineDelimiterLength(document, line);
				String lineText = document.get(lineStart, lineLen-lineDelimiterLen);
				
				lineText = SourceUtilities.appendIndentation(indentWidth, lineText.trim());
				document.replace(lineStart, lineLen-lineDelimiterLen, lineText);
			}
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("Formatting failed", e);
		}
	}
	
	private int getLineDelimiterLength(final IDocument document, final int line)
	throws BadLocationException
	{
		String lineDelimiter = document.getLineDelimiter(line);
		return lineDelimiter == null ? 0 : lineDelimiter.length();
	}
	
	/**
	 * Unbenutzt.
	 */
	public IFormattingStrategy getFormattingStrategy(final String contentType) {
		return null;
	}
}
