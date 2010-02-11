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

import java.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;

import de.defmacro.dandelion.internal.*;

/**
 * Veraltet.
 * @author Michael Bohn
 *
 */
@Deprecated
public class LispAnnotationHover 
implements IAnnotationHover
{
	public String getHoverInfo(ISourceViewer sourceViewer, int lineNumber) 
	{
		IAnnotationModel model = sourceViewer.getAnnotationModel();
		if(model == null) return null;
		
		Annotation annotation = null;
		try {
			annotation = findAnnotation(lineNumber, sourceViewer);
		} catch (BadLocationException e) {
			LispPluginActivator.logError("broken invariant: Finding Annotation for line failed", e);
			//und weiter mit annotation = null
		}
		
		if( annotation != null ) {
			return annotation.getText();
		}

		return null;
	}
	
	@SuppressWarnings("unchecked") //model.getAnnotationIterator() - Eclipse API nicht generic
	private Annotation findAnnotation(int lineNumber, ISourceViewer viewer) 
	throws BadLocationException
	{
		IAnnotationModel model = viewer.getAnnotationModel();
		IDocument document = viewer.getDocument();
		
		Iterator<Annotation> iter = model.getAnnotationIterator();
		while(iter.hasNext()) {
			Annotation annotation = iter.next();
			Position position = model.getPosition(annotation);
			int offset = position.getOffset();
			if( lineNumber == document.getLineOfOffset(offset) ) {
				return annotation;
			}
		}
		
		return null; //keine Annotation gefunden
	}
}
