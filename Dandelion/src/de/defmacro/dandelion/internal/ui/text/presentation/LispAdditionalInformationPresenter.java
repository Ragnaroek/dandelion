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

import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.DefaultInformationControl.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Display;

import de.defmacro.dandelion.internal.ui.LispUI;

/**
 * Stellt zusaetzliche Informationen fuer einen
 * Proposal dar.
 * @author Michael Bohn
 *
 */
public class LispAdditionalInformationPresenter 
implements IInformationPresenter, IInformationPresenterExtension 
{
	private Color fKeyColor; 
	
	public LispAdditionalInformationPresenter()
	{
		fKeyColor = LispUI.getUIColorManager().getColor(new RGB(122, 20, 255));
	}
	
	/**
	 * Unbenutzt. Wird nicht mehr aufgerufen da Extension implementiert.
	 */
	public String updatePresentation(Display display, String hoverInfo, TextPresentation presentation, int maxWidth, int maxHeight) {
		return null;
	}

	/**
	 * Aktualisiert die Darstellung der Informationsanzeige.
	 */
	public String updatePresentation(Drawable drawable, String hoverInfo,
			TextPresentation presentation, int maxWidth, int maxHeight) {
		int lastAmp = -1;
		while(true) {
			lastAmp = hoverInfo.indexOf('&', lastAmp+1);
			if(lastAmp < 0) {
				break;
			}
			
			int whitespace = 0;
			whitespace:
			for(int i=lastAmp+1;i<hoverInfo.length();i++) {
				if(Character.isWhitespace(hoverInfo.charAt(i))) {
					whitespace = i;
					break whitespace;
				}
			}
			
			presentation.addStyleRange(new StyleRange(lastAmp, whitespace-lastAmp, fKeyColor, null, SWT.BOLD));	
		}

		return hoverInfo;
	}
}
