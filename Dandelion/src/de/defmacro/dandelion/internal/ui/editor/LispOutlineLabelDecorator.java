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

package de.defmacro.dandelion.internal.ui.editor;

import java.util.HashMap;
import java.util.Map.Entry;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.ui.*;

/**
 * Der Dekorator fuer die Lisp-Outline.
 * @author Michael Bohn
 *
 */
public class LispOutlineLabelDecorator
extends LabelProvider
implements ILabelDecorator
{
	public static final String ID = "de.fh_trier.eclipse.lisp.decorators.outlineDecorator";
	
	private static class Key
	{
		private final String fOverlay;
		private final Image  fImage;
		
		public Key(final String overlay, final Image image) {
			fOverlay = overlay;
			fImage = image;
		}

		@Override
		public int hashCode() {
			final int PRIME = 31;
			int result = 1;
			result = PRIME * result + ((fImage == null) ? 0 : fImage.hashCode());
			result = PRIME * result + ((fOverlay == null) ? 0 : fOverlay.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			final Key other = (Key) obj;
			if (fImage == null) {
				if (other.fImage != null)
					return false;
			} else if (!fImage.equals(other.fImage))
				return false;
			if (fOverlay == null) {
				if (other.fOverlay != null)
					return false;
			} else if (!fOverlay.equals(other.fOverlay))
				return false;
			return true;
		}
		
		
	}
	
	private HashMap<Key, Image> fCache = new HashMap<Key, Image>();
	
	/**
	 * Dekoriert das ubergebene Bild fuer das uebergeben Objekt.
	 * Markiert das Bild mit einem Fehler oder einer Warnung wenn
	 * das Objekt ({@link SExpression}) eine Malformation enthaelt.
	 * @see ILabelDecorator#decorateImage(Image, Object)
	 */
	public Image decorateImage(final Image image, final Object element) 
	{
		if( !(element instanceof SExpression) ) return null;
		
		SExpression sexp = (SExpression)element;
		boolean toplevel = sexp.isToplevel();
		
		if(!sexp.hasMalformation(toplevel)) {
			return null;
		}
		
		if(sexp.hasMalformation(toplevel, TSeverity.ERROR) || sexp.hasMalformation(toplevel, TSeverity.STRUCTURE)) {
			return getImage(UIImageConstants.OVERLAY_ERROR, image);
		}
		//warning
		Image img = getImage(UIImageConstants.OVERLAY_WARNING, image);
		return img;
	}

	/**
	 * Ungenutzt.
	 */
	public String decorateText(final String text, final Object element) {
		return null;
	}
	
	private Image getImage(final String overlay, final Image image)
	{
		Key key = new Key(overlay, image);
		Image cached = fCache.get(key);
		if(cached == null) {
			cached = new OverlayImage(image, overlay).getImage();
			fCache.put(key, cached);
		}
		return cached;
	}

	private void disposeCache()
	{
		if(fCache != null) {
			for(Entry<Key, Image> entry : fCache.entrySet()) {
				if(!entry.getValue().isDisposed()) {
					entry.getValue().dispose();
				}
			}
			fCache = null;
		}
	}
	
	/**
	 * Entsorgt den Decorator.
	 * Darf danach nicht weiter verwendet werden.
	 */
	@Override
	public void dispose() {
		super.dispose();
		disposeCache();
	}
}
