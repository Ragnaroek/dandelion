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

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.ui.LispUI;

/**
 * Der {@link LabelProvider} fuer die Outline.
 * @author Michael Bohn
 *
 */
public class LispOutlineLabelProvider 
extends LabelProvider
{	
	private LispOutlineTextCase fTextCase;
	
	/**
	 * Erzeugt einen neuen {@link LabelProvider} fuer die Outline.
	 */
	public LispOutlineLabelProvider()
	{
		this.fTextCase = new LispOutlineTextCase();
	}
	
	/**
	 * Liefert das passende Bild fuer das {@link SExpression}-Objekt.
	 * @see LabelProvider#getImage(Object)
	 */
	@Override
	public Image getImage(final Object element) 
	{
		SExpression sexp = (SExpression)element;
		return sexp.typeSwitch(LispUI.getUIImageManager().getSExpressionImageCase());
	}

	/**
	 * Liefert den Text fuer das {@link SExpression}-Objekt.
	 * @see LabelProvider#getText(Object)
	 */
	@Override
	public String getText(final Object element) 
	{		
		SExpression sexp = (SExpression)element;
		return sexp.typeSwitch(fTextCase);
	}
}
