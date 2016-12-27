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

package de.defmacro.dandelion.internal.ui;

import org.eclipse.swt.graphics.Image;

import de.defmacro.dandelion.internal.core.dom.*;

/**
 * Die Falluntercheidung fuer die Darstellung
 * der Bilder des jeweiligen Typs.
 * @author Michael Bohn
 *
 */
public class SExpressionImageCase 
implements Case<Image> 
{
	private final UIImageManager fImageManager;
	
	/*protected*/ SExpressionImageCase(final UIImageManager imageManager)
	{
		this.fImageManager = imageManager;
	}
	
	public Image typeCase(final InpackageForm form) 
	{
		return fImageManager.get(UIImageConstants.ICON_IN_PACKAGE);
	}

	public Image typeCase(final DefpackageForm form) 
	{
		return fImageManager.get(UIImageConstants.ICON_DEFPACKAGE);
	}

	public Image typeCase(final LambdaForm form) 
	{
		return fImageManager.get(UIImageConstants.ICON_LAMBDA_FORM);
	}

	public Image typeCase(final DefmacroForm form) 
	{
		return fImageManager.get(UIImageConstants.ICON_DEFMACRO);
	}

	public Image typeCase(final DefunForm form) 
	{
		return fImageManager.get(UIImageConstants.ICON_DEFUN);
	}

	public Image typeCase(final Symbol symbol) 
	{
		if(symbol.getTyp() == TSExpression.READER_SYMBOL) {
			return fImageManager.get(UIImageConstants.ICON_SYMBOL_READER);
		} else if(symbol.isPrivate()) {
			return fImageManager.get(UIImageConstants.ICON_SYMBOL_PRIVATE);
		} else if(!symbol.isInterned()) {
			return fImageManager.get(UIImageConstants.ICON_SYMBOL_UNINTERNED);
		}
		//public
		return fImageManager.get(UIImageConstants.ICON_SYMBOL_PUBLIC);
	}

	public Image typeCase(final Form form) 
	{
		return fImageManager.get(UIImageConstants.ICON_FORM);
	}

	public Image typeCase(final SExpression sexp) 
	{
		return fImageManager.get(UIImageConstants.ICON_SEXPRESSION);
	}
	
	public Image typeCase(final DefiningForm form) {
		return null; //kein Bild, kein Fallthrough
	}

	public Image typeCase(final FunctionDefiningForm form) {
		return null; //kein Bild, kein Fallthrough
	}

	/**
	 * Kein Fallthrough.
	 */
	public boolean isFallthrough() {
		return false;
	}
}
