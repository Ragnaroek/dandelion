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

package de.fh_trier.eclipse.lisp.internal.ui.editor;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import de.fh_trier.eclipse.lisp.internal.ui.*;

/**
 * Der {@link LabelProvider} fuer die Typen
 * im Auswahldialog der Outline.
 * @author Michael Bohn
 *
 */
public class SExpressionTypeLabelProvider 
extends LabelProvider 
{
	private static SExpressionTypeLabelProvider fInstance;

	private SExpressionTypeLabelProvider()
	{
		/* Instanz nur ueber Factory */
	}
	
	/**
	 * Liefert die Singleton-Instanz des Providers.
	 * @return
	 */
	public static SExpressionTypeLabelProvider instanceOf() {
		if (fInstance == null) {
			fInstance = new SExpressionTypeLabelProvider();
		}
		return fInstance;
	}
	
	/**
	 * @see LabelProvider#getImage(Object)
	 */
	@Override
	public Image getImage(Object element) 
	{
		TSExpression typ = (TSExpression)element;
		switch(typ) {
		case DEFMACRO    : return LispUI.getUIImageManager().get(UIImageConstants.ICON_DEFMACRO);
		case DEFPACKAGE  : return LispUI.getUIImageManager().get(UIImageConstants.ICON_DEFPACKAGE);
		case DEFUN       : return LispUI.getUIImageManager().get(UIImageConstants.ICON_DEFUN);
		case FORM        : return LispUI.getUIImageManager().get(UIImageConstants.ICON_FORM);
		case INPACKAGE   : return LispUI.getUIImageManager().get(UIImageConstants.ICON_IN_PACKAGE);
		case LAMBDA      : return LispUI.getUIImageManager().get(UIImageConstants.ICON_LAMBDA_FORM);
		case SEXPRESSION : return LispUI.getUIImageManager().get(UIImageConstants.ICON_SEXPRESSION);
		case SYMBOL      : return LispUI.getUIImageManager().get(UIImageConstants.ICON_SYMBOL_PRIVATE);
		default : throw new IllegalArgumentException("Unknown typ");
		}
	}
	
	/**
	 * @see LabelProvider#getText(Object)
	 */
	@Override
	public String getText(Object element) 
	{
		TSExpression typ = (TSExpression)element;
		switch(typ) {
		case DEFMACRO    : return Symbol.SYM_DEFMACRO;
		case DEFPACKAGE  : return Symbol.SYM_DEFPACKAGE;
		case DEFUN       : return Symbol.SYM_DEFUN;
		case INPACKAGE   : return Symbol.SYM_IN_PACKAGE;
		case LAMBDA      : return Symbol.SYM_LAMBDA;
		case FORM	     : return "form";
		case SEXPRESSION : return "s-expression";
		case SYMBOL      : return "symbol";
		default : throw new IllegalArgumentException(typ + " has no UI String");
		}
	}
}
