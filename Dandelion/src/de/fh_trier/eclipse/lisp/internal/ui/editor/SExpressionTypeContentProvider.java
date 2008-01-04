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

import org.eclipse.jface.viewers.*;

import static de.fh_trier.eclipse.lisp.internal.core.dom.TSExpression.*;

/**
 * Der ContentProvider fuer die Typen
 * im Typauswahldialog der Outline.
 * @author Michael Bohn
 *
 */
public class SExpressionTypeContentProvider 
implements IStructuredContentProvider 
{
	private static SExpressionTypeContentProvider fInstance;
	
	private Object[] fElements;
	
	private SExpressionTypeContentProvider()
	{
		/* instanz nur ueber Factory */
	}
	
	private Object[] getElements()
	{
		if( fElements == null ) {
			fElements = new Object[8];
			fElements[0] = DEFMACRO;
			fElements[1] = DEFPACKAGE;
			fElements[2] = DEFUN;
			fElements[3] = FORM;
			fElements[4] = INPACKAGE;
			fElements[5] = LAMBDA;
			fElements[6] = SEXPRESSION;
			fElements[7] = SYMBOL;
		}
		return fElements;
	}
	
	/**
	 * Liefert die Singleton-Instanz des ContentProviders.
	 * @return Singleton-Instanz des Providers
	 */
	public static SExpressionTypeContentProvider instanceOf() 
	{
		if (fInstance == null) {
			fInstance = new SExpressionTypeContentProvider();
		}
		return fInstance;
	}
	
	/**
	 * @see IStructuredContentProvider#getElements(Object)
	 */
	public Object[] getElements(Object inputElement) 
	{
		return getElements();
	}

	/**
	 * Unbenutzt.
	 */
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		//no-op
	}
	
	/**
	 * Unbenutzt.
	 */
	public void dispose() {
		//no-op
	}
}
