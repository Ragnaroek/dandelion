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

import de.fh_trier.eclipse.lisp.internal.core.dom.SExpression;

/**
 * Der Sorter fuer den Inhalt der Outline.
 * @author Michael Bohn
 *
 */
public class LispOutlineSorter 
extends ViewerSorter 
{	
	private boolean fSortByName;
	private boolean fSortByType;
	
	/**
	 * Erzeugt einen neuen Sorter fuer die Outline.
	 * @param sortByName - initialer Zustand 
	 * @param sortByType - inttialer Zustand
	 */
	public LispOutlineSorter(final boolean sortByName, final boolean sortByType)
	{
		this.fSortByName = sortByName;
		this.fSortByType = sortByType;
	}
	
	/**
	 * Liefert den Typ der {@link SExpression}.
	 * Als int-wert kodiert (ordinal).
	 * @see ViewerSorter#category(Object)
	 */
	@Override
	public int category(Object element) 
	{
		if( fSortByType ) {
			SExpression sexp = (SExpression)element;
			return sexp.getTyp().ordinal();
		}
		return 0;
	}

	/**
	 * Sortiert die Elemente.
	 * @see ViewerSorter#sort(Viewer, Object[])
	 */
	@Override
	public void sort(Viewer viewer, Object[] elements) 
	{
		if( fSortByName || fSortByType ) {
			super.sort(viewer, elements);
		}
		return;
	}
	
	/**
	 * Setzt den neuen Zustand fuer die Sortierung nach Namen.
	 * @param sortByName - neuer Zustand
	 */
	public void setSortByName(final boolean sortByName) 
	{
		this.fSortByName = sortByName;
	}
	
	/**
	 * Setzt den neuen Zustand fuer die Sortierung nach Typ.
	 * @param sortByType - neuer Zustand
	 */
	public void setSortByType(final boolean sortByType)
	{
		this.fSortByType = sortByType;
	}
}
