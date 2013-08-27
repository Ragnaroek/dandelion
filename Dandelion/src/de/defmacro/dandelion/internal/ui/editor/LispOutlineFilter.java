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

import java.util.*;

import javax.annotation.Nonnull;

import org.eclipse.jface.viewers.*;

import de.defmacro.dandelion.internal.core.dom.*;

/**
 * Der Filter fuer den Inhalt der Outline.
 * @author Michael Bohn
 *
 */
public class LispOutlineFilter 
extends ViewerFilter
{
	@Nonnull
	private Set<TSExpression> fShowTypesToplevel;
	@Nonnull
	private Set<TSExpression> fShowTypesSublevel;
	
	private boolean fHideSublevel;
	
	/**
	 * Erzeugt einen neuen Filter fuer die Outline.
	 * @param visibleTypesToplevel - intialer Zustand der Outline
	 * @param visibleTypesSublevel - intialer Zustand der Outline
	 * @param hideSublevel - intialer Zustand der Outline
	 */
	public LispOutlineFilter(final Set<TSExpression> visibleTypesToplevel,
				             final Set<TSExpression> visibleTypesSublevel,
				             final boolean hideSublevel)
	{
		if (visibleTypesToplevel == null) {
			throw new NullPointerException("visibleTypes must not be null");
		}
		
		if (visibleTypesSublevel == null) {
			throw new NullPointerException("visibleTypesSublevel must not be null");
		}
		
		this.fShowTypesToplevel = visibleTypesToplevel;
		this.fShowTypesSublevel = visibleTypesSublevel;
		this.fHideSublevel = hideSublevel;
	}
	
	/**
	 * @see ViewerFilter#select(Viewer, Object, Object)
	 */
	@Override
	public boolean select(final Viewer viewer, final Object parentElement, final Object element) 
	{
		SExpression sexpElement = (SExpression)element;
		return select(sexpElement);
	}
	
	/**
	 * Liefert <code>true</code> wenn der Typ der {@link SExpression} in der
	 * Outline angezeigt werden soll.
	 * @param sexp - Der Ausdruck.
	 * @return <code>true</code> wenn typ angezeigt werden soll, sonst <code>false</code>
	 */
	public boolean select(final SExpression sexp)
	{	
		if( fHideSublevel ) {
			return sexp.isToplevel() &&  selectTyp(fShowTypesToplevel, sexp);
		}
		
		//invariante: sublevel soll auch angezeigt werden
		
		if( sexp.isToplevel() ) {
			return selectTyp(fShowTypesToplevel, sexp);
		}
		
		return selectTyp(fShowTypesSublevel, sexp);
	}
	
	/**
	 * Setzt die neuen Typen die auf dem Toplevel angezeigt werden sollen.
	 * @param visibleTypes - neue Typen
	 */
	public void setVisibleTypesToplevel(final Set<TSExpression> visibleTypes)
	{
		if (visibleTypes == null) {
			throw new NullPointerException("visibleTypes must not be null");
		}

		this.fShowTypesToplevel = visibleTypes;
	}
	
	/**
	 * Setzt die neuen Typen die auf dem Sublevel angezeigt werden sollen.
	 * @param visibleTypes - neue Typen
	 */
	public void setVisibleTypesSublevel(final Set<TSExpression> visibleTypes)
	{
		if (visibleTypes == null) {
			throw new NullPointerException("visibleTypes must not be null");
		}
		
		this.fShowTypesSublevel = visibleTypes;
	}
	
	/**
	 * Setzt den Zustand ob der Sublevel versteckt werden soll.
	 * @param hideSublevel - <code>true</code> wenn Sublevel nicht angezeigt werden soll
	 */
	public void setHideSublevel(final boolean hideSublevel)
	{
		this.fHideSublevel = hideSublevel;
	}
	
	private boolean selectTyp(final Set<TSExpression> selection, final SExpression sexp) 
	{
		if(sexp instanceof Symbol) { //damit auch sub-typen angezeigt werden fuer Symbol
			return selection.contains(TSExpression.SYMBOL);
 		}
 		return selection.contains(sexp.getTyp());
	}
}
