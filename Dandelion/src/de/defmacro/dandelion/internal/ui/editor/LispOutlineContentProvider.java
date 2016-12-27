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
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Der OutlineProvider fuer die Outline.
 * @author Michael Bohn
 *
 */
public class LispOutlineContentProvider 
implements ITreeContentProvider 
{
	private static final Object[] NO_ELEMENTS = new Object[0];
	
	private ISexpModel fInputModel;
	
	@Nonnull
	private LispOutlineFilter fFilter;
	
	/**
	 * Erzeugt einen neuen ContentProvider fuer die Outline.
	 * @param filter - Filter der Outline
	 */
	public LispOutlineContentProvider(final LispOutlineFilter filter)
	{
		if (filter == null) {
			throw new NullPointerException("filter must not be null");
		}
		
		this.fFilter = filter;
	}
	
	/**
	 * @see ITreeContentProvider#getChildren(Object)
	 */
	public Object[] getChildren(final Object parentElement) 
	{
		SExpression node = (SExpression)parentElement;
		List<SExpression> computedChilds = new ArrayList<SExpression>();
		findChilds(node, computedChilds);
		return computedChilds.toArray();
	}
	
	private void findChilds(final SExpression node, final List<SExpression> resultList)
	{
		if( !node.hasChildren() ) {
			return;
		}
			
		for(SExpression child : node.getChildren()) {
			if( fFilter.select(child) ) {
				resultList.add(child);
			} else {
				findChilds(child, resultList);
			}
		}
	}
	
	/**
	 * Unbenutzt.
	 */
	public Object getParent(final Object element) 
	{
		return null;
	}

	/**
	 * @see ITreeContentProvider#hasChildren(Object)
	 */
	public boolean hasChildren(final Object element) 
	{
		SExpression node = (SExpression)element;
		if(node == null) {
			return false;
		}
		
		//invariante: node != null
		return internalHasChildren(node);
	}
	
	/**
	 * Gibt <code>true</code> zurueck sobald der Knoten
	 * mindestens einen anzuzeigenden Kindknoten hat.
	 * @param node - SexpNode die auf Kindknoten untersucht werden soll
	 * @return
	 */
	private boolean internalHasChildren(final SExpression node)
	{	
		if( !node.hasChildren() ) {
			return false; //kein Knoten hat es durch den Filter geschafft
		}
			
		for(SExpression child : node.getChildren()) {
			//Der Kindknoten ist ein anzuzeigender Knoten
			if( fFilter.select(child) ) {
				return true; //gefunden
			}
			
			//evtl. Kindknoten anzuzeigender Knote?
			boolean found = internalHasChildren(child);
			if ( found ) {
				return found;
			}
		}
		
		return false;
	}

	/**
	 * @see IStructuredContentProvider#getElements(Object)
	 */
	@SuppressWarnings("EI") //NO_ELEMENTS ist immutable
	public Object[] getElements(final Object inputElement) 
	{
		if(fInputModel == null) {
			return NO_ELEMENTS;
		}
		
		List<SExpression> result = new ArrayList<SExpression>();
		for(SExpression sexp : fInputModel.getTopLevelForms()) {
			if(fFilter.select(sexp)) {
				result.add(sexp);
			}
		}
		
		return result.toArray();
	}

	/**
	 * Unbenutzt.
	 */
	public void dispose() 
	{
		//no-op
	}

	/**
	 * @see IContentProvider#inputChanged(Viewer, Object, Object)
	 */
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) 
	{
		ISexpModel newModel = (ISexpModel)newInput;
		
		if(this.fInputModel == null) { //bisher noch kein Model vorhanden
			this.fInputModel = newModel; //wir nehmen was wir kriegen koennen, selbst wenn fehlerhaft
			return;
		}
		
		//invariante: es wurde schonmal ein Model gesetzt, und ein neues wird mitgeteilt
		//wir warten auf (struktur)fehlerfreies Model
		if( newModel != null && !newModel.hasMalformation(TSeverity.STRUCTURE) ) {
			this.fInputModel = newModel;
		}
	}
}
