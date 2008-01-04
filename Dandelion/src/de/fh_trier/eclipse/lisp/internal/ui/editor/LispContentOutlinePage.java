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

import java.util.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.views.contentoutline.*;
import de.fh_trier.eclipse.lisp.internal.*;
import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import de.fh_trier.eclipse.lisp.internal.preferences.LispPreferences;

/**
 * Die Outline-Page fuer den Lisp-Editor.
 * @author Michael Bohn
 *
 */
public class LispContentOutlinePage 
extends ContentOutlinePage 
implements IPropertyChangeListener 
{	
	private ISexpModel fCurrentModel;
	private LispOutlineFilter fOutlineFilter;
	private LispOutlineSorter fOutlineSorter;
	private ILabelDecorator fDecorator;
	
	/**
	 * Erzeugt eine neue Outline-Seite.
	 */
	public LispContentOutlinePage()
	{
		fOutlineFilter = null; //wegen findbugs warning
		fOutlineSorter = null; //wegen findbugs warning
		LispPluginActivator.getDefault().getPreferenceStore().addPropertyChangeListener(this);
	}
	
	/**
	 * @see ContentOutlinePage#createControl(Composite)
	 */
	@Override
	public void createControl(final Composite parent) 
	{
		super.createControl(parent);
		
		IPreferenceStore store = LispPluginActivator.getDefault().getPreferenceStore();
		
		Set<TSExpression> showOnToplevel = getTypSet(LispPreferences.OUTLINE_SHOW_TYPES_ON_TOPLEVEL);
		Set<TSExpression> showOnSublevel = getTypSet(LispPreferences.OUTLINE_SHOW_TYPES_ON_SUBLEVEL);
		boolean hideSublevel = store.getBoolean(LispPreferences.OUTLINE_HIDE_SUBLEVEL_FORMS);
		
		fOutlineFilter = new LispOutlineFilter(showOnToplevel, showOnSublevel, hideSublevel);
		LispOutlineContentProvider contentProvider = new LispOutlineContentProvider(fOutlineFilter);
		boolean sortByName = store.getBoolean(LispPreferences.OUTLINE_SORT_BY_NAME);
		boolean sortByType = store.getBoolean(LispPreferences.OUTLINE_SORT_BY_TYPE);
		fOutlineSorter = new LispOutlineSorter(sortByName, sortByType);
		
		getTreeViewer().addFilter(fOutlineFilter);
		getTreeViewer().setContentProvider(contentProvider);
		if(fCurrentModel != null) {
			getTreeViewer().setInput(fCurrentModel);
		}
		
		fDecorator = new LispOutlineLabelDecorator();
		DecoratingLabelProvider labelProvider = new DecoratingLabelProvider(new LispOutlineLabelProvider(), fDecorator);
		getTreeViewer().setLabelProvider(labelProvider);
		getTreeViewer().setSorter(fOutlineSorter);
	}
	
	/**
	 * Setzt ein neues Modell fuer die Outline.
	 * @param model - neues Modell
	 */
	public void setInput(final ISexpModel model)
	{
		this.fCurrentModel = model;
		
		if(this.getControl() == null || this.getControl().isDisposed()) {
			return;
		}
		
		TreeViewer viewer = getTreeViewer();
		if( viewer != null ) {
			viewer.setInput(fCurrentModel);
		}
	}
	
	private Set<TSExpression> getTypSet(final String property)
	{
		String encoded = LispPluginActivator.getDefault().getPreferenceStore().getString(property);
		return LispPreferences.decodeOutlineVisibleTypes(encoded);
	}

	/**
	 * Listener-Implementierung. Nicht aufrufen.
	 */
	public void propertyChange(final PropertyChangeEvent event) 
	{
		if( !LispPreferences.affectsOutline(event.getProperty()) 
			|| event.getNewValue() == null) {
			return;
		}
		if(fOutlineFilter == null || fOutlineSorter == null) {
			return;
		}
		
		if( event.getProperty().equals(LispPreferences.OUTLINE_SHOW_TYPES_ON_TOPLEVEL) ) {
			fOutlineFilter.setVisibleTypesToplevel(LispPreferences.decodeOutlineVisibleTypes((String)event.getNewValue()));
		} else if ( event.getProperty().equals(LispPreferences.OUTLINE_SHOW_TYPES_ON_SUBLEVEL) ) {
			fOutlineFilter.setVisibleTypesSublevel(LispPreferences.decodeOutlineVisibleTypes((String)event.getNewValue()));
		} else if ( event.getProperty().equals(LispPreferences.OUTLINE_HIDE_SUBLEVEL_FORMS) ) {
			fOutlineFilter.setHideSublevel((Boolean)event.getNewValue());
		} else if ( event.getProperty().equals(LispPreferences.OUTLINE_SORT_BY_NAME) ) {
			fOutlineSorter.setSortByName((Boolean)event.getNewValue());
		} else if ( event.getProperty().equals(LispPreferences.OUTLINE_SORT_BY_TYPE) ) {
			fOutlineSorter.setSortByType((Boolean)event.getNewValue());
		} else {
			LispPluginActivator.log(IStatus.WARNING, "unknown property changed value, outline can't handle this property: " + event.getProperty(), null);
		}
		
		if(getTreeViewer() != null) {
			getTreeViewer().refresh(false);
		}
	}

	/**
	 * Entsort die Outline.
	 * Die Outline darf danach nicht weiter verwendet werden.
	 */
	@Override
	public void dispose() 
	{
		super.dispose();
		
		LispPluginActivator.getDefault().getPreferenceStore().removePropertyChangeListener(this);
		
		if(fDecorator != null) {
			fDecorator.dispose();
		}
	}
}