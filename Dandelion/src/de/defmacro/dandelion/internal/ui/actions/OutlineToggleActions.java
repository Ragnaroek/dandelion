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

package de.defmacro.dandelion.internal.ui.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.*;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.preferences.LispPreferences;

/**
 * Toggle-Aktion fuer Outline.
 * @author Michael Bohn
 *
 */
public class OutlineToggleActions 
implements IViewActionDelegate, IActionDelegate2
{
	public static final String OUTLINE_HIDE_SUBLEVEL_ID = "de.fh_trier.eclipse.lisp.views.outline.hideSubLevel";
	public static final String OUTLINE_SORT_BY_NAME_ID = "de.fh_trier.eclipse.lisp.views.outline.sortAlphab";
	public static final String OUTLINE_SORT_BY_TYPE_ID = "de.fh_trier.eclipse.lisp.views.outline.sortType";
	
	/**
	 * Initialisiert die Aktion.
	 */
	public void init(IAction action) 
	{
		boolean checked = LispPluginActivator.getDefault().getPreferenceStore().getBoolean(
				getPreferenceForAction(action.getId()));
		action.setChecked(checked);
	}

	/**
	 * Setzt den neuen Wert in den Preferences.
	 * Die Outline reagiert entsprechend.
	 */
	public void runWithEvent(IAction action, Event event) 
	{
		LispPluginActivator.getDefault().getPreferenceStore().setValue(
				getPreferenceForAction(action.getId()), 
				action.isChecked());
		//Outline-View als Listener an PreferenceStore angemeldet
		//-> bemerkt Aenderung und konfiguriert sich selbst
	}
	
	private String getPreferenceForAction(String actionID)
	{
		if ( actionID.equals(OUTLINE_HIDE_SUBLEVEL_ID) ) {
			return LispPreferences.OUTLINE_HIDE_SUBLEVEL_FORMS;
		} else if( actionID.equals(OUTLINE_SORT_BY_NAME_ID) ) {
			return LispPreferences.OUTLINE_SORT_BY_NAME;
		} else if( actionID.equals(OUTLINE_SORT_BY_TYPE_ID) ) {
			return LispPreferences.OUTLINE_SORT_BY_TYPE;
		} else {
			LispPluginActivator.log(IStatus.WARNING, "ActionDelegate not defined for this Action", null);
			return null;
		}
	}
	
	/**
	 * Ungenutzt.
	 */
	public void init(IViewPart view) 
	{
		//no-op
	}

	/**
	 * Ungenutzt.
	 */
	public void run(IAction action) 
	{
		//no-op -> runWithEvent wird aufgerufen
	}

	/**
	 * Ungenutzt.
	 */
	public void selectionChanged(IAction action, ISelection selection) 
	{
		//no-op
	}

	/**
	 * Ungenutzt.
	 */
	public void dispose() {
		//no-op
		
	}
}
