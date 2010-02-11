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

import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.*;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.core.dom.TSExpression;
import de.defmacro.dandelion.internal.preferences.LispPreferences;
import de.defmacro.dandelion.internal.ui.dialogs.VisibleTypeSelectionDialog;

/**
 * Aktion fuer Aufruf der sichtbaren Typen
 * der Outline.
 * @author Michael Bohn
 *
 */
public class OutlineVisibleTypeActions 
implements IViewActionDelegate
{
	public static final String ACTION_TOPLEVEL_ID = "de.fh_trier.eclipse.lisp.views.outline.typesToplevel";
	public static final String ACTION_SUBLEVEL_ID = "de.fh_trier.eclipse.lisp.views.outline.typesSublevel";
		
	private IViewPart fViewPart;
	
	/**
	 * Initialsiert die Aktion.
	 */
	public void init(IViewPart view) 
	{
		this.fViewPart = view;
	}

	/**
	 * Oeffnet einen Typauswahldialog und speichert die neuen Einstellung ab.
	 * Die Outline reagiert darauf entsprechend.
	 */
	public void run(IAction action) 
	{
		String id = action.getId();
		final String usePreference;
		if(ACTION_TOPLEVEL_ID.equals(id)) {
			usePreference = LispPreferences.OUTLINE_SHOW_TYPES_ON_TOPLEVEL;
		} else if (ACTION_SUBLEVEL_ID.equals(id)) {
			usePreference = LispPreferences.OUTLINE_SHOW_TYPES_ON_SUBLEVEL;
		} else {
			LispPluginActivator.log(IStatus.WARNING, "ActionDelegate not defined for this Action", null);
			return;
		}
		
		IPreferenceStore prefStore = LispPluginActivator.getDefault().getPreferenceStore();
		Set<TSExpression> defaults = LispPreferences.decodeOutlineVisibleTypes(prefStore.getDefaultString(usePreference));
		Set<TSExpression> previous = LispPreferences.decodeOutlineVisibleTypes(prefStore.getString(usePreference));
		
		VisibleTypeSelectionDialog dialog = new VisibleTypeSelectionDialog(fViewPart.getViewSite().getShell(), defaults, previous);
		dialog.open();
		
		Set<TSExpression> selection = dialog.getSelection();
		
		if(selection != null) {
			prefStore.setValue(usePreference, LispPreferences.encodeOutlineVisibleTypes(selection));
		}
	}

	/**
	 * Ungenutzt.
	 */
	public void selectionChanged(IAction action, ISelection selection) 
	{
		//no-op unabhaegig von Selektion
	}
}
