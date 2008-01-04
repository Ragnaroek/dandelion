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

package de.fh_trier.eclipse.lisp.internal.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.*;

import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;

/**
 * Die GUI-Komponente fuer die Anzeige
 * der Haupteinstellungen im Plugin.
 * @author Michael Bohn
 *
 */
public class LispPrimaryPreferences 
extends FieldEditorPreferencePage 
implements IWorkbenchPreferencePage 
{
	/**
	 * Erstellt eine neue Preference-Seite fuer Anzeige der allgemeinen Einstellungen.
	 */
	public LispPrimaryPreferences() {
		super(GRID);
		setPreferenceStore(LispPluginActivator.getDefault().getPreferenceStore());
	}
	
	/**
	 * Implementierung der abstrakten Methode aus {@link FieldEditorPreferencePage}.
	 * Erstellt die Field-Editoren fuer die Einstellungen.
	 */
	@Override
	public void createFieldEditors() 
	{
		addField(new BooleanFieldEditor(LispPreferences.P_UPPER_CASE_PROPOSAL, 
				"Upper-Case completion proposals", getFieldEditorParent()));
	}

	/**
	 * Ungenutzt.
	 */
	public void init(IWorkbench workbench) {
		//no-op
	}	
}
