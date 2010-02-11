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

import org.eclipse.ui.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.ui.views.apropos.AproposView;
import de.defmacro.dandelion.internal.ui.views.listener.*;
import de.defmacro.dandelion.internal.ui.views.macroexpand.MacroexpandView;
import de.defmacro.dandelion.internal.ui.wizards.LispFileWizard;

/**
 * Die Lisp-Perspektive. Stellt das anfaengliche Layout her.
 * @author Michael Bohn
 */
public class LispPerspective 
implements IPerspectiveFactory 
{
	/**
	 * Eindeutige ID der Perspektive aus Manifest.
	 */
	public static final String PERSPECTIVE_ID = "de.fh_trier.eclipse.lisp.lispPerspective";
	
	/**
	 * @see IPerspectiveFactory#createInitialLayout(IPageLayout)
	 */
	public void createInitialLayout(final IPageLayout layout) 
	{
		addViews(layout);
		addViewShortcuts(layout);
		addWizardShortcuts(layout);
	}
	
	private void addWizardShortcuts(final IPageLayout layout) {
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");
		layout.addNewWizardShortcut(LispFileWizard.ID);
	}

	private void addViews(final IPageLayout layout)
	{
		String editorArea = layout.getEditorArea();
		
		//restliche Views relativ zum Editor anordnen
		IFolderLayout left = layout.createFolder("left", IPageLayout.LEFT, 0.2f, editorArea);
		left.addView(IPageLayout.ID_RES_NAV); //Resourcen-View

		//untere Leiste vor rechter Leiste, damit bottom komplett
		//untere Breite einnimmt
		IFolderLayout bottom = layout.createFolder("bottom", IPageLayout.BOTTOM, 0.66f, editorArea);
		bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
		bottom.addPlaceholder(IListenerView.ID);
		bottom.addPlaceholder(AproposView.ID);
		bottom.addPlaceholder(MacroexpandView.ID);
		bottom.addPlaceholder(IPageLayout.ID_TASK_LIST);
		bottom.addPlaceholder(IPageLayout.ID_PROP_SHEET);
		bottom.addPlaceholder(IPageLayout.ID_BOOKMARKS);
		
		IFolderLayout right = layout.createFolder("right", IPageLayout.RIGHT, 0.8f, editorArea);
		right.addView(IPageLayout.ID_OUTLINE);
	}
	
	private void addViewShortcuts(final IPageLayout layout)
	{
		//die Views werden so über Window -> Show View direkt erreichbar
		layout.addShowViewShortcut(IPageLayout.ID_RES_NAV);
		layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
		layout.addShowViewShortcut(AproposView.ID);
		layout.addShowViewShortcut(MacroexpandView.ID);
		layout.addShowViewShortcut(IListenerView.ID);
		layout.addShowViewShortcut(IPageLayout.ID_TASK_LIST);
		layout.addShowViewShortcut(IPageLayout.ID_BOOKMARKS);
		layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
	}
	
	/**
	 * Oeffnet die Lisp-Perspektive.
	 */
	public static void open()
	{
		try {
			PlatformUI.getWorkbench().showPerspective(LispPerspective.PERSPECTIVE_ID, PlatformUI.getWorkbench().getActiveWorkbenchWindow());
		} catch (WorkbenchException e) {
			LispPluginActivator.logError("opening lisp perspective failed", e);
		}		
	}
}
