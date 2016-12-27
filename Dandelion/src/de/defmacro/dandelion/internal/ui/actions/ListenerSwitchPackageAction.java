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

import org.eclipse.jface.action.*;
import org.eclipse.jface.window.Window;

import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.core.meta.ISymbolStore;
import de.defmacro.dandelion.internal.ui.*;
import de.defmacro.dandelion.internal.ui.dialogs.*;
import de.defmacro.dandelion.internal.ui.views.listener.*;

/**
 * Die Aktion fuer Wechsel des Pakets im Listener.
 * @author Michael Bohn
 *
 */
public class ListenerSwitchPackageAction
extends Action
{
	private final IListenerView fListenerView;
	
	/**
	 * Erzeugt eine neuen Aktion fuer Wechsel
	 * des Paketes des Listener.
	 * @param view - Der Listener
	 */
	public ListenerSwitchPackageAction(final IListenerView view)
	{
		this.fListenerView = view;
		this.setText("Switch Package");
		this.setToolTipText("Switch to package...");
		this.setImageDescriptor(LispUI.getUIImageManager().getDescriptor(UIImageConstants.ICON_PACKAGE));
	}
	
	/**
	 * Wechselt das Paket im Listener.
	 */
	@Override
	public void run() {
		
		IEnvironment server = fListenerView.getEvalServer();
		if(server == null) {
			return;
		}
		
		ISymbolStore store = LispCore.getEnvironmentManager().getSymbolStoreFor(server);
		
		PackageSelectionDialog packageDialog = new PackageSelectionDialog(fListenerView.getSite().getShell());
		packageDialog.setElements(store.getPackages().toArray());
		int result = packageDialog.open();
		if(result == Window.OK) {
			fListenerView.switchToPackage(packageDialog.getResult()[0].toString());
		}
	}
}
