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

package de.fh_trier.eclipse.lisp.internal.ui.actions;

import org.eclipse.jface.action.*;
import org.eclipse.jface.window.Window;

import de.fh_trier.eclipse.lisp.internal.core.LispCore;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.core.meta.ISymbolStore;
import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.dialogs.*;
import de.fh_trier.eclipse.lisp.internal.ui.views.listener.*;

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
		
		ISymbolStore store = LispCore.getEvalServerManager().getSymbolStoreFor(server);
		
		PackageSelectionDialog packageDialog = new PackageSelectionDialog(fListenerView.getSite().getShell());
		packageDialog.setElements(store.getPackages().toArray());
		int result = packageDialog.open();
		if(result == Window.OK) {
			fListenerView.switchToPackage(packageDialog.getResult()[0].toString());
		}
	}
}
