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

import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.views.listener.*;

/**
 * Aktion fuer Loeschung des Listener Inhalts.
 * @author Michael Bohn
 *
 */
public class ListenerClearAction 
extends Action
{
	private final ListenerView fListenerView;
	
	/**
	 * Erzeugt eine neuen Clear-Aktion fuer den
	 * uebergebenen Listener.
	 * @param view - Der Listener
	 */
	public ListenerClearAction(final ListenerView view)
	{
		this.setToolTipText("Clear Listener");
		this.setText("Clear");
		this.setImageDescriptor(LispUI.getUIImageManager().getDescriptor(UIImageConstants.ICON_CLEAR));
		this.fListenerView = view;
	}
	
	/**
	 * Ruft die <code>clear()</code>-Methode des Listeners
	 * auf.
	 */
	@Override
	public void run() {
		fListenerView.clear();
	}
}
