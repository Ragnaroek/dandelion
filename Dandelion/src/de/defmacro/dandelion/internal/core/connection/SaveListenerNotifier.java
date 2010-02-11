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

package de.defmacro.dandelion.internal.core.connection;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

/**
 * Notfier fuer Benachrichtigung an einem bestimmten
 * Display.
 * @author Michael Bohn
 */
final class SaveListenerNotifier
{
	private final Runnable fRun;
	
	/**
	 * Erstellt einen neuen Notifiert.
	 * Das uebergeben {@link SaveRunnable}-Objekt wird im UI-Thread der Workbench ausgefuhert.
	 * @param run - SaveRunnable-Objekt
	 * @throws NullPointerException - wenn run == <code>null</code>
	 */
	public SaveListenerNotifier(final SaveRunnable run)
	{
		if (run == null) {
			throw new NullPointerException("run must not be null");
		}

		this.fRun = run;
	}
	
	/**
	 * Fuehrt das {@link SaveRunnable}-Objekt aus.
	 * Vor der Ausfuehrung wird das Display der Workbench auf Gueltigkeit ueberprueft.
	 */
	public void execInUIDisplay()
	{
		Display display = PlatformUI.getWorkbench().getDisplay();
		if(display == null || display.isDisposed()) {
			return;
		}
		display.asyncExec(fRun);
	}
}