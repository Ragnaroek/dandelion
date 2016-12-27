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
 * Runnable fuer {@link SaveListenerNotifier}.
 * @author Michael Bohn
 */
public abstract class SaveRunnable 
implements Runnable 
{
	/**
	 * Durchfuehrung der {@link SaveRunnable#run0()}-Methode.
	 * Vor der Ausfuehrung wird das Display der Workbench auf Gueltigkeit geprueft.
	 */
	public void run() {
		Display display = PlatformUI.getWorkbench().getDisplay();
		if(display == null || display.isDisposed()) {
			return;
		}
		
		run0();
	}
	
	/**
	 * Durchzufuehrende Aktion.
	 * Wird nur ausgefuehrt wenn das Display der Workbench noch gueltig ist.
	 */
	public abstract void run0();
}
