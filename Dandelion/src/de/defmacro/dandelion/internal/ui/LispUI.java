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

import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import de.defmacro.dandelion.internal.LispPluginActivator;

/**
 * Bietet statischen Zugriff auf GUI-Komponenten
 * des Plugins.
 * @author Michael Bohn
 *
 */
public class LispUI
{
	private static UIColorManager colorManager = null;
	private static UIImageManager imageManager = null;
	
	private LispUI()
	{
		//keine Instanz erlaubt
	}
	
	/**
	 * Liefert die Singleton-Instanz der Farbmanagers.
	 * @return
	 */
	public static UIColorManager getUIColorManager()
	{
		if(colorManager == null) {
			colorManager = new UIColorManager();
		}
		return colorManager;
	}
	
	/**
	 * Liefert die Singleton-Instanz des Bildmanagers.
	 * @return
	 */
	public static UIImageManager getUIImageManager()
	{
		if (imageManager == null) {
			imageManager = new UIImageManager("images/");
		}

		return imageManager;
	}
	
	public static Shell getActiveWorkbenchWindowShell() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
	}
	
	/**
	 * Zeigt einen Fehlerdialog mit der uebergebenen Mitteilung.
	 * @param shell - Shell an der ein Fehlerdialog modal geoffnet wird 
	 * @param message - Beschreibung des Fehlers
	 */
	public static void showErrorDialog(final Shell shell, final String message)
	{
		openDialogAndLog(shell, new Status(IStatus.ERROR, LispPluginActivator.ID, 0, message, null), true);
	}
	
	/**
	 * Zeigt einen Fehlerdialog mit der uebergebenen Mitteilung und dem uebergeben Fehlerobjekt.
	 * @param shell - Shell an der ein Fehlerdialog modal geoffnet wird 
	 * @param message - Beschreibung des Fehlers
	 * @param ex - Fehlerobjekt
	 */
	public static void showErrorDialog(final Shell shell, final String message, final Throwable ex)
	{		
		openDialogAndLog(shell, new Status(IStatus.ERROR, LispPluginActivator.ID, 0, message, ex),  true);
	}
	
	/**
	 * Zeigt den Fehlerdialog modal an der Shell des aktiven WorkbenchWindow.
	 */
	public static void showErrorDialog(final String message, final Throwable ex) {
		showErrorDialog(getActiveWorkbenchWindowShell(), message, ex);
	}
	
	/**
	 * Zeigt einen Fehlerdialog mit der CoreException.
	 * @param shell - Shell an der ein Fehlerdialog modal geoffnet wird 
	 * @param ex - Die {@link CoreException}
	 */
	public static void showErrorDialog(final Shell shell, final CoreException ex)
	{
		showErrorDialog(shell, ex, true);
	}
	
	/**
	 * Zeigt einen Fehlerdialog mit der CoreException.
	 * Loggt zusätzlich die Fehlermeldung wenn log = <code>true</code>.
	 * @param shell - Shell an der ein Fehlerdialog modal geoffnet wird 
	 * @param ex - Die {@link CoreException}
	 * @param log - Loggen ja/nein
	 */
	public static void showErrorDialog(final Shell shell, final CoreException ex, final boolean log)
	{		
		openDialogAndLog(shell, ex.getStatus(), log);
	}
	
	private static void openDialogAndLog(final Shell shell, final IStatus status, final boolean log)
	{
		ErrorDialog.openError(shell, "A error occured...", null, status);
		if( log ) {
			LispPluginActivator.log(status);
		}
	}
	
	/**
	 * Gibt alle UI-Ressourcen frei.
	 * Nach dem Aufruf dieser Methode kann die {@link LispUI} nicht mehr verwendet werden.
	 */
	public static void dispose()
	{
		if(colorManager != null) {
			colorManager.dispose();
			colorManager = null;
		}
	}
}
