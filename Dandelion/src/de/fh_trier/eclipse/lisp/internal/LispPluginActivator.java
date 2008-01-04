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

package de.fh_trier.eclipse.lisp.internal;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.resource.*;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import de.fh_trier.eclipse.lisp.internal.core.LispCore;
import de.fh_trier.eclipse.lisp.internal.core.connection.EnvironmentManager;
import de.fh_trier.eclipse.lisp.internal.preferences.LispPreferences;
import de.fh_trier.eclipse.lisp.internal.ui.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Der LispPluginActivator kontrolliert den Lebensyzklus 
 * des Plugins. Hier wird der {@link EnvironmentManager} initialsiert.
 */
public class LispPluginActivator 
extends AbstractUIPlugin 
{
	/**
	 * Der Namespace fuer dier Plugin-Erweiterungspunkte.
	 */
	public static final String PLUGIN_NS = "de.fh_trier.eclipse.lisp";
	
	/**
	 * Die eindeutige ID des Plugins.
	 */
	public static final String PLUGIN_ID = "de.fh_trier.eclipse.lisp";
	
	private static LispPluginActivator plugin;
	
	@SuppressWarnings("ST") //error write to static field
	public LispPluginActivator() {
		plugin = this;
	}

	/**
	 * Das Plugin wurde aktiviert.
	 * Der {@link EnvironmentManager} wird initialisiert und die Default-Einstellungen
	 * gesetzt.
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		LispPreferences.initDefaultValues(getPreferenceStore());
		try {
			LispCore.getEvalServerManager().initialize();
		} catch (CoreException e) {
			LispPluginActivator.logError("error initialising EvalServerManager", e);
		}
	}

	/**
	 * Das Plugin wurde deaktiviert.
	 * Die UI und der LispCore wird heruntergefahren. Das Plugin ist nach dem
	 * Aufruf der Methode nicht mehr verwendbar.
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	@SuppressWarnings("ST") //error write to static field
	public void stop(final BundleContext context) throws Exception {
		try {
			LispUI.dispose();
			LispCore.getEvalServerManager().dispose();
		} finally {
			plugin = null;
			super.stop(context);
		}
	}

	/**
	 * Gibt die Singleton-Instanz zum {@link LispPluginActivator} zurueck.
	 *
	 * @return Die Singleton-Instanz zum Activator
	 */
	public static LispPluginActivator getDefault() {
		return plugin;
	}
	
	/**
	 * Loggt eine Statusmeldung.
	 * Vereinfachung fuer: <br />
	 * <pre>
	 * LispPluginActivator.getDefault().getLog().log(status);
	 * </pre>
	 * 
	 * @param status - Das Status-Objekt.
	 */
	public static void log(final IStatus status)
	{
		getDefault().getLog().log(status);
	}
	
	/**
	 * Loggt eine Statsumeldung.
	 * @param severity - Der Schweregrad des Fehlers.
	 * @param message - Eine Beschreibung des Fehlers
	 * @param error - Das Fehlerobjekt.
	 */
	public static void log(final int severity, final String message, final Throwable error)
	{
		log(new Status(severity, PLUGIN_ID, 0, message, error));
	}
	
	/**
	 * Loggt eine Fehlermeldung.
	 * @param message - Eine Beschreibung des Fehlers
	 * @param error - Das Fehlerobjekt
	 */
	public static void logError(final String message, final Throwable error)
	{
		log(new Status(IStatus.ERROR, PLUGIN_ID, 0, message, error));
	}
	
	/**
	 * Schreibt Fehlermeldungen ins Log die nicht auftreten sollten. Wird ueberall
	 * dort verwendet wo das catch einer Fehlermeldung erforderlich ist.
	 * Nur verwenden wenn keine Shell fuer die Fehleranzeige im ErrorDialog
	 * vorhanden ist!
	 * @param description - Eine Beschreibung des Fehlers.
	 * @param error - Das Fehlerobjekt
	 */
	public static void logBrokenInvariant(final String description, final Throwable error)
	{
		logError("broken invariant: " + description, error);
	}
	
	/**
	 * Liefert einen {@link ImageDescriptor} aus dem Plugin-Ordner.
	 * 
	 * @param imgSpec - Bildbeschreibung
	 * @see LispUI#getUIImageManager()
	 * @return Ein {@link ImageDescriptor}.
	 */
	public static ImageDescriptor getImageDescriptor(final String imgSpec)
	{
		return AbstractUIPlugin.imageDescriptorFromPlugin(PLUGIN_ID, imgSpec);
	}
}
