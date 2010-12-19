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

package de.defmacro.dandelion.internal.ui.views.listener;

import org.eclipse.ui.IViewPart;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.connection.*;

/**
 * Schnittstelle fuer die Listener-View.
 * @author Michael Bohn
 *
 */
public interface IListenerView 
extends IViewPart, IBackgroundEvaluationListener, IEnvironmentManagementListener
{
	/**
	 * Eindeutige ID in Eclipse.
	 */
	public static final String ID = LispPluginActivator.NS + ".views.listenerView";
	
	/**
	 * EvalServer zu der dieser Listener gehoert.
	 * @param server
	 */
	public void setEvalServer(IEnvironment server, boolean connected);
	
	/**
	 * Liefert den Eval-Server zu der dieser Listener
	 * gehoert.
	 * @return
	 */
	public IEnvironment getEvalServer();
	
	/**
	 * Wechselt das Paket im Listener.
	 * @param pack
	 */
	public void switchToPackage(String pack);
}
