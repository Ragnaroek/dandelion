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

package de.fh_trier.eclipse.lisp.internal.core.connection;

import de.fh_trier.eclipse.lisp.internal.core.meta.ISymbolStore;

/**
 * Listener-Schnittstelle fuer Ueberwachung
 * des {@link EnvironmentManager}.
 * Der EvalServerManager stellt sicher das die
 * Benachrichtigungen aus dem UI-Thread erfolgen.
 * @author Michael Bohn
 */
public interface IEnvironmentManagementListener 
{
	/**
	 * Die uebergeben Lisp-Umgebung wurde verbunden.
	 * @param server - Die betreffende Lisp-Umgebung. 
	 */
	public void connect(IEnvironment server);
	
	/**
	 * Die uebergeben Lisp-Umgebung wurde getrennt.
	 * @param server - Die betreffende Lisp-Umgebung. 
	 */
	public void disconnect(IEnvironment server);
	
	/**
	 * Die uebergeben Lisp-Umgebung wurde intialisiert.
	 * Der Symbolspeicher wurde mit den vorhadenen Symbolen aufgefuellt.
	 * @param server - Die betreffende Lisp-Umgebung. 
	 * @see ISymbolStore
	 */
	public void initialized(IEnvironment server);
	
	/**
	 * Die uebergeben Lisp-Umgebung wurde gestartet.
	 * @param server - Die betreffende Lisp-Umgebung. 
	 */
	public void startup(IEnvironment server);
	
	/**
	 * Eine neue Lisp-Umgebung wurde hinzugefuegt.
	 * @param server - Die neue Lisp-Umgebung.
	 */
	public void serverAdded(IEnvironment server);
	
	/**
	 * Eine Lisp-Umgebung wurde entfernt.
	 * @param server - Die entfernte Lisp-Umgebung.
	 */
	public void serverRemoved(IEnvironment server);
	
	/**
	 * Die Default-Lisp-Umgebung wurde veraendert.
	 * @param newDefault - Die neue Default-Lisp-Umgebung.
	 */
	public void defaultChanged(IEnvironment newDefault);
}
