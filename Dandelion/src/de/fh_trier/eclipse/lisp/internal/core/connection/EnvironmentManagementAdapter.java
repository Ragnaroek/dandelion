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

/**
 * Adapter-Implementierung des {@link IEnvironmentManagementListener}-Interfaces.
 * Die einzelnen Methoden fuehren keine Aktionen aus.
 * @author Michael Bohn
 *
 */
public class EnvironmentManagementAdapter 
implements IEnvironmentManagementListener 
{
	/**
	 * @see IEnvironmentManagementListener#connect(IEnvironment)
	 */
	public void connect(final IEnvironment server) {
		//no-op
	}

	/**
	 * @see IEnvironmentManagementListener#disconnect(IEnvironment)
	 */
	public void disconnect(final IEnvironment server) {
		//no-op
	}

	/**
	 * @see IEnvironmentManagementListener#initialized(IEnvironment)
	 */
	public void initialized(final IEnvironment server) {
		//no-op
	}

	/**
	 * @see IEnvironmentManagementListener#serverAdded(IEnvironment)
	 */
	public void serverAdded(final IEnvironment server) {
		//no-op
	}

	/**
	 * @see IEnvironmentManagementListener#serverRemoved(IEnvironment)
	 */
	public void serverRemoved(final IEnvironment server) {
		//no-op
	}

	/**
	 * @see IEnvironmentManagementListener#startup(IEnvironment)
	 */
	public void startup(final IEnvironment server) {
		//no-op
	}

	/**
	 * @see IEnvironmentManagementListener#defaultChanged(IEnvironment)
	 */
	public void defaultChanged(IEnvironment newDefault) {
		//no-op
	}
}
