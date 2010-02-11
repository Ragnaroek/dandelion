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

package de.defmacro.dandelion.internal.core;

import net.jcip.annotations.*;
import de.defmacro.dandelion.internal.core.connection.EnvironmentManager;
import de.defmacro.dandelion.internal.project.ProjectManager;

/**
 * Die LispCore Klasse bietet statischen Zugriff
 * auf Kernelemente des Lisp-Plugins.
 * @author Michael Bohn
 *
 */
@ThreadSafe
public class LispCore 
{
	private LispCore() 
	{ /* nur statische Methoden */}
	
	private static EnvironmentManager sEnvironmentManagerInstance;
	private static ProjectManager sProjectManagerInstance;
	
	/**
	 * Gibt die Singleton-Instanz des {@link EnvironmentManager} zurueck.
	 * Ein Aufruf dieser Methode ist Thread-Safe.
	 * 
	 * @return Singleton-Instanz des {@link EnvironmentManager}
	 */
	@GuardedBy("LispCore.class")
	public synchronized static EnvironmentManager getEnvironmentManager()
	{
		if(sEnvironmentManagerInstance == null) {
			sEnvironmentManagerInstance = new EnvironmentManager();
		}
		
		return sEnvironmentManagerInstance;
	}
	
	@GuardedBy("LispCore.class")
	public synchronized static ProjectManager getProjectManager() {
		if(sProjectManagerInstance == null) {
			sProjectManagerInstance = new ProjectManager();
		}
		return sProjectManagerInstance;
	}
}
