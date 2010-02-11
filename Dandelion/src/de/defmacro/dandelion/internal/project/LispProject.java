/*
 Dandelion, a Lisp plugin for Eclipse.
 Copyright (C) 2008 Michael Bohn

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

package de.defmacro.dandelion.internal.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;

/**
 * Die Klasse ist nicht dazu gedacht ausserhalb des Dandelion Plugins instantiiert zu werden!
 * @author Michael Bohn
 */
public class LispProject 
implements ILispProject 
{
	private final IProject fProject;
	private IEnvironment fEnvironment;
	
	/**
	 * Erstellt ein neues LispProject aus explizit uebergebener Environment.
	 * Es wird nicht die Environment aus dem Eclipse Projekt gelesen, sondern die explizite uebergebene 
	 * Umgebung verwendet. Eine alte Environment in dem Projekt wird ueberschrieben.
	 * @param project
	 * @param environment
	 * @throws CoreException 
	 */
	public LispProject(final IProject project, final IEnvironment environment) throws CoreException {
		LispProjectPreferences.storeEnvironment(project, environment);
		this.fProject = project;
		this.fEnvironment = environment;
	}
	
	/**
	 * Erstellt ein LispProject fuer das uebergeben Eclipse Projekt. Falls die Environment die mit
	 * dem Projekt verbunden ist nicht mehr vorhanden ist, wird die Default Environment eingesetzt.
	 * @param project
	 * @throws CoreException
	 */
	public LispProject(final IProject project) throws CoreException 
	{
		this.fProject = project;
		this.fEnvironment = LispProjectPreferences.readEnvironment(project);
		
		EnvironmentManager manager = LispCore.getEnvironmentManager();
		if(!manager.getEnvironments().contains(fEnvironment)) {
			if(manager.hasEnvironment()) {
				this.fEnvironment = manager.getDefaultEnvironment();
			}
			//FIXME was machen wenn keine Environment vorhanden??? sollte nicht auftreten, da Default Env nicht geloescht werden kann
			// was aber wenn nur Plugin installiert wurde (ohne Environment)?
		}
	}

	public IEnvironment getEnvironment() {
		return fEnvironment;
	}

	public void setEnvironment(final IEnvironment environment) {
		this.fEnvironment = environment;
		try {
			LispProjectPreferences.storeEnvironment(fProject, environment);
		} catch (CoreException e) {
			LispPluginActivator.logError("Cannot store environment for project", e);
		}
		//TODO Listener benachrichtigen
	}
	
	public IProject getProject() {
		return fProject;
	}
}
