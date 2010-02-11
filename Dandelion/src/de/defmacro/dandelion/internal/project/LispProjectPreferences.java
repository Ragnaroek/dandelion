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

package de.defmacro.dandelion.internal.project;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.connection.*;

/**
 * Verwaltet Einstellung die zu einem Projekt gehoeren.
 * @author Michael Bohn
 *
 */
/*package*/ class LispProjectPreferences 
{
	private static final String QUALIFIER = "de.fh_trier.eclipse.lisp.projectPreferences";
	
	private static final QualifiedName HOST = new QualifiedName(QUALIFIER, "host");
	private static final QualifiedName PORT = new QualifiedName(QUALIFIER, "port");
	private static final QualifiedName NAME = new QualifiedName(QUALIFIER, "name");
	private static final QualifiedName VERSION = new QualifiedName(QUALIFIER, "version");
	
	private LispProjectPreferences()
	{
		//nur static
	}

	/**
	 * Speichert den zu diesem Projekt gehoerende Eval-Server.
	 * @param project - Projekt in dem der Server gespeichert werden soll
	 * @param environment - Der zu speicherenden Server
	 * @throws CoreException - wenn Projekt geschlossen
	 */
	public static void storeEnvironment(final IProject project, final IEnvironment environment)
	throws CoreException
	{
		project.setPersistentProperty(HOST, environment.getHost());
		project.setPersistentProperty(PORT, Integer.toString(environment.getPort()));
		project.setPersistentProperty(NAME, environment.getName());
		project.setPersistentProperty(VERSION, environment.getVersion());
	}
	
	/**
	 * Liest den zu diesem Projekt gehoereden Eval-Server.
	 * @param project - Projekt aus dem der Eval-Server gelesen werden soll
	 * @return Der Eval-Server des Projektes
	 * @throws CoreException - wenn Projekt geschlossen
	 */
	public static IEnvironment readEnvironment(final IProject project)
	throws CoreException
	{
		try {
			String host = project.getPersistentProperty(HOST);
			String port = project.getPersistentProperty(PORT);
			String name = project.getPersistentProperty(NAME);
			String version = project.getPersistentProperty(VERSION);
			
			//EvalServer prueft auf null -> moeglich das einstellung null ist wenn die einstellungen
			//manuell veraendert werden
			return new Environment(host, Integer.parseInt(port), name, version);
		} catch (Exception e) { //error translation
			throw new CoreException(new Status(IStatus.ERROR, LispPluginActivator.PLUGIN_ID, 0, e.getMessage(), e));
		}
	}
}
