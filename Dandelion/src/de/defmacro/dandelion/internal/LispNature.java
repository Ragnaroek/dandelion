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

package de.defmacro.dandelion.internal;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

/**
 * Die Nature fuer Lisp-Projekte.
 * @author Michael Bohn
 */
public class LispNature 
implements IProjectNature 
{
	/**
	 * Nature-ID aus Erweiterungspunktdefinition.
	 */
	public static final String ID = LispPluginActivator.NS + ".natures.lispNature";
											
	private IProject fProject;
	
	/**
	 * @see IProjectNature#configure()
	 */
	public void configure() throws CoreException {
		//no-op
	}

    /**
     * @see IProjectNature#deconfigure()
     */	
	public void deconfigure() throws CoreException {
		//no-op
	}

	/**
	 * @see IProjectNature#getProject()
	 */
	public IProject getProject() {
		return fProject;
	}

	/**
	 * @see IProjectNature#setProject(IProject)
	 */
	public void setProject(final IProject project) {
		this.fProject = project;
	}
}
