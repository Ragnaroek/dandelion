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

import java.util.*;
import java.util.Map.Entry;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import net.jcip.annotations.*;

/**
 * Diese Klasse darf nicht instantiiert werden! Zugriff
 * nur ueber {@link LispCore#getProjectManager()}. 
 */
@ThreadSafe
public class ProjectManager 
{
	private boolean fIsInitialized = false;
	private final Map<IProject, ILispProject> fProjectAssoc;
	
	private static class ResourceListener
	implements IResourceChangeListener
	{
		private final ProjectManager fManager;
		
		public ResourceListener(final ProjectManager manager) {
			this.fManager = manager;
		}
		
		public void resourceChanged(final IResourceChangeEvent event) 
		{
			if( event.getType() == IResourceChangeEvent.PRE_CLOSE ) {
				fManager.removeProject((IProject)event.getResource());
			} else if(event.getType() == IResourceChangeEvent.PRE_DELETE) {
				fManager.removeProject((IProject)event.getResource());
			} else if(event.getType() == IResourceChangeEvent.POST_CHANGE) {
				IResourceDelta delta = event.getDelta();
				for(IResourceDelta projectDelta : delta.getAffectedChildren()) {
					int flags = projectDelta.getFlags();
					if((flags & IResourceDelta.OPEN) != 0 && projectDelta.getResource() instanceof IProject) {
						IProject project = (IProject)projectDelta.getResource();
						if( project.isOpen() ) {
							projectOpened(project);
						}
					}
				}
			}
		}
		
		private void projectOpened(final IProject project) 
		{
			try {
				if( hasLispNature(project) ) {
					ILispProject lispProject = new LispProject(project);
					fManager.registerProject(lispProject);
				}
			} catch (CoreException e) {
				LispPluginActivator.logError("error opening lisp project", e);
			}
		}
	}
	
	
	/**
	 * Diese Klasse darf nicht instantiiert werden! Zugriff
	 * nur ueber {@link LispCore#getProjectManager()}. 
	 */
	public ProjectManager() {
		this.fProjectAssoc = new HashMap<IProject, ILispProject>();
	}
	
	/**
	 * Diese Methode darf nicht aufgerufen werden! Wird von der Workbench nach
	 * der Plugin Initialisierung aufgerufen.
	 */
	@GuardedBy("this")
	public synchronized void initialize() {
		
		Assert.isTrue(!fIsInitialized);
		
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		IProject[] projects = root.getProjects();
		for(IProject project : projects) {
			try {
				if( hasLispNature(project) ) {
					fProjectAssoc.put(project, new LispProject(project));
				}
			} catch (CoreException e) {
				LispPluginActivator.logError("error creating project: " + project.getName(), e);
			}
		}
		
		//FIXME remove in dispose
		workspace.addResourceChangeListener(new ResourceListener(this));
		
		fIsInitialized = true;
	}
	
	/**
	 * Liefert das LispProject fuer das uebergebene Eclipse IProject.
	 * 
	 * @param project
	 * @throws NullPointerException, wenn project == <code>null</code>
	 * @throws IllegalArgumentException, wenn Projekt nicht existiert, nicht geoffnet ist oder keine LispNature besitzt
	 * @return
	 */
	@GuardedBy("this")
	public synchronized ILispProject getLispProjectFor(final IProject project) {
		try {
			if(!hasLispNature(project)) {
				throw new IllegalArgumentException("not a Lisp project");
			}
		} catch (CoreException e) {
			throw new IllegalArgumentException("project in invalid state", e);
		}
		
		return fProjectAssoc.get(project);
	}
	
	/**
	 * Fuegt das Projekt zur Projektverwaltung hinzu. Es ist ein Fehler ein Projekt zweimal
	 * mit verschiedenen ILispProjects zu registrieren.
	 * @param project
	 */
	public synchronized void registerProject(final ILispProject project) {
		if(getLispProjectFor(project.getProject()) != null) {
			throw new IllegalArgumentException("Trying to register a project twice");
		}
		
		fProjectAssoc.put(project.getProject(), project);
	}
	
	
	/**
	 * Ueberprueft ob die Lisp-Umgebung in min. einem Projekt verwendet wird.
	 * @param server - Umgebung die ueberprueft werden soll
	 * @return <code>true</code> wenn Umgebung in min. einem Projekt verwendet wird, sonst <code>false</code>
	 */
	@GuardedBy("this")
	public synchronized boolean isUsedInProject(final IEnvironment server) 
	{
		for(Entry<IProject, ILispProject> entry : fProjectAssoc.entrySet()) {
			if(entry.getValue().getEnvironment().equals(server)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Entfernt das Projekt aus der Verwaltung. Falls die Verbindung in keinem
	 * weiteren Projekt benutzt wird, wird sie geschlossen.
	 * @param project
	 */
	@GuardedBy("this")
	public synchronized void removeProject(final IProject project) 
	{
		ILispProject lispProject = fProjectAssoc.remove(project);
		if(lispProject != null) {
			if(!isUsedInProject(lispProject.getEnvironment())) {
				try {
					LispCore.getEnvironmentManager().closeConnection(lispProject.getEnvironment());
				} catch (ConnectionException e) {
					LispPluginActivator.logError("Cannot close connection", e);
				}
			}
		}
	}
	
	@GuardedBy("this")
	public synchronized void removeProject(final ILispProject project)
	{
		removeProject(project.getProject());
	}
	
	private static boolean hasLispNature(final IProject project) throws CoreException
	{
		return project != null && project.exists() && project.isOpen() && project.hasNature(LispNature.ID);
	}
}
