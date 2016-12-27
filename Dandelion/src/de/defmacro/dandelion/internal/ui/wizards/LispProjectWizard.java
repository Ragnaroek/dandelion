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

package de.defmacro.dandelion.internal.ui.wizards;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.*;
import org.eclipse.ui.*;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.project.*;
import de.defmacro.dandelion.internal.ui.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

public class LispProjectWizard
extends Wizard 
implements INewWizard
{
	public static final String ID = LispPluginActivator.NS + ".newWizards.projectWizard";
	
	@SuppressWarnings("UwF")
	private LispProjectWizardPage fWizardPage;
	
	@Override
	public boolean performFinish() {
		createLispProject();
		LispPerspective.open();
		return true;
	}

	@Override
	public boolean needsPreviousAndNextButtons() {
		return false;
	}

	public void init(final IWorkbench workbench, final IStructuredSelection selection) {
		//no-op
	}

	@Override
	public void addPages() {
		
		fWizardPage = new LispProjectWizardPage("New Project");
		fWizardPage.setDescription("Select a project name and a environment to associate with this project.");
		fWizardPage.setTitle("New Lisp Project");
		addPage(fWizardPage);
	}
	
	private void createLispProject() 
	{
		IProject project = fWizardPage.getProjectHandle();
		try {
			project.create(null);
			project.open(null);
			addLispNature(project);
			IEnvironment selected = fWizardPage.getSelectedEvalServer();
			Assert.isNotNull(selected); //Finish nur moeglich wenn ein Server ausgewaehlt wurde
			
			ILispProject lispProject = new LispProject(project, selected);
			LispCore.getProjectManager().registerProject(lispProject);
			
		} catch (CoreException e) {
			LispUI.showErrorDialog(getShell(), "Error creating project", e);
		}
	}
	
	private void addLispNature(final IProject project)
	throws CoreException
	{
		IProjectDescription description = project.getDescription();
		String[] natures = description.getNatureIds();
		String[] newNatures = new String[natures.length + 1];
		System.arraycopy(natures, 0, newNatures, 0, natures.length);
		newNatures[natures.length] = LispNature.ID;
		description.setNatureIds(newNatures);
		project.setDescription(description, null);
	}
}
