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

package de.fh_trier.eclipse.lisp.internal.ui.wizards;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.*;
import org.eclipse.ui.*;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.part.FileEditorInput;

import de.fh_trier.eclipse.lisp.internal.LispNature;
import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.editor.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

@SuppressWarnings("SIC")
public class LispFileWizard 
extends Wizard 
implements INewWizard 
{
	private final class LispFileCreationPage extends WizardNewFileCreationPage {
		private LispFileCreationPage(String name, IStructuredSelection selection) {
			super(name, selection);
		}

		@Override
		protected boolean validatePage() {
			boolean ok = super.validatePage();
			if(ok) { //nur wenn aus super keine fehler kommen ueberpruefen
				IPath path = getContainerFullPath().append(getFileName());
		        if(path != null) {
		        	IProject project = ResourcesPlugin.getWorkspace().getRoot().getFile(path).getProject();
		        	if(project == null) return false; //das sollte nicht passieren da super.validate schon existierende Resource verlangt
		        	
		        	//invariante: project != null
		        	if( !project.isOpen() ) {
		        		setErrorMessage("Project is closed.");
		        		return false;
		        	}
		
		        	//invariante project != null && offen
		        	try {
						if( !project.hasNature(LispNature.ID) ) {
							setErrorMessage("Project " +project.getName()+ " is not a Lisp project.");
							return false;
						}
					} catch (CoreException e) {
						return false; //sollte nicht auftreten
					}
		        }
			}
			return ok;
		}
	}

	public static final String ID = "de.fh_trier.eclipse.lisp.newWizards.fileWizard";
	public static final String LISP_FILE_SUFFIX = ".lisp";

	@SuppressWarnings("UwF")
	private WizardNewFileCreationPage fPage;
	private IStructuredSelection fSelection;
	private IWorkbench fWorkbench;
	
	@Override
	public boolean performFinish() {
		createLispFile();
		LispPerspective.open();
		return true;
	}

	public void init(final IWorkbench workbench, final IStructuredSelection selection) {
		this.fWorkbench = workbench;
		this.fSelection = selection;
	}

	@Override
	public void addPages() {
		
		fPage = new LispFileCreationPage("New Lisp File", fSelection);
		fPage.setFileName(LISP_FILE_SUFFIX);
		fPage.setTitle("Lisp File");
		fPage.setDescription("Create a new Lisp file.");
		addPage(fPage);
	}
	
	private void createLispFile() {
		
		String fileName = fPage.getFileName();
		if(fileName == null) {
			return;
		}
		
		if(!fileName.endsWith(LISP_FILE_SUFFIX)) {
			fileName += LISP_FILE_SUFFIX;
			fPage.setFileName(fileName);
		}
		
		IFile file = fPage.createNewFile();
		try {
			fWorkbench.getActiveWorkbenchWindow().getActivePage().openEditor(new FileEditorInput(file), ILispEditor.ID);
		} catch (PartInitException e) {
			LispUI.showErrorDialog(getShell(), "Error opening file in editor", e);
		}
	}
}
