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

package de.defmacro.dandelion.internal.ui;

import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.PropertyPage;

import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.project.ILispProject;

/**
 * Der Beitrag zur Property-Seite eines Projektes.
 * @author Michael Bohn
 *
 */
public class LispProjectPropertyPage 
extends PropertyPage 
{
	private Combo fComboEnvironments;


	@Override
	protected Control createContents(final Composite parent) 
	{
		noDefaultAndApplyButton();
		
		IAdaptable object = getElement();
		if(object instanceof IProject) {
			
			parent.setLayout(new GridLayout(2, false));
			
			ILispProject project = LispCore.getProjectManager().getLispProjectFor((IProject)object);
			
			Label label = new Label(parent, SWT.NONE);
			label.setText("Environment: ");
			fComboEnvironments = makeEnvironmentCombo(parent, project);
			
		}
		return parent;
	}
	
	private Combo makeEnvironmentCombo(final Composite parent, final ILispProject project) {
		Combo combo = new Combo(parent, SWT.READ_ONLY);
		List<IEnvironment> environments = LispCore.getEnvironmentManager().getEnvironments();
		
		for(IEnvironment environment : environments) {
			combo.add(environment.toString());
		}
		int index = environments.indexOf(project.getEnvironment());
		combo.select(index);
		
		return combo;
	}
	
	@Override
	public boolean performOk() 
	{	
		EnvironmentManager manager = LispCore.getEnvironmentManager();
		String selectedEnvironment = fComboEnvironments.getItem(fComboEnvironments.getSelectionIndex());
		
		IEnvironment newEnvironment = null;
		for(IEnvironment environment : manager.getEnvironments()) {
			if(environment.toString().equals(selectedEnvironment)) {
				newEnvironment = environment;
				break;
			}
		}
		
		if(newEnvironment != null && getElement() instanceof IProject) {
			ILispProject project = LispCore.getProjectManager().getLispProjectFor((IProject)getElement());
			project.setEnvironment(newEnvironment);
		}
		return true;
	}
}
