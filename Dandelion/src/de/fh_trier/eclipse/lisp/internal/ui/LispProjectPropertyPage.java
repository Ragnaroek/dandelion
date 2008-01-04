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

package de.fh_trier.eclipse.lisp.internal.ui;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.PropertyPage;
import de.fh_trier.eclipse.lisp.internal.core.LispCore;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;

/**
 * Der Beitrag zur Property-Seite eines Projektes.
 * @author Michael Bohn
 *
 */
public class LispProjectPropertyPage 
extends PropertyPage 
{
	@Override
	protected Control createContents(final Composite parent) 
	{
		noDefaultAndApplyButton();
		IAdaptable object = getElement();
		if(object instanceof IProject) {
			IProject project = (IProject)object;
			Label label = new Label(parent, SWT.NONE);
			IEnvironment server = LispCore.getEvalServerManager().getEvalServerFor(project);
			if(server != null) {
				label.setText("Environment:\t" + server.toString());
			}
		}
		return parent;
	}
}
