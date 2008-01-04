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

package de.fh_trier.eclipse.lisp.internal.ui.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.*;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.ui.editor.ILispSourceSelection;
import de.fh_trier.eclipse.lisp.internal.ui.views.macroexpand.MacroexpandView;

/**
 * Die Abstrakte Aktion fuer Macroexpand.
 * @author Michael Bohn
 *
 */
public abstract class AbstractMacroexpandAction 
extends EvalAction
{
	@Override
	protected abstract Job createEvaluationJob(IConnection connection, IBackgroundEvaluationListener listener, ILispSourceSelection selection);

	@Override
	protected IBackgroundEvaluationListener getBackgroundListener(final IProject project) 
	throws PartInitException, ManagementException
	{
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		IViewPart part = window.getActivePage().showView(MacroexpandView.ID);
		return (IBackgroundEvaluationListener)part;
	}
}
