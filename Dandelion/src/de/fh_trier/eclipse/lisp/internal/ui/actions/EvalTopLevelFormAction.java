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

import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.editor.ILispSourceSelection;
import de.fh_trier.eclipse.lisp.internal.ui.text.StructureException;

/**
 * Die Aktion fuer Evaluierung des aktuellen Toplevels.
 * @author Michael Bohn
 */
public class EvalTopLevelFormAction 
extends EvalAction 
{
	private static final String EVAL_TOPLEVEL_COMMAND_ID = "de.fh_trier.eclipse.lisp.commands.editor.evalToplevel";
	
	/**
	 * Erzeugt einen neue Aktion fuer Evaluierung
	 * des aktuellen Toplevels.s
	 */
	public EvalTopLevelFormAction() {
		setText("Evaluate Selected Toplevel");
		setImageDescriptor(LispUI.getUIImageManager().getDescriptor(UIImageConstants.ICON_EVALUATE_TOPLEVEL));
		setActionDefinitionId(EVAL_TOPLEVEL_COMMAND_ID);
	}
	
	/**
	 * Liefert den aktuellen Toplevel-Ausdruck.
	 */
	@Override
	public ILispSourceSelection getSelection() 
	throws StructureException {
		return getEditor().getTopLevelFormSelection();
	}
}
