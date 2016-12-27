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

package de.defmacro.dandelion.internal.ui.actions;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.jobs.Job;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.ui.*;
import de.defmacro.dandelion.internal.ui.editor.*;
import de.defmacro.dandelion.internal.ui.text.StructureException;

/**
 * Die Macroexpand (all) Aktion.
 * @author Michael Bohn
 *
 */
public class MacroexpandAction 
extends AbstractMacroexpandAction
{
	private static final String MACROEXPAND_COMMAND_ID = LispPluginActivator.NS + ".commands.editor.macroexpand";
	
	/**
	 * Erzeugt eine neue Aktion fuer die Expandierung von
	 * Makros.
	 */
	public MacroexpandAction()
	{
		setText("Macroexpand Selection");
		setImageDescriptor(LispUI.getUIImageManager().getDescriptor(UIImageConstants.ICON_MACROEXPAND));
		setActionDefinitionId(MACROEXPAND_COMMAND_ID);
	}
	
	/**
	 * Liefert die aktuelle Selektion im Editor.
	 * @see EvalAction#getSelection()
	 */
	@Override
	public ILispSourceSelection getSelection() 
	throws StructureException 
	{
		return getEditor().getSourceSelection();
	}

	@Override
	protected Job createEvaluationJob(final IConnection connection, final IBackgroundEvaluationListener listener, final ILispSourceSelection selection) {

		Assert.isTrue(!selection.isEmpty());
		
		PackageBoundForm form = selection.getForms().get(0);
		return new EvaluationJob(connection, form.getPackage(), surroundWithMacroexpand(form.getForm()), listener);
	}
	
	private String surroundWithMacroexpand(final String form)
	{
		StringBuilder macroexpand = new StringBuilder();
		macroexpand.append("(macroexpand (quote ");
		macroexpand.append(form);
		macroexpand.append("))");
	
		return macroexpand.toString();
	}
}
