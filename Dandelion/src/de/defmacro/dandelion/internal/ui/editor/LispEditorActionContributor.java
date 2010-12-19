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

package de.defmacro.dandelion.internal.ui.editor;

import java.util.*;
import org.eclipse.jface.action.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.EditorActionBarContributor;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.ui.actions.*;

/**
 * Die zur Workbench hinzugefuegten Aktionen des Editors.
 * @author Michael Bohn
 *
 */
public class LispEditorActionContributor 
extends EditorActionBarContributor 
{
	private static final String LISP_GROUP_NAME = "Lisp";
	private static final String LISP_GROUP_ID = LispPluginActivator.NS +  ".menuContribution";
	
	private ILispEditorAction fEvalSelectionAction;
	private ILispEditorAction fEvalFileAction;
	private ILispEditorAction fEvalToplevelAction;
	private ILispEditorAction fMacroexpand;
	private ILispEditorAction fMacroexpand1;
	
	private List<ILispEditorAction> fAllActions;

	public LispEditorActionContributor() 
	{	
		fAllActions = new ArrayList<ILispEditorAction>();
		
		fEvalSelectionAction = new EvalSelectionAction();
		fAllActions.add(fEvalSelectionAction);
		fEvalFileAction = new EvalFileSelectionAction();
		fAllActions.add(fEvalFileAction);
		fEvalToplevelAction = new EvalTopLevelFormAction();
		fAllActions.add(fEvalToplevelAction);
		fMacroexpand = new MacroexpandAction();
		fAllActions.add(fMacroexpand);
		fMacroexpand1 = new MacroexpandOneAction();
		fAllActions.add(fMacroexpand1);

		//registerActions(fAllActions);
	}
	
	/*
	private void registerActions(final List<ILispEditorAction> actions)
	{
		IHandlerService handlerService = (IHandlerService)PlatformUI.getWorkbench().getService(IHandlerService.class);
		for(ILispEditorAction action : actions) {
			handlerService.activateHandler(action.getActionDefinitionId(), new ActionHandler(action));
		} 
	} */
	
	/**
	 * @see EditorActionBarContributor#contributeToMenu(IMenuManager)
	 */
	@Override
	public void contributeToMenu(final IMenuManager menuManager) 
	{
		MenuManager subMenu = new MenuManager(LISP_GROUP_NAME, LISP_GROUP_ID);
		menuManager.insertAfter(IWorkbenchActionConstants.MB_ADDITIONS, subMenu);
		subMenu.add(fEvalSelectionAction);
		subMenu.add(fEvalFileAction);
		subMenu.add(fEvalToplevelAction);
		subMenu.add(new Separator());
		subMenu.add(fMacroexpand);
		subMenu.add(fMacroexpand1);
	}

	/**
	 * @see EditorActionBarContributor#contributeToToolBar(IToolBarManager)
	 */
	@Override
	public void contributeToToolBar(final IToolBarManager toolBarManager) {
		toolBarManager.add(fEvalSelectionAction);
		toolBarManager.add(fEvalFileAction);
		toolBarManager.add(fEvalToplevelAction);
		toolBarManager.add(new Separator());
		toolBarManager.add(fMacroexpand);
		toolBarManager.add(fMacroexpand1);
	}

	/**
	 * @see EditorActionBarContributor#setActiveEditor(IEditorPart)
	 */
	@Override
	public void setActiveEditor(final IEditorPart targetEditor) 
	{
		if(targetEditor instanceof ILispEditor) {
			ILispEditor lispEditor = (ILispEditor)targetEditor;
			enableActions(lispEditor.hasInteractiveEvaluationSupport());
			forwardEditor(lispEditor);
		} else {
			enableActions(false);
		}
	}

	private void enableActions(final boolean enabled)
	{
		fEvalSelectionAction.setEnabled(enabled);
		fEvalFileAction.setEnabled(enabled);
		fEvalToplevelAction.setEnabled(enabled);
	}
	
	private void forwardEditor(final ILispEditor editor)
	{
		for(ILispEditorAction action : fAllActions) {
			action.setActiveEditor(editor);
			editor.getSite().getKeyBindingService().registerAction(action);
		}
	}
}
