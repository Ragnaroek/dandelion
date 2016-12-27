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

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PartInitException;

import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.project.ILispProject;
import de.defmacro.dandelion.internal.ui.*;
import de.defmacro.dandelion.internal.ui.editor.*;
import de.defmacro.dandelion.internal.ui.text.StructureException;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Die abstrakte Aktion fuer Evaluierungen.
 * @author Michael Bohn
 */
@SuppressWarnings("UwF")
public abstract class EvalAction 
extends Action
implements ILispEditorAction
{
	private ILispEditor fCurrentEditor;
	
	/**
	 * @see ILispEditorAction#setActiveEditor(ILispEditor)
	 */
	public void setActiveEditor(final ILispEditor editor) {
		this.fCurrentEditor = editor;
		setEnabled(editor.hasInteractiveEvaluationSupport());
	}

	/**
	 * Muss von abgeleiteten Klassen ueberschrieben werden.
	 * Die Quelltextauswahl muss hier zurueckgeliefert werden.
	 * Abhaengig von der konkreten Aktion.
	 * @return Die Quelltextauswahl
	 * @throws StructureException - wenn die Selektion ungueltig ist
	 */
	public abstract ILispSourceSelection getSelection()
	throws StructureException;
	
	/**
	 * Evaluierung die gewaehlte Selektion.
	 */
	@Override
	public void run() {
		ILispSourceSelection selection = null;
		try {
			selection = getSelection();
		} catch (StructureException e) {
			MessageDialog.openError(fCurrentEditor.getSite().getShell(), "Incomplete S-Expression", "Incomplete S-Expression in region");
			return;
		}
		
		if(doEval(selection)) {
			startEval(selection);
		}
	}
	
	protected ILispEditor getEditor()
	{
		return fCurrentEditor;
	}
	
	private boolean doEval(final ILispSourceSelection selection)
	{
		if( selection == null || selection.isEmpty() ) {
			return false;
		}
		
		if( !fCurrentEditor.hasInteractiveEvaluationSupport() ) {
			MessageDialog.openInformation(fCurrentEditor.getSite().getShell(), "Not a Lisp Project", "This Project is not a Lisp project, interactive" +
					"evaluation is deactivated. Create a Lisp project to evaluate directly in the editor" );
		}
		
		if( selection.hasErrors() ) {
			return MessageDialog.openQuestion(fCurrentEditor.getSite().getShell(), "Error in Selection", 
					"The selection contains errors, evaluate anyway?");
		}
		return true;
	}
	
	private void startEval(final ILispSourceSelection selection) 
	{
		try {
			ILispProject project = LispCore.getProjectManager().getLispProjectFor(fCurrentEditor.getInput().getProject());
			IBackgroundEvaluationListener listener = getBackgroundListener(project);
			IConnection connection = getConnection(project);
			Job job = createEvaluationJob(connection, listener, selection);
			job.schedule();
		} catch (PartInitException e) {
			LispUI.showErrorDialog(fCurrentEditor.getSite().getShell(), "Error opening ListenerView", e);
		} catch (ManagementException e) {
			LispUI.showErrorDialog(fCurrentEditor.getSite().getShell(), "Error retrieving project informatio", e);
		}
	}
	
	/**
	 * Holt die Verbindung fuer das aktuelle Projekt.
	 * @param project
	 * @return
	 * @throws ManagementException
	 * @throws ConnectionException
	 */
	private IConnection getConnection(final ILispProject project)
	throws ManagementException
	{	
		return LispCore.getEnvironmentManager().getConnectionFor(project.getEnvironment());
	}
	
	/**
	 * Erstellt den Eval-Job fuer die Ausfueherung dieser Aktion.
	 * Methode darf ueberschrieben werden.
	 * @param connection
	 * @param listener
	 * @param selection - Die angeliefert Selection ist auf jeden Fall nicht empty() oder null
	 * @return
	 */
	protected Job createEvaluationJob(final IConnection connection, final IBackgroundEvaluationListener listener, final ILispSourceSelection selection)
	{
		return new EvaluationJob(connection, selection.getForms(), listener);
	}
	
	/**
	 * Holt den BackgroundEvaluationListener fuer die Ausfuehrung dieser Aktion.
	 * Methode darf ueberschrieben werden.
	 * @param project
	 * @return
	 * @throws PartInitException
	 * @throws ManagementException
	 * @throws ConnectionException
	 */
	protected IBackgroundEvaluationListener getBackgroundListener(final ILispProject project) 
	throws PartInitException
	{
		return LispCore.getEnvironmentManager().openListenerFor(project.getEnvironment());
	}
}
