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

package de.fh_trier.eclipse.lisp.internal.core.connection;

import java.util.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.jobs.Job;

import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.core.connection.IResult.TResult;
import de.fh_trier.eclipse.lisp.internal.ui.editor.PackageBoundForm;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Dieser Job nimmt eine Evaluierung an einer Lisp-Umgebung.
 * Zur Verwendung im Eclipse-Job-Manager.
 * @author Michael Bohn
 */
public class EvaluationJob 
extends AbstractEvaluationJob
{	
	/**
	 * Der Name des {@link EvaluationJob}
	 */
	public static final String JOB_NAME = "Evaluating";

	private List<PackageBoundForm> fForms;
	
	/**
	 * Erstellt einen neuen Evaluierungsjob.
	 * Der Job kann mit {@link Job#schedule()} zur Abarbeitung an den Job-Manager uebergeben werden.
	 * @param connection - Die Verbindung an dem der Job die Evaluierung vornimmt
	 * @param forms - Eine Liste von Ausdruecken die evaluiert werden soll
	 * @param listener - Der Listener der die Durchfuehrung der Abarbeitung des Jobs ueberwacht.
	 * @throws NullPointerException - wenn connection, forms oder listener <code>null</code>
	 * @throws IllegalArgumentException - wenn Liste der Ausdruecke leer
	 */
	public EvaluationJob(final IConnection connection, final List<PackageBoundForm> forms, final IBackgroundEvaluationListener listener) {
		super(connection, listener, JOB_NAME);
		if(forms.size() < 1) {
			throw new IllegalArgumentException("at least one form must be supplied");
		}
		init(forms);
	}
	
	/**
	 * Erstellt einen neuen Evaluierungsjob, fuer die Evaluierung eines Ausdrucks.
	 * @param connection - Die Verbindung an dem der Job die Evaluierung vornimmt
	 * @param form - Der Ausdrucke der ausgewertet werden soll
	 * @param listener - Der Listener der die Durchfuehrung der Abarbeitung des Jobs ueberwacht.
	 * @throws NullPointerException - wenn connection, form oder listener <code>null</code>
	 */
	public EvaluationJob(final IConnection connection, final PackageBoundForm form, final IBackgroundEvaluationListener listener)
	{
		super(connection, listener, JOB_NAME);
		
		List<PackageBoundForm> forms = new ArrayList<PackageBoundForm>(1);
		forms.add(form);
		init(forms);
	}
	
	/**
	 * Erstellt einen neuen Evaluierungsjob, fuer die Evaluierung eines Ausdrucks.
	 * @param eval - Die Verbindung an dem der Job die Evaluierung vornimmt
	 * @param pack - Das Paket in dem die Evaluierung durchgefuehrt werden soll
	 * @param form - Der Ausdrucke der ausgewertet werden soll
	 * @param listener - Der Listener der die Durchfuehrung der Abarbeitung des Jobs ueberwacht.
	 * @throws NullPointerException - wenn connection, pack, form oder listener <code>null</code>
	 */
	public EvaluationJob(final IConnection eval, 
						 final String pack, 
						 final String form, 
						 final IBackgroundEvaluationListener listener)
	{
		this(eval, new PackageBoundForm(pack, form), listener);
	}
	
	private void init(final List<PackageBoundForm> forms)
	{
		this.fForms = forms;
	}
	
	/**
	 * @see AbstractJob#getType()
	 */
	@Override
	public JobType getType() {
		return JobType.EVAL_JOB;
	}

	/**
	 * Ausfuehrung der Evaluierung.
	 * Jeder Ausdruck wird ueber die Verbindung ausgewertet und das Ergebnis der
	 * Evaluierung wird dem {@link IBackgroundEvaluationListener} mitgeteilt.
	 * @param monitor - der ProgressMonitor
	 * @see IBackgroundEvaluationListener
	 * @sse {@link AbstractJob#run0(IProgressMonitor)}
	 */
	@Override
	@SuppressWarnings("NP") //Rueckgabe von non-null Wert wird über Methoden-Contract geregelt
	protected IStatus run0(final IProgressMonitor monitor)
	throws Exception
	{	
		monitor.subTask("Waiting for connection");
	    acquireLock();
	    
		ensureConnected(fConnection);
		callPrepare(fForms.size() > 1);
		
		monitor.beginTask(fJobName, (fForms.size() + 1) * 1000);
		
		if(monitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}
		
		for(int i=0,n=fForms.size();i<n;i++) {
			
			PackageBoundForm form = fForms.get(i);
			monitor.subTask("Starting evaluation "+ i + "/" + fForms.size());
			IEvaluation eval = fConnection.getEvaluation();
			
			eval.evalStart(form.getPackage(), form.getForm());
			
			//Output Lesen
			readAllOutput(monitor);
			
			monitor.subTask("Reading result");
			IResult evalResult = eval.evalResult();
		
			boolean more = i != n-1;
			IRestartSelection selection = callFormEvaluated(evalResult, more);
			
			monitor.worked(1000);
			
			if( evalResult.getTyp() == TResult.SUCCESS )  { //Ergebnis ok
				continue; //alles ok mit ergebnis, mit naechstem weitermachen wenn vorhanden
			} else if( evalResult.getTyp() == TResult.READ_ERROR ) { //Read error, es darf kein abort gesendet werden
				break;
			} else if ( selection != null && selection.isAborted() ) {
				abort(); //Ein Fehler, kann nur EVAL_ERROR sein, wurde abgebrochen
				break;
			} else { //EVAL_ERROR und es wurde ein Restart gewaehlt, bzw. war verfuegbar
				boolean abort = restart(monitor, selection, more); //eintritt rekursiver restart
				if( abort ) { //restart abgebrochen
					abort(); //abort senden
					break; //und (bulk-)eval komplette beenden
				}
				continue; //restart war erfolgreich -> weiter mit naechstem im bulk
			}
		}
		
		return new Status(IStatus.OK, LispPluginActivator.PLUGIN_ID, IStatus.OK, "Evaluation succeeded", null);
	}
	
	/**
	 * Abbrechen der Auswertung.
	 * @throws ConnectionException
	 */
	private void abort()
	throws ConnectionException
	{
		IEvaluation eval = fConnection.getEvaluation();
		
		int cnt = 0;
		eval.getConnection().getProtocolWriter().writeAbortRestart();
		eval.resetOutputState();
		while(eval.nextOutputLine() != null) {
			cnt++;
		}
		eval.getConnection().getProtocolReader().readSuccess();
	}
	
	/**
	 * Fuehrt einen Restart am Server durch.
	 * @param monitor
	 * @param selection
	 * @param more
	 * @return true wenn abbruch
	 * @throws ConnectionException
	 */
	private boolean restart(final IProgressMonitor monitor, final IRestartSelection selection, final boolean more)
	throws ConnectionException
	{
		monitor.subTask("Invoking restart");
		fConnection.getProtocolWriter().writeInvokeRestart(selection.getRestart(), selection.getParameter());
		readAllOutput(monitor);
		monitor.subTask("Reading restart result");
		IResult result = fConnection.getEvaluation().evalResult();
		IRestartSelection restartSelection = callFormEvaluated(result, more);
		
		if( restartSelection == null) {
			return false;
		} else if( restartSelection.isAborted() ) {
			return true;
		} else {
			//rekursiver restart
			return restart(monitor, restartSelection, more);
		}
	}
}
