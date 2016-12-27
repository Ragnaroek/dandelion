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

package de.defmacro.dandelion.internal.core.connection;

import org.eclipse.core.runtime.*;
import org.eclipse.swt.widgets.Display;

/**
 * Diese Klasse buendelt allgemeine Methoden fuer alle
 * Jobs die Evaluierungen vornehmen.
 * @author Michael Bohn
 */
public abstract class AbstractEvaluationJob 
extends AbstractJob
{
	private final class RunFormEvaluated 
	implements Runnable {
		private final boolean fMore;
		private final IResult fResult;
		private IRestartSelection fSelection;
		
		private RunFormEvaluated(final boolean more, final IResult result) {
			this.fMore = more;
			this.fResult = result;
			this.fSelection = null;
		}

		public void run() {
			this.fSelection = fResultReceiver.formEvaluated(fResult, fMore);
		}
		
		public IRestartSelection getSelection()
		{
			return fSelection;
		}
	}

	/**
	 * Das Verbindungsobjekt an dem die Evaluierung durchgefuehrt wird.
	 */
	protected final IConnection fConnection;
	
	/**
	 * Der Listener der die Durchfuehrung der Evaluierung ueberwacht. 
	 */
	protected final IBackgroundEvaluationListener fResultReceiver;
	
	/**
	 * Der Name des Jobs.
	 */
	protected final String fJobName;
	
	/**
	 * Initialisiert die protected-Variablen mit den uebergegebenen Objekten.
	 * @param connection - Das Verbindungsobjekt
	 * @param listener - Der Listener
	 * @param jobName - Der Name des Jobs
	 */
	public AbstractEvaluationJob(final IConnection connection, final IBackgroundEvaluationListener listener, final String jobName) {
		super(jobName, connection.getEvalServer());
		
		if (listener == null) {
			throw new NullPointerException("listener must not be null");
		}
		
		this.fJobName = jobName;
		this.fConnection = connection;
		this.fResultReceiver = listener;
	}
	
	/**
	 * Der Job wird ausgefuehrt.
	 * In dieser Methode wird die (abstrakte) Methode {@link AbstractEvaluationJob#run0(IProgressMonitor)}
	 * die von abgeleiteten Klassen implementiert werden muss.
	 * Stellt sicher das immer monitor.done(), callFinish() und releaseLock() aufgerufen wird.
	 * @param monitor
	 */
	@Override
	protected IStatus run(IProgressMonitor monitor) 
	{
		if(monitor == null) {
			monitor = new NullProgressMonitor();
		}
		
		if(monitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}
		
		IStatus status = null;
		try {
			status = run0(monitor);	
		} catch (Exception e) {
			status = error("Error in background evaluation", e);
			//status = new Status(IStatus.ERROR, LispPluginActivator.PLUGIN_ID, IStatus.ERROR, , e);
		} finally {
			monitor.done();
			callFinish();
			releaseLock();
		}
		
		return status;
	}

	/**
	 * Die run-Methode die von abgeleiteten Klassen implementiert werden muss.
	 * Stellt sicher das immer monitor.done(), callFinish() und releaseLock() aufgerufen wird.
	 * @return Status der Ausfuehrung
	 * @param monitor - der ProgressMonitor, garantiert nicht <code>null</code>
	 * @see AbstractEvaluationJob#run(IProgressMonitor)
	 * @throws Exception
	 */
	protected abstract IStatus run0(final IProgressMonitor monitor)
	throws Exception;
	
	/**
	 * Liest die Ausgaben am Verbindungsobjekt.
	 * Die Ausgaben werden gelesen bis das Terminierungssignal gesendet wurde.
	 * @param monitor
	 * @throws ConnectionException - Bei fehlerhaftem Lesen vom Verbindungsobjekt
	 */
	protected void readAllOutput(final IProgressMonitor monitor)
	throws ConnectionException
	{
		monitor.subTask("Reading output");
		String outputLine = null;
		IEvaluation eval = fConnection.getEvaluation();
		eval.resetOutputState();
		while((outputLine = eval.nextOutputLine()) != null) {
			//TODO Cancel abfragen -> Endlose Ausgabe abbrechen ermoeglichen, connection muss dann aber evtl. geresetet werden?
			callHandleOutput(outputLine);
		}
	}
	
	/**
	 * Sperrt das Verbindungsobjekt.
	 */
	protected void acquireLock()
	{
		fConnection.getLock().acquire();
	}
	
	/**
	 * Gibt das Verbindungsobjekt wieder frei.
	 */
	protected void releaseLock()
	{
		fConnection.getLock().release();
	}
	
	private Display getDisplay()
	{
		return fResultReceiver.syncExecOnDisplay();
	}
	
	/**
	 * Benachrichtigt den {@link IBackgroundEvaluationListener} das die Evaluierung
	 * gestart wird. Stellt sicher das die Benachrichtigung aus dem UI-Thread erfolgt, wenn
	 * der Listener ein Display fuer die Ausfuehrung bereitstellt.
	 * @param bulk - Ein oder mehrere Evaluierung werden durchgefuehrt
	 */
	protected void callPrepare(final boolean bulk)
	{
		Display display = getDisplay();
		if(display == null) {
			fResultReceiver.prepareEval(fConnection, bulk);
		} else {
			if(display.isDisposed()) return;
			display.syncExec(new Runnable() {
				public void run() {
					fResultReceiver.prepareEval(fConnection, bulk);
				}
			});
		}
	}
	
	/**
	 * Benachrichtigt den {@link IBackgroundEvaluationListener} ueber eine Ausgabe waehrend
	 * der Evaluierung. Stellt sicher das die Benachrichtigung aus dem UI-Thread erfolgt, wenn
	 * der Listener ein Display fuer die Ausfuehrung bereitstellt.
	 * @param out - Die Ausgabezeile
	 */
	protected void callHandleOutput(final String out)
	{
		Display display = getDisplay();
		if(display == null) {
			fResultReceiver.handleOutput(out);
		} else {
			if(display.isDisposed()) return;
			display.syncExec(new Runnable() {
				public void run() {
					fResultReceiver.handleOutput(out);
				}
			});
		}
	}
	
	/**
	 * Benachrichtigt den {@link IBackgroundEvaluationListener} ueber das Ergebnis einer
	 * Evaluierung. Stellt sicher das die Benachrichtigung aus dem UI-Thread erfolgt, wenn
	 * der Listener ein Display fuer die Ausfuehrung bereitstellt.
	 * @param result - das Ergebnis der Evaluierung
	 * @param more - <code>true</code> wenn weitere Ergebnismeldungen folgen koennen.
	 * @return Die Restart-Auswahl des Listeners bei fehlerhafter Evaluierung.
	 */
	protected IRestartSelection callFormEvaluated(final IResult result, final boolean more)
	{
		Display display = getDisplay();
		IRestartSelection selection = null;
		if(display == null) {
			selection =  fResultReceiver.formEvaluated(result, more);
		} else {
			if(display.isDisposed()) return RestartSelection.ABORT_SELECTION;
			RunFormEvaluated run = new RunFormEvaluated(more, result);
			display.syncExec(run); //SYNC-EXEC
			selection = run.getSelection();
		}
		return selection;
	}
	
	/**
	 * Benachrichtigt den {@link IBackgroundEvaluationListener} ueber die Fertigstellung
	 * aller Evaluierungen. Stellt sicher das die Benachrichtigung aus dem UI-Thread erfolgt, wenn
	 * der Listener ein Display fuer die Ausfuehrung bereitstellt.
	 */
	protected void callFinish()
	{
		Display display = getDisplay();
		if(display == null) {
			fResultReceiver.finishEval();
		} else {
			if(display.isDisposed()) return;
			display.syncExec(new Runnable() {
				public void run() {
					fResultReceiver.finishEval();
				}
			});
		}
	}
	
	
}