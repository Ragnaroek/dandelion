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

import org.eclipse.swt.widgets.Display;

import de.defmacro.dandelion.internal.core.connection.IResult.TResult;

/**
 * Die Listener-Schnittstelle zur Ueberwachung eines
 * {@link EvaluationJob}.
 * @author Michael Bohn
 */
public interface IBackgroundEvaluationListener
extends IOutputHandler
{	
	/**
	 * Vorbereitung auf eine Evaluierung.
	 * Wird einmalig vom {@link EvaluationJob} aufgerufen.
	 */
	public void prepareEval(IConnection connection, boolean bulk);
	
	/**
	 * Callback-Methode des {@link EvaluationJob}. Mit 
	 * dieser Methode wird dem {@link IBackgroundEvaluationListener} mitgeteilt
	 * das die uebergebene Form ausgewertet wurde.
	 * Wurde an den {@link EvaluationJob} mehrere Forms uebergeben (Bulk-Eval)
	 * wird diese Methode mehrfach aufgerufen.
	 * 
	 * Der Aufrufer darf nicht <code>null</code> zurueckgeben, wenn der Result-Typ = {@link TResult#EVAL_ERROR} oder
	 * {@link TResult#READ_ERROR} ist. Bei dem Ergebnistyp {@link TResult#SUCCESS} muss <code>null</code> zurueckgeliefert
	 * werden.
	 * 
	 * @param result - Ergebnis der Evaluierung
	 * @param more - Es folgt ein weiteres Ergebnis.
	 * @return null kein Restart aufrufen (bei {@link TResult#SUCCESS}, ansonsten abort-selection oder restart-selection.
	 */
	public IRestartSelection formEvaluated(IResult result, boolean more);
	
	/**
	 * Die Evaluierung wurde beendet.
	 * Wird einmalig vom {@link EvaluationJob} aufgerufen.
	 */
	public void finishEval();
	
	/**
	 * Die Callbacks des {@link EvaluationJob} werden an dem UI-Thread des zurueckgegeben Display ausgefuhert.
	 * Wird <code>null</code> zurueckgegeben erfolgt der Aufruf direkt vom {@link EvaluationJob}. Der
	 * Listener ist dann fuer eine noetige Synchronisiation verantwortlich. Wird eine Referenz auf ein
	 * {@link Display} geliefert, erfolgt der Callback aus dem UI-Thread des Displays.
	 * @return {@link Display} oder <code>null</code>
	 */
	public Display syncExecOnDisplay();
}
