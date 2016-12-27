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

import java.util.*;

import javax.annotation.Nonnull;

import org.eclipse.swt.widgets.Shell;

import de.defmacro.dandelion.internal.ui.dialogs.EvalFailureDialog;

/**
 * Standardimplementierung der {@link IResult}-Schnittstelle.
 * Instantzierung ueber Factory-Methoden.
 * @author Michael Bohn
 * pattern: immutable
 */
public class Result 
implements IResult
{
	/**
	 * Leere Restart-Liste, Aenderungen an dieser Liste sind nicht moeglich.
	 */
	public static final List<IRestart> EMPTY_RESTARTS = Collections.unmodifiableList(new ArrayList<IRestart>()); 
	
	/**
	 * Leere Mutliple-Value-Liste. Aenderungen an dieser Liste sind nicht moeglich.
	 */
	public static final List<String> EMPTY_MULTI_VALUE = Collections.unmodifiableList(new ArrayList<String>());
	
	private TResult fTyp;
	private String fPack;
	private String fResult;
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("UwF") //initalisiert in Factory-Methode
	private List<IRestart> fRestarts;
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("UwF") //initalisiert in Factory-Methode
	private List<String> fMultipleValues;
	private String fErrorDescription;
	
	private Result()
	{
		//Instanz nur ueber Factory Methoden erlaubt
	}
	
	/**
	 * Erstellt ein Read-Error Ergebnis.
	 * @param description - Fehlerbeschreibung.
	 * @return Read-Error Result
	 */
	public static IResult instanceOfReadError(final String description)
	{
		Result result = getFreshResult();
		result.fErrorDescription = description;
		result.fTyp = TResult.READ_ERROR;
		
		return result;
	}
	
	/**
	 * Erstellt ein Eval-Error Ergebnis.
	 * @param description - Fehlerbeschreibung.
	 * @param restarts - Verfuegbare Restart-Liste
	 * @return Eval-Error Result
	 * @throws NullPointerException - wenn restarts == <code>null</code>
	 */
	public static IResult instanceOfEvalError(final String description, final List<IRestart> restarts)
	{
		if (restarts == null) {
			throw new NullPointerException("restarts must not be null");
		}
		
		Result result = getFreshResult();
		result.fRestarts = restarts;
		result.fTyp = TResult.EVAL_ERROR;
		result.fErrorDescription = description;
		return result;
	}
	
	/**
	 * Erstellt ein Ergebnis fuer eine erfolgreiche Evaluierung.
	 * @param pack - paket nach ausfuehrung der Evaluierung
	 * @param resultForm - Hauptergebnis der Evaluierung
	 * @param multipleValueList - Multiple-Value-List des Ergebnisses 
	 * @return Result fuer erfolgreiche Evaluierung
	 * @throws NullPointerException - wenn pack, resultForm oder multipleValueList == <code>null</code>
	 */
	public static IResult instanceOfSuccess(final String pack, final String resultForm, final List<String> multipleValueList)
	{
		if (pack == null) {
			throw new NullPointerException("package must not be null");
		}

		if (resultForm == null) {
			throw new NullPointerException("resultForm must not be null");
		}
		
		if(multipleValueList == null) {
			throw new NullPointerException("multipleValueList must not be null");
		}
		
		Result result = getFreshResult();
		result.fTyp = TResult.SUCCESS;
		result.fPack = pack;
		result.fResult = resultForm;
		result.fMultipleValues = multipleValueList;
		
		return result;
	}
	
	private static Result getFreshResult()
	{		
		Result result = new Result();
		result.fRestarts = EMPTY_RESTARTS;
		result.fMultipleValues = EMPTY_MULTI_VALUE;
		return result;
	}
	
	/**
	 * @see IResult#getTyp()
	 */
	public TResult getTyp() {
		return fTyp;
	}

	/**
	 * @return Restart-Auwahl, niemals <code>null</code> zurueck.
	 * @see IResult#openRestartDialog(Shell)
	 */
	@Nonnull
	public IRestartSelection openRestartDialog(final Shell shell) 
	{
		final EvalFailureDialog dialog = new EvalFailureDialog(shell, fErrorDescription, fRestarts);
		final int code = dialog.open();
		
		return new RestartSelection(code == EvalFailureDialog.CODE_ABORT , dialog.getSelectedRestart(), dialog.getRestartParameters());
	}

	/**
	 * @see IResult#getPackage()
	 */
	public String getPackage() {
		return fPack;
	}
	
	/**
	 * @return Die zurueckgegebene List ist unveraenderlich.
	 * @see IResult#getRestarts()
	 */
	@Nonnull
	public List<IRestart> getRestarts() {
		return fRestarts;
	}

	/**
	 * @see IResult#getResult()
	 */
	public String getResult() {
		return fResult;
	}
	
	/**
	 * @see IResult#getMultipleValueResult()
	 */
	public List<String> getMultipleValueResult() {
		return fMultipleValues;
	}

	/**
	 * @see IResult#hasMultipleValues()
	 */
	public boolean hasMultipleValues() {
		return !fMultipleValues.isEmpty();
	}

	/**
	 * @see IResult#getErrorDescription()
	 */
	public String getErrorDescription() {
		return fErrorDescription;
	}

	/**
	 * String-Repraesenation eines Result-Objektes.
	 * Format der Ausgabe kann sich jederzeit aendern.
	 */
	@Override
	public String toString() {
		StringBuilder string = new StringBuilder();
		string.append("[Result: typ=");
		string.append(fTyp);
		string.append(" errorDescription=");
		string.append(fErrorDescription);
		string.append(" package=");
		string.append(fPack);
		string.append(" result=");
		string.append(fResult);
		string.append(" restarts=");
		string.append(fRestarts);
		string.append("]");
		
		return string.toString();
	}
}
