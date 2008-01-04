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

import java.util.List;
import org.eclipse.swt.widgets.Shell;

/**
 * Schnittstelle fuer das Ergebnis einer Evaluierung.
 * @author Michael Bohn
 */
public interface IResult 
{
	/**
	 * Antworttypen fuer ein Eval-Ergebnis.
	 * @author Michael Bohn
	 */
	public enum TResult {
		SUCCESS,
		EVAL_ERROR,
		READ_ERROR,
	}
	
	/**
	 * Liefert das (erste) Ergebnis der Evaluierung.
	 * <code>null</code> wenn <code>getTyp()</code> <code>TResult.ERROR</code>
	 * liefert.
	 * @see IResult#getTyp
	 * @see IResult#hasMultipleValues
	 * @see IResult#getMultipleValueResult
	 * @return Ergebnis der Evaluierung oder <code>null</code> im Fehlerfall.
	 */
	public String getResult();
	
	/**
	 * Testet ob das Ergebnis mehrere Werte als Rueckgaben liefert.
	 * @see IResult#getMultipleValueResult
	 * @return boolean - <code>true</code> wenn multiple Werte vorhanden, sonst <code>false</code>
	 */
	public boolean hasMultipleValues();
	
	/**
	 * Gibt die multiplen Wert fuer das Ergebnis der Evaluierung zurueck.
	 * @return Multiple Value List
	 */
	public List<String> getMultipleValueResult();
	
	/**
	 * Oeffnet einen Dialog aus dem ein Restart ausgewaehlt werden kann.
	 * Das Ergebnis der Auswahl wird zurueckgeliefert. Der return-Wert kann 
	 * <code>null</code> sein, wenn keine Restarts verfuegbar sind und
	 * der Benutzer 'Abbrechen' ausgewaehlt hat. Sind Restarts verfuegbar
	 * wird der Benutzer gezwungen einen dieser Restarts auszuwaehlen.
	 * @param shell - uebergeordneten Shell-Objekt
	 * @return <code>IRestart</code> Ergebnis der Auswahl, oder <code>null</code> wenn
	 *         Benutzer 'Abbrechen' gewaehlt hat.
	 */
	public IRestartSelection openRestartDialog(Shell shell);
	
	/**
	 * Liefert den Typ des Ergebnisses. Erfolgreiche Evaluierung oder Fehler.
	 * @see IResult.TResult 
	 * @return {@link TResult} - Ergebnisstatus der Evaluierung.
	 */
	public TResult getTyp();
	
	/**
	 * Liefert den Package-String nachdem die Evaluierung durchgefuehrt wurde.
	 * Wert der globalen Lisp-Variable *package* nach der Evaluierung.
	 * @return <code>String</code> - aktuelles Package nach Evaluierung.
	 */
	public String getPackage();
	
	/**
	 * Liefert eine Liste der verfuegbaren Restart.
	 * Ist Typ == <code>TResult.SUCCESS</code> wird eine leere List zurueck
	 * gegeben. Die zurueckgegebene Liste ist unveraenderlich.
	 * @return Liste der verfuegbaren Restarts
	 */
	public List<IRestart> getRestarts();
	
	/**
	 * Gibt die Fehlerbeschreibung bei fehlerhaftem Ergebnis zurueck.
	 * @return <code>null</code> wenn {@link IResult#getTyp()} {@link TResult#SUCCESS} liefert.
	 */
	public String getErrorDescription();
}	
