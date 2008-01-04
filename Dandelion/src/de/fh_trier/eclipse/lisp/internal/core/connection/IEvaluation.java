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

import org.eclipse.core.runtime.jobs.ILock;

/**
 * Schnittstelle fuer die Evaluierung eines Ausdrucks.
 * @author Michael Bohn
 */
public interface IEvaluation 
{
	/**
	 * Startet die Evaluierung. 
	 * @param pack - Package in dem evaluiert werden soll.
	 * @param form - Form die evaluiert werden soll
	 * @throws ConnectionException
	 */
	public void evalStart(String pack, String form)
	throws ConnectionException;
	
	/**
	 * Setzt den Zustand des Ausgabelesens zurueck.
	 */
	public void resetOutputState();
	
	/**
	 * Liest die naechste Ausgabezeile aus dem Output-Stream.
	 * Gibt <code>null</code> zurueck wenn das Terminierungssignal
	 * fuer das Ausgabeende gesendet wurde oder der Stream EOF meldet.
	 * 
	 * @return String - naechste Zeile der Ausgabe, oder <code>null</code> wenn
	 *        keine Zeile mehr vorhanden.
	 * @throws ConnectionException
	 */
	public String nextOutputLine()
	throws ConnectionException;
	
	/**
	 * Holt das Ergebnis der Evaluierung ab.
	 * @return <code>IResult</code> Ergebnis der Evaluierung
	 * @throws ConnectionException
	 */
	public IResult evalResult()
	throws ConnectionException;
	
	/**
	 * Gibt die das <code>IConnection</code> zurueck welches
	 * fuer die Evaluierung benutzt wird.
	 * @return IConnection - Das Verbindungsobjekt
	 */
	public IConnection getConnection();
	
	/**
	 * Gibt das Lock-Objekt fuer die Evaluierung zurueck.
	 * Verhindert das zwei Evaluierung gleichzeitig gestartet werden.
	 */
	public ILock getLock();
}
