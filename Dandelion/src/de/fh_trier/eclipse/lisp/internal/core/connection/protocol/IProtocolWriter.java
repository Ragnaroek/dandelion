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

package de.fh_trier.eclipse.lisp.internal.core.connection.protocol;

import de.fh_trier.eclipse.lisp.internal.core.connection.*;

/**
 * Interface fuer das uebermitteln von Protokollkommandos.
 * @author Michael Bohn
 */
public interface IProtocolWriter 
{
	/**
	 * Schreibt das Connect-Kommando.
	 * @param ioHost - host fuer IO-Verbindung
	 * @param ioPort - port fuer IO-Verbindung
	 * @throws ProtocolException
	 */
	public void writeConnect(String ioHost, int ioPort)
	throws ProtocolException;
	
	/**
	 * Schreibt das Disconnect-Kommando.
	 * @throws ProtocolException
	 */
	public void writeDisconnect()
	throws ProtocolException;
	
	/**
	 * Schreibt das Eval-Kommaondo.
	 * @param pack - Paket in dem evaluiert werden soll
	 * @param form - Ausdruck der Evaluiert werden soll
	 * @throws ProtocolException
	 */
	public void writeEval(String pack, String form)
	throws ProtocolException;
	
	/**
	 * Meldet einen ausgewaehlten Restart an den Eval-Server.
	 * ABORT-Restart ueber eigene Funktion ausloesbar.
	 * @see IProtocolWriter#writeAbortRestart()
	 * @param restart - Der gewaehlte Restart
	 * @param restartArgs - <code>null</code> fuer keine Argumente
	 * @throws ProtocolException
	 */
	public void writeInvokeRestart(IRestart restart, String restartArgs)
	throws ConnectionException;
	
	/**
	 * Schreibt das Abort-Kommando.
	 * @throws ConnectionException
	 */
	public void writeAbortRestart()
	throws ConnectionException;
	
	/**
	 * Schreibt das Paket-Kommando.
	 * @throws ConnectionException
	 */
	public void writePackageRequest() 
	throws ConnectionException;
	
	/**
	 * Schreibt das Funktions-Kommando.
	 * @param pack - Paket aus dem die Symbole angefordert werden
	 * @throws ConnectionException
	 */
	public void writeFunctionRequest(String pack)
	throws ConnectionException;
	
	/**
	 * Schreibt das Makro-Kommando.
	 * @param pack - Paket aus dem die Symbole gelesen werden
	 * @throws ConnectionException
	 */
	public void writeMacroRequest(String pack)
	throws ConnectionException;
}
