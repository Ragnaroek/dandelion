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

import java.io.*;

import org.eclipse.core.runtime.jobs.ILock;

import de.fh_trier.eclipse.lisp.internal.core.connection.protocol.*;

/**
 * Repraesentiert eine Verbindung zu einer Lisp-Umgebung.
 * @author Michael Bohn
 */
public interface IConnection 
{
	/**
	 * Default-Timeout fuer Verbindungssocket.
	 * value=0
	 */
	public static final int DEFAULT_TIMEOUT = 0;
	
	/**
	 * Stellt die Verbindung zur Lisp-Umgebung her.
	 * @throws ConnectionException - bei Fehlschlag der Verbindungsherstellung.
	 * @throws IllegalStateException - wenn Verbindung bereits besteht
	 * @see IConnection#isConnected()
	 */
	public void connect() throws ConnectionException;
	
	/**
	 * Trennt die Verbindung zur Lisp-Umgebung. 
	 * Das Connection-Objekt
	 * bleibt weiterhin gueltig und kann mit {@link IConnection#connect()}
	 * wieder zum Eval-Server verbunden werden.
	 * @throws ConnectionException - bei Fehlschlag der Verbindungstrennung.
	 * @throws IllegalStateException - wenn Verbindung bereits getrennt.
	 */
	public void disconnect() throws ConnectionException;
	
	/**
	 * Setzt die maximale Blockierungszeit des Verbindungssocket.
	 * Default = {@link IConnection#DEFAULT_TIMEOUT}.
	 * @param timeout - neuer max. timeout
	 * @throws ConnectionException - bei illegalem timeout parameter.
	 */
	public void setReadTimeout(int timeout) throws ConnectionException;
	
	/**
	 * Liefert das zu dieser Verbindung gehoerende {@link IEvaluation}-Objekt.
	 * @return Evaluierungsobjekt
	 * @throws ConnectionException 
	 * @see {@link IEvaluation}
	 */
	public IEvaluation getEvaluation() throws ConnectionException;
	
	/**
	 * Gibt den Eingabestrom zur verbundenen Lisp-Umgebung zurueck.
	 * @return Der Eingabestrom
	 * @throws ConnectionException
	 */
	public InputStream getIOInputStream() throws ConnectionException;
	
	/**
	 * Gibt den Ausgabestrom zu verbundenen Lisp-Umgebung zurueck.
	 * @return Der Ausgabestrom
	 * @throws ConnectionException
	 */
	public OutputStream getIOOutputStream()throws ConnectionException;
	
	/**
	 * Gibt den Eingabestrom fuer die Protokollverbindung zureck.
	 * @return Eingabestrom zu Protokollverbindung
	 * @throws ConnectionException
	 * @see {@link IConnection#getProtocolReader()}
	 */
	public InputStream getInputStream() throws ConnectionException;
	
	/**
	 * Gibt den Ausgestrom fuer die Protokollverbindung zurueck.
	 * @return Ausgabestrom zur Protkollverbindung.
	 * @throws ConnectionException
	 * @see {@link IConnection#getProtocolWriter()}
	 */
	public OutputStream getOutputStream() throws ConnectionException;
	
	/**
	 * Gibt ein {@link Writer}-Objekt zum IO-Stream zurueck.
	 * @return Writer zum IO-Stream
	 * @throws ConnectionException
	 */
	public BufferedWriter getIOWriter() throws ConnectionException;
	
	/**
	 * Gibt ein {@link Reader}-Objekt zum IO-Stream zurueck.
	 * @return Reader zum IO-Stream
	 * @throws ConnectionException
	 */
	public BufferedReader getIOReader() throws ConnectionException;
	
	/**
	 * Gibt den {@link IProtocolReader} fuer diese Verbindung zurueck.
	 * @return Der ProtocolReader dieser Verbindung
	 * @throws ConnectionException
	 * @see {@link IProtocolReader}
	 */
	public IProtocolReader getProtocolReader() throws ConnectionException;
	
	/**
	 * Gibt den {@link IProtocolWriter} fuer diese Verbindung zurueck.
	 * @return Der ProtocolWriter dieser Verbindung.
	 * @throws ConnectionException
	 * @see {@link IProtocolWriter}
	 */
	public IProtocolWriter getProtocolWriter() throws ConnectionException;
	
	/**
	 * Liefert den Verbindungszustand zur Lisp-Umgebung.
	 * @return <code>true</code> wenn Verbindung besteht, sonst <code>false</code>
	 */
	public boolean isConnected();
	
	/**
	 * Liefert die zu dieser Verbindung gehoerende Lisp-Umgebung.
	 * @return Die Lisp-Umgebung
	 */
	public IEnvironment getEvalServer();
	
	/**
	 * Lock-Objekt fuer die Verbindung
	 * @return ILock - Lock-Objekt.
	 * @see ILock
	 */
	public ILock getLock();
}