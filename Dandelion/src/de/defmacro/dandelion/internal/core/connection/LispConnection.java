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

import java.io.*;
import java.net.*;
import org.eclipse.core.runtime.jobs.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.connection.protocol.*;

/**
 * Standardimplementierung fuer das {@link IConnection}-Interface.
 * Die Implementierung ist Thread-Safe.
 * @author Michael Bohn
 */
public class LispConnection 
implements IConnection 
{
	/**
	 * Timeout am Ein-Ausgabe-Strom.
	 * value = 10000
	 */
	public static final int IO_CONNECT_TIMEOUT = 10000;
	
	private IEnvironment fServer;
	private IEvaluation fEvaluation;
	private Socket     fIOSocket; //lesen/schreiben IO-Lispimage
	private Socket     fConnectionSocket; //Protokollsocket
	private IProtocolReader fProtocolReader;
	private IProtocolWriter fProtocolWriter;
	private BufferedReader fIOReader;
	private BufferedWriter fIOWriter;
	private ILock fLock;
	
	/**
	 * Erstellt eine neue Verbindung zur Lisp-Umgebung.
	 * @param server - Die Lisp-Umgebung
	 */
	public LispConnection(final IEnvironment server)
	{
		if (server == null) {
			throw new NullPointerException("EvalServer must not be null");
		}
		
		this.fServer = server;
		this.fLock = Job.getJobManager().newLock();
	}
	
	/**
	 * Setzt die Verbindung auf disconnected zurueck.
	 * Danach kann die Verbindung wieder mit connect aufgebaut werden.
	 */
	private void resetConnectionState()
	{
		this.fEvaluation = null;
		
		try {
			//Reader + Writer nicht close, da diese Streams
			//den connectionSocket-Stream benutzen
			
			if(fIOSocket != null) {
				fIOSocket.close();
			}
			
			if(fConnectionSocket != null) {
				fConnectionSocket.close();
			}
			
		} catch (IOException e) {
			LispPluginActivator.logError("Error closing Streams", e);
		} finally {
			fEvaluation = null;
			fIOSocket = null;
			fConnectionSocket = null;
			fProtocolReader = null;
			fProtocolWriter = null;
			fIOReader = null;
			fIOWriter = null;
		}
	}
	
	/**
	 * @see de.defmacro.dandelion.internal.core.connection.IConnection#connect()
	 */
	public synchronized void connect() throws ConnectionException 
	{
		if( isConnected() ) {
			throw new IllegalStateException("Already connected");
		}
		
		try {
			fConnectionSocket = new Socket(fServer.getHost(), fServer.getPort());
			fConnectionSocket.setSoTimeout(IConnection.DEFAULT_TIMEOUT);
			
			ServerSocket ioServerSocket = new ServerSocket(0,0,InetAddress.getLocalHost());
			getProtocolWriter().writeConnect(InetAddress.getLocalHost().getHostAddress(), ioServerSocket.getLocalPort());
			
			//IO Verbindung vom Lisp-Image aufbauen
			ioServerSocket.setSoTimeout(IO_CONNECT_TIMEOUT);
			fIOSocket = ioServerSocket.accept();

			//ohne Fehler: erfolgreich verbunden zum Lisp-Image
		} catch (IOException e) {
			throw new ConnectionException(e);
		}
	}

	/**
	 * @see IConnection#setReadTimeout(int)
	 */
	public synchronized void setReadTimeout(final int timeout) 
	throws ConnectionException 
	{
		checkConnected();
		
		try {
			fConnectionSocket.setSoTimeout(timeout);
		} catch (SocketException e) {
			throw new ConnectionException("Errors setting Timeout", e);
		}
	}

	/**
	 * @see IConnection#isConnected()
	 */
	public synchronized boolean isConnected() 
	{
		return fConnectionSocket != null && fConnectionSocket.isConnected()
			   && fIOSocket != null && fIOSocket.isConnected();
	}

	/**
	 * @see IConnection#getInputStream()
	 */
	public synchronized InputStream getIOInputStream()
	throws ConnectionException
	{
		checkConnected();
		
		try {
			return fIOSocket.getInputStream();
		} catch (IOException e) {
			throw new ConnectionException("Error creating IOInputStream", e);
		}
	}

	/**
	 * @see IConnection#getIOInputStream()
	 */
	public synchronized OutputStream getIOOutputStream() 
	throws ConnectionException
	{
		checkConnected();
		
		try {
			return fIOSocket.getOutputStream();
		} catch (IOException e) {
			throw new ConnectionException("Error creating IOOutputStream", e);
		}
	}

	/**
	 * @see IConnection#getIOReader()
	 */
	public synchronized BufferedReader getIOReader() 
	throws ConnectionException 
	{
		if(fIOReader == null) {
			fIOReader = new BufferedReader(new InputStreamReader(getIOInputStream()));
		}
		return fIOReader;
	}

	/**
	 * @see IConnection#getIOWriter()
	 */
	public synchronized BufferedWriter getIOWriter() 
	throws ConnectionException 
	{
		if(fIOWriter == null) {
			fIOWriter = new BufferedWriter(new OutputStreamWriter(getIOOutputStream()));
		}
		return fIOWriter;
	}

	/**
	 * @see IConnection#getInputStream()
	 */
	public synchronized InputStream getInputStream() 
	throws ConnectionException 
	{
		//Aufruf ohne connected moeglich, da InputStream erst nach connect
		//erstellt wird
		
		if(fConnectionSocket == null) {
			throw new IllegalStateException("getInputStream called before connect");
		}
		
		try {
			return fConnectionSocket.getInputStream();
		} catch (IOException e) {
			throw new ConnectionException("Error creating InputStream", e);
		}
	} 

	/**
	 * @see IConnection#getOutputStream()
	 */
	public synchronized OutputStream getOutputStream() 
	throws ConnectionException 
	{
		if(fConnectionSocket == null) {
			throw new IllegalStateException("getOutputStream called before connected");
		}
		
		try {
			return fConnectionSocket.getOutputStream();
		} catch (IOException e) {
			throw new ConnectionException("Error creating OutputStream", e);
		}
	}
	
	/**
	 * @see IConnection#getProtocolReader()
	 */
	public synchronized IProtocolReader getProtocolReader() 
	throws ConnectionException 
	{
		if(fProtocolReader == null) {
			fProtocolReader = new ProtocolReader(this);
		}
		return fProtocolReader;
	}

	/**
	 * @see IConnection#getProtocolWriter()
	 */
	public synchronized IProtocolWriter getProtocolWriter() 
	throws ConnectionException 
	{
		if(fProtocolWriter == null) {
			fProtocolWriter = new ProtocolWriter(this);
		}
		return fProtocolWriter;
	}

	/**
	 * @see IConnection#disconnect()
	 */
	public synchronized void disconnect() throws ConnectionException 
	{
		checkConnected();
		
		try {
			getProtocolWriter().writeDisconnect();
			getProtocolReader().readSuccess(); //Fehler wenn nicht OK gesendet wird
		} finally {
			resetConnectionState(); //in jedem Fall Verbindung zuruecksetzen
		}
	}
	
	/**
	 * @see IConnection#getEvaluation()
	 */
	public synchronized IEvaluation getEvaluation()
	throws ConnectionException
	{
		checkConnected();
		
		if (fEvaluation == null) {
			fEvaluation = new LispEvaluation(this);
			
		}
		return fEvaluation;
	}
	
	/**
	 * @see IConnection#getEvalServer()
	 */
	public synchronized IEnvironment getEvalServer() {
		return fServer;
	}

	/**
	 * @see IConnection#getLock()
	 */
	public synchronized ILock getLock() {
		return fLock;
	}

	private void checkConnected()
	{
		if( !isConnected() ) {
			throw new IllegalStateException("Not connected");
		}
	}
}
