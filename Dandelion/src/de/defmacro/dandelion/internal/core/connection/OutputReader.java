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
import java.util.*;

/**
 * Thread der Ausgaben aus einem Stream liest.
 * @author Michael Bohn
 */
public class OutputReader 
extends Thread
{
	private final List<IProcessControlListener> fListener;
	private final BufferedReader fStream;
	private final IEnvironment fServer;
	private final boolean fErrReader;
	
	private boolean fReading;
	
	/**
	 * Erstellt einen neuen OutputReader.
	 * @param stream - Stream von dem die Ausgaben gelesen werden sollen.
	 * @param server - Betreffende Lisp-Umgebung.
	 * @param errReader - <code>true</code> wenn der uebergebene Strom ein Error-Stream ist
	 */
	public OutputReader(final InputStream stream, final IEnvironment server, final boolean errReader) {
		this.fListener = new Vector<IProcessControlListener>();
		this.fServer = server;
		this.fErrReader = errReader;
		this.fStream = new BufferedReader(new InputStreamReader(stream));
		this.fReading = true;
	}
	
	/**
	 * Lesen der Ausgaben.
	 * Wird beendet wenn der Stream geschlossen wird oder der OutputReader beendet wird.
	 */
	@Override
	public void run() {
		try {
			while(isReading()) {
				String output = fStream.readLine();
				if(output == null) { //stream beendet
					setReading(false);
					break; //schleife verlassen
				}
				//sonst: output an listener melden
				fireOutputRead(output, fErrReader);
			}
		} catch (IOException e) {
			setReading(false);
		} finally {
			if(fStream != null) {
				try {
					fStream.close();
				} catch (IOException e) {/*no-op*/}
			}
		}
	}
	
	private synchronized void fireOutputRead(final String output, final boolean err)
	{
		for(IProcessControlListener listener : fListener) {
			if(err) {
				listener.processErrorOutput(fServer, output);
			} else {
				listener.processOutput(fServer, output);
			}
		}
	}

	/**
	 * Lesen fortsetzen.
	 * Wird <code>false</code> uebergeben wird das Lesen vom Stream beendet und 
	 * der Stream geschlossen. 
	 * @param reading
	 */
	protected synchronized void setReading(final boolean reading)
	{
		this.fReading = reading;
	}
	
	/**
	 * Liest der Thread vom Strom.
	 * @return
	 */
	public synchronized boolean isReading()
	{
		return fReading;
	}
	
	/**
	 * Beendet das Lesen vom Stream.
	 */
	public synchronized void stopReading()
	{
		this.fReading = false;
		try {
			fStream.close(); //Stream schliessen, Thread faengt IOExecption, wichtig da stopReading sehr wahrscheinlich 
			                 //waehrend readLine() aufgerufen wird
		} catch (IOException e) {
			//no-op
		}
	}
	
	/**
	 * Fuegt einen neuen Listener hinzu. Der Listener bekommt Meldungen
	 * ueber die Ausgaben am Strom.
	 * @see IProcessControlListener
	 * @param listener
	 */
	public synchronized void addProcessControlListener(final IProcessControlListener listener)
	{
		fListener.add(listener);
	}
	
	/**
	 * Entfernt einene Listener.
	 * @param listener
	 */
	public synchronized void removeProcessControlListener(final IProcessControlListener listener)
	{
		fListener.remove(listener);
	}
}
