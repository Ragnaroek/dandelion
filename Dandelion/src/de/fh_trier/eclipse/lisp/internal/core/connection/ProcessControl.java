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

/**
 * Thread der einen gestarteten Prozess ueberwacht.
 */
public class ProcessControl 
extends Thread
{
	private final List<IProcessControlListener> fListener;
	private final Process fProcess;
	private final IEnvironment fServer;
	
	private OutputReader fErrReader;
	private OutputReader fOutReader;
	
	/**
	 * Erstellt eine neue {@link ProcessControl}.
	 * @param process - Der zu ueberwachende Prozess
	 * @param server - Die Lisp-Umgebung des Prozesses
	 * @throws NullPointerException - wenn process == <code>null</code>
	 */
	public ProcessControl(final Process process, final IEnvironment server) {
		this.fListener = new Vector<IProcessControlListener>();
		this.fProcess  = process;
		this.fServer   = server;
		fErrReader = new OutputReader(process.getErrorStream(), server, true);
		fOutReader = new OutputReader(process.getInputStream(), server, false);
	}
	
	/**
	 * Startet die Prozesskontrolle.
	 * Die Standard- und Err-Ausgaben Ueberwachung wird gestartet.
	 */
	@Override
	public synchronized void start() 
	{
		fErrReader.start();
		fOutReader.start();
		super.start();
	}

	/**
	 * Wartet auf das Prozessende.
	 * Benachrichtigt die angemeldeten Listener.
	 */
	@Override
	public void run() {
		try {
			int code = fProcess.waitFor();
			fireDied(code);
		} catch (InterruptedException e) {
			fireDied(-1);
		} finally {
			fErrReader.stopReading();
			fOutReader.stopReading();
		}
	}

	/**
	 * Prozess explizit beenden.
	 */
	public synchronized void kill()
	{
		fProcess.destroy();
	}
	
	/**
	 * Fuegt einen neuen {@link IProcessControlListener} hinzu.
	 * @param listener - Der neue Listener
	 */
	public synchronized void addProcessControlListener(final IProcessControlListener listener)
	{
		fListener.add(listener);
		fErrReader.addProcessControlListener(listener);
		fOutReader.addProcessControlListener(listener);
	}
	
	/**
	 * Entfernte einen {@link IProcessControlListener}.
	 * @param listener
	 */
	public synchronized void removeProcessControlListener(final IProcessControlListener listener)
	{
		fListener.remove(listener);
		fErrReader.removeProcessControlListener(listener);
		fOutReader.removeProcessControlListener(listener);
	}
	
	private void fireDied(int code)
	{
		for(IProcessControlListener listener : fListener) {
			listener.processDied(fServer, code);
		}
	}
}
