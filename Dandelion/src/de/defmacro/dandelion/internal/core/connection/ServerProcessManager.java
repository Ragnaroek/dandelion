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
import org.eclipse.core.runtime.jobs.*;

import de.defmacro.dandelion.internal.core.connection.AbstractJob.JobType;

/**
 * Verwaltet Prozesse zu Lisp-Umgebungen.
 * Die Implementierung ist Thread-Safe.
 * @author Michael Bohn
 */
public class ServerProcessManager 
extends JobChangeAdapter 
implements IProcessControlListener
{
	private final IJobHandler fJobHandler;
	private final Map<IEnvironment, ProcessControl> fRunningProcesses;
	
	/**
	 * Erzeugt einen neuen Manager.
	 * @param handler
	 */
	public ServerProcessManager(final IJobHandler handler) {
		this.fJobHandler = handler;
		this.fRunningProcesses = new Hashtable<IEnvironment, ProcessControl>();
	}
	
	/**
	 * Startet einen neuen Prozess ueber einen {@link StartupJob}.
	 * @param server - Umgebung die gestartet werden soll
	 * @param config - Informationen zum Start der Umgebung.
	 */
	public synchronized void startProcess(final IEnvironment server, final IEnvironmentConfiguration config)
	{
		if(!fJobHandler.jobPending(JobType.START_JOB, server)) {
			StartupJob job = new StartupJob(server, config);
			job.addJobChangeListener(this);
			fJobHandler.startJob(job);
		}
	}
	
	/**
	 * Beenden den Prozess der Umgebung.
	 * Die Listener werden benachrichtigt.
	 * @param server
	 */
	public synchronized void killProcess(final IEnvironment server)
	{
		ProcessControl control = fRunningProcesses.get(server);
		if(control != null) {
			control.kill();
		}
	}
	
	/**
	 * Liefert <code>true</code> wenn ein Prozess fuer diese Umgebung laueft.
	 * @param server - zu testende Umgebung
	 * @return
	 */
	public synchronized boolean isProcessRunning(final IEnvironment server)
	{
		return fRunningProcesses.containsKey(server);
	}

	/**
	 * Listener-Implementierung, nicht aufrufen.
	 */
	@Override
	public synchronized void done(final IJobChangeEvent event) {
		//kann nur StartupJob sein
		StartupJob job = (StartupJob)event.getJob();
		Process process = job.getProcess();
		
		if(process != null) {
			ProcessControl control = new ProcessControl(process, job.getEvalServer());
			control.addProcessControlListener(this);
			fRunningProcesses.put(job.getEvalServer(), control);
			control.start();
		}
	}

	/**
	 * Listener-Implementierung, nicht aufrufen.
	 */
	public synchronized void processDied(final IEnvironment server, final int code) {
		System.out.println("DEBUG: Process died");
		fRunningProcesses.remove(server);
	}

	/**
	 * Listener-Implementierung, nicht aufrufen.
	 */
	public synchronized void processOutput(final IEnvironment server, final String output) {
		System.out.println("DEBUG output="+output);
		//no-op
	}

	/**
	 * Listener-Implementierung, nicht aufrufen.
	 */
	public synchronized void processErrorOutput(final IEnvironment server, final String output) {
		System.out.println("DEBUG err="+output);
		//no-op
	}
}
