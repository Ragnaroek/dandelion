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

import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.jobs.Job;
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;

/**
 * Abstrakte Basisklasse fuer Jobs die Aktionen
 * an einer Lisp-Umgebung durchfuehren.
 * @author Michael Bohn
 */
public abstract class AbstractJob 
extends Job 
{
	/**
	 * Konstaten fuer die Darstellung einer Jobart.
	 * @author Michael Bohn
	 */
	public enum JobType
	{
		START_JOB(0),
		INIT_JOB(2),
		EVAL_JOB(3);
		
		
		private final int fPriority;
		
		JobType(final int priority)
		{
			this.fPriority = priority;
		}
		
		/**
		 * Liefert die Prioritaet dieses Jobtyps.
		 * Eine kleinere Zahl bedeutet eine hoehere Prioritaet.
		 * @return
		 */
		public int getPriority()
		{
			return fPriority;
		}
	}
	
	private IEnvironment fServer;
	private boolean fDidConnect;
	
	/**
	 * Initialisiert einen neuen Umgebungsjob.
	 * @param jobName - Name des Jobs
	 * @param server - {@link IEnvironment}-Objekt an dem die Aktion durchgefuehrt wird.
	 */
	public AbstractJob(final String jobName, final IEnvironment server)
	{
		super(jobName);
		setSystem(false);
		this.fServer = server; //null-check in EvalServer Rule
		setRule(new EnvironmentRule(server, getType()));
	}
	
	/**
	 * @see Job
	 */
	@Override
	protected abstract IStatus run(IProgressMonitor monitor);
	
	/**
	 * Liefert den Typ des Jobs.
	 * @return {@link JobType} der Typ des Jobs
	 */
	public abstract JobType getType();
	
	/**
	 * Liefert <code>true</code> wenn dieser Job die Verbindung hergestellt hat
	 * vor der Verwendung.
	 * @return
	 */
	public boolean hasConnected()
	{
		return fDidConnect;
	}
	
	/**
	 * Liefert die Lisp-Umgebung des Jobs.
	 * @return
	 */
	public IEnvironment getEvalServer()
	{
		return fServer;
	}
	
	/**
	 * @see Job#belongsTo(Object)
	 */
	@Override
	public boolean belongsTo(final Object family) 
	{	
		if( !(family instanceof IEnvironment) ) {
			return false;
		}
		//invariante != null
		return ((IEnvironment)family).equals(fServer);
	}
	
	/**
	 * Stellt eine hergestellt Verbindung zur Lisp-Umgebung sicher.
	 * Alle abgeleiteten Klassen sollten diese Methode zur Verbindungsherstellung verwenden.
	 * @param connection - Das Verbindungsobjekt
	 * @throws ConnectionException - wenn ein herstellen der Verbindung nicht moeglich ist
	 */
	protected void ensureConnected(final IConnection connection)
	throws ConnectionException
	{
		if(!connection.isConnected()) {
			connection.connect();
			fDidConnect = true;
		}
	}
	
	/**
	 * Erzeugt ein {@link IStatus}-Objekt fuer einen aufgetretenen Fehler.
	 * @param message - Die Fehlerbeschreibung
	 * @param e - Das Fehlerobjekt
	 * @return Ein {@link IStatus}-Objekt
	 */
	protected IStatus error(final String message, final Exception e) {
		return new Status(IStatus.ERROR, LispPluginActivator.PLUGIN_ID, 0, message, e);
	}
}
