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

import org.eclipse.core.runtime.jobs.IJobChangeListener;

import de.fh_trier.eclipse.lisp.internal.core.connection.AbstractJob.JobType;

/**
 * Schnittstelle fuer einen Job-Handler.
 * Ein Job-Handler ist fuer die Verwaltung von Jobs verantwortlich.
 * @author Michael Bohn
 */
public interface IJobHandler 
extends IJobChangeListener
{
	/**
	 * Startet den uebergebenen Job.
	 * @param job - Der zu startende Job.
	 */
	public void startJob(final AbstractJob job);
	
	/**
	 * Entfernt einen noch nicht gestarteten Job aus der Warteschlange.
	 * @param job - Der zu entferneden Job.
	 */
	public void removeJob(final AbstractJob job);
	
	/**
	 * Gibt <code>true</code> zurueck wenn ein Job vom uebergeben Typ an 
	 * der uebergebenen Lisp-Umgebung auf eine Ausfuehrung wartet.
	 * @param type - Der Jobtype
	 * @param server - Die Lisp-Umgebung
	 * @return <code>true</code> wenn ein Job vom Typ an der Umgebung auf Ausfuehrung wartet.
	 */
	public boolean jobPending(final JobType type, final IEnvironment server);
}
