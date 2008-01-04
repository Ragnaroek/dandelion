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

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import de.fh_trier.eclipse.lisp.internal.core.connection.AbstractJob.JobType;

/**
 * Diese Klasse implementiert die Logik fuer die Ausfuehrung
 * von Jobs im Eclipse-Job-Manager.
 * @author Michael Bohn
 */
public class EnvironmentRule 
implements ISchedulingRule 
{
	private JobType fType;
	private IEnvironment fEvalServer;
	
	/**
	 * Erstellt eine neue Regel.
	 * @param server - Die Lisp-Umgebung an dem der Job die Ausfuehrung vornimmt
	 * @param type - Der Typ des Jobs
	 */
	public EnvironmentRule(final IEnvironment server, final JobType type)
	{
		if(server == null) {
			throw new NullPointerException();
		}
		
		if (type == null) {
			throw new NullPointerException("Type must not be null");
		}
		this.fType = type;
		this.fEvalServer = server;
	}
	
	/**
	 * Zurueckgefuhert auf {@link EnvironmentRule#isConflicting(ISchedulingRule)}
	 * @see ISchedulingRule#contains(ISchedulingRule).
	 */
	public boolean contains(final ISchedulingRule rule) {
		return isConflicting(rule);
	}

	/**
	 * Testet ob ein Konflikt zu der uebergebenen Regel besteht.
	 * Ein Konflikt besteht wenn: <br />
	 * Die uebergeben Regel ebenfalls eine {@link EnvironmentRule} ist <br />
	 * + Der Job am selben Lisp-Umgebung ausgefuhert wird <br />
	 * + Der Job vom selben Typ ({@link JobType}) ist <br />
	 * @see ISchedulingRule#isConflicting(ISchedulingRule)
	 */
	public boolean isConflicting(final ISchedulingRule rule) {
		if(rule == this) {
			return true;
		}
		//unbekannte Rule -> laut contract false zurueckgeben
		if( !(rule instanceof EnvironmentRule) ) {
			return false;
		}
		
		EnvironmentRule otherRule = (EnvironmentRule)rule;
		if(!otherRule.fEvalServer.equals(this.fEvalServer)) {
			return false; //zwei verschiedene Server -> auf keinen Fall ein Konflikt
		}
		
		//invariante: Job am selben EvalServer
		return this.fType != otherRule.fType;
	}
}
