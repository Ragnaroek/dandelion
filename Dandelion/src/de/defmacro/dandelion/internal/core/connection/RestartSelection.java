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

/**
 * Standardimplementierung der {@link IRestartSelection}-Schnittstelle.
 * @author Michael Bohn
 * pattern: immutable
 */
public class RestartSelection 
implements IRestartSelection 
{
	/**
	 * Singleton-Instanz einer Abort-Auswahl.
	 */
	public static final IRestartSelection ABORT_SELECTION = new RestartSelection(true, null, null);
	
	private boolean fAborted;
	private IRestart fRestart;
	private String fParameter;

	/**
	 * Erstellt eine neue Restart-Auwahl.
	 * @see RestartSelection#ABORT_SELECTION fuer Abort-Restartauswahl.
	 * @param aborted - Abbrechen-Auswahl
	 * @param restart - Der gewaehlte Restart
	 * @param parameter - Die Parameter des Restarts.
	 */
	public RestartSelection(final boolean aborted, final IRestart restart, final String parameter)
	{
		this.fAborted = aborted;
		this.fRestart = restart;
		this.fParameter = parameter;
	}

	/**
	 * @see IRestartSelection#getParameter()
	 */
	public String getParameter() {
		return fParameter;
	}

	/**
	 * @see IRestartSelection#getRestart()
	 */
	public IRestart getRestart() {
		return fRestart;
	}

	/**
	 * @see IRestartSelection#isAborted()
	 */
	public boolean isAborted() {
		return fAborted;
	}
}