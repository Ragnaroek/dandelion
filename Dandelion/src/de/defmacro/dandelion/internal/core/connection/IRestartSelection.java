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
 * Schnittstelle zur Auswahl eines Restarts.
 * @author Michael Bohn
 */
public interface IRestartSelection 
{
	/**
	 * Der ausgewaehlte Restart.
	 * @return Der Restart
	 */
	public IRestart getRestart();
	
	/**
	 * Die Parameter die an den Restart uebergeben werden
	 * sollen.
	 * @return Parameter die der Restart erwartet.
	 */
	public String getParameter();
	
	/**
	 * Ein Abort-Restart.
	 * <code>true</code> wenn die Auswertung abgebrochen
	 * werden soll.
	 * @return
	 */
	public boolean isAborted();
}
