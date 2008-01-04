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

package de.fh_trier.eclipse.lisp.internal.core.dom;

import java.util.*;

/**
 * Schnittstelle fuer Klassen die
 * Fehler speichern.
 * @author Michael Bohn
 */
public interface IMalformationProvider 
{
	/**
	 * Abfrage ob Fehler vorhanden.
	 * @return
	 */
	public boolean hasMalformation();
	
	/**
	 * Abfrage ob Fehler von bestimmten Typ vorhanden.
	 * @param severity
	 * @return
	 */
	public boolean hasMalformation(TSeverity severity);
	
	/**
	 * Fuegt einen neuen Fehler hinzu.
	 * @param malformation
	 */
	public void addMalformation(ISyntacticalMalformation malformation);
	
	/**
	 * Fuegt eine Menge von Fehlern hinzu.
	 * @param malformations
	 */
	public void addAll(Collection<ISyntacticalMalformation> malformations);
	
	/**
	 * Liefert alle in diesem Speicher vorhanden Fehler.
	 * @return
	 */
	public List<ISyntacticalMalformation> getMalformations();
	
	/**
	 * Loescht alle Fehler.
	 */
	public void clearMalformations();
}
