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

import org.eclipse.jface.text.Position;


/**
 * Standardimplementierung der {@link ISyntacticalMalformation}-Schnittstelle.
 * @author Michael Bohn
 */
public class Malformation 
implements ISyntacticalMalformation, Comparable<Malformation>
{
	private final TSeverity fSeverity;
	private final Position  fPosition;
	private final String    fDescription;
	
	/**
	 * Erstellt ein neues Fehlerobjekt.
	 * @param severity - Der Schweregrad
	 * @param position - Die Position des Fehlers
	 * @param description - Die Beschreibung des Fehlers
	 */
	public Malformation(final TSeverity severity, final Position position, final String description)
	{
		if (severity == null) {
			throw new NullPointerException("severity must not be null");
		}
		
		if (position == null) {
			throw new NullPointerException("position must not be null");
		}

		if (description == null) {
			throw new NullPointerException("description must not be null");
		}
		
		this.fSeverity = severity;
		this.fPosition = position;
		this.fDescription = description;
	}
	
	/**
	 * @see ISyntacticalMalformation#getDescription()
	 */
	public String getDescription() 
	{
		return fDescription;
	}

	/**
	 * @see ISyntacticalMalformation#getPosition()
	 */
	public Position getPosition() 
	{
		return fPosition;
	}

	/**
	 * @see ISyntacticalMalformation#getSeverity()
	 */
	public TSeverity getSeverity() 
	{
		return fSeverity;
	}

	public int compareTo(final Malformation o) {
		if(o == this) {
			return 0;
		}
		return -1;
	}
}
