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
 * Standardimplementierung der {@link IRestart}-Schnittstelle.
 * @author Michael Bohn
 * pattern: immutable
 */
public class Restart 
implements IRestart 
{	
	private final String fName;
	private final String fDescription;
	
	/**
	 * Erzeugt einen neuen Restart.
	 * @param name
	 * @param description
	 */
	public Restart(final String name, final String description)
	{
		this.fName = name;
		this.fDescription = description;
	}
	
	/**
	 * @see IRestart#getDescription()
	 */
	public String getDescription() 
	{
		return fDescription;
	}

	/**
	 * @see IRestart#getName()
	 */
	public String getName() 
	{
		return fName;
	}

	@Override
	public int hashCode() 
	{
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result + ((fDescription == null) ? 0 : fDescription.hashCode());
		result = PRIME * result + ((fName == null) ? 0 : fName.hashCode());
		return result;
	}

	/**
	 * Gleich wenn Name und Beschreibung gleich.
	 */
	@Override
	public boolean equals(final Object obj) 
	{
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final Restart other = (Restart) obj;
		if (fDescription == null) {
			if (other.fDescription != null)
				return false;
		} else if (!fDescription.equals(other.fDescription))
			return false;
		if (fName == null) {
			if (other.fName != null)
				return false;
		} else if (!fName.equals(other.fName))
			return false;
		return true;
	}

	/**
	 * String-Repraesentation eines Restart-Objektes. 
	 * Das Format der Ausgabe kann sich jederzeit aendern.
	 */
	@Override
	public String toString() 
	{
		return "[Restart: name=" + fName + " description=" + fDescription + "]"; 
	}
}
