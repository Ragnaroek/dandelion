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

package de.defmacro.dandelion.internal.ui.text;

import org.eclipse.jface.text.Position;

/**
 * Fehlermeldung fuer fehlerhafte Struktur im
 * Dokument.
 * @author Michael Bohn
 *
 */
public class StructureException 
extends Exception
{
	private static final long serialVersionUID = 7135739900105179040L;
	
	private Position fAtPosition;
	
	public StructureException(final Position position) {
		this(position, null);
	}

	public StructureException(final Position position, final String message) {
		super(message);
		fAtPosition = position;
	}

	public Position atPosition() {
		return fAtPosition;
	}
}
