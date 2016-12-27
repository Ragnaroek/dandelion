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
 * Eine Lisp-Umgebung.
 * @author Michael Bohn
 */
public interface IEnvironment
extends Comparable<IEnvironment>
{
	/**
	 * Liefert den Host auf dem die Lisp-Umgebung laueft.
	 * @return Hostname oder IP-Adresse
	 */
	public String getHost();
	
	/**
	 * Liefert den Name der Lisp-Umgebung.
	 * @return Name der Lisp-Umgebung.
	 */
	public String getName();
	
	/**
	 * Liefert die Portnummer auf der die Lisp-Umgebung zu erreichen ist.
	 * @return Portnummer der Lisp-Umgebung.
	 */
	public int getPort();
	
	/**
	 * Liefert die Versionsnummer der Lisp-Umgebung.
	 * @return Versionsnummer der Lisp-Umgebung.
	 */
	public String getVersion();
}