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

//TODO Refactoring in Environment

/**
 * Standardimplementierung des {@link IEnvironment}-Interface. 
 */
public class Environment 
implements IEnvironment 
{
	private String fHost;
	private int fPort;
	private String fName;
	private String fVersion;
	
	/**
	 * Initialisiert das Objekt.
	 * @param host - Hostname oder IP-Adresse der Umgebung
	 * @param port - Portnummer der Umgebung
	 * @param name - Name der Umgebung
	 * @param version - Versionsnummer der Umgebung
	 * @throws NullPointerException - wenn host, port, name oder version == <code>null</code>
	 */
	public Environment(final String host, final Integer port, final String name, final String version)
	{
		if (host == null) {
			throw new NullPointerException("host must not be null");
		}
		
		if (port == null) {
			throw new NullPointerException("port must not be null");
		}
		
		if (name == null) {
			throw new NullPointerException("name must not be null");
		}

		if (version == null) {
			throw new NullPointerException("version must not be null");
		}
		
		if(port < 0 || port > 65535) {
			throw new IllegalArgumentException("Illegal port range " + port);
		}
		
		this.fHost = host;
		this.fPort = port;
		this.fName = name;
		this.fVersion = version;
	}

	/**
	 * @see de.defmacro.dandelion.internal.core.connection.IEnvironment#getHost()
	 */
	public String getHost() {
		return fHost;
	}

	/**
	 * @see de.defmacro.dandelion.internal.core.connection.IEnvironment#getName()
	 */
	public String getName() {
		return fName;
	}

	/**
	 * @see de.defmacro.dandelion.internal.core.connection.IEnvironment#getPort()
	 */
	public int getPort() {
		return fPort;
	}

	/**
	 * @see de.defmacro.dandelion.internal.core.connection.IEnvironment#getVersion()
	 */
	public String getVersion() {
		return fVersion;
	}

	/**
	 * Liefert eine String-Repraesentation der Umgebung.
	 * Die String-Repraesentation besteht aus dem Umgebungsnamen und der Versionsnummer.
	 */
	@Override
	public String toString() {
		return fName + " [" + fVersion + "]";
	}

	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result + ((fHost == null) ? 0 : fHost.hashCode());
		result = PRIME * result + ((fName == null) ? 0 : fName.hashCode());
		result = PRIME * result + fPort;
		result = PRIME * result + ((fVersion == null) ? 0 : fVersion.hashCode());
		return result;
	}
	
	/**
	 * Vergleich der Lisp-Umgebung mit einer anderen.
	 * Der Vergleich basiert auf der Rueckgabe der {@link Environment#toString()}-Methode
	 * <p \>
	 * <strong>Notiz:</strong> Diese Klasse besitzt eine natuerliche Ordnung die inkonsitent mit 
	 * der equals-Methode ist.
     * inconsistent with equals.
     * @see Comparable
	 */
	public int compareTo(final IEnvironment o) {
		return toString().compareTo(o.toString());
	}

	/**
	 * Test auf Gleichheit.
	 * Eine Umgebung ist gleich wenn host, name, port und version gleich sind.
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final Environment other = (Environment) obj;
		if (fHost == null) {
			if (other.fHost != null)
				return false;
		} else if (!fHost.equals(other.fHost))
			return false;
		if (fName == null) {
			if (other.fName != null)
				return false;
		} else if (!fName.equals(other.fName))
			return false;
		if (fPort != other.fPort)
			return false;
		if (fVersion == null) {
			if (other.fVersion != null)
				return false;
		} else if (!fVersion.equals(other.fVersion))
			return false;
		return true;
	}
}
