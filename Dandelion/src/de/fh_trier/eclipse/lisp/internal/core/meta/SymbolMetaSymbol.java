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

package de.fh_trier.eclipse.lisp.internal.core.meta;

import java.util.*;

/**
 * Implementierung der {@link IMetaSymbol}-Schnittstelle fuer Symbole.
 * @author Michael Bohn
 */
public class SymbolMetaSymbol 
implements IMetaSymbol
{
	public static final String NO_DOCUMENTATION = "<no documentation available>";
	public static final String NO_ARGUMENTS     = "<no arguments>";
	protected static final List<String> EMPTY_ARGUMENTS = Collections.emptyList();

	private final String fPackage;
	private final String fSymbolName;
	private final TMetaType fType;
	
	/**
	 * Erstellt eine neues Meta-Symbol.
	 * Der Paket-String und Symbol-Name-String sind Case-Sensitive.
	 * Daher sym1 != SYM1. Bei der Erstellung des Symbols muss der Paket- bzw. Symbolname
	 * evlt. in Upper-Case umgewandelt werden. Eine automatische Umwandlung ist nicht
	 * moeglich da bei lokal definierten Symbolen zwischen dem Case unterschieden wird.
	 * 
	 * @param pack
	 * @param symbolName
	 * @param type
	 */
	public SymbolMetaSymbol(final String pack, final String symbolName, final TMetaType type)
	{
		if (pack == null) {
			throw new NullPointerException("pack must not be null");
		}

		if (symbolName == null) {
			throw new NullPointerException("symbolName must not be null");
		}
		
		if (type == null) {
			throw new NullPointerException("type must not be null");
		}
		
		this.fPackage = pack;
		this.fSymbolName = symbolName;
		this.fType = type;
	}
	
	/**
	 * Vergleich dieses MetaSymbols mit einem anderen.<br />
	 * Die Reihenfolge ist: <br />
	 * Symbolname<br />
	 * bei gleichem Symbolname: Packagename<br/>
	 * bei gleichem Symbol- und Packagename: Ordnungsnummer aus {@link TMetaType}
	 */
	public int compareTo(final IMetaSymbol sym2) 
	{	
		int c = this.getSymbolName().compareToIgnoreCase(sym2.getSymbolName());
		if(c != 0) {
			return c;
		}
		
		c = this.getPackage().compareTo(sym2.getPackage());
		if(c != 0) {
			return c;
		}
		
		return this.getType().getOrderNum() - sym2.getType().getOrderNum();
	}

	/**
	 * @see IMetaSymbol#getArgumentList()
	 */
	public List<String> getArgumentList() {
		return EMPTY_ARGUMENTS;
	}

	/**
	 * @see IMetaSymbol#getArgumentString(boolean)
	 */
	public String getArgumentString(final boolean format) {
		return null;
	}

	/**
	 * @see IMetaSymbol#getDocumentation()
	 */
	public String getDocumentation() {
		return null;
	}

	/**
	 * @see IMetaSymbol#getPackage()
	 */
	public String getPackage() {
		return fPackage;
	}

	/**
	 * @see IMetaSymbol#getSymbolName()
	 */
	public String getSymbolName() {
		return fSymbolName;
	}

	/**
	 * @see IMetaSymbol#getType()
	 */
	public TMetaType getType() {
		return fType;
	}

	/**
	 * @see IMetaSymbol#hasArguments()
	 */
	public boolean hasArguments() {
		return false;
	}

	/**
	 * Hashcode des Objektes
	 * Automatisch von Eclipse generiert.
	 */
	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result + ((fPackage == null) ? 0 : fPackage.hashCode());
		result = PRIME * result + ((fSymbolName == null) ? 0 : fSymbolName.hashCode());
		result = PRIME * result + ((fType == null) ? 0 : fType.hashCode());
		return result;
	}

	/**
	 * Test auf Gleichheit.
	 * Automatisch von Eclipse generiert.
	 */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final SymbolMetaSymbol other = (SymbolMetaSymbol) obj;
		if (fPackage == null) {
			if (other.fPackage != null)
				return false;
		} else if (!fPackage.equals(other.fPackage))
			return false;
		if (fSymbolName == null) {
			if (other.fSymbolName != null)
				return false;
		} else if (!fSymbolName.equals(other.fSymbolName))
			return false;
		if (fType == null) {
			if (other.fType != null)
				return false;
		} else if (!fType.equals(other.fType))
			return false;
		return true;
	}
}
