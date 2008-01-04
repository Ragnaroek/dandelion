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
 * Funktionsmetasymbol.
 * @author Michael Bohn
 * pattern: immutable
 * testcase
 */
public class FunctionMetaSymbol 
extends SymbolMetaSymbol
{
	private final String fDocumentation;
	private final List<String> fArgumentList;
	
	/**
	 * Erstellt ein neues Funktionsmetasymbol.
	 * @param pack - Paket in dem sich Symbol befindet
	 * @param symbolName - Name des Symbols
	 * @param type - Typ des Symbols
	 * @throws NullPointerException - wenn pack, symbolName oder type == <code>null</code>
	 */
	public FunctionMetaSymbol(final String pack, final String symbolName, final TMetaType type)
	{
		this(pack, symbolName, null, null, type);
	}
	
	/**
	 * Erstellt ein neues Funktionsmetasymbol.
	 * Der Dokumentationsstring und die Argumenten-Liste duerfen <code>null</code> sein.
	 * @param symbolName
	 * @param documentation
	 * @param argumentList
	 * @throws NullPointerException - wenn symbolName == <code>null</code>
	 */
	public FunctionMetaSymbol(final String pack, final String symbolName, final String documentation, final List<String> argumentList, final TMetaType type) 
	{
		super(pack, symbolName, type);
		
		if(documentation == null) {
			this.fDocumentation = NO_DOCUMENTATION;
		} else {
			this.fDocumentation = documentation;
		}
		
		if(argumentList == null) {
			this.fArgumentList = EMPTY_ARGUMENTS;
		} else {
			this.fArgumentList = Collections.unmodifiableList(argumentList);
		}
		
	}

	/**
	 * @see IMetaSymbol#hasArguments()
	 */
	@Override
	public boolean hasArguments() {
		return !fArgumentList.isEmpty();
	}

	/**
	 * Die zurueckgelieferte Liste ist immutable.
	 * @see IMetaSymbol#getArgumentList()
	 */
	@Override
	public List<String> getArgumentList() {
		return fArgumentList;
	}
	
	/**
	 * @see IMetaSymbol#getArgumentString(boolean)
	 */
	@Override
	public String getArgumentString(final boolean format) 
	{	
		String newline = format ? "\n" : "";
		
		if(!hasArguments()) {
			return NO_ARGUMENTS + newline;
		}
		
		StringBuilder buffer = new StringBuilder();
		for(int i=0; i<fArgumentList.size(); i++) {
			String arg = fArgumentList.get(i);
			if(arg.startsWith("&") && i != 0) {
				buffer.append(newline);
			}
			buffer.append(arg);
			buffer.append(" ");
		}
		buffer.append(newline);
		return buffer.toString();
	}

	/**
	 * @see IMetaSymbol#getDocumentation()
	 */
	@Override
	public String getDocumentation() {
		return fDocumentation;
	}

	/**
	 * Hashcode des Objektes
	 * Automatisch von Eclipse generiert.
	 */
	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = super.hashCode();
		result = PRIME * result + ((fArgumentList == null) ? 0 : fArgumentList.hashCode());
		result = PRIME * result + ((fDocumentation == null) ? 0 : fDocumentation.hashCode());
		return result;
	}

	/**
	 * Test auf Gleichheit.
	 * Automatisch von Eclipse generiert.
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		final FunctionMetaSymbol other = (FunctionMetaSymbol) obj;
		if (fArgumentList == null) {
			if (other.fArgumentList != null)
				return false;
		} else if (!fArgumentList.equals(other.fArgumentList))
			return false;
		if (fDocumentation == null) {
			if (other.fDocumentation != null)
				return false;
		} else if (!fDocumentation.equals(other.fDocumentation))
			return false;
		return true;
	}
}
