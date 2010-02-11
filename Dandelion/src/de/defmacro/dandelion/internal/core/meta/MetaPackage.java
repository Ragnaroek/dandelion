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

package de.defmacro.dandelion.internal.core.meta;

import java.util.*;

/**
 * Ein Paket im Symbolspeicher.
 * Enthaelt Funktions- und Makrossymbole.
 * Nicht fuer Instanzierung ausserhalb der {@link ISymbolStore}-Implementierung gedacht.
 * @author Michael Bohn
 */
/*protected*/ class MetaPackage 
{
	private SortedSet<IMetaSymbol> fFunctionSymbols;
	private SortedSet<IMetaSymbol> fMacroSymbols;
	
	public MetaPackage()
	{
		fFunctionSymbols = Collections.synchronizedSortedSet(new TreeSet<IMetaSymbol>());
		fMacroSymbols = Collections.synchronizedSortedSet(new TreeSet<IMetaSymbol>());
	}
	
	/**
	 * Liefert alle Funktionssymbole.
	 * Das zurueckgegeben Objekt ist
	 * Thread-Safe.
	 * @return
	 */
	public SortedSet<IMetaSymbol> getFunctionSymbols()
	{
		return fFunctionSymbols;
	}
	
	/**
	 * Liefert alle Makrosymbole.
	 * Das zurueckgegeben Objekt ist
	 * Thread-Safe.
	 * @return
	 */
	public SortedSet<IMetaSymbol> getMacroSymbols()
	{
		return fMacroSymbols;
	}
}
