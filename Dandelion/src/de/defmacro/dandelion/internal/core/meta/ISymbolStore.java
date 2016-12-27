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
 * Schnittstelle fuer einen Symbolspeicher.
 * @author Michael Bohn
 */
public interface ISymbolStore 
{	
	/**
	 * Fuegt ein neues Symbol zum Symbolspeicher.
	 * 
	 * @param pack
	 */
	public void addPackage(String pack);
	
	/**
	 * Fuegt ein neues Funktionsysmbol zum Symbolspeicher.
	 * Das Paket wird dem Meta-Symbol entnommen.
	 * @param symbol
	 */
	public void internFunctionSymbol(IMetaSymbol symbol);
	
	/**
	 * Fuegt eine Liste von Funktionssymbolen ins angegebene Paket
	 * des Symbolspeichers.
	 * @param pack
	 * @param symbols
	 */
	public void internFunctionSymbol(String pack, List<IMetaSymbol> symbols);
	
	/**
	 * Fuegt ein neues Makrosysmbol zum Symbolspeicher.
	 * Das Paket wird dem Meta-Symbol entnommen.
	 * @param symbol
	 */
	public void internMacroSymbol(IMetaSymbol symbol);
	
	/**
	 * Fuegt eine Liste von Makrosymbolen ins angegebene Paket
	 * des Symbolspeichers.
	 * @param pack
	 * @param symbol
	 */
	public void internMacroSymbol(String pack, List<IMetaSymbol> symbol);
	
	/**
	 * Test ob Symbolspeicher leer.
	 * @return
	 */
	public boolean isEmpty();
	
	/**
	 * Liefert <code>true</code> zurueck wenn der Store komplett initialisiert wurde.
	 * @return
	 */
	public boolean isInitialized();
	
	/**
	 * Setzt den Intialisierungszustand des Speichers.
	 * @param init
	 */
	public void setInitialized(boolean init);
	
	public boolean hasPackage(String pack);
	
	/**
	 * Liefert eine Liste aller vorhandenen Pakete.
	 * @return
	 */
	public SortedSet<String> getPackages();
	/**
	 * Liefert die Liste aller vorhandenen Pakete als Meta-Symbole.
	 * @return
	 */
	public SortedSet<IMetaSymbol> getPackagesAsMetaSymbol();
	
	/**
	 * Liefert eine Liste aller vorhandenen Pakete die mit prefix
	 * beginnen.
	 * @param prefix
	 * @return
	 */
	public SortedSet<IMetaSymbol> getPackagesAsMetaSymbol(String prefix);
	
	/**
	 * Liefert die Anzahl der vorhandenen Symbole im Speicher.
	 * @return
	 */
	public int getSymbolCount();
	
	/**
	 * Liefert die Anzahl der vorhandenen Symbole im Paket.
	 * @return
	 */
	public int getSymbolCount(String pack);
	
	/**
	 * Liefert alle Funktionssymbole im angegebenen Paket.
	 * @param pack
	 * @return
	 */
	public SortedSet<IMetaSymbol> getFunctionSymbols(String pack);
	
	/**
	 * Liefert alle Makrosymbole im angegebenen Paket.
	 * @param pack
	 * @return
	 */
	public SortedSet<IMetaSymbol> getMacroSymbols(String pack);
	
	/**
	 * Gibt alle Symbole mit prefix zurueck.
	 * @param prefix
	 * @return
	 */
	public SortedSet<IMetaSymbol> prefixQuery(String prefix);
	
	/**
	 * Gibt alle Symbole mit prefix im angegeben package zurueck.
	 * @param prefix
	 * @return
	 */
	public SortedSet<IMetaSymbol> prefixQuery(String pack, String prefix);
	
	/**
	 * Liefert alle Symbole die die angegeben Woerter enthalten.
	 * Test auf Aufnahme zur Ergebnismenge abhaengig von mode.
	 * @param words
	 * @param mode
	 * @return
	 */
	public SortedSet<IMetaSymbol> fullTextQuery(final List<String> words, final TSearchMode mode);
	
	/**
	 * Liefert alle Symbole aus dem Paket die die angegeben Woerter enthalten.
	 * @param pack
	 * @param words
	 * @param mode
	 * @return
	 */
	public SortedSet<IMetaSymbol> fullTextQuery(final String pack, final List<String> words, final TSearchMode mode);
} 
