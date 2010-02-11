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
import edu.umd.cs.findbugs.annotations.NonNull;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Implementierung der {@link ISymbolStore}-Schnittstelle.
 * Die uebergebenen IMetaSymbol-Objekte sollten in Upper-Case geinterned werden.
 * Sonst Verhalten undefiniert. Auch die Rueckgaben der query-Funktionen sind
 * in Upper-Case und muessen bei Bedarf umgewandelt werden. Die zurueckgegeben Symbolmengen
 * sind immutable, eine Aenderung an diesen Listen ist nicht erlaubt.
 * Die Implementierung ist Thread-Safe.
 * @author Michael Bohn
 * testcase
 */
public class SymbolStore 
implements ISymbolStore 
{	
	private static final SortedSet<IMetaSymbol> EMPTY_SET = Collections.unmodifiableSortedSet(new TreeSet<IMetaSymbol>());
	
	private boolean fInitialized;
	private Map<String, MetaPackage> fPackages;
	private SortedSet<String> fPackagesSorted; //view auf packages, sortiert
	
	/**
	 * Erzeugt einen neuen Symbolspeicher.
	 */
	public SymbolStore() {
		this.fPackages = new Hashtable<String, MetaPackage>();
		this.fPackagesSorted = Collections.synchronizedSortedSet(new TreeSet<String>());
	}
	
	/**
	 * Der Package-String wird in Upper-Case umgewandelt.
	 * @see ISymbolStore#addPackage(String)
	 */
	public synchronized void addPackage(final String pack) {
		createPackage(pack);
	}

	/**
	 * @see ISymbolStore#internFunctionSymbol(IMetaSymbol)
	 */
	public synchronized void internFunctionSymbol(final IMetaSymbol symbol) {
		
		assertLegal(symbol, TMetaType.FUNCTION);
		
		MetaPackage metaPack = createPackage(symbol.getPackage());
		metaPack.getFunctionSymbols().add(symbol);
	}

	/**
	 * @see ISymbolStore#internFunctionSymbol(String, List)
	 */
	public synchronized void internFunctionSymbol(final String pack, final List<IMetaSymbol> symbols) {
		MetaPackage metaPack = createPackage(pack);
		Set<IMetaSymbol> functions = metaPack.getFunctionSymbols();
		
		for(IMetaSymbol sym : symbols) {
			assertLegal(sym, TMetaType.FUNCTION);
			functions.add(sym);
		}
	}

	/**
	 * @see ISymbolStore#internMacroSymbol(IMetaSymbol)
	 */
	public synchronized void internMacroSymbol(final IMetaSymbol symbol) {
		
		assertLegal(symbol, TMetaType.MACRO);
		
		MetaPackage metaPack = createPackage(symbol.getPackage());
		metaPack.getMacroSymbols().add(symbol);
	}

	/**
	 * @see ISymbolStore#internMacroSymbol(String, List)
	 */
	public synchronized void internMacroSymbol(final String pack, final List<IMetaSymbol> symbols) {
		MetaPackage metaPack = createPackage(pack);
		Set<IMetaSymbol> macros = metaPack.getMacroSymbols();
		
		for(IMetaSymbol sym : symbols) {
			assertLegal(sym, TMetaType.MACRO);
			macros.add(sym);
		}
	}
	
	private void assertLegal(final IMetaSymbol sym, TMetaType expectedType) {
		if(sym.getType() != expectedType) {
			throw new IllegalArgumentException("Trying to intern a " + sym.getType() + " can only intern type " + expectedType);
		}
	}
	
	/**
	 * Stellt sicher das das Package in der Map angelegt ist. 
	 * Tut nichts wenn das Package bereits vorhanden ist.
	 * @param pack - evtl. anzulegendes Paket
	 * @return
	 */
	@SuppressWarnings("Dm")
	private MetaPackage createPackage(String pack)
	{
		pack = pack.toUpperCase();
		MetaPackage metaPackage = fPackages.get(pack);
		if(metaPackage == null) {
			metaPackage = new MetaPackage();
			fPackages.put(pack, metaPackage);
			fPackagesSorted.add(pack);
		}
		return metaPackage;
	}
	
	@SuppressWarnings("Dm")
	private MetaPackage getMetaPackage(final String pack) {
		return fPackages.get(pack.toUpperCase());
	}
	
	/**
	 * @see ISymbolStore#getPackagesAsMetaSymbol() 
	 */
	public synchronized SortedSet<IMetaSymbol> getPackagesAsMetaSymbol() 
	{
		return Collections.unmodifiableSortedSet(getPackagesAsMetaSymbol(getPackages(), null));
	}

	/**
	 * @see ISymbolStore#getPackagesAsMetaSymbol(String)
	 */
	public synchronized SortedSet<IMetaSymbol> getPackagesAsMetaSymbol(final String prefix) 
	{
		return Collections.unmodifiableSortedSet(getPackagesAsMetaSymbol(getPackages(), prefix));
	}
	
	/**
	 * <code>null</code> oder "" fuer keinen Prefix.
	 * @param packages
	 * @param prefix
	 * @return
	 */
	@SuppressWarnings("Dm")
	private SortedSet<IMetaSymbol> getPackagesAsMetaSymbol(final SortedSet<String> packages, String prefix)
	{
		if(prefix == null) {
			prefix = "";
		} else {
			prefix = prefix.toUpperCase();
		}
		
		SortedSet<IMetaSymbol> result = new TreeSet<IMetaSymbol>();
		for(String pack : packages) {
			if(pack.startsWith(prefix)) {
				result.add(new SymbolMetaSymbol(pack, pack, TMetaType.PACKAGE));
			}
		}
		return result;
	}

	/**
	 * @see ISymbolStore#getPackages()
	 */
	public synchronized SortedSet<String> getPackages() {
		return Collections.unmodifiableSortedSet(fPackagesSorted);
	}

	/**
	 * @see ISymbolStore#isEmpty()
	 */
	public synchronized boolean isEmpty() {
		return fPackages.isEmpty();
	}

	/**
	 * @see ISymbolStore#isInitialized()
	 */
	public synchronized boolean isInitialized() {
		return fInitialized;
	}

	/**
	 * @see ISymbolStore#setInitialized(boolean)
	 */
	public synchronized void setInitialized(final boolean initState) {
		this.fInitialized = initState;
	}
	
	/**
	 * @see ISymbolStore#hasPackage(String)
	 */
	@SuppressWarnings("Dm")
	public synchronized boolean hasPackage(final String pack) {
		return fPackages.containsKey(pack.toUpperCase());
	}

	/**
	 * @see ISymbolStore#getSymbolCount()
	 */
	public synchronized int getSymbolCount()
	{
		int count = 0;
		for(String pack : getPackages()) {
			count += getSymbolCount(pack);
		}
		return count;
	}
	
	/**
	 * @see ISymbolStore#getSymbolCount(String)
	 */
	public synchronized int getSymbolCount(final String pack)
	{		
		MetaPackage metaPack = getMetaPackage(pack);
		if(metaPack == null) {
			return 0;
		}
		return metaPack.getFunctionSymbols().size() + metaPack.getMacroSymbols().size();
	}
	
	/**
	 * @see ISymbolStore#getFunctionSymbols(String)
	 */
	@NonNull
	@SuppressWarnings("Dm")
	public synchronized SortedSet<IMetaSymbol> getFunctionSymbols(final String pack) {
		MetaPackage metaPack = fPackages.get(pack.toUpperCase());
		
		if( metaPack == null) {
			return EMPTY_SET;
		}
		
		return Collections.unmodifiableSortedSet(metaPack.getFunctionSymbols());
	}

	/**
	 * @see ISymbolStore#getMacroSymbols(String)
	 */
	@NonNull
	public synchronized SortedSet<IMetaSymbol> getMacroSymbols(final String pack) {
		MetaPackage metaPack = getMetaPackage(pack);
		if( metaPack == null ) {
			return EMPTY_SET;
		}
		
		return Collections.unmodifiableSortedSet(metaPack.getMacroSymbols());
	}

	/**
	 * @see ISymbolStore#prefixQuery(String)
	 */
	@NonNull
	public synchronized SortedSet<IMetaSymbol> prefixQuery(final String prefix) {
		
		SortedSet<IMetaSymbol> resultSet = new TreeSet<IMetaSymbol>();
		for(String pack : getPackages()) {
			SortedSet<IMetaSymbol> set = prefixQuery(pack, prefix);
			resultSet.addAll(set);
		}
		return resultSet;
	}

	/**
	 * @see ISymbolStore#prefixQuery(String, String)
	 */
	@NonNull
	public synchronized SortedSet<IMetaSymbol> prefixQuery(final String pack, final String prefix) {
	
		MetaPackage metaPack = getMetaPackage(pack);
		if(metaPack == null) {
			return EMPTY_SET;
		}
		
		SortedSet<IMetaSymbol> result = new TreeSet<IMetaSymbol>();
		result.addAll(searchSetPrefix(metaPack.getFunctionSymbols(), prefix));
		result.addAll(searchSetPrefix(metaPack.getMacroSymbols(), prefix));		
		
		return Collections.unmodifiableSortedSet(result);
	}
	
	@SuppressWarnings("Dm")
	private SortedSet<IMetaSymbol> searchSetPrefix(final SortedSet<IMetaSymbol> set, final String prefix)
	{	
		String upperCasePrefix = prefix.toUpperCase();
		SortedSet<IMetaSymbol> result = new TreeSet<IMetaSymbol>(set);
		for( Iterator<IMetaSymbol> iter = result.iterator(); iter.hasNext(); ) {
			IMetaSymbol meta = iter.next();
			if( !meta.getSymbolName().toUpperCase().startsWith(upperCasePrefix)) {
				//System.out.println("removed pack=" + meta.getPackage() + " name="+meta.getSymbolName());
				iter.remove();
			}
		}
		return result;
	}
	
	/**
	 * @see ISymbolStore#fullTextQuery(List, TSearchMode)
	 */
	public synchronized SortedSet<IMetaSymbol> fullTextQuery(final List<String> words, final TSearchMode mode)
	{
		if(words == null || words.isEmpty()) {
			return EMPTY_SET;
		}
		
		List<String> uppercaseWords = convertToUppercase(words);
		
		SortedSet<IMetaSymbol> result = new TreeSet<IMetaSymbol>();
		SortedSet<String> packages = getPackages();
		for(String pack : packages) { //alle Symbole in Paketen durchsuchen
			internalSearchFullTextPackage(uppercaseWords, mode, getMetaPackage(pack), result);
		}
		// + Pakete selbst
		internalSearchFullTextSortedSet(uppercaseWords, mode, getPackagesAsMetaSymbol(packages, null), result);
		
		return Collections.unmodifiableSortedSet(result);
	}
	
	/**
	 * @see ISymbolStore#fullTextQuery(String, List, TSearchMode)
	 */
	public synchronized SortedSet<IMetaSymbol> fullTextQuery(final String pack, final List<String> words, final TSearchMode mode)
	{
		if(pack == null || words == null || words.isEmpty()) {
			return EMPTY_SET;
		}
		
		SortedSet<IMetaSymbol> result = new TreeSet<IMetaSymbol>();
		internalSearchFullTextPackage(convertToUppercase(words), mode, getMetaPackage(pack), result);
		return Collections.unmodifiableSortedSet(result);
	}

	@SuppressWarnings("Dm")
	private List<String> convertToUppercase(final List<String> words) {
		List<String> result = new ArrayList<String>(words.size());
		for(String word : words) {
			result.add(word.toUpperCase());
		}
		return result;
	}
	
	private void internalSearchFullTextPackage(final List<String> words, final TSearchMode mode, final MetaPackage metaPack, final SortedSet<IMetaSymbol> result)
	{
		if(metaPack != null) {
			internalSearchFullTextSortedSet(words, mode, metaPack.getFunctionSymbols(), result);
			internalSearchFullTextSortedSet(words, mode, metaPack.getMacroSymbols(), result);
		}
	}
	
	private void internalSearchFullTextSortedSet(final List<String> words, final TSearchMode mode, final SortedSet<IMetaSymbol> set, final SortedSet<IMetaSymbol> result)
	{
		//abfrage mode nicht in schleife, damit nicht fuer jedes Symbol der Mode verglichen werden muss
		if(mode == TSearchMode.MODE_AND) {
			for(IMetaSymbol sym : set) {
				if(and(sym, words)) {
					result.add(sym);
				}
			}
		} else if(mode == TSearchMode.MODE_OR) {
			for(IMetaSymbol sym : set) {
				if(or(sym, words)) {
					result.add(sym);
				}
			}
		} else if(mode == TSearchMode.MODE_NOT) {
			for(IMetaSymbol sym : set) {
				if(not(sym, words)) {
					result.add(sym);
				}
			}
		} else {
			throw new IllegalArgumentException("unknown mode");
		}
	}
	
	private boolean and(final IMetaSymbol meta, final List<String> words)
	{
		for(String word : words) {
			String symName = meta.getSymbolName();
			if(!symName.contains(word)) { //eins der Worte aus Liste kommt nicht in Symbol vor
				return false; //and bedingung nicht erfuellt
			}
		}
		return true;
	}
	
	private boolean or(final IMetaSymbol meta, final List<String> words)
	{
		for(String word : words) {
			String symName = meta.getSymbolName();
			if(symName.contains(word)) { //eins der Worte aus Liste kommt in Symbol vor
				return true; //or bedinung erfuellt
			}
		}
		return false;
	}
	
	private boolean not(final IMetaSymbol meta, final List<String> words)
	{
		for(String word : words) {
			String symName = meta.getSymbolName();
			if(symName.contains(word)) { //eins der Worte aus Liste kommt in Symbol vor
				return false; //not nicht erfuellt
			}
		}
		//invariante: keines der Worte aus Liste kommt in Symbol vor
		return true; //not erfuellt
	}
}
