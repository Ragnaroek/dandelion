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

import java.util.List;

/**
 * Schnittstelle fuer Symbole aus Lisp-Umgebung.
 * @author Michael Bohn
 */
public interface IMetaSymbol 
extends Comparable<IMetaSymbol>
{
	/**
	 * Liefert den Typ des Symbols.
	 * @return
	 */
	public TMetaType getType();
	/**
	 * Liefert das Paket in dem sich das Symbol befindet.
	 * @return
	 */
	public String getPackage();
	/**
	 * Liefert den Symbolnamen.
	 * @return
	 */
	public String getSymbolName();
	/**
	 * Liefert den Dokumentationsstring.
	 * @return
	 */
	public String getDocumentation();
	/**
	 * Test ob Symbol Argumente erwartet.
	 * Nur fuer Funktionssymbole.
	 * @return
	 */
	public boolean hasArguments();
	/**
	 * Liefert die Argumentliste.
	 * Nur fuer Funktionssymbole.
	 * @return
	 */
	public List<String> getArgumentList();
	/**
	 * Liefert die Argument als String.
	 * @param format - Formatierung mit Zeileumbruechen vornehmen.
	 * @return
	 */
	public String getArgumentString(boolean format);
}
