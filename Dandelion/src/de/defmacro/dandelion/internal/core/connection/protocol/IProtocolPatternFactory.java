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

package de.defmacro.dandelion.internal.core.connection.protocol;

import java.util.regex.*;

/**
 * Interface fuer die Protocol-Factory.
 * @author Michael Bohn
 */
public interface IProtocolPatternFactory 
{	
	/**
	 * Liefert das Pattern fuer eine Erfolgsmeldung.
	 * @return Pattern Erfolgsmeldung
	 */
	public Pattern getSuccessPattern();
	
	/**
	 * Liefert das Pattern fuer eine Evaluierungsergebnis.
	 * @return Pattern Evaluierungsergebnis
	 */
	public Pattern getEvalResultPattern();
	
	/**
	 * Liefert das Pattern fuer das Initialsierungskommando
	 * der Pakete.
	 * @return Pattern Init Pakete
	 */
	public Pattern getPackageListInitPattern();
	
	/**
	 * Liefert das Pattern fuer eine einzelne Paketmitteilung.
	 * @return Pattern Paket
	 */
	public Pattern getPackageListElementPattern();
	
	/**
	 * Liefert das Pattern fuer das Intialisierungskommando
	 * der Funktionen.
	 * @return Pattern Init Funktion
	 */
	public Pattern getFunctionListInitPattern();
	
	/**
	 * Liefert das Pattern fuer eine einzelne Funktionsmitteilung.
	 * @return Pattern Funktion
	 */
	public Pattern getFunctionListElementPattern();
	
	/**
	 * Liefert das Pattern fuer die Zerlegung einer Mitteilung.
	 * @return Pattern Zerlegung Mitteilung
	 */
	public Pattern getWordSplitPattern();
}
