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

package de.fh_trier.eclipse.lisp.internal.core.connection.protocol;

import java.util.List;

import de.fh_trier.eclipse.lisp.internal.core.connection.IResult;
import de.fh_trier.eclipse.lisp.internal.core.meta.*;

/**
 * Interface fuer das Lesen der Protokollkommandos.
 * @author Michael Bohn
 */
public interface IProtocolReader 
{
	/**
	 * Liest ein Evaluierungsergebnis.
	 * @return Ergebnis der Evaluierung
	 * @throws ProtocolException - wenn ungueltige Antwort gesendet wurde
	 */
	public IResult readEvalResult()
	throws ProtocolException;
	
	/**
	 * Erwartet als Antwort vom Server ein OK.
	 * Wird _kein_ OK empfangen wird eine ProtocolException ausgeloest.
	 * 
	 * @throws ProtocolException - wenn nicht OK-Kommando empfangen wurde.
	 */
	public void readSuccess()
	throws ProtocolException;
	
	/**
	 * Liest die Paketliste.
	 * @return Liste von Paketen
	 * @throws ProtocolException - wenn ungueltige Antwort gesendet wurde
	 */
	public List<String> readPackageList()
	throws ProtocolException;
	
	/**
	 * Liest die Funktionssymbole aus dem Paket.
	 * @param pack - Paket aus dem die Symbole gelesen werden sollen, fuer die Erzeugung der {@link IMetaSymbol}-Objekte.
	 * @param type - Typ der Symbole die gelesen wurden.
	 * @return Liste vom MetaSymbolen
	 * @throws ProtocolException - wenn ungueltige Antwort gesendet wurde
	 */
	public List<IMetaSymbol> readFunctionSymbols(String pack, TMetaType type)
	throws ProtocolException;
}
