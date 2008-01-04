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

import org.apache.commons.codec.binary.Base64;

/**
 * Diese Klasse bietet Hilfsfunktionen fuer
 * die Protokollimplementierung.
 * @author Michael Bohn
 */
public class ProtocolUtilities 
{
	private static final Base64 BASE64 = new Base64();
	
	/**
	 * Kodierung eines Strings in Base64.
	 * @param string - zu kodierender String
	 * @return
	 */
	public static String encodeBase64(String string)
	{
		return new String(BASE64.encode(string.getBytes()));
	}
	
	/**
	 * Dekodierung eines String aus Base64.
	 * @param base64 - Base64 kodierter String
	 * @return
	 */
	public static String decodeBase64(String base64)
	{
		return new String(BASE64.decode(base64.getBytes()));
	}
}
