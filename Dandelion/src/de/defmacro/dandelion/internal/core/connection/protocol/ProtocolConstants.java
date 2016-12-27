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

/**
 * Bietet Zugriff auf Konstanten fuer
 * die Protokollimplementierung.
 * @author Michael Bohn
 */
public class ProtocolConstants 
{
	private ProtocolConstants()
	{ /* keine Instanz */ }
	
	public static final String TOKEN_OK 			= "OK";
	public static final String TOKEN_ERROR 			= "ERROR";
	public static final String TOKEN_EVAL_ERROR     = "EVAL-ERROR";
	public static final String TOKEN_READ_ERROR     = "READ-ERROR";
	public static final String TOKEN_CONNECT 		= "CONNECT";
	public static final String TOKEN_DISCONNECT 	= "DISCONNECT";
	public static final String TOKEN_INVOKE_RESTART = "INVOKE-RESTART";
	public static final String TOKEN_EVAL       	= "EVAL";
	public static final String TOKEN_ABORT          = "ABORT";
	public static final String TOKEN_PACKAGELIST    = "PACKAGE-LIST";
	public static final String TOKEN_PACKAGES       = "PACKAGES";
	public static final String TOKEN_FUNCTIONS 		= "FUNCTIONS";
	public static final String TOKEN_MACROS 		= "MACROS";
	public static final String TOKEN_FUNCTIONLIST   = "FUNCTION-LIST";
	
	public static final String TOKEN_TERMINATION = "\n";
	public static final String TOKEN_BLANK   = " ";
	
	public static final String REGEX_BLANK  = "\\s+";
	public static final String REGEX_SYMBOL = "[^\\s]+"; //"[^\\s()]+";
	public static final String REGEX_SEXP   = ".+";
	public static final String REGEX_BASE64 = "[a-zA-Z0-9+/=]+";
	public static final String REGEX_INTEGER = "\\d+";
}
