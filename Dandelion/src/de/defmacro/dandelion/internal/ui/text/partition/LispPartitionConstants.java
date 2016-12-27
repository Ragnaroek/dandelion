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

package de.defmacro.dandelion.internal.ui.text.partition;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * Konstanten der Lisp-Partitionierung.
 * @author Michael Bohn
 */
public class LispPartitionConstants 
{
	private LispPartitionConstants()
	{ /* keine Instanz */}
	
	/**
	 * Quelltextpartioniertung.
	 */
	public static final String PARTITION_ID = "__lispPartition";
	
	/**
	 * Kommentarpartionierung.
	 */
	public static final String PARTITION_COMMENT_ID = "__comm_part";
	public static final String LISP_PARTITION_COMMENT = "__partition_comment";
	public static final String LISP_PARTITION_STRING = "__partition_string";
	public static final String LISP_PARTITION_CHAR   = "__partition_char";
	public static final String LISP_PARTITION_DEFAULT = IDocument.DEFAULT_CONTENT_TYPE;
	
	public static final IToken TOKEN_COMMENT = new Token(LISP_PARTITION_COMMENT);
	public static final IToken TOKEN_STRING  = new Token(LISP_PARTITION_STRING);
	public static final IToken TOKEN_CHAR    = new Token(LISP_PARTITION_CHAR);
	public static final IToken TOKEN_DEFAULT = new Token(LISP_PARTITION_DEFAULT);
	
	/**
	 * Alle Partitionstypen fuer Quelltext.
	 * Das Array darf nicht veraendert werden!
	 */
	/*package*/ static final String[] PARTITIONS = new String[] 
	{
		LISP_PARTITION_COMMENT, 
		LISP_PARTITION_STRING,
		LISP_PARTITION_CHAR,
		LISP_PARTITION_DEFAULT
	};
	
	/**
	 * Alle Partitionstypen fuer Kommentar.
	 * Das Array darf nicht veraendert werden!
	 */
	/*package*/ static final String[] COMMENT_PARTITIONS = new String[] 
	{ 
		LISP_PARTITION_STRING,
		LISP_PARTITION_CHAR,
		LISP_PARTITION_DEFAULT
	};
}
