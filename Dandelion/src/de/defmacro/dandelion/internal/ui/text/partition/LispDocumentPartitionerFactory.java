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

import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;

/**
 * Factory fuer Lisp-Partitionierer. 
 * @author Michael Bohn
 *
 */
public class LispDocumentPartitionerFactory
{
	private LispDocumentPartitionerFactory()
	{
		//keine direkte Instanz erlaubt
	}

	/**
	 * Liefert einen neuen Partitionierer fuer die Partitionierung
	 * von Lisp Dokumenten.
	 * @return {@link IDocumentPartitioner} - Der Lisp-Source-Partionierer.
	 */
	public static IDocumentPartitioner getPartitioner() {
		return new FastPartitioner(
				LispPartitionScanner.instanceOf(),
				LispPartitionConstants.PARTITIONS);
	}
	
	/**
	 * Liefert einen neuen Partitionierer fuer die Partitionierung
	 * des Kommentars von Lisp-Dokumenten.
	 * @return {@link IDocumentPartitioner} - Der Lisp-Kommentar-Partionierer.
	 */
	public static IDocumentPartitioner getCommentPartitioner() {
		return new FastPartitioner(
				LispPartitionScanner.instanceOfCommentScanner(),
				LispPartitionConstants.COMMENT_PARTITIONS);
	}
}
