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

package de.defmacro.dandelion.internal.ui.editor;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;
import org.eclipse.jface.text.*;

import de.defmacro.dandelion.internal.ui.text.partition.*;

/**
 * Der Konfigurator fuer die Dokumente
 * des Lisp-Editors.
 * @author Michael Bohn
 *
 */
public class LispDocumentSetupParticipant 
implements IDocumentSetupParticipant 
{
	/**
	 * Das Dokument wird synchronisiert.
	 * {@link IDocumentSetupParticipant#setup(IDocument)}
	 */
	public void setup(final IDocument doc) 
	{
		IDocumentExtension3 docExtension = (IDocumentExtension3)doc;
		
		IDocumentPartitioner partitioner = LispDocumentPartitionerFactory.getPartitioner();
		docExtension.setDocumentPartitioner(LispPartitionConstants.PARTITION_ID, partitioner);
		partitioner.connect(doc);
		
		IDocumentPartitioner commentPartitioning = LispDocumentPartitionerFactory.getCommentPartitioner();
		docExtension.setDocumentPartitioner(LispPartitionConstants.PARTITION_COMMENT_ID, commentPartitioning);
		commentPartitioning.connect(doc);
		
		((ISynchronizable)doc).setLockObject(new Object());
	}
}
