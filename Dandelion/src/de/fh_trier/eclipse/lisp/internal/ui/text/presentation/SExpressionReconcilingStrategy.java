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

package de.fh_trier.eclipse.lisp.internal.ui.text.presentation;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.reconciler.*;
import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import de.fh_trier.eclipse.lisp.internal.ui.editor.*;
import de.fh_trier.eclipse.lisp.internal.ui.text.*;

/**
 * Erstellt das Modell des Editors und teilt es diesem
 * mit.
 * @author Michael Bohn
 *
 */
public class SExpressionReconcilingStrategy 
implements IReconcilingStrategy, IReconcilingStrategyExtension
{	
	protected ILispEditor fEditor; //fuer closure zugriff
	private IProgressMonitor fProgressMonitor;
	private VisitableDocument fVisitableDocument;
	private TopLevelFormFoldingStructureProvider fFoldingStructurProvider;
	
	public SExpressionReconcilingStrategy(ILispEditor editor)
	{
		if (editor == null) {
			throw new NullPointerException("LispEditor must not be null");
		}
		
		this.fVisitableDocument = new VisitableDocument();
		this.fEditor = editor;
		this.fFoldingStructurProvider = new TopLevelFormFoldingStructureProvider(editor);
	}
	
	public void initialReconcile() 
	{
		doReconcile(true);
	}

	public void reconcile(final DirtyRegion dirtyRegion, final IRegion subRegion) 
	{
		doReconcile(false);
	}

	public void reconcile(final IRegion partition) 
	{
		doReconcile(false);
	}

	public void setProgressMonitor(IProgressMonitor monitor) 
	{
		if(monitor == null) {
			monitor = new NullProgressMonitor();
		}
		
		this.fProgressMonitor = monitor;
	}
	
	private IProgressMonitor getProgressMonitor()
	{
		return fProgressMonitor == null ? fProgressMonitor = new NullProgressMonitor() : fProgressMonitor;
	}
	
	public void setDocument(IDocument document) 
	{
		fVisitableDocument.setDocument(document);
	}
	
	private void doReconcile(boolean initial)
	{	
		final SexpModel model = new SexpModel(fVisitableDocument);
		synchronized(((ISynchronizable)fVisitableDocument.getDocument()).getLockObject()) {
			model.createDOM();
		}
		
		//Bei intialer Berechnung Information verwenden bis zur Fehlerstelle
		//Bei Fehler Folding nicht updaten, warten auf Fehlerfreien Text
		if( !model.hasMalformation(TSeverity.STRUCTURE) || initial ) {
			fFoldingStructurProvider.updateFolding(model, getProgressMonitor());
		}
		
		fEditor.setSExpressionModel(model);
	}
}
