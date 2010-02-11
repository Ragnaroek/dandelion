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

package de.defmacro.dandelion.internal.ui.text.presentation;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.jface.text.formatter.*;
import org.eclipse.jface.text.reconciler.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import de.defmacro.dandelion.internal.ui.editor.*;
import de.defmacro.dandelion.internal.ui.text.partition.LispPartitionConstants;

/**
 * Konfiguration eines Editors fuer
 * Lisp-Quelltextbearbeitung.
 * @author Michael Bohn
 *
 */
public class LispSourceEditConfiguration 
extends LispSourceViewConfiguration 
{
	private ILispEditor fEditor;
	private boolean fInteractiveEvalSupport;
	
	/**
	 * Erzeugt einen neue Konfiguration fuer den Editor.
	 * @param editor - Konfiguration fuer diesen Editor.
	 * @param interactiveEvalSupport - Interaktive Evaluierung angeschaltet ja/nein
	 * @throws NullPointerException - wenn editor == <code>null</code>
	 */
	public LispSourceEditConfiguration(final ILispEditor editor, final boolean interactiveEvalSupport)
	{
		if (editor == null) {
			throw new NullPointerException("editor must not be null");
		}
		this.fEditor = editor;
		this.fInteractiveEvalSupport = interactiveEvalSupport;
	}

	/**
	 * Liefert den Reconciler fuer die Modellerstellung.
	 * @see SourceViewerConfiguration#getReconciler(ISourceViewer)
	 */
	@Override
	public IReconciler getReconciler(final ISourceViewer sourceViewer) 
	{
		MonoReconciler reconciler = new MonoReconciler(new SExpressionReconcilingStrategy(fEditor), false);
		//der Modellerstellungsreconciler
		//reconciler.setReconcilingStrategy(new SExpressionReconcilingStrategy(fEditor), LispPartitionConstants.LISP_PARTITION_DEFAULT);
		return reconciler;
	}
	
	/**
	 * Liefert den {@link LispContentFormatter}.
	 * @see SourceViewerConfiguration#getContentFormatter(ISourceViewer)
	 */
	@Override
	public IContentFormatter getContentFormatter(final ISourceViewer sourceViewer) {
		return new LispContentFormatter(sourceViewer);
	}

	/**
	 * Liefert den {@link ContentAssistant} fuer Lisp.
	 * @see SourceViewerConfiguration#getContentAssistant(ISourceViewer)
	 */
	@Override
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("SIC")
	public IContentAssistant getContentAssistant(final ISourceViewer sourceViewer) 
	{
		if(!fInteractiveEvalSupport) { //kein ContentAssistant
			return null;
		}
		ContentAssistant assistant = new ContentAssistant();
		
		assistant.setDocumentPartitioning(LispPartitionConstants.PARTITION_ID);
		assistant.setContentAssistProcessor(new LispProposalProcessor(fEditor), LispPartitionConstants.LISP_PARTITION_DEFAULT);
		
		//BUG workaround: auch fuer alle Partitionen ProposalProcessor anmelden, da sonst NPE bei STRG+LEER in partition 
		assistant.setContentAssistProcessor(new EmptyProposalProcessor(), LispPartitionConstants.LISP_PARTITION_COMMENT);
		assistant.setContentAssistProcessor(new EmptyProposalProcessor(), LispPartitionConstants.LISP_PARTITION_CHAR);
		assistant.setContentAssistProcessor(new EmptyProposalProcessor(), LispPartitionConstants.LISP_PARTITION_STRING);
		
		assistant.setProposalSelectorBackground(PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_WHITE));
		assistant.setAutoActivationDelay(500);
		assistant.enableAutoActivation(true);
		assistant.enablePrefixCompletion(true);
		//assistant.setShowEmptyList(true);
		assistant.setInformationControlCreator(new IInformationControlCreator() {
			public IInformationControl createInformationControl(Shell parent) {
				return new DefaultInformationControl(parent, SWT.WRAP,  new LispAdditionalInformationPresenter());
			}
		});
	
		return assistant;
	}

	/**
	 * Liefert die Auto-Edit Strategien fuer Lisp.
	 * @see SourceViewerConfiguration#getAutoEditStrategies(ISourceViewer, String)
	 */
	@Override
	public IAutoEditStrategy[] getAutoEditStrategies(final ISourceViewer sourceViewer, final String contentType) 
	{
		if(LispPartitionConstants.LISP_PARTITION_DEFAULT.equals(contentType)) {
			return new IAutoEditStrategy[] { new LispIndentationStrategy(), new LispTabStrategy() };
		}
		return new IAutoEditStrategy[] { new DefaultIndentLineAutoEditStrategy() };
	} 
}
