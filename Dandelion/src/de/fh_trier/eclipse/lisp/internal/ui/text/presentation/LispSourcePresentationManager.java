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

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.PaintManager;
import org.eclipse.jface.text.source.*;
import org.eclipse.swt.graphics.*;

import de.fh_trier.eclipse.lisp.internal.ui.LispUI;
import de.fh_trier.eclipse.lisp.internal.ui.editor.*;
import de.fh_trier.eclipse.lisp.internal.ui.text.partition.LispDocumentPartitionerFactory;
import de.fh_trier.eclipse.lisp.internal.ui.text.partition.LispPartitionConstants;


/**
 * Stellt Methoden für die Konfiguration von
 * SourceViewern bereit.
 * @author Michael Bohn
 *
 */
public class LispSourcePresentationManager 
{
	private static ICharacterPairMatcher instancePairMatcher;
	private static SourceViewerConfiguration instanceSourceViewerConfig;
	private static SyntaxHighlightingDefinition syntaxHighlighting = null;
	
	private LispSourcePresentationManager()
	{
		//keine Instanz erlaubt
	}
	
	/**
	 * Installiert den Lisp Edit Support am Editor.
	 * @param editor
	 * @param interactiveEvalSupport
	 */
	public static void installSourceEditSupport(final ILispEditor editor, final boolean interactiveEvalSupport)
	{
		editor.setSourceViewerConfiguration(new LispSourceEditConfiguration(editor, interactiveEvalSupport));
	}
	
	/**
	 * Installiert einen Painter fuer passende Klammermarkierung.
	 * @param viewer
	 * @return
	 */
	public static PaintManager installMatchingCharacterPainter(final ISourceViewer viewer)
	{
		return installMatchingCharacterPainter(viewer, LispUI.getUIColorManager().getColor(new RGB(192, 192, 192)));
	}
	
	/**
	 * Installiert einen Painter fuer passende Klammermarkierung mit der angegebenen Farbe.
	 * @param viewer
	 * @param matchColor
	 * @return
	 */
	public static PaintManager installMatchingCharacterPainter(ISourceViewer viewer, Color matchColor)
	{
		MatchingCharacterPainter parenPainter = new MatchingCharacterPainter(viewer, getParenthesisMatcher());
		parenPainter.setColor(matchColor);
		PaintManager paintManager = new PaintManager(viewer);
		paintManager.addPainter(parenPainter); //PaintManager den MatchingCharPainter zuordnen
		return paintManager;
	}
	
	/**
	 * Installiert die SourceViewerConfiguration ohne AnnotationModel.
	 * @see LispSourcePresentationManager#installSourceViewSupport(SourceViewer, IDocument, IAnnotationModel)
	 * @param viewer
	 * @param document
	 */
	public static void installSourceViewSupport(final SourceViewer viewer, final IDocument document)
	{
		installSourceViewSupport(viewer, document, null);
	}
	
	/**
	 * Der uebergebenen SourceViewer wird fuer die Anzeige von Lisp SourceCode
	 * konfiguriert.
	 * @param viewer - der zu konfigurierende SourceViewer
	 * @param doc - IDocument, muss die Erweiterung IDocumentExtension3 besitzen
	 * 
	 * @throws NullPointerException - Wenn <code>IDocument</code> oder <code>SourceViewer</code> == <code>null</code> 
	 * @throws IllegalArgumentException - Wenn <code>IDocument</code> nicht die benoetigte Erweiterung besitzt.
	 */
	public static void installSourceViewSupport(final SourceViewer viewer, final IDocument doc, final IAnnotationModel annotationModel)
	{
		if(doc == null) {
			throw new NullPointerException("Document must not be null");
		}
		
		if(!(doc instanceof IDocumentExtension3)) {
			throw new IllegalArgumentException("Document has not the required IDocumentExtension3");
		}

		viewer.configure(getSourceViewConfiguration());
		viewer.refresh(); //!!!!
		
		IDocumentPartitioner partitioner = LispDocumentPartitionerFactory.getPartitioner();
		IDocumentExtension3 docExtension = (IDocumentExtension3)doc;
		docExtension.setDocumentPartitioner(LispPartitionConstants.PARTITION_ID, partitioner);
		viewer.setDocumentPartitioning(LispPartitionConstants.PARTITION_ID);
		
		partitioner.connect(doc);
		viewer.setDocument(doc, annotationModel);
	}
	
	private static SourceViewerConfiguration getSourceViewConfiguration()
	{
		if(instanceSourceViewerConfig == null) {
			instanceSourceViewerConfig = new LispSourceViewConfiguration();
		}
		return instanceSourceViewerConfig;
	}
	
	private static ICharacterPairMatcher getParenthesisMatcher()
	{
		if(instancePairMatcher == null) {
			instancePairMatcher = new LispParenthesisMatcher();
		}
		
		return instancePairMatcher;
	}

	/**
	 * Liefert die Singleton-Instanzt der Defintion der Syntaxhervorhebung.
	 * @return
	 */
	public static SyntaxHighlightingDefinition getSyntaxHighlightingDefinition()
	{
		if(syntaxHighlighting == null) {
			syntaxHighlighting = new SyntaxHighlightingDefinition(LispUI.getUIColorManager());
		}
		return syntaxHighlighting;
	}	
}
