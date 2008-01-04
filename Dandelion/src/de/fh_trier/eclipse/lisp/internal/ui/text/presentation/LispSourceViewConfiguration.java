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

import java.util.ArrayList;
import java.util.List;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.*;
import de.fh_trier.eclipse.lisp.internal.ui.text.partition.LispPartitionConstants;
import de.fh_trier.eclipse.lisp.internal.ui.text.presentation.SyntaxHighlightingDefinition.SyntaxType;

/**
 * Konfiguration fuer da Anschauen von Lisp-Quelltext
 * im Editor.
 * @author Michael Bohn
 *
 */
/*package*/ class LispSourceViewConfiguration 
extends SourceViewerConfiguration 
{
	/**
	 * Liefert die Lisp-Doppelklickstrategie.
	 * @see SourceViewerConfiguration#getDoubleClickStrategy(ISourceViewer, String)
	 */
	@Override
	public ITextDoubleClickStrategy getDoubleClickStrategy(final ISourceViewer sourceViewer, String contentType) {
		return LispDoubleClickStrategy.instanceOf();
	}

	/**
	 * Liefert den Reconciler fuer die Lisp-Syntaxhervorhebung.
	 * @see SourceViewerConfiguration#getPresentationReconciler(ISourceViewer)
	 */
	@Override
	public IPresentationReconciler getPresentationReconciler(final ISourceViewer sourceViewer) 
	{
		SyntaxHighlightingDefinition highlightingDefinition = LispSourcePresentationManager.getSyntaxHighlightingDefinition();
		
		IToken comment = new Token(highlightingDefinition.getTextAttribute(SyntaxType.COMMENT));
		
		PresentationReconciler reconciler = new PresentationReconciler();
		//Reconciler fuer Partitionierungsid anmelden
		reconciler.setDocumentPartitioning(LispPartitionConstants.PARTITION_ID);
		List<IRule> rules = new ArrayList<IRule>();
		
		//Regel fuer Strings
		RuleBasedScanner stringScanner = new RuleBasedScanner();
		stringScanner.setDefaultReturnToken(new Token(highlightingDefinition.getTextAttribute(SyntaxType.STRING)));
		//rules.add(new StringRule(new Token(highlitingDefinition.getTextAttribute(SyntaxType.STRING))));
		stringScanner.setRules(makeRuleArray(rules));
		
		//Regel fuer Kommentare in Kommentar-Content-Typ
		RuleBasedScanner commentScanner = new RuleBasedScanner();
		commentScanner.setDefaultReturnToken(comment);
		commentScanner.setRules(makeRuleArray(rules));
		
		//kompletter Char-Content-Type wird gleich einfgefaerbt,
		//deshalb Scanner ohne Regeln, nur mit Default-Token anlegen
		RuleBasedScanner charScanner = new RuleBasedScanner();
		charScanner.setDefaultReturnToken(new Token(highlightingDefinition.getTextAttribute(SyntaxType.CHARACTER)));
		
		//Damager und Repairer erstellen
		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(stringScanner);
		reconciler.setDamager(dr, LispPartitionConstants.LISP_PARTITION_STRING);
		reconciler.setRepairer(dr, LispPartitionConstants.LISP_PARTITION_STRING);
		
		dr = new DefaultDamagerRepairer(commentScanner);
		reconciler.setDamager(dr, LispPartitionConstants.LISP_PARTITION_COMMENT);
		reconciler.setRepairer(dr, LispPartitionConstants.LISP_PARTITION_COMMENT);
		
		dr = new DefaultDamagerRepairer(charScanner);
		reconciler.setDamager(dr, LispPartitionConstants.LISP_PARTITION_CHAR);
		reconciler.setRepairer(dr, LispPartitionConstants.LISP_PARTITION_CHAR);
		
		dr = new DefaultDamagerRepairer(new LispCodeScanner());
		reconciler.setDamager(dr, LispPartitionConstants.LISP_PARTITION_DEFAULT); //lisp code
		reconciler.setRepairer(dr, LispPartitionConstants.LISP_PARTITION_DEFAULT);
		
		return reconciler;
	}
	
	private IRule[] makeRuleArray(final List<IRule> rules)
	{
		return rules.toArray(new IRule[rules.size()]);
	}
	
	/**
	 * Liefert eine {@link IAnnotationHover} fuer die Hoveranzeige von Fehlermeldungen.
	 * Die super-Klasse liefert hier <code>null</code>.
	 * @see SourceViewerConfiguration#getAnnotationHover(ISourceViewer).
	 */
	@Override
	public IAnnotationHover getAnnotationHover(final ISourceViewer sourceViewer) {
		return new DefaultAnnotationHover();
	}
}
