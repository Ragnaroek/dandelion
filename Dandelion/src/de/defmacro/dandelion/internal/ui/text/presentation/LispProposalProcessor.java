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

import java.util.*;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.jface.util.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.core.meta.*;
import de.defmacro.dandelion.internal.preferences.LispPreferences;
import de.defmacro.dandelion.internal.project.ILispProject;
import de.defmacro.dandelion.internal.ui.editor.ILispEditor;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Berechnet die Vorschlaege.
 * @author Michael Bohn
 *
 */
public class LispProposalProcessor 
implements IContentAssistProcessor 
{
	private static final char[] COMPLETION_AUTO_ACTIVATION_CHARS = new char[] {'(', ':'};
	private static final char[] EMPTY_CHARS = new char[0];
	private static final IContextInformation[] EMPTY_CONTEXT  = new IContextInformation[0];
	private static final ICompletionProposal[] EMPTY_PROPOSAL = new ICompletionProposal[0];
		
	private ILispEditor fEditor;
	private IContextInformationValidator fValidator;
	private boolean fUpperCaseProposal = LispPluginActivator.getDefault().getPreferenceStore().getBoolean(LispPreferences.P_UPPER_CASE_PROPOSAL);
	
	/**
	 * Erzeugt einen neuen Prozessor fuer den Editor.
	 * @param editor
	 */
	public LispProposalProcessor(final ILispEditor editor)
	{
		if (editor == null) {
			throw new NullPointerException("editor must not be null");
		}
		
		this.fEditor = editor;
		this.fValidator = new ContextInformationValidator();
		LispPluginActivator.getDefault().getPreferenceStore().addPropertyChangeListener(new IPropertyChangeListener() {
			public void propertyChange(final PropertyChangeEvent event) {
				if(!event.getProperty().equals(LispPreferences.P_UPPER_CASE_PROPOSAL)) {
					return;
				}
				fUpperCaseProposal = (Boolean)event.getNewValue();
			}
		});
	}
	
	/**
	 * Berechnet die Proposals fuer den aktuelle Kontext.
	 * @see IContentAssistProcessor#computeCompletionProposals(ITextViewer, int)
	 */
	public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer, final int offset) 
	{	
		IDocument document = viewer.getDocument();
		// den SymbolStore jedes Mal neu holen, Datei wurde evtl. verschoben
		ILispProject project = LispCore.getProjectManager().getLispProjectFor(fEditor.getInput().getProject());
		ISymbolStore store = LispCore.getEnvironmentManager().getSymbolStoreFor(project.getEnvironment());
		Assert.isNotNull(store);
		
		Context context = new Context(document, offset);
		context.compute();
			
		switch(context.getContextType()) {
		case LOCAL_PROPOSAL            : return makeLocalProposal(context, store, offset);
		case PACKAGE_PROPOSAL          : return createProposals(offset, 0, store.getPackagesAsMetaSymbol());
		case FUNCTION_CALL_PROPOSAL    : return makeFunctionCallProposal(context, store ,offset);
		case PACKAGE_VARIABLE_PROPOSAL : return makePackageVariableProposal(context, store,offset);
		case NONE ://fallthrough nach default
		default : return EMPTY_PROPOSAL;
		}
	}
	
	private ICompletionProposal[] makeLocalProposal(final Context context, final ISymbolStore store, final int offset)
	{
		return createProposals(offset, context.getContextWord().length(), collectLocalSymbols(context.getContextWord(), store, offset));
	}
	
	private ICompletionProposal[] makeFunctionCallProposal(final Context context, final ISymbolStore store, final int offset)
	{
		String prefix = context.getContextWord();
		SortedSet<IMetaSymbol> combined = collectSelfDefinedFunctions(prefix);
		combined.addAll(store.prefixQuery(prefix));
		combined.addAll(store.getPackagesAsMetaSymbol(prefix));
		return createProposals(offset, prefix.length(), combined);
	}
	
	private ICompletionProposal[] makePackageVariableProposal(final Context context, final ISymbolStore store, final int offset)
	{
		SortedSet<IMetaSymbol> result = store.prefixQuery(context.getContextPackage(), context.getContextWord());
		return createProposals(offset, context.getContextWord().length(), result);
	}
	
	private SortedSet<IMetaSymbol> collectSelfDefinedFunctions(final String prefix)
	{
		ISexpModel model = getModel();
		if(model == null) {
			return new TreeSet<IMetaSymbol>();
		}
		
		FunctionProposalCollector collector = new FunctionProposalCollector(prefix);
		model.accept(collector);
		return collector.getResult();
	}
	
	private SortedSet<IMetaSymbol> collectLocalSymbols(final String prefix, final ISymbolStore store, final int offset)
	{
		ISexpModel model = getModel();
		if(model == null) {
			return new TreeSet<IMetaSymbol>();
		}
		
		//globale definierte Variable suchen
		GlobalSymbolProposalCollector globalCollector = new GlobalSymbolProposalCollector(prefix);
		model.accept(globalCollector);
		SortedSet<IMetaSymbol> result = globalCollector.getResult();
		
		//Symbole aus aktuellem Top-Level holen
		TopLevelSymbolProposalCollector topLevelCollector = new TopLevelSymbolProposalCollector(prefix);
		model.acceptTopLevel(topLevelCollector, offset);
		result.addAll(topLevelCollector.getResult());
		
		//Paketsymbole
		result.addAll(store.getPackagesAsMetaSymbol(prefix));
		
		return result;
	}
	
	private ISexpModel getModel()
	{
		ISexpModel model = fEditor.getLastStructureCorrectModel();
		if(model == null) {
			model = fEditor.getSExpressionModel();
			return model;
		}
		return model;
	}
	
	private ICompletionProposal[] createProposals(int offset, int prefixLen, SortedSet<IMetaSymbol> set) 
	{
		ICompletionProposal[] proposals = new ICompletionProposal[set.size()];
		
		int cnt = 0;
		for(IMetaSymbol sym : set) {
			proposals[cnt++] = new LispProposal(sym, offset, prefixLen, fUpperCaseProposal);
		}

		return proposals;
	}
	
	/**
	 * Unbenutzt. Liefert leeren Context.
	 * @see IContentAssistProcessor#computeContextInformation(ITextViewer, int)
	 */
	public IContextInformation[] computeContextInformation(final ITextViewer viewer, final int offset) 
	{	
		return EMPTY_CONTEXT;
	}

	/**
	 * Automatische Aktivierung bei ( und :.
	 * @see IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
	 */
	@SuppressWarnings("EI")
	public char[] getCompletionProposalAutoActivationCharacters() {
		return COMPLETION_AUTO_ACTIVATION_CHARS;
	}

	/**
	 * Unebutzt. Liefert leeres Array.
	 * @see IContentAssistProcessor#getContextInformationAutoActivationCharacters()
	 */
	public char[] getContextInformationAutoActivationCharacters() {
		return EMPTY_CHARS;
	}

	/**
	 * Liefert den Validator fuer die Kontextinformation.
	 * @see IContentAssistProcessor#getContextInformationValidator()
	 */
	public IContextInformationValidator getContextInformationValidator() {
		return fValidator;
	}

	/**
	 * Unbenutz. Liefert immer <code>null</code>.
	 * @see IContentAssistProcessor#getErrorMessage()
	 */
	public String getErrorMessage() {
		return null;
	}
}

/**
 * Der Validator fuer die Anzeige der Kontextinformationen.
 * Ueberprueft ob die Kontextinformation noch angzeigt werden soll.
 * @author Michael Bohn
 */
class ContextInformationValidator
implements IContextInformationValidator
{
	@SuppressWarnings("UwF")
	private IDocument fDocument;
	
	public void install(final IContextInformation info, final ITextViewer viewer, final int offset) {
		this.fDocument = viewer.getDocument();
	}

	public boolean isContextInformationValid(final int offset) {
		
		if(offset-1 < 0) {
			return false;
		}
		
		char c;
		try {
			c = fDocument.getChar(offset-1);
		} catch (BadLocationException e) {
			return false;
		}
		return c != ')';
	}
}
