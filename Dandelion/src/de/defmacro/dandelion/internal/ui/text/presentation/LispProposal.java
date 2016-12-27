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
import org.eclipse.swt.graphics.*;

import de.defmacro.dandelion.internal.core.meta.*;
import de.defmacro.dandelion.internal.ui.LispUI;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * {@link ICompletionProposal}-Implementierung fuer 
 * Lisp-Proposals.
 * @author Michael Bohn
 *
 */
public class LispProposal 
implements ICompletionProposal, ICompletionProposalExtension, ICompletionProposalExtension3 
{
	private int fOffset;
	private int fPrefixLen;
	private IMetaSymbol fMetaSymbol;
	private String fAppliedText;
	private boolean fProposeInUpperCase;
	
	/**
	 * Erzeugt ein neues Proposal fuer den Meta-Typ.
	 * @param metaSymbol
	 * @param offset - Position an dem Propsoal aktiviert wurde
	 * @param prefixLen - Laenge des evtl. eingegeben Praefixes
	 * @param upperCase - Proposal in Upper-Case vornehmen, ja/nein
	 * @throws NullPointerException - wenn metaSymbol == <code>null</code>
	 */
	public LispProposal(final IMetaSymbol metaSymbol, final int offset, final int prefixLen, final boolean upperCase)
	{
		if (metaSymbol == null) {
			throw new NullPointerException("metaSymbol must not be null");
		}

		this.fOffset = offset;
		this.fPrefixLen = prefixLen;
		this.fMetaSymbol = metaSymbol;
		this.fProposeInUpperCase = upperCase;
	}
	
	/**
	 * Unbenutzt, da Extension implementiert.
	 */
	public void apply(final IDocument document) {
		//wird nicht mehr aufgerufen, da Extension implementiert
	}
	
	/**
	 * Fuegt das Proposal in de Text ein.
	 * @see ICompletionProposalExtension#apply(IDocument, char, int)
	 */
	public void apply(IDocument document, char trigger, int offset) 
	{
		fAppliedText = applyText(); //eingesetzten Text zwischenspeichern, wird fuer getSelection benoetigt
		int start = offset - (offset - fOffset) - fPrefixLen;
		int len = fPrefixLen + (offset - fOffset);
		try {
			document.replace(start, len, fAppliedText);
		} catch (BadLocationException e) {
			//ignore
		}
	}
	
	/**
	 * Die Auswahl nach Propsoal-Anwendung.
	 * @see ICompletionProposal#getSelection(IDocument)
	 */
	public Point getSelection(final IDocument document) 
	{	
		int length = fAppliedText == null ? 0 : fAppliedText.length();
		return new Point(fOffset-fPrefixLen + length, 0);
	}
	
	private String applyText()
	{
		TMetaType type = fMetaSymbol.getType();
		if(type == TMetaType.FUNCTION || type == TMetaType.MACRO) {
			if(fMetaSymbol.hasArguments()) {
				return getSymbolName() + " ";
			}
			return getSymbolName() + ")";
		}
		return getSymbolName();
	}
	
	/**
	 * Die Kontextinformation nach Anwendung.
	 * Liefert die Parameterliste.
	 * @see ICompletionProposal#getContextInformation()
	 */
	public IContextInformation getContextInformation() {
		String argString = fMetaSymbol.getArgumentString(false);
		if(argString != null) {
			return new ContextInformation(argString, argString); 
		}
		return null;
	}

	/**
	 * Anzuzeigender String in Proposalliste.
	 * @see ICompletionProposal#getDisplayString()
	 */
	public String getDisplayString() {
		return getSymbolName() + " : " + fMetaSymbol.getPackage();
	}

	/**
	 * Anzuzeigendes Bild in Proposalliste.
	 * @see ICompletionProposal#getImage()
	 */
	public Image getImage() {
		return LispUI.getUIImageManager().getImageForMetaType(fMetaSymbol.getType());
	}
	
	/**
	 * Die naeheren Information bei Selektion eines Proposals.
	 * @see ICompletionProposal#getAdditionalProposalInfo()
	 */
	public String getAdditionalProposalInfo() 
	{	
	    String argString = fMetaSymbol.getArgumentString(true);
	    String documentation = fMetaSymbol.getDocumentation();
	    
	    if(argString == null && documentation == null) {
	    	return null;
	    }
		
		return (argString == null ? "" : argString) + "\n" + (documentation == null ? "" : documentation);
	}

	/**
	 * Unbenutzt. Liefert immer 0.
	 * @see ICompletionProposalExtension#getContextInformationPosition()
	 */
	public int getContextInformationPosition() {
		return 0;
	}

	/**
	 * Unbenutzt. Liefert immer <code>null</code>.
	 * @see ICompletionProposalExtension#getTriggerCharacters()
	 */
	public char[] getTriggerCharacters() {
		return null;
	}

	/**
	 * Prueft ob das Proposal noch gueltig ist nach einer Eingabe von Zeichen.
	 * Zum verkleiner der Propsoalliste verwendet.
	 * @see ICompletionProposalExtension#isValidFor(IDocument, int)
	 */
	public boolean isValidFor(final IDocument document, final int offset) 
	{
		try {
			String textSoFar = document.get(fOffset-fPrefixLen, offset-(fOffset-fPrefixLen));
			
			//whitespace eingegeben -> completion proposal beenden
			if( TextUtilities.indexOf(de.defmacro.dandelion.internal.ui.text.TextUtilities.WHITESPACE_BAG, textSoFar, 0)[0]>= 0 ) {
				return false;
			}
			//proposal gueltig wenn mit bisher eingegebenem Text beginnt
			return getSymbolName().toLowerCase().startsWith(textSoFar);
		} catch (BadLocationException e) {
			return false;
		}
	}

	/**
	 * Unbenutzt. Liefert immer <code>null</code>
	 * @see ICompletionProposalExtension3#getInformationControlCreator()
	 */
	public IInformationControlCreator getInformationControlCreator() {
		return null;
	}

	/**
	 * Die Laenge des Praefix. Fuer Auto-Vervollstaendigung.
	 * @see ICompletionProposalExtension3#getPrefixCompletionStart(IDocument, int)
	 */
	public int getPrefixCompletionStart(final IDocument document, final int completionOffset) {
		return fOffset-fPrefixLen;
	}

	/**
	 * Der Text der Praefixes. Fuer Auto-Vervollstaendigung.
	 * @see ICompletionProposalExtension3#getPrefixCompletionText(IDocument, int)
	 */
	public CharSequence getPrefixCompletionText(final IDocument document, final int completionOffset) {
		return getSymbolName();
	}
	
	@SuppressWarnings("Dm")
	private String getSymbolName()
	{
		if(fProposeInUpperCase) {
			return fMetaSymbol.getSymbolName().toUpperCase();
		}
		return fMetaSymbol.getSymbolName().toLowerCase();
	}
}
