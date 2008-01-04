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

import org.eclipse.jface.text.*;

import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import edu.umd.cs.findbugs.annotations.*;

/**
 * Berechnet den aktuelle Kontext im 
 * Dokument fuer Proposals.
 * @author Michael Bohn
 *
 */
public class Context 
{
	public enum ContextType
	{
		LOCAL_PROPOSAL,
		FUNCTION_CALL_PROPOSAL,
		PACKAGE_PROPOSAL,
		PACKAGE_VARIABLE_PROPOSAL,
		NONE,
	}
	
	private final IDocument fDocument;
	private final int fOffset;
	private String fContextWord;
	private String fContextPackage;
	private ContextType fContextType;
	
	/**
	 * Erzeugt einen neuen Kontext fuer das Dokument am Offset.
	 * Bevor die Kontextdaten abgeholt werden koennen, muss der Kontext mit
	 * {@link Context#compute()} berechnet werden.
	 * @param document
	 * @param offset
	 */
	public Context(final IDocument document, final int offset)
	{
		this.fDocument = document;
		this.fOffset = offset;
		this.fContextType = ContextType.NONE;
	}
	
	/**
	 * Das betreffende Wort des Kontextes.
	 * <code>null</code> wenn nicht vorhanden oder anwendbar.
 	 * @return Wort des Kontextes
	 */
	@Nullable
	public String getContextWord()
	{
		return fContextWord;
	}
	
	/**
	 * Das betreffende Paket des Kontextes.
	 * <code>null</code> wenn nicht vorhanden oder anwendbar.
	 * @return Paket des Kontextes
	 */
	@Nullable
	public String getContextPackage()
	{
		return fContextPackage;
	}
	
	/**
	 * Liefert den Typ des Kontextes.
	 * @return Typ des Kontextes
	 */
	@NonNull
	public ContextType getContextType()
	{
		return fContextType;
	}
	
	/**
	 * Berechnet den Kontext.
	 */
	public void compute()
	{
		try {
			if(isQualifiedSymbolProposal(fDocument, fOffset)) {
				fContextType = ContextType.PACKAGE_VARIABLE_PROPOSAL;
			} else if(isFunctionCallProposal(fDocument, fOffset)) {
				fContextWord = computePreviousWord(fDocument, fOffset);
				fContextType = ContextType.FUNCTION_CALL_PROPOSAL;
			} else {
			   computeFunctionContext(); //context fuer ausgewaehlte funktionen
			}
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("computing context failed", e);
		}
	}
	
	/**
	 * Wenn <code>true</code> zurueckgegeben wird ist das zeichen vor dem offset ein :
	 * und das Zeichen davor kein Whitespace.
	 */
	private boolean isQualifiedSymbolProposal(final IDocument doc, final int offset)
	throws BadLocationException
	{
		int wordOffset = skipWhitespace(doc, offset);
		String word = computePreviousWord(doc, wordOffset);
	
		if(word == null || word.equals("")) {
			return false;
		}
		
		Symbol sym = TypeFactory.createSymbol(word, null);
		if(sym.isQualified()) {
			fContextPackage = sym.getQualifier();
			fContextWord = sym.getSymbolName();
			return true;
		}
		return false;
	}
	
	private void computeFunctionContext()
	throws BadLocationException
	{
		int wordEndOffset = skipWhitespace(fDocument, fOffset); //Wortende vorheriges Wort suchen
		String word = computePreviousWord(fDocument, wordEndOffset);
		if(word.equalsIgnoreCase(Symbol.SYM_IN_PACKAGE) && 
				previousCharIs(fDocument, wordEndOffset-word.length(), '(')) { //in-package vorschlag
			fContextType = ContextType.PACKAGE_PROPOSAL;
			fContextPackage = Symbol.SYM_IN_PACKAGE;
		} else {
			fContextType = ContextType.LOCAL_PROPOSAL;
			if(wordEndOffset == fOffset) { //zwischen Wortende und offset kein Whitespace => word = bereits eingegeben Teil des Wortes
				fContextWord = word;
			} else {
				fContextWord = ""; //whitespace zwischen Kontextworte -> alle Symbol vorschlagen
			}
		}
	}
	
	private boolean isFunctionCallProposal(final IDocument document, final int offset)
	throws BadLocationException
	{
		if(offset <= 0) {
			return false;
		}
		
		int curOffset = offset;
		while(true) {
			char c = document.getChar(--curOffset);
			if(c == '(') {
				return true;
			}
			if(curOffset == 0 || Character.isWhitespace(c)) {
				return false;
			}
		}
	}
	
	/**
	 * Berechnet das Wort das vor dem offset liegt. Ist das Zeichen vor dem Wort
	 * ein Leerzeichen, (, ) wird der leere String zurueckgegeben.
	 * @param document
	 * @param offset
	 * @return
	 */
	@NonNull
	private String computePreviousWord(final IDocument document, final int offset)
	throws BadLocationException
	{	
		if(offset <= 0) { //<= 0, da auf jeden Fall offset -1 vor zugriff
			return "";
		}
		
		int curOffset = offset;
		StringBuilder prefix = new StringBuilder();
		
		while(true) {
			char c = document.getChar(--curOffset);
			if(Character.isWhitespace(c) || c == '(' || c == ')' || c == '\'') {
				return prefix.reverse().toString();
			}
			prefix.append(c);
			if(curOffset == 0) {
				return prefix.reverse().toString();
			}
		}
	}
	
	/**
	 * Ueberspringen von Whitespace-Zeichen, bis Dokumentanfang oder
	 * nicht Whitespace gefunden wurde
	 * @param document
	 * @param offset
	 * @return
	 */
	private int skipWhitespace(final IDocument document, final int offset)
	throws BadLocationException
	{
		if(offset <= 0) {
			return 0;
		}

		int curOffset = offset;

		while(true) {
			if(!Character.isWhitespace(document.getChar(--curOffset)) ) {
				return curOffset+1;
			}
			if(curOffset == 0) {
				return curOffset;
			}
		}
	}
	
	/**
	 * Der vorhergehende character, wobei whitespace zeichen uebersprungen werden, entspricht
	 * dem uebergebenen character.
	 * @param document
	 * @param offset
	 * @param c
	 * @return
	 * @throws BadLocationException
	 */
	private boolean previousCharIs(final IDocument document, final int offset, final int c)
	throws BadLocationException
	{
		if(offset <= 0) {
			return false;
		}
		
		int curOffset = offset;
		while(curOffset > 0) {
			char docChar = document.getChar(--curOffset);
			if( !Character.isWhitespace(docChar) ) {
				return docChar == c;
			}
		}
		return false;
	}
}
