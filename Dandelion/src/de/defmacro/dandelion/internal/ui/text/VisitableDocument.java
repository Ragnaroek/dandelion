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

package de.defmacro.dandelion.internal.ui.text;

import java.util.Stack;

import org.eclipse.jface.text.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import static de.defmacro.dandelion.internal.ui.text.partition.LispPartitionConstants.*;

/**
 * Ein Besuchbares Lisp-Dokument. Ueber einen Vistor
 * werden die Positionen der einzelnen Syntaxelemente mitgeteilt.
 * Kann wiederverwendet werden.
 * @author Michael Bohn
 * testcase
 */
public class VisitableDocument 
{
	private IDocument fDocument;
	private IDocumentExtension3 fDocExtension;
	private Stack<Integer>  fParenthesisStack;
	
	/**
	 * Erzeugt ein neuen {@link VisitableDocument}.
	 * Das Dokument muss vor dem Aufruf von {@link VisitableDocument#accept(ISexpressionVisitor)}
	 * mit {@link VisitableDocument#setDocument(IDocument)} gesetzt werden.
	 */
	public VisitableDocument()
	{/*nichts*/}
	
	/**
	 * Erzeugt ein neues {@link VisitableDocument} fuer 
	 * das uebergebene {@link IDocument}.
	 * @param doc
	 */
	public VisitableDocument(final IDocument doc)
	{
		setDocument(doc);
	}
	
	/**
	 * Setzt ein neues Dokument.
	 * @param doc
	 */
	public void setDocument(final IDocument doc)
	{
		if(doc == null) {
			throw new NullPointerException("IDocument must not be null");
		}
		
		if( !(doc instanceof IDocumentExtension3) ) {
			throw new IllegalArgumentException("IDocument needs IDocumentExtension3");
		}
		
		this.fDocExtension = (IDocumentExtension3)doc;
		this.fDocument = doc;
		this.fParenthesisStack = new Stack<Integer>();
	}
	
	/**
	 * Liefert das aktuelle zugrundeliegende Dokument.
	 * @return Das aktuelle Dokument.
	 */
	public IDocument getDocument()
	{
		return fDocument;
	}
	
	/**
	 * Besuch eines Visitor. An den Visitor werden
	 * die einzelnen Position der syntaktischen Element gemeldet.
	 * Es wird das komplette Dokumente besucht.
	 * @param visitor - Der Visitor
	 */
	public void accept(final ISexpressionVisitor visitor) 
	{
		try {
			accept(visitor, 0, fDocument.getLength(), true);
		} catch (BadLocationException e) {
			LispPluginActivator.logError("broken invariant while visiting Document", e);
		}
	}
	
	/**
	 * Besuch eines Visitors im angegebenen Bereich.
	 * @param visitor - Der Visitor
	 * @param offset Position Start
	 * @param end - Position Bereich Ende (exklusiv)
	 * @param skipComment - Kommentare bei Besuch ueberspringen
	 * @throws BadLocationException
	 * @throws {@link IllegalStateException} - wenn kein {@link Document} gesetzt wurde.
	 * @throws NullPointerException - wenn visitor <code>null</code>
	 */
	public void accept(final ISexpressionVisitor visitor, final int offset, final int end, final boolean skipComment)
	throws BadLocationException
	{	
		if(fDocument == null) {
			throw new IllegalStateException("no document set");
		}
		
		fParenthesisStack.clear(); //stack ruecksetzen, VisitableDocument kann wiederverwendet werden
		visitor.preVisit(fDocument);
		
		boolean stop = false;
		int count = 0;
		
		try {
			for(int i=offset; i<end; i++) {
				char c;
				c = fDocument.getChar(i);
				
				if(skipComment && inContentType(i, LISP_PARTITION_COMMENT)) {
					IRegion comment = fDocExtension.getPartition(PARTITION_ID, i, false);
					i = comment.getOffset() + comment.getLength()-1; //Ende Kommentar, NICHT + length da auch mitten in
					                                                 //einem Kommentarblock begonnen werden kann
					continue;
				}
				
				if(c == '(' && isCodeContentType(i)) {
					if(count == 0) {
						stop = visitor.visitTopLevelOpen(i);
					} else {
						stop = visitor.visitParenthesisOpen(i);
					}
					fParenthesisStack.add(i);
					count++;
				} else if (c == ')' && isCodeContentType(i)) {
					count--;
					if( !fParenthesisStack.isEmpty() ) {
						fParenthesisStack.pop();
					}
					if(count == 0) { //form gefunden
						stop = visitor.visitTopLevelClose(i);
					} else if(count < 0) {
						stop = signalMalformation(visitor, i, count);
						if( !stop ) {
							i = recoverFromNestingError(i, end); //bei naechster oeffnender Klammer neu aufsetzen
							count = 0;
						}
					} else {
						stop = visitor.visitParenthesisClose(i);
					}
				} else if (c == '\'' && isCodeContentType(i)) { //quote, nicht in String oder char
					stop = visitor.visitQuote(i);
				} else if (c == '`' && isCodeContentType(i)) { //backquote, nicht in String oder char
					stop = visitor.visitBackquote(i);
				} else if(c == ',' && isCodeContentType(i)) { //comma, nicht in String oder char
					stop = visitor.visitComma(i);
				} else if ( count == 0 && !Character.isWhitespace(c)) {
					stop = visitor.visitTopLevelSymbolStart(i);
					if(stop) return;
					i += symbolLength(i, end, skipComment);
					stop = visitor.visitTopLevelSymbolEnd(i);
				} else if ( count > 0 && !Character.isWhitespace(c)) {
					stop = visitor.visitSymbolStart(i);
					if(stop) return;
					i += symbolLength(i, end, skipComment);
					stop = visitor.visitSymbolEnd(i);
				}
				
				if(stop) {
					return;
				}
			} //Document komplett durchlaufen
			
			if(count != 0) { //nicht alle Klammern komplett geschlossen
				signalMalformation(visitor, fParenthesisStack.peek(), count);
			}
			
		} catch (BadPartitioningException e) {
			LispPluginActivator.logBrokenInvariant("error while visiting document", e);
		}
	}
	
	/**
	 * Gibt <code>true</code> zurueck wenn nicht im Char oder String! Also auch true wenn im Kommtarblock angwendet wird.
	 * Noetig da auch Code im Kommentar ausgewertet werden kann. Der Visitor kann dann einfach auf den Kommentarbereich
	 * angewendet werden.
	 * @param offset
	 * @return
	 * @throws BadPartitioningException
	 * @throws BadLocationException
	 */
	private boolean isCodeContentType(final int offset)
	throws BadPartitioningException, BadLocationException
	{
		String contentType = fDocExtension.getContentType(PARTITION_ID, offset, false);
		return !contentType.equals(LISP_PARTITION_CHAR) && !contentType.equals(LISP_PARTITION_STRING);
	}
	
	private int symbolLength(final int offset, final int end, final boolean skipComment) 
	throws BadLocationException, BadPartitioningException
	{
		String symContentTyp = getContentType(offset, skipComment);
		//String- oder Char-Symbol -> in jedem Fall komplette Laenge nehmen
		if(symContentTyp.equals(LISP_PARTITION_STRING) || symContentTyp.equals(LISP_PARTITION_CHAR)) {
			return getPartition(offset, skipComment).getLength()-1;
		}
		
		//invariante: kein String oder Char Content-Type
		
		int endOffset = offset;
		for(int i=offset; i<end; i++) {
			char c = fDocument.getChar(i);
			//wird Kommentare nicht uebersprungen ist nur leerzeichen, (, ) Trennzeichen fuer Symbol
			
			//String ct = fDocExtension.getContentType(LispPartitionConstants.PARTITION_ID, i, false);
			String ct = getContentType(i, skipComment);
			
			if(ct.equals(LISP_PARTITION_DEFAULT)) {
				if( isStopChar(c) ) {
					break;
				}
			} else if (ct.equals(LISP_PARTITION_CHAR) || ct.equals(LISP_PARTITION_STRING)) {
				ITypedRegion region = getPartition(i, skipComment);
				//ITypedRegion region = fDocExtension.getPartition(PARTITION_ID, i, false);
				//System.out.println(fDocument.get(region.getOffset(), region.getLength()));
				i += region.getLength()-1;
			} else { //kommentar partition
				if(skipComment) {
					break;
				}
				if( isStopChar(c) ) {
					break;
				}
			}
			
			endOffset = i;
		}
		
		return endOffset-offset;
	}
	
	private ITypedRegion getPartition(final int offset, final boolean skipCommentEnabled) 
	throws BadLocationException, BadPartitioningException
	{
		if(skipCommentEnabled) {
			return fDocExtension.getPartition(PARTITION_ID, offset, false);
		}
		return fDocExtension.getPartition(PARTITION_COMMENT_ID, offset, false);
	}
	
	private String getContentType(final int offset, final boolean skipCommentEnabled) 
	throws BadLocationException, BadPartitioningException
	{
		if(skipCommentEnabled) { //normaler ct holen
			return fDocExtension.getContentType(PARTITION_ID, offset, false);
		} 
		return fDocExtension.getContentType(PARTITION_COMMENT_ID, offset, false);
	}
	
	private boolean isStopChar(final char c)
	{
		return Character.isWhitespace(c) || c == ')' || c == '(';
	}
	
	private int recoverFromNestingError(int offset, int end) 
	throws BadLocationException, BadPartitioningException
	{
		for(int i=offset; i<end;i++) {
			char c = fDocument.getChar(i);
			if(c == '(' && inContentType(i, LISP_PARTITION_DEFAULT)) {
				return i-1;
			}
		}
		return end;
	}
	
	private boolean signalMalformation(final ISexpressionVisitor visitor, int offset, int balance)
	{
		return visitor.parenthesisMalformation(offset, balance);
	}
	
	private boolean inContentType(final int offset, final String contentTyp) 
	throws BadLocationException, BadPartitioningException 
	{
		return contentTyp.equals(fDocExtension.getContentType(PARTITION_ID, offset, false));
	}
}
