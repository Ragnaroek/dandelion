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

package de.defmacro.dandelion.internal.core.dom;

import java.util.*;

import org.eclipse.jface.text.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.ui.text.*;
import edu.umd.cs.findbugs.annotations.Nullable;

/**
 * Erstellt ein Modell aus einem 
 * {@link VisitableDocument}.
 */
/*package*/ class DOMCreator 
implements ISexpressionVisitor
{
	private RootNode fRoot;
	
	private int fLastTopLevelStartOffset;
	private int fLastSymbolStart;
	private boolean fTopLevelExpectsObject;
	private Symbol fTopLevelExpectingObject;
	
	private IDocument fVisitingDocument;
	private List<IMalformationListener> fMalformationListener;
	
	private Stack<StackEntry> fParenthesisStack; //aktuell aufgebauter Klammerzustand im sublevel
	private Stack<QuoteEntry> fQuoteStack; //aktuell aufgebauter quote zustand
	private Stack<QuoteEntry> fLastTopLevelQuoteState; //der gesicherte toplevel quote zustand wird hier gespeichert
	
	private static class QuoteEntry
	{
		public static final int QUOTE = 0;
		public static final int BACKQUOTE = 1;
		public static final int COMMA = 2;
		
		public QuoteEntry(final int type, final int offset)
		{
			this.type = type;
			this.offset = offset;
		}
		
		public int type;
		public int offset;
	}
	
	private static class StackEntry {
		public Stack<QuoteEntry> quoteState;
		public boolean lastSymbolExpectsObject;
		public Symbol lastSymbol;
		public int offset; //fuer Effizienz public, interne Klasse
		public List<SExpression> childs = new ArrayList<SExpression>();
	}
	
	/**
	 * Erstellt einen neuen DomCreator.
	 * @param root - Wurzel an den die Ausdruecke gehangen werden.
	 */
	public DOMCreator(final RootNode root)
	{
		if (root == null) {
			throw new NullPointerException("root must not be null");
		}

		this.fMalformationListener = new ArrayList<IMalformationListener>();
		this.fParenthesisStack = new Stack<StackEntry>();
		initStack();
		this.fRoot = root;
	}
	
	private void initStack()
	{
		fParenthesisStack.clear();
		StackEntry basement = new StackEntry();
		fParenthesisStack.push(basement);
	}
	
	public void addMalformationListener(final IMalformationListener listener)
	{
		fMalformationListener.add(listener);
	}
	
	public void removeMalformationListener(final IMalformationListener listener)
	{
		fMalformationListener.remove(listener);
	}
	
	public void preVisit(final IDocument document) 
	{
		this.fVisitingDocument = document;
	}
	
	//######Sublevel Visit
	
	public boolean visitParenthesisOpen(final int offset) 
	{
		StackEntry entry = new StackEntry();
		entry.offset = offset;
		entry.quoteState = currentQuoteState(); //aktueller Zustand im Entry sichern
		fParenthesisStack.push(entry);
		
		resetState();
		return false;
	}
	
	public boolean visitSymbolStart(final int offset) 
	{
		fLastSymbolStart = offset;
		return false;
	}
	
	public boolean visitParenthesisClose(final int offset) 
	{
		//im sublevel muss kein parent gesetzt werden, da die konstruierte sexp auf jeden Fall
		//irgendwo angehangen wird
		boolean backquoted = isBackquoted(); //VOR pop() aufrufen
		StackEntry entry = fParenthesisStack.pop(); 
		StackEntry parent = fParenthesisStack.peek();
		SExpression sexp = createForm(entry.childs, makePosition(entry.offset, offset), !backquoted);
		//peek geht hier immer, da an unterster Stelle ein Dummy Objekt liegt
		List<SExpression> parentChilds = parent.childs;
		
		if(parent.lastSymbolExpectsObject) {
			addToList(parent.lastSymbol.getChildrenCreateIfNeccessary(), null, sexp, offset).setParent(parent.lastSymbol);
			if(!checkNeedsMore(parent.lastSymbol)) {
				parent.lastSymbolExpectsObject = false;
				parent.lastSymbol = null;
			}
			fixPositions(sexp, offset);
		} else {
			//Die Form als Kind des parent anhaengen
			addToList(parentChilds, entry.quoteState, sexp, offset);
		}
		
		resetState();
		return false;
	}
	
	private boolean expectsObject(final Symbol symbol)
	{
		if(symbol.getTyp() == TSExpression.READER_SYMBOL) {
			ReaderSymbol readerSym = (ReaderSymbol)symbol;
			return readerSym.expectsObject();
		} 
		return false;
	}
	
	private boolean symbolAllowed(final Symbol symbol) {
		if(symbol.getTyp() == TSExpression.READER_SYMBOL) {
			ReaderSymbol readerSym = (ReaderSymbol)symbol;
			return readerSym.symbolAllowed();
		}
		return false;
	}
	
	public boolean visitSymbolEnd(final int offset) 
	{
		//im sublevel muss kein parent gesetzt werden, da die konstruierte sexp auf jeden Fall
		//irgendwo angehangen wird
		StackEntry stackTop = fParenthesisStack.peek();
		
		String symString = getString(fLastSymbolStart, offset);
		Symbol symbol = createSymbol(symString, makePosition(fLastSymbolStart, offset));
		
		List<SExpression> addTo;
		if(stackTop.lastSymbolExpectsObject && symbolAllowed(stackTop.lastSymbol)) {
			addTo = stackTop.lastSymbol.getChildrenCreateIfNeccessary();
			addToList(addTo, currentQuoteState(), symbol, offset).setParent(stackTop.lastSymbol);
			fixPositions(symbol, offset);
		} else {
			addTo = stackTop.childs;
			addToList(addTo, currentQuoteState(), symbol, offset);
		}
		
		if(expectsObject(symbol)) {
			stackTop.lastSymbolExpectsObject = true;
			stackTop.lastSymbol = symbol;
		} else if( stackTop.lastSymbolExpectsObject && !checkNeedsMore(stackTop.lastSymbol)) {
			stackTop.lastSymbolExpectsObject = false; //auch hier zuruecksetzen, evtl. kommt Symbol und kein Form
			stackTop.lastSymbol = null;
		}
		
		resetState();
		return false;
	}
	
	//	######Toplevel Visit
	
	public boolean visitTopLevelOpen(final int offset) 
	{
		fLastTopLevelStartOffset = offset;
		fLastTopLevelQuoteState = currentQuoteState();
		resetState();
		return false;
	}
	
	public boolean visitTopLevelSymbolStart(final int offset) 
	{
		fLastTopLevelStartOffset = offset;
		return false;
	}
	
	public boolean visitTopLevelClose(final int offset) 
	{
		SExpression topLevelSexp;
		topLevelSexp = createForm(fParenthesisStack.peek().childs, makePosition(fLastTopLevelStartOffset, offset), !isBackquoted());
		
		SExpression added;
		if(fTopLevelExpectsObject) {
			added = addToList(fTopLevelExpectingObject.getChildrenCreateIfNeccessary(), null, topLevelSexp, offset);
			added.setParent(fTopLevelExpectingObject);
			if( !checkNeedsMore(fTopLevelExpectingObject) ) {
				fixPositions(topLevelSexp, offset);
				fTopLevelExpectsObject = false;
				fTopLevelExpectingObject = null;
			}
		} else {
			added = addToList(fRoot.getChildrenCreateIfNeccessary(), fLastTopLevelQuoteState, topLevelSexp, offset);
			added.setParent(fRoot);
		}
		
		initStack(); //stack vorbereiten fuer naechsten Top-Level Besuch
		resetState();
		fLastTopLevelQuoteState = null;
		return false; 
	}

	private boolean checkNeedsMore(final Symbol sym)
	{
		if( sym.getTyp() == TSExpression.READER_SYMBOL ) {
			return ((ReaderSymbol)sym).needMore();
		}
		return false;
	}
	
	public boolean visitTopLevelSymbolEnd(final int offset) 
	{
		//hier kann keine Sexp dazwischen kommen
		
		Position position = makePosition(fLastTopLevelStartOffset, offset);
		String symString = getString(fLastTopLevelStartOffset, offset);
		Symbol symbol = createSymbol(symString, position);
		
		List<SExpression> addTo;
		if(fTopLevelExpectsObject && symbolAllowed(fTopLevelExpectingObject)) { //vorheriges Symbol hat gesetzt
			addTo = fTopLevelExpectingObject.getChildrenCreateIfNeccessary();
			addToList(addTo, currentQuoteState(), symbol, offset).setParent(fTopLevelExpectingObject);
			fixPositions(symbol, offset);
		} else {
			addTo = fRoot.getChildrenCreateIfNeccessary();
			addToList(addTo, currentQuoteState(), symbol, offset).setParent(fRoot);
		}
		
		//fuer naechsten Besuch setzen
		if( !(fTopLevelExpectsObject && checkNeedsMore(fTopLevelExpectingObject)) ) {
			fTopLevelExpectsObject = expectsObject(symbol);
			if(fTopLevelExpectsObject) {
				fTopLevelExpectingObject = symbol;
			}
		}
		
		resetState();
		fLastTopLevelQuoteState = null;
		return false;
	}
	
	private void fixPositions(final SExpression sexp, final int newEndOffset)
	{
		SExpression parent = sexp.getParent();
		while(parent != null && parent != fRoot) {
			if(parent.getTyp() == TSExpression.FORM) {
				Form form = (Form)parent;
				String formName = form.getFunctionSymbol().getSymbolName();
				if(formName.equals(Symbol.SYM_BACKQUOTE) || formName.equals(Symbol.SYM_QUOTE) || formName.equals(Symbol.SYM_COMMA)) {
					Position pos = form.getPosition();
					form.setPosition(new Position(pos.getOffset(), newEndOffset+1-pos.getOffset()));
				} else {
					break;
				}
			} else if(parent.getTyp() == TSExpression.READER_SYMBOL) {
				ReaderSymbol sym = (ReaderSymbol)parent;
				Position pos = sym.getPosition();
				sym.setPosition(new Position(pos.getOffset(), newEndOffset+1-pos.getOffset()));
			} else {
				break;
			}
			parent = parent.getParent();
		}
	}
	
	/**
	 * 
	 * @param list
	 * @param state - <code>null</code> erlaubt
	 * @param sexp
	 * @param endOffset
	 * @return
	 */
	private SExpression addToList(final List<SExpression> list, final Stack<QuoteEntry> state, final SExpression sexp, final int endOffset) 
	{
		if(state == null || state.isEmpty()) {
			list.add(sexp);
			return sexp;
		}
		
		//invariante: das symbol/form ist gequotet, gekommat oder gebackquoted. evtl. sogar mehrfach
		
		SExpression added = sexp;
		
		while(!state.isEmpty()) { //quote stack abbauen
			QuoteEntry quoteState = state.pop();
			Position position = makePosition(quoteState.offset, endOffset);
			if(quoteState.type == QuoteEntry.QUOTE) {
				added = quote(added, position);
			} else if(quoteState.type == QuoteEntry.BACKQUOTE) {
				added = backquote(added, position);
			} else { //kann nur comma sein
				added = comma(added, position);
			}
		}
		
		list.add(added);
		return added;
	}
	
	//#### Ende Toplevel
	
	public boolean visitQuote(final int offset) 
	{
		addState(new QuoteEntry(QuoteEntry.QUOTE, offset));
		return false;
	}
	
	public boolean visitBackquote(final int offset) {
		addState(new QuoteEntry(QuoteEntry.BACKQUOTE, offset));
		return false;
	}

	public boolean visitComma(final int offset) 
	{
		addState(new QuoteEntry(QuoteEntry.COMMA, offset));
		return false;
	}

	public boolean parenthesisMalformation(final int atOffset, final int balance)
	{
		String text;
		if(balance < 0) {
			text = "Syntax error, delete \")\"";
		} else {
			text = "Syntax error, \")\" expected";
		}
		
		fireMalformationDetected(new Malformation(TSeverity.STRUCTURE, new Position(atOffset, 1), text));
		return false;
	}
	
	private boolean isBackquoted()
	{
        //im aktuellen quote-zustand ein backquote (direktes backquote symbol oder sexp) 
		if(containsBackquote(fQuoteStack)) {
			return true;
		}
				
		//in uebergeordneten sexps ein backquote
		for(int i=0,n=fParenthesisStack.size();i<n;i++) {
			StackEntry entry = fParenthesisStack.get(i);
			if(containsBackquote(entry.quoteState)) {
				return true;
			}
			
			if(entry.lastSymbolExpectsObject) {
				if(backquoteInTree(entry.lastSymbol)) {
					return true;
				}
			}
		}
		
		//oder top-level form gebackquoted
		if(containsBackquote(fLastTopLevelQuoteState)) {
			return true;
		}
		
		// toplevelexpecting object ist noch nicht direkt parent der Form
		// deshalb gesondert durchsuchen
		if(fTopLevelExpectsObject) {
			if(backquoteInTree(fTopLevelExpectingObject)) {
				return true;
			}
		}
		
		return false;
	}
	
	private boolean backquoteInTree(final SExpression sexp)
	{
		SExpression parent = sexp;
		while((parent = parent.getParent()) != null) {
			if(parent instanceof Form) {
				if(((Form)parent).getFunctionSymbol().getSymbolName().equals(Symbol.SYM_BACKQUOTE)) {
					return true;
				}
			}
		}
		return false;
	}
	
	private boolean containsBackquote(final Stack<QuoteEntry> stack) 
	{
		if(stack == null) {
			return false;
		}
		
		//invariante: stack != null
		for(QuoteEntry entry : stack) {
			if(entry.type == QuoteEntry.BACKQUOTE) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Aktueller Quote-Zustand. Kann <code>null</code> sein.
	 * @return
	 */
	@Nullable
	private Stack<QuoteEntry> currentQuoteState()
	{
		return fQuoteStack;
	}
	
	private void addState(final QuoteEntry state)
	{
		//lazy-init, nur wenn wirklich gebraucht erstellen
		if(fQuoteStack == null) {
			fQuoteStack = new Stack<QuoteEntry>();
		}
		fQuoteStack.add(state);
	}
	
	private void resetState()
	{
		fQuoteStack = null;
	}
	
	/**
	 * Parent der zu quotenden sepx wird auf das quote gesetzt.
	 * @param sexp
	 * @param quotePosition
	 * @return
	 */
	private Form quote(final SExpression sexp, final Position quotePosition)
	{
		return consQuoteStructure(Symbol.SYM_QUOTE, sexp, quotePosition);
	}
	
	private Form backquote(final SExpression sexp, final Position backquotePosition)
	{
		return consQuoteStructure(Symbol.SYM_BACKQUOTE, sexp, backquotePosition);
	}
	
	private Form comma(final SExpression sexp, final Position commaPosition)
	{
		return consQuoteStructure(Symbol.SYM_COMMA, sexp, commaPosition);
	}
	
	private Form consQuoteStructure(final String structName, final SExpression sexp, final Position position)
	{
		Symbol sym = new Symbol(structName);
		sym.setRoot(fRoot);
		sym.setPosition(new Position(position.getOffset(), 1)); // '
		List<SExpression> quoteChilds = new ArrayList<SExpression>(2);
		quoteChilds.add(sym);
		quoteChilds.add(sexp);
		Form quote = new Form(quoteChilds);
		quote.setRoot(fRoot);
		quote.setPosition(position);
		sym.setParent(quote);
		sexp.setParent(quote);
		return quote;
	}
	
	/**
	 * Die Root-Node wird als Root eingetragen.
	 * @param childs
	 * @param position
	 * @return
	 */
	private SExpression createForm(final List<SExpression> childs, final Position position, final boolean checkSyntax)
	{   
		SExpression sexp = TypeFactory.createSExpression(childs, position, checkSyntax);
		propagateError(sexp);
		sexp.setRoot(fRoot);
		return sexp;
	}
	/**
	 * Die Root-Node wird als Root eingetragen, aber KEIN parent.
	 * @param symString
	 * @param position
	 * @return
	 */
	private Symbol createSymbol(final String symString, final Position position)
	{
		Symbol symbol = TypeFactory.createSymbol(symString, position);
		propagateError(symbol);
		symbol.setRoot(fRoot);
		return symbol;
	}
	
	private void propagateError(final SExpression sexp)
	{
		if( sexp.hasMalformation() ) {
			for(ISyntacticalMalformation malformation : sexp.getMalformations() ) {
				fireMalformationDetected(malformation);
			}
		}
	}
	
	private Position makePosition(final int startOffset, final int endOffset)
	{
		return new Position(startOffset, endOffset+1-startOffset);
	}
	
	private String getString(final int offset, final int endOffset)
	{
		try {
			return fVisitingDocument.get(offset, endOffset+1-offset);
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("extractFormSymbol failed", e);
			return null;
		}
	}
	
	private void fireMalformationDetected(final ISyntacticalMalformation malformation)
	{
		for(IMalformationListener listener : fMalformationListener) {
			listener.malformationDetected(malformation);
		}
	}
}
