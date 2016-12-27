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

package de.defmacro.dandelion.internal.core.dom.parse;

import java.util.*;

import javax.annotation.Nonnull;

import de.defmacro.dandelion.internal.core.dom.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;


/**
 * Abstrakte Parser Klasse fuer Syntaxueberpruefung.
 * Bietet allgemeine Hilfmethoden fuer Syntaxpruefung.
 * check... Methoden werfen Fehler wenn Kriterium nicht erfuellt
 * is... Methoden geben true oder false zurueck, ohne einen Fehler zu werfen
 * 
 * @author Michael Bohn
 * @param <T>
 */
@SuppressWarnings("UwF")
public abstract class AbstractParser<T extends IMalformationProvider> 
extends DefaultMalformationProvider 
implements IParser<T> 
{
	private IParserInput fCurrentInput;
	
	public T parse(final IParserInput input) {
		
		try {
			cleanUp();
			fCurrentInput = input;
			setInput(input);
			parse0();
			checkAllRead();
		} catch (ParseException e) {
			storeMalformation(e);
		}
		T object = constructObject();
		object.addAll(this.getMalformations());
		return object;
	}
	
	protected abstract void parse0() throws ParseException;
	protected abstract void cleanUp0();
	
	@Nonnull
	protected abstract T constructObject();
	protected abstract void setInput(IParserInput input);
	
	protected String parsingType()
	{
		return "form";
	}
	
	protected ISyntacticalMalformation error(final String text, final SExpression sexp)
	throws ParseException
	{
		throw new ParseException(new Malformation(TSeverity.ERROR, sexp.getPosition(), text));
	}
	
	protected void storeMalformation(final ParseException exception) {
		addMalformation(exception.getMalformation());
	}
	
	protected void storeMalformation(final IMalformationProvider provider) {
		addAll(provider.getMalformations());
	}
	
	protected void cleanUp()
	{
		this.clearMalformations();
		cleanUp0();
	}
	
	protected boolean isNextOfType(final TSExpression... expectedTypes) 
	{
		if(!fCurrentInput.hasNext()) {
			return false;
		}
		
		SExpression next = fCurrentInput.next();
		for(TSExpression type : expectedTypes) {
			if(next.getTyp() == type) {
				return true;
			}
		}
		return false;
	}
	
	protected boolean isNextSymbol(final String expectedSymbolName)
	{
		if(!isNextOfType(TSExpression.SYMBOL)) {
			return false;
		}
		
		if(!((Symbol)fCurrentInput.current()).getSymbolName().equalsIgnoreCase(expectedSymbolName)) {
			return false;
		}
		return true;
	}
	
	private void checkAllRead()
	throws ParseException
	{
		if(fCurrentInput.hasNext()) {
			error("Unexpected token", fCurrentInput.next());
		}
	}
	
	protected void checkHasNext()
	throws ParseException
	{
		checkHasNext("Unexpected end of " + parsingType());
	}
	
	protected void checkHasNext(final String customMessage)
	throws ParseException
	{
		if( !fCurrentInput.hasNext() ) {
			error(customMessage, fCurrentInput.getInputSExpression());
		}
	}

	protected void checkNextIsType(final TSExpression... expectedTypes)
	throws ParseException
	{
		checkHasNext(typesToString(expectedTypes) + " expected");
	
		boolean foundMatchingType = isNextOfType(expectedTypes);		
		if( !foundMatchingType ) {
			error(typesToString(expectedTypes) + " expected", fCurrentInput.current());
		}
	}

	protected void checkNextIsStringDesignator()
	throws ParseException
	{
		checkHasNext("StringDesignator expected");
		SExpression sexp = fCurrentInput.next();
		
		if( !(sexp instanceof Symbol) ) {
			error("StringDesignator expected", sexp);
		}
		
		Symbol sym = (Symbol)sexp;
		if( !sym.isStringDesignator() ) {
			error("StringDesignator expected", sym);
		}
	}
	
	@SuppressWarnings("Dm")
	private String typesToString(final TSExpression[] types) {
		StringBuilder builder = new StringBuilder();
		for(TSExpression type : types) {
			builder.append(type.toString().toLowerCase());
			if(type != types[types.length-1]) {
				builder.append(", ");
			}
		}
		return builder.toString();
	}
	
	/**
	 * getType() == {@link TSExpression#SYMBOL}
	 * @param input
	 * @param expectedSymbolName
	 */
	protected void checkNextIsSymbol(final String expectedSymbolName) 
	throws ParseException
	{
		if(!isNextSymbol(expectedSymbolName)) {
			error(expectedSymbolName + " expected", fCurrentInput.current());
		}
	}
	
	protected List<SExpression> copyRest() 
	{
		List<SExpression> copied = new ArrayList<SExpression>();
		while(fCurrentInput.hasNext()) {
			copied.add(fCurrentInput.next());
		}
		return copied;
	}
	
	protected <E> E delegateParse(final IParser<E> parser, final SExpression input) {
		E result = parser.parse(new ParserInput(input));
		storeMalformation(parser);
		return result;
	}
}
