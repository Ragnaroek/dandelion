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

package de.fh_trier.eclipse.lisp.internal.core.dom.parse;

import java.util.*;

import org.eclipse.jface.text.Position;

import de.fh_trier.eclipse.lisp.internal.core.dom.SExpression;
import edu.umd.cs.findbugs.annotations.*;

/**
 * Standardimplementierung der {@link IParserInput}-Schnittstelle.
 * @author Michael Bohn
 * testcase
 */
public class ParserInput
implements IParserInput
{
	@Nullable
	private List<SExpression> fInput;
	@NonNull
	private SExpression fInputSexpression;
	private int fOffset;
	
	/**
	 * Erstellt einen neuen ParserInput aus einer Liste von Kindknoten.
	 * @param position - Positionsanagabe des zu pruefenden Ausdrucks, fuer die Fehlermeldungen.
	 * @param childs - Kindknoten
	 */
	public ParserInput(final Position position, final List<SExpression> childs) {
		this(new SExpression(childs, position));
	}
	
	/**
	 * Erstellt einen neuen ParserInput aus einem Ausdruck.
	 * @param input
	 */
	public ParserInput(final SExpression input) 
	{
		if( input.hasChildren() ) {
			this.fInput = input.getChildren();
		}
		this.fInputSexpression = input;
		this.fOffset = 0;
	}

	/**
	 * @see IParserInput#getInputSExpression()
	 */
	@NonNull
	public SExpression getInputSExpression() 
	{
		return fInputSexpression;
	}

	/**
	 * @see Iterator#hasNext()
	 */
	public boolean hasNext() 
	{
		return fInput != null && fOffset < fInput.size();
	}

	/**
	 * @see IParserInput#current()
	 */
	public SExpression current() {
		int lastOffset = fOffset-1;
		if(lastOffset < 0) {
			throw new IllegalStateException();
		}
		return fInput.get(fOffset-1);
	}

	/**
	 * @see Iterator#next()
	 */
	public SExpression next() 
	{
		if(fOffset >= size()) {
			throw new IllegalStateException();
		}
		
		SExpression next = fInput.get(fOffset);
		fOffset++;
		return next;
	}

	/**
	 * @see IParserInput#pushBack()
	 */
	public void pushBack() {
		if(fOffset-1 < 0) {
			throw new IllegalStateException();
		}
		fOffset--;
	}

	/**
	 * Nicht unterstuetzt. Wirft eine {@link UnsupportedOperationException}.
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see IParserInput#size()
	 */
	public int size() {
		return fInput == null ? 0 : fInput.size();
	}
}
