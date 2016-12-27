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

import java.util.List;

import de.defmacro.dandelion.internal.core.dom.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Ueberprueft einen Lambda-Ausdruck.
 * @author Michael Bohn
 */
@SuppressWarnings("UwF")
public class LambdaParser 
extends AbstractParser<LambdaForm>
{
	private IParserInput fInput;
	private OrdinaryLambdaList fLambdaList;
	private List<SExpression> fBody;
	
	@Override
	protected void cleanUp0() {
		fLambdaList = null;
		fBody = null;
	}

	@Override
	protected LambdaForm constructObject() {
		return new LambdaForm(fInput.getInputSExpression().getChildren(), fInput.getInputSExpression().getPosition(), fLambdaList, fBody);
	}

	@Override
	protected void parse0() 
	throws ParseException 
	{	
		checkNextIsSymbol(Symbol.SYM_LAMBDA);
		checkHasNext();
		OrdinaryLambdaListParser lambdaListParser = ParserFactory.getOrdinaryLambdaListParser();
		fLambdaList = delegateParse(lambdaListParser, fInput.next());
		
		fBody = copyRest();
	}

	@Override
	protected void setInput(final IParserInput input) {
		this.fInput = input;
	}
}
