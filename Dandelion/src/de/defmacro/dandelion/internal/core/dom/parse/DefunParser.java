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

import de.defmacro.dandelion.internal.core.dom.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Ueberprueft einen Defun-Ausdruck auf Korrektheit.
 * @author Michael Bohn
 */
@SuppressWarnings("UwF")
public class DefunParser 
extends AbstractParser<DefunForm>
{	
	private List<SExpression> fBody;
	private OrdinaryLambdaList fLambdaList;
	private Symbol fDefinedName;
	
	private IParserInput fInput;
	
	@Override
	protected DefunForm constructObject() 
	{
 		return new DefunForm(fInput.getInputSExpression().getChildren(), 
					fInput.getInputSExpression().getPosition(), fDefinedName, fLambdaList, fBody);
	}

	@Override
	protected void parse0() 
	throws ParseException 
	{
		checkNextIsSymbol(Symbol.SYM_DEFUN);
		checkNextIsType(TSExpression.SYMBOL);
		fDefinedName = (Symbol)fInput.current();
		
		checkHasNext();
		OrdinaryLambdaListParser parser = ParserFactory.getOrdinaryLambdaListParser();
		fLambdaList = delegateParse(parser, fInput.next());
 		
 		//kopiere body, body optional
 		fBody = copyRest();
	}
	
	@Override
	protected void cleanUp0() {
		fBody = null;
		fLambdaList = null;
		fDefinedName = null;
		fInput = null;
	}

	@Override
	protected void setInput(final IParserInput input) {
		this.fInput = input;
	}

	@Override
	protected String parsingType() {
		return "defun";
	}
}
