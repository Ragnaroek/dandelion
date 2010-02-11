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

import de.defmacro.dandelion.internal.core.dom.*;

/**
 * Ueberprueft eine Makro-Lambda-Liste.
 * @author Michael Bohn
 */
public class MacroLambdaListParser
extends AbstractLambdaListParser<MacroLambdaList>
{
	private boolean fReadEnv;
	private Symbol fEnv;
	private Symbol fWhole;
	
	@Override
	protected void cleanUp0() {
		super.cleanUp0();
		this.fReadEnv = false;
		this.fEnv = null;
		this.fWhole = null;
	}

	@Override
	protected MacroLambdaList constructObject() 
	{
		if(fInput.size() == 0 ||  
				(fRequired == null && fOptional == null && fRest == null && fBody == null && fKey == null && fAux == null
						&& fEnv == null && fWhole == null)) {
			return MacroLambdaList.NIL;
		}
		
		return new MacroLambdaList(fRequired, fOptional, fRest, fBody, fKey, fAllowOtherKeys, fAux, fWhole, fEnv);		
	}

	@Override
	protected void parse0() throws ParseException {
		macroLambdaList();
	}
	
	private void macroLambdaList()
	throws ParseException
	{
		nil();
		
		if(fInput.hasNext()) {
			whole();
			environment();
			requiredParamDestructured();
			environment();
			optionalParam();
			environment();
			bodyOrRest();
			environment();
			keywordParam();
			environment();
			auxParam(false); //false = nicht defun aux
			environment();
		}
	}
	
	private void environment()
	throws ParseException
	{
		if(!fInput.hasNext()) return;
		
		if( isNextSymbol(TOKEN_ENVIRONMENT) ) {
			if(fReadEnv) {
				error("Unexpected symbol " + TOKEN_ENVIRONMENT, fInput.current());
			}
			checkNextIsType(TSExpression.SYMBOL);
			fEnv = (Symbol)fInput.current();
			fReadEnv = true;
		} else {
			fInput.pushBack();
		}
	}
	
	private void whole()
	throws ParseException
	{
		if(!fInput.hasNext()) return;
		
		if( isNextSymbol(TOKEN_WHOLE)) {
			checkNextIsType(TSExpression.SYMBOL);
			fWhole = (Symbol)fInput.current();
		} else {
			fInput.pushBack();
		}
	}
	
//	@Override
//	protected boolean isSpecialSymbol(SExpression sexp) {
//		
//		boolean superResult = super.isSpecialSymbol(sexp);
//		if(!superResult) return false;
//		
//		Symbol symbol = (Symbol)sexp;
//		return symbol.getSymbolName().equalsIgnoreCase(TOKEN_ENVIRONMENT)
//		      || symbol.getSymbolName().equalsIgnoreCase(TOKEN_WHOLE);
//	}
	
	
}
