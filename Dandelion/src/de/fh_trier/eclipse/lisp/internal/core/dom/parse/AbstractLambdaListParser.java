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

import de.fh_trier.eclipse.lisp.internal.core.dom.*;

/**
 * Abstrakter Lambda-Listen Parser.
 * Bietet allgemeine Hilfsmethoden fuer ableitende Klassen.
 * @author Michael Bohn
 * @param <T>
 */
public abstract class AbstractLambdaListParser<T extends IMalformationProvider> 
extends AbstractParser<T> 
{
	public static final String TOKEN_OPTIONAL = "&optional";
	public static final String TOKEN_KEY = "&key";
	public static final String TOKEN_REST = "&rest";
	public static final String TOKEN_BODY = "&body";
	public static final String TOKEN_AUX = "&aux";
	public static final String TOKEN_ALLOW_OTHER_KEYS = "&allow-other-keys";
	public static final String TOKEN_ENVIRONMENT = "&environment";
	public static final String TOKEN_WHOLE = "&whole";
	
	protected IParserInput fInput;
	protected List<RequiredParameterDefinition> fRequired;
	protected List<ParameterDefinition> fOptional;
	protected Symbol fRest;
	protected Symbol fBody;
	protected List<ParameterDefinition> fKey;
	protected boolean fAllowOtherKeys;
	protected List<ParameterDefinition> fAux;

	@Override
	protected void setInput(final IParserInput input) {
		this.fInput = input;
	}
	
	@Override
	protected void cleanUp0()
	{
		fInput = null;
		fRequired = null;
		fOptional = null;
		fBody = null;
		fRest = null;
		fKey = null;
		fAux = null;
		fAllowOtherKeys = false;
	}
	
	protected void nil()
	throws ParseException
	{
		if(fInput.getInputSExpression() instanceof Symbol) {
			if(!fInput.getInputSExpression().isNil()) {
				error("NIL or lambda list expected", fInput.getInputSExpression());
			}
		}
	}
	
	protected void requiredParam() throws ParseException {
		if(!fInput.hasNext()) return; //leere Lambda List
		
		if(fInput.next().getTyp() == TSExpression.SYMBOL) {
			Symbol symbol = (Symbol)fInput.current();
			if(isSpecialSymbol(symbol)) 
			{
				fInput.pushBack();
				return;
			}
			
			addRequired(symbol);
			requiredParam();
			
		} else {
			error("Unknown symbol in required parameter list", fInput.current());
		}
	}
	
	protected void requiredParamDestructured() throws ParseException
	{
		if(!fInput.hasNext()) return;
		
		SExpression next = fInput.next();
		if(next.getTyp() == TSExpression.SYMBOL) {
			Symbol symbol = (Symbol)next;
			if(isSpecialSymbol(symbol)) {
				fInput.pushBack();
				return;
			}
			addRequired(symbol);
			requiredParamDestructured();
		} else if (next.getTyp() == TSExpression.SEXPRESSION || next instanceof Form) {	//echte sexp oder form, Symbol wird oben abgefragt
			                                                                            //String-, Key- oder Reader-Symbole = error
			addRequired(next);
			requiredParamDestructured();
		} else {
			error("Symbol or destructuring lambda list expected", fInput.current());
		}
	}

	protected boolean isSpecialSymbol(final SExpression sexp) {
		if(sexp.getTyp() != TSExpression.SYMBOL) return false;
		
		Symbol symbol = (Symbol)sexp;
		String symName = symbol.getSymbolName();
		return symName.equalsIgnoreCase(TOKEN_OPTIONAL)
		|| symName.equalsIgnoreCase(TOKEN_BODY)
		|| symName.equalsIgnoreCase(TOKEN_KEY)
		|| symName.equalsIgnoreCase(TOKEN_REST)
		|| symName.equals(TOKEN_AUX)
		|| symName.equals(TOKEN_ALLOW_OTHER_KEYS)
		|| symName.equals(TOKEN_WHOLE) 
		|| symName.equals(TOKEN_ENVIRONMENT);
	}

	private void addRequired(final SExpression form) {
		if(fRequired == null) {
			fRequired = new LinkedList<RequiredParameterDefinition>();
		}
		fRequired.add(new RequiredParameterDefinition(form));
	}
	
	private void addRequired(final Symbol symbol) {
		if(fRequired == null) {
			fRequired = new LinkedList<RequiredParameterDefinition>();
		}
		fRequired.add(new RequiredParameterDefinition(symbol));
	}

	protected void optionalParam() throws ParseException {
		if(!fInput.hasNext()) return; //leere Lambda Liste
		
		if(fInput.next().getTyp() == TSExpression.SYMBOL) {
			Symbol symbol = (Symbol)fInput.current();
			if( symbol.getSymbolName().equalsIgnoreCase(TOKEN_OPTIONAL) ) {
				
				//if(!fInput.hasNext()) { //&optional gesehen, aber kein weiteres symbol angegeben
				//	error("At least one &optional parameter must be specified", fInput.getInputSExpression());
				//}
				//addOptional(varInit(fInput.next())); //mindesten ein Symbol ist angegeben
				
				while(true) {
					if( !fInput.hasNext() ) {
						break;
					}
					
					SExpression next = fInput.next();
					if(isSpecialSymbol(next)) {
						fInput.pushBack();
						break;
					}
					
					addOptional(varInit(next));
				}
	
			} else {
				fInput.pushBack();
				return;
			}
		} else {
			error("Unknown symbol in optional parameter list: &optional expected", fInput.current());
		}
	}

	private ParameterDefinition varInit(final SExpression varInit) throws ParseException {	
		if(isSpecialSymbol(varInit)) {
			error("Unexpected symbol: " + ((Symbol)varInit).getSymbolName(), varInit);
		}
		
		if(varInit.getTyp() == TSExpression.SYMBOL) { //einfaches Symbol
			return new ParameterDefinition((Symbol)varInit, null, null);
		} else if(varInit.getTyp() == TSExpression.FORM) { //komplexe Vardeklaration
			return initForm((Form)varInit);
		} else {
			error("Illegal parameter definition", varInit);
			return null; //wird nie erreicht
		}
	}

	private ParameterDefinition initForm(final Form form) throws ParseException {
		if(form.isNil()) {
			error("NIL not allowed here", form);
		}
		
		List<SExpression> childs = form.getChildren();
		if(childs.size() == 2) {
			return simpleInit(form);
		} else if(childs.size() == 3) {
			return suppliedTestInit(form);
		} else {
			error("Parameter list must be of length 2 or 3", form);
			return null; //wird nie erreicht
		}
	}

	private ParameterDefinition simpleInit(final Form form) throws ParseException {
		//invariante: laenge childs == 2
	
		checkFirstAndSecond(form);
		return new ParameterDefinition(form.getFunctionSymbol(), form.getChild(1), null);
	}

	private ParameterDefinition suppliedTestInit(final Form form) throws ParseException {
		//invariante: laenge childs == 3
		checkFirstAndSecond(form);
		
		if(form.getChild(2).getTyp() != TSExpression.SYMBOL) {
			error("symbol expected", form.getChild(2));
		}
		
		return new ParameterDefinition(form.getFunctionSymbol(), form.getChild(1), (Symbol)form.getChild(2));
	}

	private void checkFirstAndSecond(final Form form) throws ParseException {		
		SExpression initVal = form.getChild(1);
		
		//als init-Wert ist Symbol (oder Subtyp) oder eine Form erlaubt. 
		//Nur s-expression reicht nicht aus
		if( !initVal.isNil() && !(initVal instanceof Symbol) && initVal.getTyp() != TSExpression.FORM ) {
			error("form or symbol expected", form.getChild(1));
		}
	}

	private void addOptional(final ParameterDefinition definition) {
		if(fOptional == null) {
			fOptional = new LinkedList<ParameterDefinition>();
		}
		fOptional.add(definition);
	}

	protected void rest()
	throws ParseException
	{
		if( !fInput.hasNext() ) return; //testen ob ueberhaupt noch was vorhanden
		
		if( !isNextSymbol(TOKEN_REST) ) { //Rest Symbol
			fInput.pushBack();  //nein, dann symbol wieder zuruecklegen, evtl. &key oder &aux
			return;
		}
		
		checkNextIsType(TSExpression.SYMBOL);
		fRest = (Symbol)fInput.current();
	}
	
	protected void bodyOrRest() throws ParseException {
		if( !fInput.hasNext() ) return;
		
		if(fInput.next().getTyp() == TSExpression.SYMBOL) {
			Symbol symbol = (Symbol)fInput.current();
			if(symbol.getSymbolName().equalsIgnoreCase(TOKEN_BODY)) {
				fBody = readRestOrBodySymbol("body");
			} else if(symbol.getSymbolName().equalsIgnoreCase(TOKEN_REST)) {
				fRest = readRestOrBodySymbol("rest");
			} else {
				fInput.pushBack(); //evtl. &key
				return;
			}
		} else {
			error("Unknown symbol in rest or body list: &body or &rest expected", fInput.current());
		}
	}

	private Symbol readRestOrBodySymbol(final String bodyRest) throws ParseException {
		if( !fInput.hasNext() ) {
			error(bodyRest + " symbol expected", fInput.getInputSExpression());
		}
		
		SExpression symbol = fInput.next();
		if(symbol.getTyp() == TSExpression.SYMBOL && !isSpecialSymbol(symbol)) { //echtes symbol
			return (Symbol)symbol;
		}
		
		error(bodyRest + " symbol expected", symbol);
		return null; //wird nie erreicht
	}

	protected void keywordParam() throws ParseException {
		if( !fInput.hasNext() ) return;
		
		if( fInput.next().getTyp() == TSExpression.SYMBOL) {
			Symbol symbol = (Symbol)fInput.current();
			if(symbol.getSymbolName().equalsIgnoreCase(TOKEN_KEY)) {
				
				while(true) {
					if(!fInput.hasNext()) {
						break;
					}
					
					SExpression next = fInput.next();
					if( isSpecialSymbol(next) ) {
						fInput.pushBack();
						break;
					}
					
					addKey(varInit(next));
				}
				
				allowOtherKeys();
			} else {
				fInput.pushBack();
			}
		} else {
			error("Unknown symbol in key list: &key expected", fInput.getInputSExpression());
		}
	}

	private void allowOtherKeys() {
		if(!fInput.hasNext()) return;
		
		fAllowOtherKeys = isNextSymbol(TOKEN_ALLOW_OTHER_KEYS);
		if(!fAllowOtherKeys) {
			fInput.pushBack();
		}
	}

	private void errorWrongOrder() throws ParseException {
		error("Wrong order of & parameters, order is: &optional &rest or &body &key", fInput.getInputSExpression());
	}

	private void addKey(final ParameterDefinition key) {
		if(fKey == null) {
			fKey = new LinkedList<ParameterDefinition>();
		}
		
		fKey.add(key);
	}

	protected void auxParam(final boolean defun) throws ParseException {
		if(!fInput.hasNext()) return;
		
		checkNextIsSymbol(TOKEN_AUX);
		//checkHasNext("At least one &aux parameter must be specified");
		//addAux(varInit(fInput.next()));
		
		while(true) {
			if( !fInput.hasNext() ) {
				break;
			}
			
			SExpression next = fInput.next();
			if(isSpecialSymbol(next)) {
				if(defun) {
					errorWrongOrder();
				} else {
					fInput.pushBack();
					break;
				}
			}
			
			addAux(varInit(next));
		}
	}

	private void addAux(final ParameterDefinition aux) {
		if(fAux == null) {
			fAux = new LinkedList<ParameterDefinition>();
		}
		fAux.add(aux);
	}

}