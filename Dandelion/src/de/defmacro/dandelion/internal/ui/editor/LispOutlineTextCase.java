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

package de.defmacro.dandelion.internal.ui.editor;

import de.defmacro.dandelion.internal.core.dom.*;

/**
 * Die Fallunterscheidung fuer den Text der Typen
 * in der Outline.
 * @author Michael Bohn
 *
 */
public class LispOutlineTextCase 
implements Case<String> 
{
	private static final String ERROR = "<error in form>";
	
	/**
	 * Fallthrough angeschaltet.
	 */
	public boolean isFallthrough() {
		return true;
	}

	public String typeCase(final InpackageForm form) 
	{
		Symbol pack = form.getPackage();
		if(pack == null) {
			return ERROR;
		}
		return pack.getSymbolName();
	}

	public String typeCase(final DefpackageForm form) {
		return null; //fallthrough nach DefiningForm
	}

	public String typeCase(final LambdaForm form) {
		return "lambda function";
	}

	public String typeCase(final DefmacroForm form) {
		return null; //fallthrough nach DefiningForm
	}

	public String typeCase(final DefunForm form) {
		return null; //falthrough nach DefiningForm
	}

	public String typeCase(final Symbol symbol) {
		return symbol.getSymbolName();
	}

	public String typeCase(final Form form) 
	{	
		Symbol functionSymbol = form.getFunctionSymbol();
		if(functionSymbol == null) {
			return ERROR;
		}
		
		return functionSymbol.getSymbolName();
	}

	public String typeCase(final SExpression sexp) 
	{
		if(sexp.isNil()) {
			return Symbol.SYM_NIL;
		}
		
		//invariante: hat mindestens ein Child
		StringBuilder string = new StringBuilder();
		
		SExpression sexpchild = sexp.getChild(0);
		
		if(sexpchild instanceof Symbol) {
			String symName = ((Symbol)sexpchild).getSymbolName();
			//invariante: symName != null
			string.append(symName);
			if(sexp.getChildren().size() > 1) {
				string.append(" ...");
			}
			
		} else {
			string.append("(...)");
		}
		
		return string.toString();
	}

	public String typeCase(final FunctionDefiningForm form) {
		return null; //fallthrough nach DefiningForm
	}

	public String typeCase(final DefiningForm form) 
	{
		Symbol name = form.getDefinedName();
		if( form.hasMalformation() ) {
			if(name == null) {
				return ERROR;
			}
		}
		
		return name.getSymbolName();
	}
}
