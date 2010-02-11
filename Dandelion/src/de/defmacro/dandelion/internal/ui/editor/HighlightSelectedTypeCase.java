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

import org.eclipse.jface.text.Position;

import de.defmacro.dandelion.internal.core.dom.*;

/**
 * Die Fallunterscheidung fuer Selektion im Editor
 * fuer einen bestimmten Typ.
 * @author Michael Bohn
 *
 */
public class HighlightSelectedTypeCase 
implements Case<Position> 
{
	private HighlightSelectedTypeCase()
	{
		//Instanz ueber Factory-Methode
	}
	
	private static HighlightSelectedTypeCase instance;
	
	public static HighlightSelectedTypeCase instanceOf() {
		if (instance == null) {
			HighlightSelectedTypeCase.instance = new HighlightSelectedTypeCase();
		}
		return instance;
	}
	
	/**
	 * Fallthrough angeschaltet.
	 */
	public boolean isFallthrough() {
		return true;
	}

	public Position typeCase(final DefiningForm form) {
		Symbol sym = form.getDefinedName();
		if(sym == null) {
			return form.getPosition();
		}
		return sym.getPosition();
	}

	public Position typeCase(final DefmacroForm form) {
		//fallthrough nach DefiningForm
		return null;
	}

	public Position typeCase(final DefpackageForm form) {
		//fallthrough nach DefiningForm
		return null;
	}

	public Position typeCase(final DefunForm form) {
		//fallthrough nach DefiningForm
		return null;
	}

	public Position typeCase(final Form form) {
		Symbol sym = form.getFunctionSymbol();
		if(sym == null || sym.getSymbolName().equals(Symbol.SYM_BACKQUOTE) 
				|| sym.getSymbolName().equals(Symbol.SYM_QUOTE)
				|| sym.getSymbolName().equals(Symbol.SYM_COMMA)) {
			return form.getPosition();
		}
		return sym.getPosition();
	}

	public Position typeCase(final FunctionDefiningForm form) {
		//fallthroug nach DefiningForm
		return null;
	}

	public Position typeCase(final InpackageForm form) {
		Symbol pack = form.getPackage();
		if(pack == null) {
			return form.getPosition();
		}
		return pack.getPosition();
	}

	public Position typeCase(final LambdaForm form) {	
		return form.getPosition();
	}

	public Position typeCase(final SExpression sexp) {
		return sexp.getPosition();
	}

	public Position typeCase(final Symbol form) {
		return form.getPosition();
	}
}
