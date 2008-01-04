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

package de.fh_trier.eclipse.lisp.internal.ui.text.presentation;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;

/**
 * Sammelt die globalen Symbol aus dem Modell.
 * @author Michael Bohn
 *
 */
public class GlobalSymbolProposalCollector
extends AbstractProposalCollector
{
	/**
	 * Erzeugt einen Vistor fuer das einsammeln der globalen Symbole.
	 * @param prefix - Praefix mit dem Symbole beginnen muessen.
	 */
	public GlobalSymbolProposalCollector(final String prefix) {
		super(prefix);
	}

	/**
	 * @see ISexpDOMVisitor#visit(Symbol)
	 */
	@Override
	public boolean visit(final Form form) {
		Symbol formSym = form.getFunctionSymbol();
		if(formSym == null) {
			return false;
		}
		
		String formName = formSym.getSymbolName();
		if(formName.equalsIgnoreCase("defparameter") ||
				formName.equalsIgnoreCase("defconstant") ||
				formName.equalsIgnoreCase("defvar") ) {
						
			if(form.hasChildren() && form.getChildren().size() > 1) {
				SExpression name = form.getChild(1);
				if(name.getTyp() == TSExpression.SYMBOL) {
					Symbol nameSym = (Symbol)name;
					if(checkPrefix(nameSym)) {
						cons(constructMetaSymbol(nameSym));
					}
				}
			}
		}
		
		return false; //nicht weiter suchen
	}
}
