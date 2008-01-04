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
import de.fh_trier.eclipse.lisp.internal.core.meta.*;

/**
 * Sammelt Vorschlaege fuer Lisp-Funktionen/Makros.
 * @author Michael Bohn
 *
 */
public class FunctionProposalCollector 
extends AbstractProposalCollector
{
	/**
	 * Erzeugt einen neuen Collector fuer Funktionssymbole mit Praefix.
	 * @param prefix - Praefix mit dem die Symbole beginnen muessen.
	 */
	public FunctionProposalCollector(final String prefix)
	{
		super(prefix);
	}
	
	/**
	 * @see ISexpDOMVisitor#visit(DefmacroForm)
	 */
	@Override
	public boolean visit(final DefmacroForm form) {
		addMetaSymbol(form, TMetaType.MACRO);
		return true;
	}

	/**
	 * @see ISexpDOMVisitor#visit(DefunForm)
	 */
	@Override
	public boolean visit(final DefunForm form) {
		addMetaSymbol(form, TMetaType.FUNCTION);
		return true;
	}
	
	private void addMetaSymbol(final FunctionDefiningForm form, final TMetaType type) {
		Symbol sym = form.getDefinedName();
		if(sym == null) {
			return;
		}
		if(checkPrefix(sym)) {
			cons(constructMetaSymbol(form, type));
		}
	}
}
