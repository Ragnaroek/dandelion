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

import java.util.List;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;

/**
 * Sammelt alle Symbole auf dem Toplevel.
 * @author Michael Bohn
 *
 */
public class TopLevelSymbolProposalCollector 
extends AbstractProposalCollector
{
	public TopLevelSymbolProposalCollector(final String prefix)
	{
		super(prefix);
	}

	@Override
	public boolean visit(final DefmacroForm form) 
	{	
		if(form.hasMalformation()) {
			return true;
		}

		consArguments(getArgumentNames(form.getLambdaList()));
		return super.visit(form);
	}

	@Override
	public boolean visit(final DefunForm form) 
	{	
		if(form.hasMalformation()) {
			return true;
		}
		consArguments(getArgumentNames(form.getLambdaList()));
		return true;
	}
	
	private void consArguments(final List<Symbol> symbols) {
		for(Symbol sym : symbols) {
			if(checkPrefix(sym)) {
				cons(constructMetaSymbol(sym));
			}
		}
	}

	@Override
	public boolean visit(final Symbol symbol) 
	{
		if(collectType(symbol.getTyp()) && checkPrefix(symbol)) {
			cons(constructMetaSymbol(symbol));
		}
		return true;
	}
	
	private boolean collectType(final TSExpression type)
	{
		//nur 'echte' symbole und keyword-symbole aufnehmen
		return type == TSExpression.SYMBOL || type == TSExpression.KEYWORD_SYMBOL;
	}
}
