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

package de.fh_trier.eclipse.lisp.internal.core.dom;

/**
 * Adapter-Implementierung der {@link ISexpDOMVisitor}-Schnittstelle.
 * @author Michael Bohn
 */
public class SexpressionDOMVisitorAdapter 
implements ISexpDOMVisitor 
{
	public void preVisit(final ISexpModel model) {
		//no-op
	}

	public boolean visit(final InpackageForm form) {
		return true;
	}

	public boolean visit(final DefpackageForm form) {
		return true;
	}

	public boolean visit(final LambdaForm form) {
		return true;
	}

	public boolean visit(final DefmacroForm form) {
		return true;
	}

	public boolean visit(final DefunForm form) {
		return true;
	}

	public boolean visit(final Symbol form) {
		return true;
	}

	public boolean visit(final Form form) {
		return true;
	}

	public boolean visit(final SExpression sexp) {
		return true;
	}
}
