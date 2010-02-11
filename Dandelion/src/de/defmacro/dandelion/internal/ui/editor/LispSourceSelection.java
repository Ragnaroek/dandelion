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

import java.util.*;

import org.eclipse.jface.viewers.ISelection;

/**
 * Standarimplementierung der {@link ILispSourceSelection}-Schnittstelle.
 * @author Michael Bohn
 *
 */
/*package*/ class LispSourceSelection 
implements ILispSourceSelection 
{
	/**
	 * Die leere Auswahl.
	 */
	public static final ILispSourceSelection NULL_SELECTION = new ILispSourceSelection()
	{
		public List<PackageBoundForm> getForms() {
			return Collections.emptyList();
		}

		public boolean hasErrors() {
			return false;
		}

		public boolean isEmpty() {
			return true;
		}
	};
	
	private boolean fHasErrors;
	private List<PackageBoundForm> fForms;
	
	/**
	 * Erzeugt eine neue Quelltextauswahl fuer einen Ausdruck.
	 * @param pack - Paket
	 * @param form - Ausdruck
	 * @param hasErrors - enthaelt Fehler ja/nein
	 */
	public LispSourceSelection(final String pack, final String form, final boolean hasErrors)
	{
		List<PackageBoundForm> forms = new ArrayList<PackageBoundForm>(1);
		forms.add(new PackageBoundForm(pack, form));
		
		init(forms, hasErrors);
	}
	
	/**
	 * Erzeugt eine neue QuelltextAuswahl fuer eine Liste von Ausdruecken.
	 * @param forms - Liste von {@link PackageBoundForm}-Ausdruecken
	 * @param hasErrors - enthaelt Feheler ja/nein
	 * @throws NullPointerException - wenn forms == <code>null</code>
	 */
	public LispSourceSelection(final List<PackageBoundForm> forms, final boolean hasErrors) 
	{
		if (forms == null) {
			throw new NullPointerException("forms must not be null");
		}

		init(forms, hasErrors);
	}
	
	private void init(final List<PackageBoundForm> forms, final boolean hasErrors)
	{
		this.fForms = Collections.unmodifiableList(forms);
		this.fHasErrors = hasErrors;
	}

	/**
	 * Die zurueckgegebene Liste ist immutable.
	 * @see ILispSourceSelection#getForms()
	 */
	public List<PackageBoundForm> getForms() {
		return fForms;
	}

	/**
	 * @see ILispSourceSelection#hasErrors()
	 */
	public boolean hasErrors() {
		return fHasErrors;
	}

	/**
	 * @see ISelection#isEmpty()
	 */
	public boolean isEmpty() {
		return fForms.isEmpty();
	}
}
