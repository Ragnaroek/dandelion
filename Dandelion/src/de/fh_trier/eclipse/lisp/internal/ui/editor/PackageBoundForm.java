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

package de.fh_trier.eclipse.lisp.internal.ui.editor;

/**
 * Buendelt die Angabe einer Form und das zugehoerige
 * Package.
 * @author Michael Bohn
 *
 */
public class PackageBoundForm
{
	private final String fPack;
	private final String fForm;
	
	/**
	 * Erzeugt eine neuen Ausdruck mit zugehoeriger Paketangabe.
	 * @param pack - Paket
	 * @param form - Der Ausdruck
	 */
	public PackageBoundForm(final String pack, final String form)
	{	
		if (pack == null) {
			throw new NullPointerException("pack must not be null");
		}
		
		if (form == null) {
			throw new NullPointerException("form must not be null");
		}
		
		this.fPack = pack;
		this.fForm = form;
	}
	
	/**
	 * Liefert das Paket der Form.
	 * @return
	 */
	public String getPackage()
	{
		return fPack;
	}
	
	/**
	 * Liefert den Ausdruck.
	 * @return
	 */
	public String getForm()
	{
		return fForm;
	}

	/**
	 * Eindeutiger hashcode.
	 * Von Eclipse generiert.
	 */
	@Override
	public int hashCode() {
		final int PRIME = 31;
		int result = 1;
		result = PRIME * result + ((fForm == null) ? 0 : fForm.hashCode());
		result = PRIME * result + ((fPack == null) ? 0 : fPack.hashCode());
		return result;
	}

	/**
	 * Test auf Gleichheit.
	 * Von Eclipse generiert.
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final PackageBoundForm other = (PackageBoundForm) obj;
		if (fForm == null) {
			if (other.fForm != null)
				return false;
		} else if (!fForm.equals(other.fForm))
			return false;
		if (fPack == null) {
			if (other.fPack != null)
				return false;
		} else if (!fPack.equals(other.fPack))
			return false;
		return true;
	}

	/**
	 * String-Repraesentatin des Objektes.
	 * Format kann sich jederzeit aendern.
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("[");
		builder.append(getClass().getName());
		builder.append(": ");
		builder.append("in-package=");
		builder.append(fPack);
		builder.append(" form=");
		builder.append(fForm);
		builder.append("]");
		return builder.toString();
	}
}