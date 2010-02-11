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

package de.defmacro.dandelion.internal.core.dom;

/**
 * Schnittstelle fuer das TypeSwitch.
 * @author Michael Bohn
 *
 * @param <T>
 */
public interface Case<T> 
{
	public T typeCase(InpackageForm form);
	public T typeCase(DefpackageForm form);
	public T typeCase(LambdaForm form);
	public T typeCase(DefmacroForm form);
	public T typeCase(DefunForm form);
	public T typeCase(Symbol form);
	public T typeCase(Form form);
	public T typeCase(SExpression sexp);
	public T typeCase(FunctionDefiningForm form);
	public T typeCase(DefiningForm form);
	
	/**
	 * Wird hier <code>true</code> zurueckgegeben, werden
	 * nur Typen am Ende der Typhierarchie besucht. Andernfalls
	 * wird die komplette Typhierarchie aufgerufen. Der Wert des ersten
	 * typeCase-Aufruf (ausgehend vom Ende der Hierarchie) der nicht
	 * <code>null</code> zurueckgibt, wird als Ergebnis geliefert.
	 * @return
	 */
	public boolean isFallthrough();
}
