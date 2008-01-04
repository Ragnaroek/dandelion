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

import java.util.List;

import org.eclipse.jface.text.Position;

/**
 * Klasse fuer Ausdruecke die einen Namen definieren.
 * @author Michael Bohn
 */
public abstract class DefiningForm 
extends Form 
{
	public DefiningForm(final List<SExpression> childs, Position position) {
		super(childs, position);
	}
	
	/**
	 * Liefert den von diesem Ausdruck definierten Name.
	 * @return 
	 */
	public abstract Symbol getDefinedName();
	
	/**
	 * @see Case
	 */
	@Override
	public <T> T typeSwitch(final Case<T> c)
	{
		T result = c.typeCase(this);
		if(c.isFallthrough() && result == null) {
			return super.typeSwitch(c);
		}
		return result;
	}
}
