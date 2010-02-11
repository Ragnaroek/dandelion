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

import java.util.*;

import de.defmacro.dandelion.internal.ui.text.IDocumentVisitor;

/**
 * Ein {@link ISexpDOMVisitor} der alle in-package Ausdruecke
 * aus dem Modell sammelt.
 * @author Michael Bohn
 */
public class InpackageCollector 
extends SexpressionDOMVisitorAdapter 
{
	private List<InpackageForm> fInpackages;
	private boolean fDiscardNullPositions;
	
	/**
	 * Erstellt einen neuen {@link InpackageCollector}.
	 * Null Positionen werden in die Ergebnismenge aufgenommen.
	 */
	public InpackageCollector()
	{
		this(false);
	}
	
	/**
	 * Erstellt einen nuene {@link InpackageCollector}.
	 * @param discardNullPositions - null Positionen aus Ergebnismenge entfernen.
	 */
	public InpackageCollector(final boolean discardNullPositions) {
		this.fDiscardNullPositions = discardNullPositions;
	}
	
	/**
	 * @see IDocumentVisitor#preVisit(org.eclipse.jface.text.IDocument)
	 */
	@Override
	public void preVisit(final ISexpModel model) {
		fInpackages = new ArrayList<InpackageForm>();
	}

	/**
	 * @see ISexpDOMVisitor#visit(InpackageForm)
	 */
	@Override
	public boolean visit(final InpackageForm form) {
		if(fDiscardNullPositions) {
			if(form.getPosition() != null) {
				fInpackages.add(form);
			}
		} else {
			fInpackages.add(form);
		}
		
		return true;
	}
	
	/**
	 * Gibt die gesammelten in-package Ausdruecke zurueck.
	 * Liefert <code>null</code> wenn an ein Modell angelegt wurde.
	 * @return
	 */
	public List<InpackageForm> getInpackages()
	{
		return fInpackages;
	}
}
