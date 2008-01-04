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

import java.util.*;

/**
 * Standardimplementierung der
 * {@link IMalformationProvider}-Schnittstelle.
 * @author Michael Bohn
 */
public class DefaultMalformationProvider 
implements IMalformationProvider
{
	private List<ISyntacticalMalformation> fMalformationStore;
	
	/**
	 * Erzeugt einen neuen MalformationProvider.
	 */
	public DefaultMalformationProvider()
	{
		fMalformationStore = new LinkedList<ISyntacticalMalformation>();
	}
	
	/**
	 * @see IMalformationProvider#getMalformations()
	 */
	public List<ISyntacticalMalformation> getMalformations() 
	{
		return Collections.unmodifiableList(fMalformationStore);
	}

	/**
	 * @see IMalformationProvider#hasMalformation()
	 */
	public boolean hasMalformation() 
	{
		return !fMalformationStore.isEmpty();
	}

	/**
	 * @see IMalformationProvider#hasMalformation(TSeverity)
	 */
	public boolean hasMalformation(TSeverity severity) 
	{
		for(ISyntacticalMalformation malformation : fMalformationStore) {
			if(malformation.getSeverity() == severity) {
				return true;
			}
		}
		
		return false;
	}

	/**
	 * @see IMalformationProvider#addMalformation(ISyntacticalMalformation)
	 */
	public void addMalformation(ISyntacticalMalformation malformation) 
	{
		fMalformationStore.add(malformation);
	}
	
	/**
	 * @see IMalformationProvider#addAll(Collection)
	 */
	public void addAll(Collection<ISyntacticalMalformation> malformations) 
	{
		if( !malformations.isEmpty() ) {
			fMalformationStore.addAll(malformations);
		}
	}

	/**
	 * @see IMalformationProvider#clearMalformations()
	 */
	public void clearMalformations() {
		fMalformationStore.clear();
	}
}
