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

package de.defmacro.dandelion.internal.ui.views.apropos;

import org.eclipse.jface.viewers.*;

import de.defmacro.dandelion.internal.core.meta.IMetaSymbol;

class AproposQueryResultSorter 
extends ViewerSorter 
{
	public enum TSortBy
	{
		DEFAULT,
		TYP,
		SYMBOL_NAME,
		PACKAGE,
		ARGUMENTS,
		DOCUMENTATION,
	}
	
	private TSortBy fSortBy;
	
	public AproposQueryResultSorter(final TSortBy sortBy)
	{
		this.fSortBy = sortBy;
	}
	
	@Override
	public int category(final Object element) 
	{
		IMetaSymbol meta = (IMetaSymbol)element;
		return meta.getType().getOrderNum();
	}

	@Override
	public int compare(final Viewer viewer, final Object e1, final Object e2) 
	{	
		IMetaSymbol meta1 = (IMetaSymbol)e1;
		IMetaSymbol meta2 = (IMetaSymbol)e2;
		
		switch(fSortBy) {
		case ARGUMENTS : return stringCompare(meta1.getArgumentString(false), meta2.getArgumentString(false));
		case DOCUMENTATION : return stringCompare(meta1.getDocumentation(), meta2.getDocumentation());
		case PACKAGE : return stringCompare(meta1.getPackage(), meta2.getPackage());
		case SYMBOL_NAME : return stringCompare(meta1.getSymbolName(), meta2.getSymbolName());
		case TYP  : return meta1.getType().getOrderNum() - meta2.getType().getOrderNum();
		case DEFAULT : //fallthrough nach default
		default: return meta1.compareTo(meta2);
		}
	}
	
	private int stringCompare(final String s1, final String s2)
	{
		if(s1 == null && s2 != null) {
			return -1;
		} else if(s1 != null && s2 == null) {
			return 1;
		} else if(s1 == null && s2 == null) {
			return 0;
		}
		
		return s1.compareTo(s2);
	}
}