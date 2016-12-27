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
import org.eclipse.swt.graphics.Image;

import de.defmacro.dandelion.internal.core.meta.IMetaSymbol;
import de.defmacro.dandelion.internal.ui.LispUI;

class AproposQueryResultLabelProvider 
extends LabelProvider 
implements ITableLabelProvider 
{	
	public String getColumnText(final Object obj, final int index) 
	{
		if(obj == null) {
			return null;
		}
		
		IMetaSymbol meta = (IMetaSymbol)obj;
		switch(index) {
		case 1 : return meta.getPackage();
		case 2 : return meta.getSymbolName();
		case 3 : return meta.getType().toString();
		case 4 : return meta.getArgumentString(false);
		case 5 : return meta.getDocumentation();
		default : return null;
		}
	}
	
	public Image getColumnImage(final Object obj, final int index) 
	{
		if(obj == null) {
			return null;
		}
		
		if(index == 0) {
			return LispUI.getUIImageManager().getImageForMetaType(((IMetaSymbol)obj).getType());
		}
		return null;
	}
}