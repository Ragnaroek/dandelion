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

package de.defmacro.dandelion.internal.ui.views;

import org.eclipse.jface.text.source.*;
import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

public class RangeIndicator 
extends Annotation
implements IAnnotationPresentation 
{
	private Color color;
	private int   systemColor = SWT.COLOR_INFO_BACKGROUND;
	
	public RangeIndicator(Color color)
	{
		if (color == null) {
			throw new NullPointerException("Color must not be null");
		}
		
		this.color = color;
	}
	
	public RangeIndicator(int systemColor)
	{
		this.systemColor = systemColor;
	}
	
	public int getLayer() {
		return IAnnotationPresentation.DEFAULT_LAYER;
	}

	public void paint(GC gc, Canvas canvas, Rectangle bounds) {
		
		if(color == null) {
			gc.setBackground(canvas.getDisplay().getSystemColor(systemColor));
		} else {
			gc.setBackground(color);
		}
		
		gc.fillRectangle(bounds);
	}
}
