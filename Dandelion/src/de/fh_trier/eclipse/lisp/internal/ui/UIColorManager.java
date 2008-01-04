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

package de.fh_trier.eclipse.lisp.internal.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

import de.fh_trier.eclipse.lisp.internal.IDisposable;

/**
 * Verwaltet Farben.
 * @author Michael Bohn
 *
 */
public class UIColorManager 
implements IDisposable
{
	private Map<RGB, Color> colorMap = new HashMap<RGB, Color>();
	
	/**
	 * Liefert die Farbe fuer das RGB-Objekt.
	 * Ist das Color-Objekt noch nicht vorhanden, wird es erzeugt.
	 * Ist das Color-Objekt bereits vorhanden, wird dieses Objekt zurueckgeliefert.
	 * @param rgb
	 * @return Das Farbojekt fuer die RGB-Angabe.
	 * @throws NullPointerException - wenn der Manager mit {@link UIColorManager#dispose()} entsorgt wurde
	 */
	public Color getColor(final RGB rgb)
	{
		Color color = colorMap.get(rgb);
		if(color == null) {
			color = new Color(Display.getCurrent(), rgb);
			colorMap.put(rgb, color);
		}
		return color;
	}
	
	/**
	 * Entsorgt alle Farben in diesem Farbmanager.
	 * Der Farbmanager darf danach nicht mehr verwendet werden.
	 * @throws NullPointerException - wenn mehr als zweimal aufgerufen wird.
	 */
	public void dispose()
	{
		for(Map.Entry<RGB, Color> entry : colorMap.entrySet()) {
			entry.getValue().dispose();
		}
		colorMap.clear();
		colorMap = null;
	}
}
