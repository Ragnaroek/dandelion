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

package de.defmacro.dandelion.internal.ui.text;

import java.util.Calendar;

/**
 * Bietet verschiedene Hilfmethoden fuer 
 * die Textbearbeitung.
 * @author Michael Bohn
 *
 */
public class TextUtilities 
{
	/**
	 * Ein Array mit Strings die alle Whitespace-Zeichen enthalten.
	 * Das Array darf nicht veraendert werden!
	 */
	public static String[] WHITESPACE_BAG = new String[] {" ", "\n", "\t", "\r"};
	
	private static Calendar fCalendar = Calendar.getInstance();
	
	private TextUtilities()
	{
		//nur static methoden
	}
	
	/**
	 * Liefert eine formatierte Uhrzeitangabe aus den Millisekunden.
	 * Format HH:MM:SS.
	 * @param millis
	 * @return Formatierte Zeitangabe
	 */
	public static String getTimePresentation(long millis)
	{
		StringBuilder time = new StringBuilder();
		
		fCalendar.setTimeInMillis(millis);
		time.append(fill(fCalendar.get(Calendar.HOUR_OF_DAY)));
		time.append(":");
		time.append(fill(fCalendar.get(Calendar.MINUTE)));
		time.append(":");
		time.append(fill(fCalendar.get(Calendar.SECOND)));
		
		return time.toString();
	}
	
	private static String fill(int num)
	{
		if(num < 10) {
			return "0" + num;
		}
		return ""+num;
	}
}
