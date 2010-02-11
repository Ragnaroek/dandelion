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

package de.defmacro.dandelion.internal.ui.text.rules;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.rules.ICharacterScanner;

public class LispPrefixWordEndDetector 
implements IPrefixedWordEndDetector 
{
	private static final List<Character> validEndChars;
	private int minReads;
	
	static {
		validEndChars = new ArrayList<Character>();
		validEndChars.add(')');
		validEndChars.add(';');
	}
	
	public LispPrefixWordEndDetector()
	{
		this(0);
	}
	
	/**
	 * 
	 * @param minReads - Mindestens diese Anzahl chars muss gelesen
	 * 	werden, bevor mit der Suche nach Ende begonnen wird.
	 */
	public LispPrefixWordEndDetector(int minReads)
	{
		this.minReads = minReads;
	}
	
	public void detectEnd(ICharacterScanner scanner) 
	{
		//Lese min. Anzahl Zeichen
		for(int i=0;i<minReads;i++) {
			scanner.read();
		}
		
		int ch = scanner.read();
		while(ch != ICharacterScanner.EOF
				&& isWordEnd((char)ch))
		{
			ch = scanner.read();
		}
		scanner.unread();
	}
	
	public static boolean isWordEnd(char c)
	{
		return !Character.isWhitespace(c) && !validEndChars.contains(c);
	}
}
