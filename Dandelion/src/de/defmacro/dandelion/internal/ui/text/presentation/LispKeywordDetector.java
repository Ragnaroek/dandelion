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

package de.defmacro.dandelion.internal.ui.text.presentation;

import org.eclipse.jface.text.rules.IWordDetector;

/**
 * Sucht nach Lisp-Keywordsymbolen.
 * @author Michael Bohn
 *
 */
/*package*/ class LispKeywordDetector 
implements IWordDetector 
{
	private final String[] keywords;
	
	/**
	 * Ein Detector fuer Lisp-Keywoerter.
	 * @param keywords - Array der Keywoerter
	 */
	public LispKeywordDetector(final String[] keywords)
	{
		this.keywords = keywords;
	}
	
	/**
	 * Kommt char in einem Keyword vor.
	 * @see IWordDetector#isWordPart(char)
	 */
	public boolean isWordPart(char c) 
	{
		for(String word : keywords) {
			if(word.indexOf(Character.toLowerCase(c)) > -1) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Beginnt Keywort mit char.
	 * @see IWordDetector#isWordStart(char)
	 */
	public boolean isWordStart(char c) 
	{	
		for(String word : keywords) {
			if(word.charAt(0) == Character.toLowerCase(c)) {
				return true;
			}
		}
		return false;
	}
}
