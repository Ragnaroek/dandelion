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

package de.fh_trier.eclipse.lisp.internal.ui.text.rules;

import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

public class PrefixedWordRule 
implements IRule
{	
	private String prefix;
	private IToken returnToken;
	private IPrefixedWordEndDetector endDetector;
	
	public PrefixedWordRule(String prefix, IToken token, IPrefixedWordEndDetector endDetector)
	{
		if(prefix == null) {
			throw new IllegalArgumentException("Prefix must not be null");
		}
		
		if(endDetector == null) {
			throw new IllegalArgumentException("EndDetector must not be null");
		}
		
		this.prefix = prefix;
		this.returnToken = token;
		this.endDetector = endDetector;
	}

	public IToken evaluate(ICharacterScanner scanner) {
		int reads = 1;
		int ch = scanner.read();
		int pch;
		while(reads <= prefix.length()) {
			pch = prefix.charAt(reads-1);
			if(ch == ICharacterScanner.EOF || pch != ch) {
				for(int i=1;i<=reads;i++)
					scanner.unread();
				return Token.UNDEFINED;
			}
			ch = scanner.read();
			reads++;
		}
		
		//prefix passt, lese bis zu Endtoken(s)
		endDetector.detectEnd(scanner);
		
		return returnToken;
	}
	
}
