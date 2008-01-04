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

package de.fh_trier.eclipse.lisp.internal.ui.text.partition;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;

import de.fh_trier.eclipse.lisp.internal.ui.text.rules.BlockCommentRule;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.CharacterRule;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.InlineCommentRule;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.StringRule;

/**
 * Der Scanner fuer Lisp-Partitionen.
 * Instanzierung ueber Factory-Methoden.
 * @author Michael Bohn
 */
/*package*/ class LispPartitionScanner 
extends RuleBasedPartitionScanner
{
	private static RuleBasedPartitionScanner INSTANCE; //singleton
	private static RuleBasedPartitionScanner COMMENT_INSTANCE;
	
	private LispPartitionScanner(final boolean commentScanner)
	{
		List<IPredicateRule> rules = new ArrayList<IPredicateRule>();
		
		if(!commentScanner) {
			rules.add(new BlockCommentRule(LispPartitionConstants.TOKEN_COMMENT));
			rules.add(new InlineCommentRule(LispPartitionConstants.TOKEN_COMMENT));
		}
		
		rules.add(new StringRule(LispPartitionConstants.TOKEN_STRING));
		rules.add(new CharacterRule(LispPartitionConstants.TOKEN_CHAR));
		
		setPredicateRules(rules.toArray(new IPredicateRule[rules.size()]));
	}

	/**
	 * Liefert die Singleton-Instanz des Quelltextscanners.
	 * @return Singleton-Instanz Quelltextscanner
	 */
	public static RuleBasedPartitionScanner instanceOf()
	{
		if(INSTANCE == null) {
			INSTANCE = new LispPartitionScanner(false);
		}
		return INSTANCE;
	}
	
	/**
	 * Liefert die Singleton-Instanz des Kommantarscanners.
	 * @return - Singletion-Instanz Kommentarscanner
	 */
	public static RuleBasedPartitionScanner instanceOfCommentScanner()
	{
		if(COMMENT_INSTANCE == null) {
			COMMENT_INSTANCE = new LispPartitionScanner(true);
		}
		return COMMENT_INSTANCE;
	}
}
