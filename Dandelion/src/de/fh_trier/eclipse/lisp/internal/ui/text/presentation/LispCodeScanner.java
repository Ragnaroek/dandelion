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

package de.fh_trier.eclipse.lisp.internal.ui.text.presentation;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;

import de.fh_trier.eclipse.lisp.internal.ui.text.presentation.SyntaxHighlightingDefinition.SyntaxType;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.CaseInsensitiveWordRule;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.IPrefixedWordEndDetector;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.KeywordRule;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.KeywordSymbolRule;
import de.fh_trier.eclipse.lisp.internal.ui.text.rules.LispPrefixWordEndDetector;

/**
 * Der Scanner fuer Lisp-Code.
 * @author Michael Bohn
 *
 */
/*package*/ class LispCodeScanner 
extends RuleBasedScanner
{	
	/**
	 * Erzeugt einen neuen Scanner fuer Lisp-Quelltext.
	 */
	public LispCodeScanner()
	{
		//Tokens erstellen
		SyntaxHighlightingDefinition def = LispSourcePresentationManager.getSyntaxHighlightingDefinition();
		IToken functionToken = new Token(def.getTextAttribute(SyntaxType.FUNCTION_DECLARATION));
		IToken variableToken = new Token(def.getTextAttribute(SyntaxType.VARIABLE_DECLARATION));
		IToken typToken      = new Token(def.getTextAttribute(SyntaxType.TYP_DECLARATION));
		IToken localVariableToken = new Token(def.getTextAttribute(SyntaxType.LOCAL_VARIABLE_DECLARATION));
		IToken keywordSymbolToken = new Token(def.getTextAttribute(SyntaxType.KEYWORD_SYMBOL));
		
		List<IRule> rules = new ArrayList<IRule>();
		rules.add(makeWordRule(LispKeywords.FUNCTION_DEFINITION, functionToken));
		rules.add(makeWordRule(LispKeywords.VARIABLE_DEFINITION, variableToken));
		rules.add(makeWordRule(LispKeywords.TYP_DEFINITION, typToken));
		rules.add(makeWordRule(LispKeywords.LOCAL_VARIABLE_DEFINITION, localVariableToken));
		
		//KeywordSymbol
		IPrefixedWordEndDetector prefixEndDetector = new LispPrefixWordEndDetector();
		rules.add(new KeywordSymbolRule(keywordSymbolToken, prefixEndDetector));
		
		setRules(rules.toArray(new IRule[rules.size()]));
	}
	
	
	private IRule makeWordRule(String[] keywords, IToken token) 
	{
		CaseInsensitiveWordRule rule = new KeywordRule(new LispKeywordDetector(keywords));
		
		for(String word : keywords) {
			rule.addWord(word, token);
		}
		
		return rule;
	}
	
}
