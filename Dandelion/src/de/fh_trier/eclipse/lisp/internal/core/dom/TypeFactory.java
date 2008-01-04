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

package de.fh_trier.eclipse.lisp.internal.core.dom;

import java.util.List;
import org.eclipse.jface.text.Position;
import de.fh_trier.eclipse.lisp.internal.core.dom.parse.*;

/**
 * Erstellte die korrekten Lisp-Typen.
 * @author Michael Bohn
 *
 */
public class TypeFactory 
{
	private TypeFactory()
	{
		//kein Instanz erlaubt
	}
	
	/**
	 * Erstellt aus den Kindelementen einer S-Expression den korrekten Typ. 
	 * Wird fuer checkSyntax <code>false</code> uebergeben kann die Rueckgabe nur vom Type S-Expression oder Form
	 * sein! 
	 * @param childs - Die Kindknoten aus dem der Ausdruck erstellt wird
	 * @param position - Positionsangabe fuer Ausdruck.
	 * @param checkSyntax - <code>true</code> wenn Syntax geprueft werden soll
	 * @return
	 */
	public static SExpression createSExpression(final List<SExpression> childs, final Position position, final boolean checkSyntax)
	{
		//NIL oder nicht Form -> Sexp
		if(childs == null || childs.isEmpty() || childs.get(0).getTyp() != TSExpression.SYMBOL) {
			SExpression sexp = new SExpression(childs, position);
			return sexp;
		}
		
		//invariante: zu erstellender Type ist auf jeden Fall Form
		
		if(!checkSyntax) { //kein syntax-check, nicht die parser anwenden
			return new Form(childs, position);
		}
		
		//invariante: Syntax soll geprueft werden
		
		Symbol sym = ((Symbol)childs.get(0));
		String s = sym.getSymbolName();
		
		SExpression sexp;
		if(s.equalsIgnoreCase("defun")) {
			sexp = ParserFactory.getDefunParser().parse(new ParserInput(position, childs));
		} else if (s.equalsIgnoreCase("in-package")) {
			sexp = ParserFactory.getInpackageParser().parse(new ParserInput(position, childs));
		} else if(s.equalsIgnoreCase("defpackage")) {
			//TODO Defpackage Form parsen, hier nur dummy element angegeben
			sexp = new DefpackageForm(childs, position);
		} else if(s.equalsIgnoreCase("defmacro")) {
			sexp = ParserFactory.getDefmacroParser().parse(new ParserInput(position, childs));
		} else if(s.equalsIgnoreCase("lambda")) {
			sexp = ParserFactory.getLambdaParser().parse(new ParserInput(position, childs));
		} else { 
			//kann nur Form sein
			sexp = new Form(childs, position);
		}
	
		return sexp;
	}
	
	/**
	 * Erstellt ein Lisp-Symbol aus dem String.
	 * Ist string == <code>null</code> wird das NIL-Symbol zurueckgeliefert.
	 * Wird bei der Erstellung des Symbols ein Fehler entdeckt wird eine entsprechende
	 * Fehlermeldung an die Malformation-Liste des Symbols gehangen. Beispielsweise
	 * fuer ein Symbol das mit " anfaengt aber nicht mit " endet. 
	 * @param string
	 * @param position - darf <code>null</code> sein, das Symbol bekommt dann die Position(0, 0) als Positionsangaben
	 *                   uebergeben.
	 * @return
	 */
	public static Symbol createSymbol(final String fullName, Position position)
	{
		if(position == null) {
			position = new Position(0, 0);
		}
		
		if(fullName == null) {
			return new Symbol(Symbol.SYM_NIL, position);
		} else if(fullName.length() == 0) {
			return new Symbol("", position);
		}
		
		//invariante: string != null + mindestens ein Zeichen
				
		char first = fullName.charAt(0);
		Symbol symbol = null;
		switch (first) {
		case '\"' : symbol = stringSymbol(fullName, position);
					break;
		case ':'  : symbol = new KeywordSymbol(fullName, position);
				    break;
		case '#'  : symbol = readerSymbol(fullName, position);
					break;
		default   : 
			       DestructuredSymbol destructuredSymbol = destructureSymbol(fullName);
				   symbol = new Symbol(destructuredSymbol.getSymbolName(), position);
				   symbol.setInterned(true);
				   symbol.setPrivate(destructuredSymbol.isPrivate());
				   symbol.setQualifier(destructuredSymbol.getQualifier());
					
			       break;
		}
		
		return symbol;
	}
	
	private static Symbol readerSymbol(final String string, final Position position) 
	{
		if(string.length() <= 1) { //nur #, Vector erwartet
			Symbol symbol = new ReaderSymbol(ReaderSymbol.VECTOR, position, '(');
			return symbol;
		} 
		
		//invariante: string laenge >= 2
		
		char dispatchChar = string.charAt(1);
		Symbol symbol = null;
		if(dispatchChar == ':') {
			symbol = new Symbol(string.substring(2), position, null, false, false);
			symbol.setInterned(false);
		} else if(Character.isDigit(dispatchChar)) { //z.B. #1(4 2 3)
			if(string.length() >= 3) {
				symbol  = new ReaderSymbol(string.substring(3), position, string.charAt(2));
			} else {//evtl. vector
				symbol = new ReaderSymbol(ReaderSymbol.VECTOR, position, '(');
			}
		} else {
			symbol = new ReaderSymbol(string.substring(2), position, string.charAt(1));    
		}
		
		return symbol;
	}

	private static Symbol stringSymbol(final String string, final Position position) 
	{
		Symbol sym = new StringSymbol(string, position);
		if( string.length() == 1 || !string.endsWith("\"") ) {
			sym.addMalformation(new Malformation(TSeverity.ERROR, position, "String literal ist not properly closed by double quote"));
		}
		return sym;
	}
	
	/**
	 * 
	 * @param string
	 * @return
	 * 
	 * !testcase
	 */
	public static DestructuredSymbol destructureSymbol(final String string) {
		int pos = string.indexOf(':');
		if(pos < 0) return new DestructuredSymbol(null, string, false); //kein package
		
		DestructuredSymbol symbol;
		if(pos+1 < string.length() && string.charAt(pos+1) == ':') { //private symbol, zweimal ::
			symbol = new DestructuredSymbol(string.substring(0, pos), string.substring(pos+2), true);
		} else {
			symbol = new DestructuredSymbol(string.substring(0, pos), string.substring(pos+1), false);
		}
		
		return symbol;
	}
}
