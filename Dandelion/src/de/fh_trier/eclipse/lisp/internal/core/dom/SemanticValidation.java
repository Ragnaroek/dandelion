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

import java.util.*;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Visitor fuer Semantische Ueberpruefung des Modells.
 * @author Michael Bohn
 */
public class SemanticValidation 
implements ISexpDOMVisitor
{
	private ISexpModel fCurrentModel;
	private SortedSet<String> fDefinedNames;
	
	/**
	 * Erstellt ein neues Visitor-Objekt.
	 */
	public SemanticValidation()
	{
		fCurrentModel = null; //wegen findbugs warning
		fDefinedNames = new TreeSet<String>(); 
	}
	
	/**
	 * @see ISexpDOMVisitor#preVisit(ISexpModel)
	 */
	public void preVisit(final ISexpModel model) 
	{
		this.fCurrentModel = model;
	}

	/**
	 * Ueberpruefung des Defmacro-Ausdrucks.
	 * Testet ob &ampoptional und &ampkey zusammen vorkommen.
	 * Testet ob Name bereits vergeben.
	 */
	public boolean visit(final DefmacroForm form) 
	{
		if( form.hasMalformation() ) {
			return true;
		}
		checkOptionalAndKey(form, form.getLambdaList());
		checkNameDefined(form);
		
		return true;
	}

	/**
	 * Ueberpruefung des Defun-Ausdrucks.
	 * Testet ob &ampoptional und &ampkey zusammen vorkommen.
	 * Testet ob Name bereits vergeben.
	 * Testet ob Defun auf Toplevel.
	 */
	public boolean visit(final DefunForm form) 
	{
		//Bei Fehler kein semantischer Check
		if( form.hasMalformation() ) {
			return true;
		}
		
		//invariante: Form hat keine Fehler

		if( !form.isToplevel() ) {
			warn(form, "Defun not on toplevel");
		}
		
		OrdinaryLambdaList lambdaList = form.getLambdaList();
		checkOptionalAndKey(form, lambdaList);
		checkNameDefined(form);
		
		return true;
	}

	private void checkOptionalAndKey(final Form form, final OrdinaryLambdaList lambdaList) 
	{
		if( lambdaList.hasOptionalParameters() && lambdaList.hasKeywordParameters() ) {
			warn(form, "&optional and &key in lambda list");
		}
	}
	
	@SuppressWarnings("Dm")
	private void checkNameDefined(final DefiningForm form)
	{
		if(form == null || form.getDefinedName() == null) {
			return;
		}
		
		String definedName = form.getDefinedName().getSymbolName().toUpperCase();
		if(fDefinedNames.contains(definedName)) {
			warn(form, "Duplicate function/macro: " + definedName);
		} else {
			fDefinedNames.add(definedName);
		}
	}
	
	/**
	 * @see ISexpDOMVisitor#visit(Form)
	 */
	public boolean visit(Form form) {
		return true;
	}

	/**
	 * @see ISexpDOMVisitor#visit(DefpackageForm)
	 */
	public boolean visit(final DefpackageForm form) 
	{
		return true;
	}
	
	/**
	 * Uberpruefung eines in-package Ausdrucks.
	 * Testet ob in-package auf toplevel.
	 * Testet ob paketangabe mit String in upper-case.
	 */
	@SuppressWarnings("Dm")
	public boolean visit(final InpackageForm form) 
	{
		if( form.hasMalformation() ) {
			return true;
		}
		
		//invariante: Form hat kein Fehler
		
		if( !form.isToplevel() ) {
			warn(form, "in-package not on toplevel");
		}
		
		Symbol packageSymbol = form.getPackage();
		if( packageSymbol.getTyp() == TSExpression.STRING_SYMBOL && 
				!packageSymbol.getSymbolName().toUpperCase().equals(packageSymbol.getSymbolName())) {
			warn(packageSymbol, "Lower case characters in string package name");
		}
		
		return false; //inpackage hat keine childs
	}

	/**
	 * Ueberpruefung eines Lambda-Ausdrucks.
	 * Testet ob &ampoptional und &ampkey zusammen vorkommen.
	 */
	public boolean visit(final LambdaForm form) 
	{
		if(form.hasMalformation()) { //kein check bei fehler in form
			return true;
		}
		
		checkOptionalAndKey(form, form.getLambdaList());
		
		return true;
	}

	/**
	 * @see ISexpDOMVisitor#visit(SExpression)
	 */
	public boolean visit(SExpression sexp) {
		return true;
	}

	/**
	 * @see ISexpDOMVisitor#visit(Symbol)
	 */
	public boolean visit(Symbol form) {
		return true;
	}
	
	private void warn(final SExpression sexp, final String text) 
	{
		Malformation malformation = new Malformation(TSeverity.WARNING, sexp.getPosition(), text);
		sexp.addMalformation(malformation);
		fCurrentModel.addMalformation(malformation);
	}
}
