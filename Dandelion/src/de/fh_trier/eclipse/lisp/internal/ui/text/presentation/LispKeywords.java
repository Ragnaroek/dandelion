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

/**
 * Alle Schluessewoerter der Syntaxhervorhebung.
 * Die Arrays duerfen nicht veraendert werden.
 * @author Michael Bohn
 *
 */
/*package*/ class LispKeywords 
{
	//NOTIZ: Alle Keywoerter muessen in Lower-Case beschrieben werden,
	//		 da sonst das Case-Insensitive Matching nicht funktioniert.
	
	/**
	 * Funktionskeywoerter.
	 */
	/*package*/ static final String[] FUNCTION_DEFINITION = new String[]
	{
		"defun",
		"defmacro",
		"defmethod",
		"defgeneric",
		"define-compiler-macro",
		"define-setf-expander",
		"defsetf",
	};
	
	/**
	 * Variablenkeywoerter.
	 */
	/*package*/ static final String[] VARIABLE_DEFINITION = new String[]
	{
		"defparameter",
		"defvar",
		"defconstant",
	};
	
	/**
	 * Keywoerter, Funktionen die Typen deklarieren.
	 */
	/*package*/ static final String[] TYP_DEFINITION = new String[]
	{
		"defstruct",
		"deftype",
		"defclass",
		"defpackage",
		"define-condition",
		"in-package"
	};
	
	/**
	 * Lokale Variablen/Funktionsdefinition.
	 */
	/*package*/ static final String[] LOCAL_VARIABLE_DEFINITION = new String[]
	{
		"let",
		"let*",
		"flet",
		"labels",
		"macrolet"
	};
}
