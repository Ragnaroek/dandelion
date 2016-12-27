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

package de.defmacro.dandelion.internal.core.dom;

import java.util.List;
import org.eclipse.jface.text.*;

/**
 * Schnittstelle fuer das Modell des Lisp-Quelltextes.
 * @author Michael Bohn
 */
public interface ISexpModel
extends IMalformationProvider
{
	/**
	 * Der Besucher startet ein Visit.
	 * @param vistor
	 */
	public void accept(ISexpDOMVisitor vistor);
	
	/**
	 * Visit der Toplevel-Form die an der Stelle offset liegt.
	 * Wird an offset keine Toplevel-Form gefunden, tut diese Methode nichts.
	 * @param visitor
	 * @param offset
	 */
	public void acceptTopLevel(ISexpDOMVisitor visitor, int offset);
	
	/**
	 * Erstellt das Modell.
	 */
	public void createDOM();
	
	/**
	 * Erstellt das Modell aus dem angegeben Bereich im Dokument.
	 * @param offset - start position
	 * @param end - end position (exklusive)
	 * @param skipComment - Kommentare bei Erstellung ueberspringen.
	 * @throws BadLocationException - bei illegaler Positionsangabe
	 */
	public void createDOM(final int offset, final int end, final boolean skipComment)
	throws BadLocationException;
	
	/**
	 * Semantische Ueberpruefung des Modells nach Erstellung an- oder ausschalten.
	 * @param detailed
	 */
	public void setSemanticValidation(boolean detailed);
	
	/**
	 * Der Wurzelknoten des Modells.
	 * @return Wurzelknoten
	 */
	public SExpressionNode getRoot();
	
	/**
	 * Liefert eine Liste alle Ausdruecke auf dem Top-Level des Dokuments.
	 * @return Liste aller Top-Level Ausdruecke
	 */
	public List<SExpression> getTopLevelForms();
	
	/**
	 * Das Dokument aus dem das Modell erstellt wurde.
	 * @return
	 */
	public IDocument getDocument();
	
	/**
	 * Berechnet die Zeilenanzahl die der uebergebene Ausdruck in Dokument einnimmt.
	 * @param sexp - Ausdruck dessen Zeillenzahl berechenet werden soll.
	 * @return Zeilenanzahl
	 */
	public int formSpansLines(SExpression sexp);
	
	/**
	 * Holt die Zeilennummer von der angegebenen Position.
	 * @param offset
	 * @return Zeilennummer
	 * @throws BadLocationException - bei ungueltiger Positionsangabe
	 */
	public int getLine(int offset)
	throws BadLocationException;
	
	/**
	 * Vervollstaendigt die Positionsangabe des uebergebenen Ausdrucks auf komplette Zeilenpositionen.
	 * @param sexp - der zu vervollstaendigenden Ausdruck
	 * @return Position des kompletten Zeilenbereichs
	 */
	public Position completePositionToFullLines(SExpression sexp);
	
	/**
	 * Gibt das mit in-package deklarierte package fuer die uebergebene
	 * Position zureck.
	 * @param position - position fuer das die in-package anweisung gesucht werden soll
	 * @return - der zur Position gehoerige in-package ausdruck
	 */
	public InpackageForm getInpackage(Position position);
	
	/**
	 * Liefert <code>true</code> wenn an der angegeben Postionsangabe ein Fehler vorhanden ist.
	 * @param position
	 * @return <code>true</code> wenn Fehler an Position, sonst <code>false</code>
	 */
	public boolean containsError(Position position);
	
	/**
	 * Gibt den umschliessenden Toplevel Ausdruck an der Position zurueck.
	 * @param offset - Positionsangabe
	 * @return Ausdruck, oder <code>null</code> wenn nicht gefunden
	 */
	public SExpression getEnclosingTopLevelForm(int offset);
}
