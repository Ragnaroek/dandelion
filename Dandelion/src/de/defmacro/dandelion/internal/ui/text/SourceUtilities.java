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

import java.util.*;
import org.eclipse.jface.text.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.ui.text.partition.LispPartitionConstants;

/**
 * Diverse Hilfsmethode für Lisp-Sourcecode Bearbeitung.
 * @author Michael Bohn
 * 
 * testcase
 */
public class SourceUtilities 
{
	private static final TopLevelFormPresentVisitor PRESENT_VISITOR = 
		new TopLevelFormPresentVisitor();
	
	/**
	 * System-Newline String.
	 * @see System#getProperty(String)
	 */
	public static final String SYS_NEWLINE = System.getProperty("line.separator");
	
	/**
	 * Normalisierter Newline-String. Dieser String wird 
	 * vom Eval-Server als Zeilenende erwartet.
	 */
	public static final String NORMALIZED_NEWLINE = new String(new char[] {'\n'});
	
	/**
	 * Die Standardeinrueckungstiefe fuer die automatische und manuelle 
	 * Quelltextformatierung.
	 */
	public static final int DEFAULT_INDENT_WIDTH = 2;
	
	/**
	 * Ueberprueft ob der angegebene Textbereich im Dokument ein ungueltige
	 * Klammerstruktur enthaelt. Eine Rueckgabe von <code>false</code> bedeutet
	 * das die Klammerstruktur evtl. noch korrekt angegeben werden kann. Wird
	 * <code>true</code> zurueckgeliefert ist die Klammerung auf jeden Fall inkorrekt.
	 * Die Negation des Ergebnisses gibt _nicht_ an, ob die Klammerstruktur in Ordnung ist.
	 * Hierfuer muss die Funktion {@link SourceUtilities#hasValidParenthesisNesting(IDocument, int, int, String)}
	 * verwendet werden.
	 * <p>Beispiel:<br />
	 * (let ((my-var nil) - Klammerstruktur kann noch vervollstaendigt werden, deshalb wird <code>false</code> zurueckgegeben<br />
	 * ()) - Klammerstruktur in jedem Fall falsch, da die schliessende Klammer am Ende zuviel. Das kann
	 * auch nicht durch einfuegen einer weiteren Klammer am Ende korrigiert werden.
	 * </p>
	 * 
	 * @see SourceUtilities#hasValidParenthesisNesting(IDocument, int, int, String)
	 * 
	 * @param doc - IDocument in dem Klamerstruktur ueberprueft werden soll.
	 * @param offset - Offset Position an dem die Pruefung startet
	 * @param end - Offset bis wohin das Dokument maximal untersucht wird (exklusiv)
	 * @param contentType - ContentType in welchem die Struktur ueberprueft wird
	 * 
	 * @return <code>true</code> wenn Klammerstruktur in jedem Fall ungueltig, sonst <code>false</code>
	 * 
	 * @throws BadLocationException - wenn ungueltige Offsets angegeben werden
	 * @throws BadPartitioningException - wenn ungueltiger contentType angegeben wird
	 * 
	 * testcase
	 */
	public static boolean hasInvalidParenthesisNesting(final IDocument doc, final int offset, final int end, final String contentType)
	throws BadLocationException, BadPartitioningException
	{		
		int count = 0;
		for(int i=offset; i<end; i++) {
			if(doc.getChar(i) == ')' && inContentType(i, contentType, doc)) {
				count--;
			}
			
			if(doc.getChar(i) == '(' && inContentType(i, contentType, doc)) {
				count++;
			}
			
			if(count < 0) {
				return true; //ungueltige Klammerstruktur
			}
		}
		
		return false;
	}
	
	/**
	 * Ueberprueft ob der angegebene Textbereich im Dokument ein gueltige
	 * Klammerstruktur enthaelt. Die Funktion liefert <code>true</code> wenn
	 * die Klammerstruktur in Ordnung ist. 
	 * Notiz: <br />
	 * Die Negation der Rueckgabe liefert ein anderes Ergebnis als 
	 * {@link SourceUtilities#hasInvalidParenthesisNesting(IDocument, int, int, String)}
	 * <p>Beispiel:<br />
	 * (let ((my-var nil) - liefert <code>false</code>, da zwei schliessenden Klammern fehlen<br />
	 * ()) - liefert <code>false</code>, da oeffnenden Klammer fehlt<br />
	 * ()  - liefert <code>true</code>, Klammerstruktur in Ordnung.
	 * </p>
	 * 
	 * @see SourceUtilities#hasInvalidParenthesisNesting(IDocument, int, int, String)
	 * 
	 * @param doc - IDocument in dem Klamerstruktur ueberprueft werden soll.
	 * @param offset - Offset Position an dem die Pruefung startet
	 * @param end - Offset bis wohin das Dokument maximal untersucht wird (exklusiv)
	 * @param contentType - ContentType in welchem die Struktur ueberprueft wird
	 * 
	 * @return <code>true</code> wenn Klammerstruktur gueltig, sonst <code>false</code>
	 * 
	 * @throws BadLocationException - wenn ungueltige Offsets angegeben werden
	 * @throws BadPartitioningException - wenn ungueltiger contentType angegeben wird
	 * 
	 * testcase
	 */
	public static boolean hasValidParenthesisNesting(final IDocument doc, final int offset, final int end, final String contentType)
	throws BadLocationException, BadPartitioningException
	{	
		int count = 0;
		for(int i=offset; i<end; i++) {
			if(doc.getChar(i) == ')' && inContentType(i, contentType, doc)) {
				count--;
			}
			
			if(doc.getChar(i) == '(' && inContentType(i, contentType, doc)) {
				count++;
			}
		}
		
		return count == 0;
	}
	
	/**
	 * Wie {@link SourceUtilities#findMatchingParenthesis(IDocument, int, boolean, String)}, offsetContentType ist
	 * der contentType aus Document an offset.
	 * @param doc
	 * @param offset
	 * @param right
	 * @return
	 * @throws BadLocationException
	 * @throws BadPartitioningException
	 */
	public static IRegion findMatchingParenthesis(final IDocument doc, final int offset, final boolean right) 
	throws BadLocationException, BadPartitioningException
	{
		return findMatchingParenthesis(doc, offset, right, ((IDocumentExtension3)doc).getContentType(LispPartitionConstants.PARTITION_ID, offset, false));
	}
	
	/**
	 * Sucht die korrespondiere Klammer in einem uebergebene Dokument. Aufruf der Methode fuehrt zu evtl. unerwartetem
	 * Verhalten wenn sich an der offset-Position keine schliessende oder oeffnende Klammer befindet.
	 * 
	 * @see SourceUtilities#resolveSearchDirection(char, char)
	 * 
	 * @param doc - Dokument in dem die korrespondierende Klammer gesucht werden soll
	 * @param offset - Position des Characters fuer den die korrespondierende Klammer gesucht werden soll.
	 * @param right - <code>true</code> wenn nach schliessender ')' nach rechts von der Offset-Position gesucht werden soll, ansonsten
	 *                wird nach der korrespondierenden oeffnender Klammer '(' link von der Offsest-Position gesucht.
	 * 
	 * @return IRegion - minimale Region die die beiden Character enthaelt, oder <code>null</code> wenn korrespondiere Klammer nicht gefunden.
	 * 
	 * @throws IllegalArgumentException - wenn {@link IDocument} nicht die benoetigte {@link IDocumentExtension3} besitzt
	 * @throws BadLocationException - wenn ungueltiger Bereich angegeben wurde
	 * @throws BadPartitioningException
	 * 
	 * testcase
	 */
	public static IRegion findMatchingParenthesis(final IDocument doc, final int offset, final boolean right, final String offsetContentType) 
	throws BadLocationException, BadPartitioningException
	{
		//Notiz: Ueber Visitor nicht moeglich da auch rueckwaerts gesucht werden kann.	
		
		int end = doc.getLength();
		int level = right ? 1 : -1;
		final int direction = level; //vor oder rueckwaerts iterieren
		int itOffset  = offset + direction;

		//maximal bis zum ende, bzw. anfang des Dokuments laufen
		while(itOffset >= 0 && itOffset < end) {
			char ch = doc.getChar(itOffset);
			//nur Klammer in Default-Contenttype wird als Treffer gewertet,
			//da sonst auch Klammer in String, Kommentar oder char markiert wird

			if(ch == ')' && inContentType(itOffset, offsetContentType, doc)) {
				level--;
			} else if(ch == '(' && inContentType(itOffset, offsetContentType, doc)) {
				level++;
			}

			if(level == 0) { //gefunden!
				//berechne Region
				if(right) {
					return new Region(offset, (itOffset - offset)+1);
				} 
				return new Region(itOffset, (offset - itOffset)+1);
			}

			//weiter mit naechstem char (+ oder -)
			itOffset += direction;
		}

		return null;
	}
		
	/**
	 * Hilfsmethode fuer <code>findMatchingParenthesis</code>.
	 * Ueberprueft ob der ContentType mit dem ContentType in dem die Ueberpruefung
	 * gestart wurde uebereinstimmt.
	 */
	private static boolean inContentType(int offset, String startedContentType, IDocument document) 
	throws BadLocationException, BadPartitioningException 
	{
		return startedContentType.equals(((IDocumentExtension3)document).getContentType(LispPartitionConstants.PARTITION_ID, offset, false));
	}
		
	/**
	 * Entscheidet fuer die uebergeben chars die Suchrichtung.
	 * Die Funktion liefert <code>null</code> wenn die Suchrichtung
	 * unentscheidbar ist.
	 * 
	 * @see SourceUtilities#translateOffset(char, char, int)
	 * 
	 * @param beforeCaret - Character vor Caret-Position
	 * @param atCaret	  - Character an Caret-Position
	 * @return Boolean    - <code>true</code> fuer Rechtssuche, <code>false</code> fuer Linkssuche.
	 * 						<code>null</code> wenn unentscheidbar.
	 */
	public static Boolean resolveSearchDirection(char beforeCaret, char atCaret)
	{
		if(beforeCaret == '(' && atCaret == ')') {
			return null; //undefiniert
		}
		
		if(atCaret == '(') {
			return true;
		} else if(beforeCaret == ')') {
			return false;
		} else if(atCaret == ')') {
			return false;
		} else if(beforeCaret == '(') {
			return true;
		}
		
		return null;
	}
	
	/**
	 * Liefert den korrekten Offset zur zugehoerigen searchDirection.
	 * 
	 * @see SourceUtilities#resolveSearchDirection(char, char)
	 * 
	 * @param beforeCaret
	 * @param atCaret
	 * @param offset
	 * 
	 * @return int - Offset zur Suchrichtung
	 */
	public static int translateOffset(char beforeCaret, char atCaret, int offset)
	{
		if(atCaret == '(') {
			return offset;
		} else if(beforeCaret == ')') {
			return offset-1;
		} else if(atCaret == ')') {
			return offset;
		} else if(beforeCaret == '(') {
			return offset-1;
		}
		
		return offset;
	}
	
	/**
	 * Entscheidet ob der String ein gueltige Eval-Form darstellt.
	 * Gueltig sobald Toplevel-Klammerung vollstaendig oder String
	 * ein Symbol darstellt.
	 * <p>
	 * Hinweis: | stellt die Caret-Position dar <br /><br />
	 * <code>
	 * 	(defun my-function| ;keine gueligte Eval-Form <br />
	 * 	(defun my-function () (setf x 'a)| ;kein gueltige Eval-Form <br />
	 * 	(defun my-function () (setf x 'a))| ;gueltige Eval-Form <br />
	 *   <br />
	 *   <br />
	 *   ; noch nicht gueltig, da das Leerzeichen noch nicht angegeben wurde <br/>
	 *     welches das Symbol endgueltig vervollstaendigt<br />
	 *   my-symbol|<br />
	 *   my-symbol | ; gueltige Eval-Form
	 *   
	 * </code>
	 * </p>
	 * 
	 * @param doc - Dokument aus der die Form extrahiert werden soll
	 * @param offset - Offset der zu untersuchenden Form
	 * @param end - End-Offset in Dokument
	 * 
	 * @return <code>true</code> wenn Form eine gueltige Eval-Form darstellt, sonst <code>false</code>
	 * 
	 * @throws NullPointerException - wenn IDocument <code>null</code>
	 * @throws BadLocationException - wenn Offset-Angaben ungueltig
	 * 
	 * testcase
	 */
	public static boolean validForm(final IDocument doc, final int offset, final int end)
	throws BadLocationException
	{
		VisitableDocument visitDocument = new VisitableDocument(doc);
		visitDocument.accept(PRESENT_VISITOR, offset, end, true);
		
		return PRESENT_VISITOR.isFormSeen() && !PRESENT_VISITOR.isMalformed();
	}
	
	/**
	 * Extrahierung der Top-Level-Forms im angegebenen Bereich mit Beachtung des Content-Type.
	 * Befindet sich offset innerhalb eines Kommentars, wird der reine Text ohne Kommentarbeachtung
	 * untersucht. (Fuer Selektionen von Forms aus Kommentaren).
	 * Befindent sich offset in Quellcode, werden alle Top-Level-Forms ohne Kommentar extrahiert.
	 * @param document
	 * @param offset
	 * @param end
	 * @return
	 * @throws StructureException - wenn Bereich ungueltig Klammerstruktur enthaelt.
	 * @throws BadLocationException - bei ungueltigen Bereichsangaben fuer diese Dokument.
	 */
	public static List<Position> extractFormsCheckContentType(final IDocument document, final int offset, final int end) 
	throws StructureException, BadLocationException
	{		
		try {
			final String contentTypeAtOffset = ((IDocumentExtension3)document).getContentType(LispPartitionConstants.PARTITION_ID, offset, false);
			final char c = document.getChar(offset);
			if( contentTypeAtOffset.equals(LispPartitionConstants.LISP_PARTITION_COMMENT) ) {
				//erster selektierter char in Kommentar ist ; -> ueberspringen Kommentar
				return extractForms(document, offset, end, isCommentStart(c, offset, end, document)); 
			}
			
			return extractForms(document, offset, end, true);
		} catch (BadPartitioningException e) {
			LispPluginActivator.logBrokenInvariant("extracting content type failed", e);
			return Collections.emptyList();
		}
	}
	
	/**
	 * Nur in Kommentar-Content-Type aufrufen.
	 * @param c
	 * @param offset
	 * @param document
	 * @return
	 */
	private static boolean isCommentStart(final char c, final int offset, final int end, final IDocument document)
	throws BadLocationException
	{
		if(c == ';') {
			return true;
		}
		if(c == '#') {
			if(offset+1 >= end) {
				return false;
			} 
			return document.getChar(offset+1) == '|';
		}
		return false;
	}
	
	/**
	 * Extrahiert alle Top-Level-Form aus dem angegebenen Bereich. Kommentare werde
	 * immer uebersprungen.
	 * @param document
	 * @param offset
	 * @param end
	 * @return
	 * @throws StructureException
	 * @throws BadLocationException
	 */
	public static List<Position> extractForms(final IDocument document, final int offset, final int end)
	throws StructureException, BadLocationException
	{
		return extractForms(document, offset, end, true);
	}
	
	/**
	 * Extrahiert aus einem Teilbereich eines Dokuments alle gueltigen
	 * Top-Level-Forms.
	 * 
	 * @param document - IDocument aus dem die Top-Level-Forms extrahiert werden sollen
	 * @param offset - Startoffset
	 * @param end - Endoffset (exklusiv)
	 * 
	 * @return List - Liste aller Top-Level-Forms, geordnet nach vorkommen in Dokument
	 * 
	 * @throws StructureException - wenn ungueltige Klammerstruktur in Dokumentenbereich
	 * 
	 * testcase siehe Test TopLevelFormVisitor
	 */
	public static List<Position> extractForms(final IDocument document, final int offset, final int end, final boolean skipComments)
	throws StructureException, BadLocationException
	{
		ISexpModel model = new SexpModel(document);
		model.createDOM(offset, end, skipComments);
		
		for(ISyntacticalMalformation m : model.getMalformations()) {
			if(m.getSeverity() == TSeverity.STRUCTURE) {
				throw new StructureException(m.getPosition());
			}
		}
		
		//invariante: keine Fehler
		
		List<Position> positions = new ArrayList<Position>();
		for(SExpression sexp : model.getTopLevelForms()) {
			positions.add(sexp.getPosition());
		}
		
		return positions;
	}
	
	/**
	 * Extrahiert alle Top-Level-Forms im uegergebenen Dokument. Die Methode buendelt
	 * das erstellen eines <code>VisitableDocument</code> und Extrahierung der
	 * Top-Level-Forms mittels eines <code>TopLevelFormVisitor</code>.
	 * 
	 * @param document - IDocument aus dem die TopLevel-forms extrahiert werden sollen.
	 * @return List - Liste aller TopLevel-Forms, geordnet nach vorkommen in Dokument.
	 * @throws StructureException - wenn Dokument ungueltige Klammerstruktur enthaelt
	 * 
	 * testcase siehe Test TopLevelFormVisitor
	 */
	public static List<Position> extractForms(final IDocument document)
	throws StructureException, BadLocationException
	{		
		return extractForms(document, 0, document.getLength());
	}
		
	/**
	 * Ersetzt einen System Newline durch den Normalisierten
	 * Newline (\n). Wird kein Newline am Ende vorgefunden, wird
	 * der Normalisierte Newline-String angehangen.
	 * 
	 * @param buffer - StringBuilder in dem das Newline am Ende normalisiert werden soll.
	 * 
	 * @return StringBuilder - Der uebergebene, jetzt veraenderte, StringBuilder.
	 * 
	 * testcase
	 */
	public static StringBuilder normalizeNewline(StringBuilder buffer)
	{
		String string = buffer.toString();
		if(string.endsWith(SYS_NEWLINE)) {
			int start = buffer.length() - SYS_NEWLINE.length();
			buffer.delete(start, buffer.length());
			buffer.append(NORMALIZED_NEWLINE);
		} else if(!string.endsWith(NORMALIZED_NEWLINE)) {
			//kein SYS_LINE_SEPARATOR und kein NORMALIZED_SEPARATOR
			buffer.append(NORMALIZED_NEWLINE);
		}
		
		return buffer;
	}
	
	
	/**
	 * Berechnet den Prefix fuer die angegebene Positions.
	 * Ein Prefix endent bei oeffnender Klammer oder Whitespace.
	 * @param document
	 * @param offset
	 * @return Position des Prefixbeginn.
	 */
	/*
	public static int prefixOffset(final IDocument document, final int offset)
	{
		int curOffset = offset-1;
		if(curOffset < 0) {
			return -1;
		}
		
		try {
			while(true) {
				char c = document.getChar(curOffset--);
				if(Character.isWhitespace(c) || c == '(') {
					return curOffset;
				}
			}
		} catch (BadLocationException e) {
			return -1;
		}
	}*/
	
	/**
	 * Berechnet die relative weite der Einrueckung fuer den gegebenen Offset.
	 * Die weite der Einrueckung wird anhand der Einrueckung der vorherigen Zeile bestimmt.
	 * Fuer nicht relative Berechnung der Einrueckung siehe {@link SourceUtilities#calculateIndentationDepth(IDocument, int)}.
	 * @param document
	 * @param offset
	 * @return Weite der Einrueckung
	 */
	public static int calculateIndentationWidth(final IDocument document, final int offset) 
	{
		try {
			IRegion matchingParenthesis = SourceUtilities.findMatchingParenthesis(document, offset, false, LispPartitionConstants.LISP_PARTITION_DEFAULT);
			if(matchingParenthesis == null) { //keine Klammer offen, normalen Umbruch durchfuehren
				return 0;
			}
			IRegion line = document.getLineInformationOfOffset(matchingParenthesis.getOffset());
			int indentChars = (matchingParenthesis.getOffset() - line.getOffset()) + DEFAULT_INDENT_WIDTH;
			return indentChars;
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("Calculate indentation failed", e);
			return 0;
		} catch (BadPartitioningException e) {
			LispPluginActivator.logBrokenInvariant("Calculate indentation failed", e);
			return 0;
		}
	}

	/**
	 * Berechnet die Einrueckungstiefe fuer die angegeben Position.
	 * @param doc
	 * @param offset
	 * @return Tiefe der Einrueckung
	 */
	public static int calculateIndentationDepth(final IDocument doc, final int offset)
	{	
		try {
			IRegion matchingParen = SourceUtilities.findMatchingParenthesis(doc, offset, false, LispPartitionConstants.LISP_PARTITION_DEFAULT);
			if(matchingParen == null) {
				return 0;
			}
			
			//rekursive weiter bis zum toplevel
			return calculateIndentationDepth(doc, matchingParen.getOffset())+1;
		} catch (BadLocationException e) {
			e.printStackTrace();
		} catch (BadPartitioningException e) {
			e.printStackTrace();
		}
		return 0;
	}
	
	/**
	 * Fuegt vor den Text enstprechende Leerzeichen an.
	 * Die Anzahl der Leerzeichen wird von {@link SourceUtilities#DEFAULT_INDENT_WIDTH} bestimmt.
	 * @param indentWidth
	 * @param text
	 * @return - Der neue Text mit Einrueckung
	 */
	public static String appendIndentation(final int indentWidth, final String text)
	{
		StringBuilder newText = new StringBuilder();
		for(int i=0;i<indentWidth;i++) {
			newText.append(' ');
		}
		newText.append(text);
		return newText.toString();
	}
}
