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

import org.eclipse.jface.text.*;

import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.ui.text.*;
import edu.umd.cs.findbugs.annotations.NonNull;

/*ACHTUNG !!!!!!!!!!!!!!!!!
 * 
 * Wird das Model erweitert um Listener-Anmeldung und Delta Mitteilung muss
 * das Model geeignet synchronisiert werden! Bisher wird das Model im Reconciling
 * immer neu erstellt und an den Editor (synchronized) uebergeben. Zugriffe danach
 * finden immer nur im UI-Thread statt.
 * 
 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 */

/**
 * Implementierung der {@link ISexpModel}-Schnittstelle.
 * @author Michael Bohn
 * testcase
 */
public class SexpModel
extends DefaultMalformationProvider
implements ISexpModel
{	
	private static final List<SExpression> EMPTY_TL_FORMS = Collections.unmodifiableList(new ArrayList<SExpression>(0));
	
	private boolean fSemanticValidation = true;
	private RootNode fRoot;
	private VisitableDocument fVisitingDocument;
	private MalformationCollector fMalformationCollector;
	
	/**
	 * Erstellt eine neues Modell fuer dieses Dokument.
	 * Das Modell muss mit {@link SexpModel#createDOM()} aufgebaut werden.
	 * @param document - Dokument aus dem das Modell erstellt wird
	 */
	public SexpModel(final IDocument document)
	{
		this(new VisitableDocument(document));
	}
	
	/**
	 * Erstellt eine neues Modell fuer dieses {@link VisitableDocument}.
	 * Das Modell muss mit {@link SexpModel#createDOM()} aufgebaut werden.
	 * @param document - {@link VisitableDocument} aus dem das Modell erstellt wird
	 */
	public SexpModel(final VisitableDocument document)
	{
		if (document == null) {
			throw new NullPointerException("document must not be null");
		}
		
		this.fVisitingDocument = document;
		this.fMalformationCollector = new MalformationCollector(this);
	}
	
	/**
	 * @see ISexpModel#accept(ISexpDOMVisitor)
	 */
	public void accept(final ISexpDOMVisitor visitor) 
	{
		checkCreated();
		prepareVisit(visitor);
		fRoot.accept(visitor);
	}
	
	/**
	 * @see ISexpModel#acceptTopLevel(ISexpDOMVisitor, int)
	 */
	public void acceptTopLevel(final ISexpDOMVisitor visitor, int offset) {
		checkCreated();
		prepareVisit(visitor);
		SExpression sexp = getEnclosingTopLevelForm(offset);
		if(sexp != null) {
			sexp.accept(visitor);
		}
	}
	
	private void prepareVisit(final ISexpDOMVisitor visitor)
	{
		visitor.preVisit(this);
	}
	
	private void checkCreated()
	{
		if(!created()) {
			throw new IllegalStateException("createDOM must first be called");
		}
	}

	/**
	 * @see ISexpModel#createDOM()
	 */
	public void createDOM()
	{
		IDocument document = fVisitingDocument.getDocument();
		try {
			createDOM(0, document.getLength(), true);
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("creating DOM failed", e);
		}
	}
	
	/**
	 * @see ISexpModel#createDOM(int, int, boolean)
	 */
	public void createDOM(final int offset, final int end, final boolean skipComment)
	throws BadLocationException
	{
		fRoot = new RootNode();
		DOMCreator creator = new DOMCreator(fRoot);
		creator.addMalformationListener(fMalformationCollector);
		fVisitingDocument.accept(creator, offset, end, skipComment);
		creator.removeMalformationListener(fMalformationCollector);
		
		if( fSemanticValidation ) {
			accept(new SemanticValidation());
		}
	}
	
	private boolean created()
	{
		return fRoot != null;
	}
	
	/**
	 * Default ist <code>true</code>.
	 * @see ISexpModel#setSemanticValidation(boolean)
	 */
	public void setSemanticValidation(boolean detailed) 
	{
		fSemanticValidation = detailed;
	}

	/**
	 * @see ISexpModel#getRoot()
	 */
	public SExpressionNode getRoot() 
	{
		return fRoot;
	}

	/**
	 * @see ISexpModel#getTopLevelForms()
	 */
	@NonNull
	public List<SExpression> getTopLevelForms()
	{
		if( fRoot == null || fRoot.getChildren() == null) {
			return EMPTY_TL_FORMS;
		}
		return fRoot.getChildren();
	}
	
	/**
	 * @see ISexpModel#formSpansLines(SExpression)
	 */
	public int formSpansLines(final SExpression sexp) 
	{
		Position position = sexp.getPosition();
		if(position == null) return 0;
		
		try {
			return getDocument().getNumberOfLines(position.getOffset(), position.getLength());
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("calculating formSpansLine failed", e);
			return 0;
		}
	}

	/**
	 * @see ISexpModel#getLine(int)
	 */
	public int getLine(final int offset)
	throws BadLocationException
	{
		return getDocument().getLineOfOffset(offset);
	}

	/**
	 * @see ISexpModel#completePositionToFullLines(SExpression)
	 */
	public Position completePositionToFullLines(final SExpression sexp)
	{
		Position formRegion = sexp.getPosition();
		IRegion lineInformation;

		IDocument document = getDocument();
		
		int start = 0;
		int end = 0;
		try {
			lineInformation = document.getLineInformationOfOffset(formRegion.getOffset() + formRegion.getLength());
			start = formRegion.getOffset();
			end = lineInformation.getOffset() + lineInformation.getLength();
			int line = document.getLineOfOffset(end);
			String lineDelimiter = document.getLineDelimiter(line);
			if(lineDelimiter != null) {
				end += lineDelimiter.length();
			}
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("calculating completePositionToLine failed", e);
			return formRegion;
		}
		
		return new Position(start, end-start);
	}

	/**
	 * @see ISexpModel#getDocument()
	 */
	public IDocument getDocument() {
		return fVisitingDocument.getDocument();
	}

	/**
	 * @see ISexpModel#getInpackage(Position)
	 */
	public InpackageForm getInpackage(final Position position) {
		
		if( !created() ) {
			return InpackageForm.defaultPackage();
		}
		
		InpackageCollector collector = new InpackageCollector(true); //kein null Positions, zur Sicherheit
		this.accept(collector);
		List<InpackageForm> inpackages = collector.getInpackages();
		
		if(inpackages.isEmpty()) { //keins gefunden
			return InpackageForm.defaultPackage();
		}
		
		return calculateNearestInpackage(inpackages, position);
	}
	
	private InpackageForm calculateNearestInpackage(final List<InpackageForm> inpackages, final Position position)
	{
		Collections.sort(inpackages, SExpression.POSITION_COMPARATOR);
		//kleinster offset steht oben, groesster unten, keine null Positions
		InpackageForm found = null;
		for(int i=0, n=inpackages.size(); i<n; i++) {
			InpackageForm x = inpackages.get(i); 
			if( x.getPosition().getOffset()+x.getPosition().getLength() > position.getOffset() ) { //hinter dem form-offset angelangt
				break;
			}
			found = x; //letztes in-package das gefunden wurde
		}
		
		if(found == null) { //nichts gefunden -> default
			return InpackageForm.defaultPackage();
		}
		return found;
 	}

	/**
	 * Gibt <code>true</code> zurueck wenn Position einen Fehler vom Typ ERROR enthaelt (kein WARNING).
	 * @see ISexpModel#containsError(Position)
	 */
	public boolean containsError(final Position position) {
		List<ISyntacticalMalformation> malformations = getMalformations();
		int positionEndOffset = position.getOffset() + position.getLength();
		for(ISyntacticalMalformation malformation : malformations) {
			Position malformationPosition = malformation.getPosition();
			int malformationEndOffset = malformationPosition.getOffset() + malformationPosition.getLength();
			//Position liegt in einem von einem Fehler abgedeckten Bereich || oder innerhalb der Position kommt ein Fehler vor
			if(position.getOffset() >= malformationPosition.getOffset() && position.getOffset() <= malformationEndOffset ||
					malformationPosition.getOffset() >= position.getOffset() && malformationEndOffset <= positionEndOffset) {
				return malformation.getSeverity() == TSeverity.ERROR;
			}
		}
		return false;
	}

	/**
	 * @see ISexpModel#getEnclosingTopLevelForm(int)
	 */
	public SExpression getEnclosingTopLevelForm(int offset) 
	{	
		for(SExpression sexp : getTopLevelForms()) {
			Position position = sexp.getPosition();
			int end = position.getOffset() + position.getLength();
			if(offset >= position.getOffset() && offset <= end) {
				return sexp;
			}
		}
		
		return null;
	}
}

/**
 * Listener der alle Fehlermeldungsmitteilungen speichert.
 * @author Michael Bohn
 */
class MalformationCollector 
implements IMalformationListener
{
	private final IMalformationProvider fMalformationProvider;
	
	public MalformationCollector(final IMalformationProvider provider)
	{
		if (provider == null) {
			throw new NullPointerException("provider must not be null");
		}

		this.fMalformationProvider = provider;
	}
	
	public void malformationDetected(final ISyntacticalMalformation malformation) 
	{
		fMalformationProvider.addMalformation(malformation);
	}
}
