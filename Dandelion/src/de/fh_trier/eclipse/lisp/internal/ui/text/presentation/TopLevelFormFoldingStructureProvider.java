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

import java.util.*;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.projection.*;
import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import de.fh_trier.eclipse.lisp.internal.ui.editor.*;

/**
 * Stellt das Folding der Toplevel-Forms im Editor
 * bereit.
 * @author Michael Bohn
 *
 */
public class TopLevelFormFoldingStructureProvider 
{
	private static final Annotation[] EMPTY_ANNOTATION = new Annotation[0];
	
	private ILispEditor fEditor;
	
	public TopLevelFormFoldingStructureProvider(ILispEditor editor)
	{
		this.fEditor = editor;
	}
	
	public void updateFolding(ISexpModel sexpModel, IProgressMonitor pgmon)
	{
		ProjectionAnnotationModel annotationModel = (ProjectionAnnotationModel)fEditor.getAdapter(ProjectionAnnotationModel.class);
		if(annotationModel == null) return;
		
		Map<Annotation, Position> added = new HashMap<Annotation, Position>();
		Annotation[] removed = computeDifferences(sexpModel, annotationModel, added);
		
		if(pgmon.isCanceled()) return;
		
		annotationModel.modifyAnnotations(removed, added, EMPTY_ANNOTATION);
	}
	
	private Annotation[] computeDifferences(ISexpModel sexpModel, ProjectionAnnotationModel annotationModel, Map<Annotation, Position> additions)
	{
		//Positionen duerfen nicht als key in hashmap benutzt werden
		Map<Annotation, Position> annotationModelPositions = new HashMap<Annotation, Position>();
		List<Position> topLevelModelPositions = new LinkedList<Position>();
		
		//Positionen aus AnnotationModel kopieren
		copyAnnotationPositions(annotationModel, annotationModelPositions);
		copyTopLevelModelPositions(sexpModel, topLevelModelPositions);
		
		Iterator<Position> iterPos = topLevelModelPositions.iterator();
		
		//Schnittmenge berechnen und aus beiden Mengen entfernen
		while(iterPos.hasNext()) {
			Position position = iterPos.next();
			if( annotationModelPositions.containsValue(position) ) {
				annotationModelPositions.values().remove(position);
				iterPos.remove();
			}
		}
		
		//Positionen in TopLevelModel entsprechen jetzt den hinzugefuegten TL-Forms
		for( Position position : topLevelModelPositions ) {
			additions.put(new ProjectionAnnotation(), position);
		}
		
		//Positionen in AnnotationModel entsprechen den geloeschten TL-Forms
		return pullAnnotationsOut(annotationModelPositions);
	}

	private Annotation[] pullAnnotationsOut(Map<Annotation, Position> map)
	{
		Set<Annotation> set = map.keySet();
		return set.toArray(new Annotation[set.size()]);
	}
	
	private void copyTopLevelModelPositions(ISexpModel sexpModel, List<Position> topLevelModelPositions) 
	{
		for(SExpression sexp : sexpModel.getTopLevelForms()) {
			if( sexpModel.formSpansLines(sexp) > 1 ) {
				topLevelModelPositions.add(sexpModel.completePositionToFullLines(sexp));
			}
		}
	}
	
	@SuppressWarnings("unchecked") //Schnittstelle zu Eclipse nicht generisch
	private void copyAnnotationPositions(ProjectionAnnotationModel annotationModel, Map<Annotation, Position> annotationModelPositions) 
	{
		Iterator<Annotation> iter = annotationModel.getAnnotationIterator();
		
		while(iter.hasNext()) {
			Annotation annotation = iter.next();
			annotationModelPositions.put(annotation, annotationModel.getPosition(annotation));
		}
	}
}