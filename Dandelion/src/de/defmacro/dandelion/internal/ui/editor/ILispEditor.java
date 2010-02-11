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

package de.defmacro.dandelion.internal.ui.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;

import de.defmacro.dandelion.internal.core.dom.ISexpModel;
import de.defmacro.dandelion.internal.ui.text.StructureException;

/**
 * Die Schnittstelle fuer den Lisp-Editor.
 * @author Michael Bohn
 *
 */
public interface ILispEditor
extends IEditorPart, ITextEditor
{
	/**
	 * Eindeutige ID fuer den Lisp Editor.
	 */
	public static final String ID = "de.fh_trier.eclipse.lisp.editor";
	
	/**
	 * Teilt dem Editor ein neues Modell mit.
	 * @param model
	 */
	public void setSExpressionModel(final ISexpModel model);
	
	/**
	 * Liefert das aktuelle Modell des Editors.
	 * @return Aktuelles Modell
	 */
	public ISexpModel getSExpressionModel();
	/**
	 * Liefert das letzte Modell ohne Strukturfehler.
	 * <code>null</code> wenn nicht vorhanden.
	 * @return Struturfehlerfreies Modell
	 */
	public ISexpModel getLastStructureCorrectModel();
	
	/**
	 * Erhoehte Sichtbarkeit der normalen Editor-Klasse.
	 * @param configuration
	 */
	public void setSourceViewerConfiguration(SourceViewerConfiguration configuration);
	
	/**
	 * Liefert <code>true</code> wenn der Editor
	 * die Interaktive Evaluierung unterstützt.
	 * @return
	 */
	public boolean hasInteractiveEvaluationSupport();
	
	/**
	 * Liefert die Datei die der Editor bearbeitet.
	 * @return
	 */
	public IFile getInput();
	
	/**
	 * Liefert die aktuelle Selektion des Quelltextes
	 * im Editor.
	 * @return Selektion, nie <code>null</code>
	 * @throws StructureException
	 */
	public ILispSourceSelection getSourceSelection()
	throws StructureException;
	
	/**
	 * Liefert die Selektion der kompletten Datei
	 * im Editor.
	 * @return Selektion, nie <code>null</code>
	 * @throws StructureException
	 */
	public ILispSourceSelection getFileSelection()
	throws StructureException;
	
	/**
	 * Liefert die aktuelle Toplevel-Form an der Cursor-Position.
	 * @return Selektion, nie <code>null</code>
	 * @throws StructureException
	 */
	public ILispSourceSelection getTopLevelFormSelection()
	throws StructureException;
}
