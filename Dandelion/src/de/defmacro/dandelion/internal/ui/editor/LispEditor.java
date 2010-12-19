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

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.jface.text.source.projection.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.*;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.project.ILispProject;
import de.defmacro.dandelion.internal.ui.*;
import de.defmacro.dandelion.internal.ui.text.*;
import de.defmacro.dandelion.internal.ui.text.presentation.LispSourcePresentationManager;
import edu.umd.cs.findbugs.annotations.Nullable;
import edu.umd.cs.findbugs.annotations.NonNull;

/**
 * Implementierung der {@link ILispEditor}-Schnittstelle.
 * @author Michael Bohn
 *
 */
public class LispEditor 
extends AbstractDecoratedTextEditor
implements ISelectionChangedListener, ILispEditor
{	
	private static final String ERROR_ANNOTATION = "org.eclipse.jdt.ui.error";
	private static final String WARNING_ANNOTATION = "org.eclipse.jdt.ui.warning";
	private static final String EDITOR_SCOPE = LispPluginActivator.NS + ".contexts.lispEditorScope";
	
	private IAction fOpenListenerAction = new Action("Open Listener") {
		@Override
		public void run() {
			try {
				ILispProject project = LispCore.getProjectManager().getLispProjectFor(getInput().getProject());
				LispCore.getEnvironmentManager().openListenerFor(project.getEnvironment());
			} catch (PartInitException e) {
				LispUI.showErrorDialog(getSite().getShell(), e);
			}
		}
	};
	
	private UIColorManager fColorManager;
	private ProjectionSupport fProjectionSupport;
	protected LispContentOutlinePage fOutlinePage;
	private ISexpModel fSexpModel;
	private ISexpModel fLastErrorFreeModel;
	private boolean fInteractiveEvaluation;

	/**
	 * Erzeugt einen neuen Lisp-Editor.
	 */
	public LispEditor()
	{	
		this.fColorManager = LispUI.getUIColorManager();
		fOutlinePage = new LispContentOutlinePage();
		fOutlinePage.addSelectionChangedListener(this);
	}

	/**
	 * @see IEditorPart#init(IEditorSite, IEditorInput)
	 */
	//nicht synchronized da nur intern aufgerufen wird
	@Override
	public void init(final IEditorSite site, final IEditorInput input) 
	throws PartInitException 
	{
		super.init(site, input);
		
		if( !(input instanceof IFileEditorInput) ) {
			throw new PartInitException("invalid input: IFileEditorInput expected");
		}
		
		checkProject(getInput().getProject()); //vor installSourceEditSupport aufrufen!!!
		LispSourcePresentationManager.installSourceEditSupport(this, fInteractiveEvaluation);
	}
	
	@Override
	protected void editorContextMenuAboutToShow(final IMenuManager menu) {
		super.editorContextMenuAboutToShow(menu);
		menu.add(new Separator());
		fOpenListenerAction.setEnabled(hasInteractiveEvaluationSupport());
		menu.add(fOpenListenerAction);
	}

	/**
	 * @see AbstractDecoratedTextEditor#createPartControl(Composite)
	 */
	//nicht synchronized da nur intern aufgerufen wird
	@Override
	public void createPartControl(final Composite parent) {
		super.createPartControl(parent);
		
		//TODO Farbe einstellbar machen
		//Klammermarkierung installieren
		LispSourcePresentationManager.installMatchingCharacterPainter(getSourceViewer(), 
				fColorManager.getColor(new RGB(0, 0, 0)));
		
		installProjectionSupport();
	}
	
	@Override
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("SIC")
	protected void createActions() {
		super.createActions();
		
		IAction action = new TextOperationAction( new ListResourceBundle()
		{
			@Override
			protected Object[][] getContents() { //TODO ResourceBundel Anbindung bei deutscher Lokalisierung
				return new String[][] {
						{"label", "Content Assist@Ctrl+SPACE"},
						{"tooltip", "Content Assist"},
						{"description", "Content Assist"},
				};
			}
		}, "ContentAssistProposal.", this, ISourceViewer.CONTENTASSIST_PROPOSALS );
		action.setActionDefinitionId( ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
		setAction("ContentAssistProposal", action);
		
		action = new Action() {
			@Override
			@edu.umd.cs.findbugs.annotations.SuppressWarnings("IMA")
			public void run() {
				ITextOperationTarget target = (ITextOperationTarget)getSourceViewer();
				if(target != null && target.canDoOperation(ISourceViewer.FORMAT)) {
					target.doOperation(ISourceViewer.FORMAT);
				}
			}
		};
		action.setActionDefinitionId(LispPluginActivator.NS + ".commands.editor.format");
		setAction("QuickFormat", action);
	}

	@Override
	protected void initializeKeyBindingScopes()
	{
		setKeyBindingScopes(new String[] {EDITOR_SCOPE});
	} 

	@Override
	protected ISourceViewer createSourceViewer(final Composite parent, final IVerticalRuler ruler, int styles) 
	{  
		ISourceViewer viewer = new ProjectionViewer(parent, ruler, getOverviewRuler(), isOverviewRulerVisible(), styles);
		//!!!ohne diesen Aufruf wird der OverviewRuler nicht angezeigt und die Error Squiggles werden nicht angezeigt
		getSourceViewerDecorationSupport(viewer); 
		return viewer;
	}
	
	/**
	 * Speichert den aktuelle Editorinhalt und sezt die Markierungen.
	 * @see AbstractDecoratedTextEditor#doSave(IProgressMonitor)
	 */
	@Override
	public synchronized void doSave(final IProgressMonitor progressMonitor) {
		super.doSave(progressMonitor);
		updateMarkers(getSourceViewer().getAnnotationModel());
	}
	
	/**
	 * Sichtbarkeit erhoeht. Damit ueber LispSourcePresentationManager initialisierbar.
	 */
	@Override
	public synchronized void setSourceViewerConfiguration(final SourceViewerConfiguration configuration) {
		super.setSourceViewerConfiguration(configuration);
	}
	
	/**
	 * Liefert die Outline fuer den Editor.
	 * @see IAdaptable#getAdapter(Class)
	 */
	@SuppressWarnings("unchecked") //Schnitstelle zu Eclipse nicht generisch
	@Override
	public synchronized Object getAdapter(final Class required) 
	{
		//Adapter fuer Projektionsupport
		if( fProjectionSupport != null ) {
			Object adapter = fProjectionSupport.getAdapter(getSourceViewer(), required);
			if (adapter != null) {
				return adapter;
			}
		}
		
		if(IContentOutlinePage.class.equals(required)) {
			if(fSexpModel != null && fOutlinePage != null) {
				fOutlinePage.setInput(fSexpModel);
			}
			return fOutlinePage;
		}
		
		//weiter in Supertyp nach Adapter suchen
		return super.getAdapter(required);
	}
	
	/**
	 * Entsorgt den Editor.
	 * Der Editor darf danach nicht mehr weiter verwendet werden.
	 */
	@Override
	public synchronized void dispose() 
	{	
		if(fOutlinePage != null) {
			fOutlinePage.removeSelectionChangedListener(this);
			fOutlinePage.dispose();
		}
		
		super.dispose();
	}
	
	private boolean checkProject(final IProject project) 
	{
		boolean projectOK = true;
		try {
			
			if( !project.exists() || !project.isOpen() || !project.hasNature(LispNature.ID) ) {
				projectOK = false;
				LispPluginActivator.log(IStatus.WARNING, "Not a Lisp project " + project.getName() + " interactive evaluation deactivated", null);
			}
		} catch (CoreException e) {
			projectOK = false;
			LispPluginActivator.logError("Error retriving nature of project, interactive evaluation deactivated", e);
		}
		
		enableInteractiveEvaluationSupport(projectOK);
		return projectOK;
	}
	
	private void enableInteractiveEvaluationSupport(final boolean enabled)
	{
		fInteractiveEvaluation = enabled;
	}
	
	private void installProjectionSupport()
	{
		ProjectionViewer projectionViewer = (ProjectionViewer)getSourceViewer();
		fProjectionSupport = new ProjectionSupport(projectionViewer, getAnnotationAccess(), getSharedColors());
		fProjectionSupport.addSummarizableAnnotationType(ERROR_ANNOTATION);
		fProjectionSupport.addSummarizableAnnotationType(WARNING_ANNOTATION);
		fProjectionSupport.install();
		
		if(projectionViewer.canDoOperation(ProjectionViewer.TOGGLE)) {
			projectionViewer.doOperation(ProjectionViewer.TOGGLE);
		}
	}
	
	/**
	 * @see ILispEditor#getInput()
	 */
	public synchronized IFile getInput() {
		return ((IFileEditorInput)getEditorInput()).getFile();
	}

	/**
	 * @see ILispEditor#hasInteractiveEvaluationSupport()
	 */
	public synchronized boolean hasInteractiveEvaluationSupport() {
		return fInteractiveEvaluation;
	}
	
	/**
	 * @see ILispEditor#getSExpressionModel()
	 */
	@Nullable
	public synchronized ISexpModel getSExpressionModel()
	{
		return fSexpModel;
	}
	
	/**
	 * @see ILispEditor#getLastStructureCorrectModel()
	 */
	@Nullable
	public synchronized ISexpModel getLastStructureCorrectModel() {
		return fLastErrorFreeModel;
	}

	/**
	 * @see ILispEditor#setSExpressionModel(ISexpModel)
	 */
	//wird von fremden Thread aufgerufen
	@edu.umd.cs.findbugs.annotations.SuppressWarnings("IMA")
	public synchronized void setSExpressionModel(final ISexpModel model)
	{
		if(this.getSite() == null || this.getSite().getShell() == null || this.getSite().getShell().isDisposed()) {
			return;
		}
		Assert.isNotNull(model);
		
		final boolean initial = fSexpModel == null;
		this.fSexpModel = model;
		if(!fSexpModel.hasMalformation(TSeverity.STRUCTURE)) {
			fLastErrorFreeModel = fSexpModel;
		}
		
		//update der outline
		PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
			@edu.umd.cs.findbugs.annotations.SuppressWarnings("IMA")
			public void run() {
				if(fOutlinePage == null || fOutlinePage.getControl() == null || fOutlinePage.getControl().isDisposed()) {
					return;
				}
				fOutlinePage.setInput(model); //Aenderung OutlinePage mitteilen

				if(getSourceViewer() != null) {
					if(initial || !isDirty()) { //bei initalem setzen des Models -> volles update!
						//!isDirty() - es kann sein das das Model nach einem save gesetzt wird, da reconcil-thread mit 500ms
						//verzoegerung laeuft. kommt ein neues Model und der inhalt wurde bereits gespeichert -> auch
						//marker updaten
						updateAnnotations();
						updateMarkers(getSourceViewer().getAnnotationModel());
					} else {
						updateAnnotations();
					}
				}
			}
		});
	}
	
	protected void updateAnnotations()
	{
		if(fSexpModel == null) { return; }
		
		IAnnotationModel annotationModel =  getSourceViewer().getAnnotationModel();
		if(annotationModel == null) { return; }
		
		removeAnnotations(annotationModel);
		
		if(fSexpModel.hasMalformation()) {
			for(ISyntacticalMalformation malformation : fSexpModel.getMalformations()) {
				String type = null;
				switch(malformation.getSeverity()) {
				case STRUCTURE : //fallthrough
				case ERROR : type = ERROR_ANNOTATION;
					         break;
				case WARNING : type = WARNING_ANNOTATION;
					         break;
				default: throw new IllegalArgumentException("Unknown TSeverity supplied");
				}
				
				annotationModel.addAnnotation(new Annotation(type, false, malformation.getDescription()), malformation.getPosition());
			}
		}
	}
	
	@SuppressWarnings("unchecked") //Schnitstelle zu Eclipse nicht generisch
	private void removeAnnotations(final IAnnotationModel annotationModel) 
	{
		for(Iterator iter = annotationModel.getAnnotationIterator(); iter.hasNext();) {
			Annotation annotation = (Annotation)iter.next();
			annotationModel.removeAnnotation(annotation);
		}
	}

	@SuppressWarnings("unchecked")
	protected void updateMarkers(final IAnnotationModel annotationModel)
	{
		if(annotationModel == null) {
			return;
		}
		
		IFile file = getInput();
		if( !file.exists() ) {
			return;
		}
		
		try {
			file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
			
			boolean hasErrors = false;
			boolean hasWarnings = false;
			
			for(Iterator iter = annotationModel.getAnnotationIterator(); iter.hasNext(); ) {
				Annotation annotation = (Annotation)iter.next();
				String type = annotation.getType();
				int severity = 0;
				if(type.equals(ERROR_ANNOTATION)) {
					severity = IMarker.SEVERITY_ERROR;
					hasErrors = true;
				} else if(type.equals(WARNING_ANNOTATION)) {
					severity = IMarker.SEVERITY_WARNING;
					hasWarnings = true;
				} else {
					continue;
				}
				
				IMarker marker = file.createMarker(IMarker.PROBLEM);
				Map<String, Object> attributes = new HashMap<String, Object>();
				attributes.put(IMarker.TRANSIENT, true);
				attributes.put(IMarker.SEVERITY, severity);
				Position position = annotationModel.getPosition(annotation);
				attributes.put(IMarker.CHAR_START, position.getOffset());
				attributes.put(IMarker.CHAR_END, position.getOffset()+position.getLength());
				attributes.put(IMarker.MESSAGE, annotation.getText());
				attributes.put(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
				marker.setAttributes(attributes);
			}
			
			markResource(file, hasErrors, hasWarnings); //immer aufrufen
		} catch (CoreException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Error updating markers", e);
		}
	}
	
	private void markResource(final IFile file, final boolean hasErrors, final boolean hasWarnings)
	{
		try {
			String mark = null;
			if(hasErrors) {
				mark = LispResourceDecorator.ERROR;
			} else if(hasWarnings) {
				mark = LispResourceDecorator.WARNING;
			}
			file.setSessionProperty(LispResourceDecorator.MALFORMATION_MARK, mark);
			file.getProject().setSessionProperty(LispResourceDecorator.MALFORMATION_MARK, mark);
			LispResourceDecorator decorator = LispResourceDecorator.getDecorator();
			if( decorator != null ) {
				decorator.refresh(new IResource[] {file, file.getProject() }); //label provider updaten
			}
		} catch (CoreException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Setting malformation mark to resource failed", e);
		}
	}
	
	/**
	 * Listener Implementierung. Nicht aufrufen.
	 */
	public synchronized void selectionChanged(final SelectionChangedEvent event) 
	{
		//Benutzer hat Knoten in der OutlineView gewaehlt
		if(event.getSource() == fOutlinePage) {
			TreeSelection selection = (TreeSelection)event.getSelection();
			if( !selection.isEmpty() ) {
				SExpression sexp = (SExpression)selection.getFirstElement();
				
				Position pos = sexp.typeSwitch(HighlightSelectedTypeCase.instanceOf());
				if(pos != null) {
					selectAndReveal(pos.getOffset(), pos.getLength());
				}
			}
		}
	}

	/**
	 * @see ILispEditor#getSourceSelection()
	 */
	@NonNull
	public synchronized ILispSourceSelection getSourceSelection()
	throws StructureException
	{
		TextSelection selection = (TextSelection)this.getSelectionProvider().getSelection();
		if(selection == null || selection.isEmpty() || fSexpModel == null 
				|| selection.getText() == null || selection.getText().trim().equals("")) {
			return LispSourceSelection.NULL_SELECTION;
		}
		//selektierter Bereich
		Position selectedPosition = new Position(selection.getOffset(), selection.getLength());
		IDocument document = fSexpModel.getDocument();
		
		try {
			//Selektionsanfang ist im Kommentarblock -> Form Extrahierung auch aus Kommentar
			//Selektionsanfang ist Source-Code -> Kommentar wird uebersprungen, damit auch grosse Bloecke mit 
			//eingestreuten Kommentaren korrekt gescannt werden
			List<Position> formPositions = SourceUtilities.extractFormsCheckContentType(document, selectedPosition.getOffset(), 
					selectedPosition.getOffset()+selectedPosition.getLength());

			List<PackageBoundForm> forms = extractForms(formPositions, document);
			boolean errors = fSexpModel.containsError(selectedPosition);	
			return new LispSourceSelection(forms, errors);
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("calculating source selection failed", e);
			return LispSourceSelection.NULL_SELECTION;
		}
	}
	
	/**
	 * @see ILispEditor#getFileSelection()
	 */
	@NonNull
	public synchronized ILispSourceSelection getFileSelection() 
	throws StructureException
	{
		if(fSexpModel == null) {
			return LispSourceSelection.NULL_SELECTION;
		}
		
		if(fSexpModel.hasMalformation(TSeverity.STRUCTURE)) {
			throw new StructureException(new Position(0, 0));
		}
		
		List<SExpression> topLevelForms = fSexpModel.getTopLevelForms();
		if(topLevelForms.size() == 0) {
			return LispSourceSelection.NULL_SELECTION;
		}
		
		List<Position> topLevelPositions = new LinkedList<Position>();
		for(SExpression sexp : topLevelForms) {
			topLevelPositions.add(sexp.getPosition());
		}
		
		try {
			List<PackageBoundForm> forms;
			forms = extractForms(topLevelPositions, fSexpModel.getDocument());
			return new LispSourceSelection(forms, fSexpModel.hasMalformation(TSeverity.ERROR));
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("calculating top level forms failed", e);
			return LispSourceSelection.NULL_SELECTION;
		}
	}

	/**
	 * @see ILispEditor#getTopLevelFormSelection()
	 */
	@NonNull
	public synchronized ILispSourceSelection getTopLevelFormSelection() 
	throws StructureException 
	{
		if(fSexpModel == null) {
			return LispSourceSelection.NULL_SELECTION;
		}
		
		int offset = getSourceViewer().getSelectedRange().x;
		
		SExpression topLevelForm = fSexpModel.getEnclosingTopLevelForm(offset);
		if(topLevelForm == null) {
			return LispSourceSelection.NULL_SELECTION;
		}
		Position position = topLevelForm.getPosition();
		
		try {
			String form = fSexpModel.getDocument().get(position.getOffset(), position.getLength());
			InpackageForm inpackage = fSexpModel.getInpackage(position);
			return new LispSourceSelection(inpackage.getPackage().getSymbolName(), form, fSexpModel.containsError(position));
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("calculating enclosing top level form failed", e);
			return LispSourceSelection.NULL_SELECTION;
		}
	}
	
	/**
	 * Forms aus Document als Text extrahieren und fuer jede form package berechnen.
	 * @param positions
	 * @return
	 */
	@NonNull
	private List<PackageBoundForm> extractForms(final List<Position> positions, final IDocument document)
	throws BadLocationException
	{
		List<PackageBoundForm> result = new ArrayList<PackageBoundForm>(positions.size());
		for(Position position : positions) {
			
			String form = document.get(position.getOffset(), position.getLength());
			InpackageForm inPackage = fSexpModel.getInpackage(position);
			
			result.add(new PackageBoundForm(inPackage.getPackage().getSymbolName(), form));
		}
		return result;
	}
}
