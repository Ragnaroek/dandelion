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

package de.fh_trier.eclipse.lisp.internal.ui.views.listener;

import java.io.*;
import java.util.*;
import java.util.List;
import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.*;
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.core.LispCore;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.core.connection.IResult.TResult;
import de.fh_trier.eclipse.lisp.internal.core.dom.Symbol;
import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.actions.*;
import de.fh_trier.eclipse.lisp.internal.ui.text.*;
import de.fh_trier.eclipse.lisp.internal.ui.text.partition.*;
import de.fh_trier.eclipse.lisp.internal.ui.text.presentation.LispSourcePresentationManager;
import de.fh_trier.eclipse.lisp.internal.ui.views.*;
import edu.umd.cs.findbugs.annotations.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

import static de.fh_trier.eclipse.lisp.internal.ui.text.SourceUtilities.SYS_NEWLINE;

//TODO Copy-Paste von mehreren Forms in Listener ermoeglichen (ueber bulk-eval-job)
public class ListenerView 
extends ViewPart
implements IBackgroundEvaluationListener, ITextListener, IListenerView
{
	private static final Image  ERROR_IMG 		= PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
	private static final String DEFAULT_PROMPT 	= "?> ";
	private static final String DEFAULT_PACKAGE = Symbol.SYM_CL_USER;
	
	private String fPrompt  = DEFAULT_PROMPT;
	private String fPackage = DEFAULT_PACKAGE; //package in dem sich der Listener gerade befindet
	
	private List<Annotation>  fAnnotationList;
	protected SourceViewer 	  fSourceViewer; //protected fuer closure
	private IDocument 	 	  fDocument;
	private IEvalHistory      fEvalHistory;
	private IEnvironment       fEvalServer;
	private IConnection       fConnection; //wird in prepareEval gesetzt
	private boolean fRedirectInput = false;
	private StringBuilder fRedirectBuffer;
	
	private String fSecondaryID;
 	private Combo  fServerCombo;
	
	//nur bis hierhin ist Aenderung im Dokument moeglich
	protected int     	fLastPromptOffset; //protected fuer closure
	private List<Position> fLastOutputPositions;
	
	//Aktionen der View
	private IAction		fActionClear;
	private IAction     fActionSwitchPackage;
	private IAction     fActionHistory;
	
	private class ListenerVerifyKey 
	implements VerifyKeyListener
	{
		public void verifyKey(final VerifyEvent event) {
			int caret = fSourceViewer.getTextWidget().getCaretOffset();
			
			if( caret < fLastPromptOffset ) {
				//nur Pfeiltasten und CTRL+C erlaubt
				event.doit = event.keyCode == SWT.ARROW_DOWN ||
							 event.keyCode == SWT.ARROW_UP   ||
							 event.keyCode == SWT.ARROW_LEFT ||
							 event.keyCode == SWT.ARROW_RIGHT ||
							 isCTRLC(event);
			}
			
			if(caret == fLastPromptOffset) {
				event.doit = event.keyCode != SWT.BS;
			}
		}
		
		private boolean isCTRLC(final VerifyEvent event) {
			return event.stateMask == SWT.CTRL && event.character == 0x03;
		}
	}
		
	public ListenerView()
	{	
		fDocument = new Document();
		fAnnotationList = new LinkedList<Annotation>();
		fLastOutputPositions = new LinkedList<Position>();
		fEvalHistory    = new EvalHistory(50);
		fRedirectBuffer = new StringBuilder();
		
		setPrompt(DEFAULT_PACKAGE);
	}
	
	@Override
	public void init(final IViewSite site, final IMemento memento) 
	throws PartInitException 
	{
		super.init(site, memento);
		
		fSecondaryID = site.getSecondaryId();
		
		String host = null;
		Integer port = null;
		String name = null;
		String version = null;
		
		if(memento != null) {
			host = memento.getString("host");
			port = memento.getInteger("port");
			name = memento.getString("name");
			version = memento.getString("version");
		}
		
		//die view wird zum ersten mal geladen, wir warten auf das setEvalServer vom Manager
		//beim ersten zugriff auf evalserver dann fehlermeldung falls nicht gesetzt wurde
		if(!(host == null || port == null || name == null || version == null)) {
			fEvalServer = new Environment(host, port, name, version);
		}
		LispCore.getEvalServerManager().addEvalServerManagementListener(this);
	}

	@Override
	public void saveState(final IMemento memento) 
	{
		if(fEvalServer != null) {
			memento.putString("host", fEvalServer.getHost());
			memento.putInteger("port", fEvalServer.getPort());
			memento.putString("name", fEvalServer.getName());
			memento.putString("version", fEvalServer.getVersion());
		}	
	}

	@Override
	public void createPartControl(final Composite parent) 
	{
		if(fSecondaryID != null) {
			createListenerControl(parent);
		} else {
			createOverviewControl(parent);
		}
	}
	
	private void createListenerControl(final Composite parent)
	{
		IVerticalRuler ruler = new VerticalRuler(16);
		fSourceViewer = new SourceViewer(parent, ruler, SWT.V_SCROLL);
		
		//Features des Lisp-Editors hinzufuegen
		LispSourcePresentationManager.installMatchingCharacterPainter(fSourceViewer);
		LispSourcePresentationManager.installSourceViewSupport(fSourceViewer, fDocument, new AnnotationModel());
		
		//Aktionen hinzufuegen
		makeActions();
		contributeToActionBars();
		setInfoServer(false);
		
		//Busy fuer alle Job-Typen dieser Art aktivieren
		if(fEvalServer != null) {
			getProgressService().showBusyForFamily(fEvalServer);
		}
		
		prompt(false);
		
		//Listener zuletzt anmelden
		fSourceViewer.appendVerifyKeyListener(new ListenerVerifyKey());
		fSourceViewer.addTextListener(this);
	}
	
	private void createOverviewControl(final Composite parent)
	{
		parent.setLayout(new GridLayout(2, false));
		
		Label label = new Label(parent, SWT.NONE);
		label.setText("Choose a Listener to open:");
		{
			GridData data = new GridData();
			data.horizontalSpan = 2;
			label.setLayoutData(data);
		}
		fServerCombo = new Combo(parent, SWT.READ_ONLY);
		{
			GridData data = new GridData();
			data.widthHint = 200;
			fServerCombo.setLayoutData(data);
		}
		fillServerCombo();

		Button button = new Button(parent, SWT.PUSH);
		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				openListener();
			}
			
		});
		button.setText("Open Listener");
	}
	
	protected void openListener()
	{
		String text = fServerCombo.getText();
		IEnvironment selectedServer = null;
		for(IEnvironment server : LispCore.getEvalServerManager().getEvalServer()) {
			if(server.toString().equals(text)) {
				selectedServer = server;
				break;
			}
		}
		
		if(selectedServer != null) {
			try {
				LispCore.getEvalServerManager().openListenerFor(selectedServer);
			} catch (PartInitException e) {
				LispUI.showErrorDialog(getSite().getShell(), e);
			}
		}
	}
	
	private void fillServerCombo()
	{
		if(fServerCombo != null) { //null wenn secondary != null
			fServerCombo.removeAll();
			for(IEnvironment server : LispCore.getEvalServerManager().getEvalServer(true)) {
				fServerCombo.add(server.toString());
			}
			if(fServerCombo.getItemCount() > 0) {
				fServerCombo.select(0);
			}
		}
	}
	
	public void connect(final IEnvironment server) {
		if(server.equals(fEvalServer)) {
			setInfoServer(true);
		}
	}

	public void disconnect(final IEnvironment server) {
		if(server.equals(fEvalServer)) {
			setInfoServer(false);
		}
	}
	
	public void startup(final IEnvironment server) {
		//no-op
	}

	public void initialized(final IEnvironment server) {
		//no-op
	}
	
	public void serverAdded(final IEnvironment server) {
		fillServerCombo();
	}

	public void serverRemoved(final IEnvironment server) {
		fillServerCombo();
	}
	
	public void defaultChanged(IEnvironment newDefault) {
		//no-op
	}

	private void setInfoServer(final boolean connected)
	{
		if(fEvalServer == null) {
			setContentDescription("<not connected>");
			setPartName("Listener <unknown>");
		} else {
			setContentDescription(fEvalServer + " (" + (connected ? "connected" : "disconnected") + ")" );
			setPartName("Listener " + fEvalServer);
		}
	}
	
	//IListenerView-Protokoll implementierung
	
	public void setEvalServer(final IEnvironment server, final boolean connected) {
		
		if (server == null) {
			throw new NullPointerException("server must not be null");
		}
		
		this.fEvalServer = server;
		setInfoServer(connected);
		getProgressService().showBusyForFamily(fEvalServer);
	}
	
	@Nullable
	public IEnvironment getEvalServer() {
		return fEvalServer;
	}

	
	//<-----------------------
	
	@Override
	public void setFocus() 
	{
		if(fSourceViewer != null) {
			fSourceViewer.getControl().setFocus();
		}
	}
	
	private void prompt(final boolean withNewline)
	{
		String prompt = null;
		if(withNewline) {
			prompt = SYS_NEWLINE + fPrompt;
		} else {
			prompt = fPrompt;
		}
		
		startIgnoreTextChanges();
		appendToDocument(prompt);
		stopIgnoreTextChanges();
		
		fSourceViewer.setSelectedRange(fDocument.getLength(), 0);
		fSourceViewer.revealRange(fDocument.getLength(), 0);
		fLastPromptOffset = fDocument.getLength(); //position direkt hinter prompt
	}
	
	@SuppressWarnings("Dm") //Local default aktuelle in Eclipse
	private void setPrompt(String pack)
	{
		if(pack == null) {
			pack = DEFAULT_PACKAGE;
		}
		
		this.fPrompt = pack.toUpperCase() + "> ";
	}
	
	public void textChanged(final TextEvent event) 
	{	
		if( event.getDocumentEvent() == null ) {
			return;
		}
		
		if( fRedirectInput ) {
			fRedirectBuffer.append(event.getText());
			if( containsNewline(event.getText()) && fConnection != null) {
				sendBufferToIO();
				fRedirectBuffer = new StringBuilder();
			}
			return;
		}

		if( formError() || !containsNewline(event.getText()) || !formPresent()) {
			return;
		}

		//invariante: Es liegt mindestens eine auswertbare TL-Form am Listener an.
		internalListenerEval();	
	}

	private boolean containsNewline(final String text)
	{
		//return TextUtilities.endsWith(fDocument.getLegalLineDelimiters(), text) != -1;
		return text.contains(SYS_NEWLINE) || text.indexOf('\n') >= 0 || text.indexOf('\r') >= 0;
	}
	
	private void sendBufferToIO() 
	{
		try {
			SourceUtilities.normalizeNewline(fRedirectBuffer);
			Writer writer = fConnection.getIOWriter();
			writer.write(fRedirectBuffer.toString());
			writer.flush();
		} catch (ConnectionException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Retrieving input stream failed", e);
		} catch (IOException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Sending input to server failed", e);
		}
	}

	private void internalListenerEval()
	{
		if(fEvalServer == null) {
			LispUI.showErrorDialog(getSite().getShell(), "No eval server set");
		}
		IConnection connection = null;
		try {
			connection = LispCore.getEvalServerManager().getConnectionFor(fEvalServer);
		} catch (ManagementException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Server not found (may have been deleted)", e);
			return;
		}
		
		String form = getCurrentForm();	
		fEvalHistory.addEntry(form);
		lockInput(true);
		
		EvaluationJob job = 
			new EvaluationJob(connection, fPackage, form, this);
		job.schedule();
	}
	
	private boolean formError()
	{
		try {
			boolean invalid = SourceUtilities.hasInvalidParenthesisNesting(fDocument, fLastPromptOffset, fDocument.getLength(), 
					LispPartitionConstants.LISP_PARTITION_DEFAULT);
			
			if(invalid) {
				Position position = new Position(fLastPromptOffset-1, fDocument.getLength()-fLastPromptOffset);
				IAnnotationModel model = fSourceViewer.getAnnotationModel();
				Annotation annotation = new ErrorAnnotation(ERROR_IMG);
				model.addAnnotation(annotation, position);
				fAnnotationList.add(annotation);
				return true;
			}
			
			//invariante: Form ok, evtl. alte Error-Annotation entfernen
			deleteAllAnnotations();
			return false;
			
		} catch (BadLocationException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Checking form failed", e);
		} catch (BadPartitioningException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Checking form failed", e);
		}
		
		return false;
	}
	
	private void deleteAllAnnotations()
	{
		if(fAnnotationList.size() > 0) { //sollte maximal eine Annotation sein
			IAnnotationModel model = fSourceViewer.getAnnotationModel();
			for(Annotation annotation : fAnnotationList) {
				model.removeAnnotation(annotation);
			}
			fAnnotationList.clear();
		}
	}
	
	/**
	 * Gibt alle TL-Form die am Listener anliegen
	 * zurueck.
	 * @return
	 */
	private String getCurrentForm()
	{
		try {
			List<Position> topLevelPositions = SourceUtilities.extractForms(fDocument, fLastPromptOffset, fDocument.getLength());
			Position region = topLevelPositions.get(0); //TODO alle angegeben Forms auslesen
			return fDocument.get(region.getOffset(), region.getLength());
			//return fDocument.get(fLastPromptOffset, fDocument.getLength()-fLastPromptOffset);
		} catch (BadLocationException e) {
			LispUI.showErrorDialog(getSite().getShell(), "Extracting form failed", e);
		} catch (StructureException e) {
			//sollte nie passieren, da vorher geprueft wird ob eine korrekte Form anliegt
			LispPluginActivator.logBrokenInvariant("extracting current form failed", e);
		}
		
		return "";
	}
	
	private boolean formPresent()
	{
		try {
			return SourceUtilities.validForm(fDocument, fLastPromptOffset, fDocument.getLength());
		} catch (BadLocationException e) {
			LispPluginActivator.logBrokenInvariant("checking whether form is present failed", e);
			return false;
		}
	}
	
	private void lockInput(final boolean lock)
	{
		enableActions(!lock);
	}

	//Interface IBackgroundEvalutationListener Implementierung
	
	public void prepareEval(final IConnection connection, final boolean bulk) {	
		fConnection = connection;
		lockInput(true); //locken bis finishEval aufgerufen wird
		fRedirectInput = true;
		fLastOutputPositions.clear();
	}
	
	/**
	 * Callback-Methode von BackgroundEvaluation.
	 */
	public void handleOutput(final String out)
	{
		startIgnoreTextChanges();
		int offsetStart = fDocument.getLength();
		appendToDocument(out+SYS_NEWLINE);
		int offsetEnd = fDocument.getLength();
		int length =  offsetEnd - offsetStart - SYS_NEWLINE.length();
		if(length > 0) {
			fLastOutputPositions.add(new Position(offsetStart, length));
		}
		stopIgnoreTextChanges();
	}
	
	/**
	 * Callback-Methode von ListenerProcessor.
	 */
	public IRestartSelection formEvaluated(final IResult result, final boolean more) 
	{
		if( !more ) {
			markOutput();
		}

		return processResult(result, more);
	}
	
	public void finishEval()
	{
		fRedirectInput = false;
		fConnection = null;
		lockInput(false);
		prompt(true);
	}
	
	public Display syncExecOnDisplay() {
		return PlatformUI.getWorkbench().getDisplay();
	}

	private IRestartSelection processResult(final IResult result, final boolean more)
	{	
		if(result == null) {
			return RestartSelection.ABORT_SELECTION;
		}
	
		if(result.getTyp() == TResult.EVAL_ERROR) {
			IRestartSelection selection = result.openRestartDialog(getSite().getShell());
			return selection;
		} else if (result.getTyp() == TResult.READ_ERROR) {
			result.openRestartDialog(getSite().getShell()); //Benutzer kann nur Abort waehlen
			return RestartSelection.ABORT_SELECTION;
		} 
		
	    //invariante: Typ == SUCCESS
		startIgnoreTextChanges();
		appendResult(result, more);
		fPackage = result.getPackage();
		setPrompt(result.getPackage());
		stopIgnoreTextChanges();
		
		return null; //alles ok waehrend eval
	}
	
	//protected fuer closure
	protected void markOutput()
	{
		for(Position outputPos : fLastOutputPositions) {
			Annotation annotation = new RangeIndicator(SWT.COLOR_INFO_BACKGROUND);
			fSourceViewer.getAnnotationModel().addAnnotation(annotation, outputPos);
		}
	}
	
	private void appendResult(final IResult result, final boolean more)
	{   //ignore text changes
		StringBuilder buffer = new StringBuilder();
		buffer.append(SYS_NEWLINE);
		buffer.append(result.getResult());
		if( result.hasMultipleValues() ) {
			for(String form : result.getMultipleValueResult() ) {
				buffer.append(SYS_NEWLINE);
				buffer.append(form);
			}
		}
		
		if( !more ) {
			buffer.append(SYS_NEWLINE); //abstand
		}

		appendToDocument(buffer.toString());
	}
	
	private void startIgnoreTextChanges()
	{
		fSourceViewer.removeTextListener(this);
	}
	
	private void stopIgnoreTextChanges()
	{
		fSourceViewer.addTextListener(this);
	}
	
	private void appendToDocument(final String text)
	{
		try {
			fDocument.replace(fDocument.getLength(), 0, text);
		} catch (BadLocationException e) {
			LispPluginActivator.logError("broken invariant: Appending to Document failed", e);
		}
	}
	
	@Override
	public void dispose() {
		if(fSourceViewer != null) {
			fSourceViewer.removeTextListener(this);
		}
		LispCore.getEvalServerManager().removeEvalServerManagementListener(this);
		super.dispose();
	}
	
	private IWorkbenchSiteProgressService getProgressService() {
		Object siteService= getSite().getAdapter(IWorkbenchSiteProgressService.class);
		if(siteService != null) {
			return (IWorkbenchSiteProgressService) siteService;
		}
			
		return null;
	}
	
	public void clear()
	{
		try {
			startIgnoreTextChanges();
			lockInput(true);
			fDocument.replace(0, fDocument.getLength(), "");
			prompt(false);
			lockInput(false);
			stopIgnoreTextChanges();
		} catch (BadLocationException e) {
			LispPluginActivator.logError("broken invariant: Deleting Document content failed", e);
		}
	}
	
	public void switchToPackage(final String pack)
	{
		this.fPackage = pack;
		setPrompt(pack); //vor startEval!
		prompt(true);
	}
	
	/**
	 * Fuegt an den Listener den uebergebenen Text an.
	 * @param input
	 */
	public void addText(String input)
	{
		if( !input.endsWith(SYS_NEWLINE) ) {
			input += SYS_NEWLINE;
		}
		
		appendToDocument(input);
	}
	
	//Aktionen
	
	private void makeActions()
	{
		fActionClear         = new ListenerClearAction(this);
		fActionSwitchPackage = new ListenerSwitchPackageAction(this);
		fActionHistory       = new ListenerHistoryAction(fEvalHistory, this);
	}
	
	private void contributeToActionBars()
	{
		IActionBars bars = getViewSite().getActionBars();
		
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}
	
	private void fillLocalPullDown(final IMenuManager manager) 
	{
		manager.add(fActionClear);
		manager.add(fActionSwitchPackage);
	}
	
	private void fillLocalToolBar(final IToolBarManager manager) 
	{
		manager.add(fActionClear);
		manager.add(fActionSwitchPackage);
		manager.add(fActionHistory);
	}
	
	private void enableActions(final boolean b)
	{
		fActionClear.setEnabled(b);
		fActionSwitchPackage.setEnabled(b);
		fActionHistory.setEnabled(b);
	}
}
