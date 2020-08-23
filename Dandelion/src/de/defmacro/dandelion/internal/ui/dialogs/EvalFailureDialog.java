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

package de.defmacro.dandelion.internal.ui.dialogs;

import java.util.List;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.*;

import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.ui.*;

/**
 * Der Dialog fuer fehlerhafte Evaluierungen.
 * @author Michael Bohn
 *
 */
public class EvalFailureDialog 
extends TitleAreaDialog
implements ISelectionChangedListener
{
	/**
	 * Der Rueckgabewert fuer die Abort-Auswahl.
	 */
	public static final int CODE_ABORT = -1;
	
	private static final String TITLE = "Evaluation failed";
	
	private List<IRestart> 	fRestarts;
	private TableViewer 	fTableViewer;
	private Text 			fText;
	private String 			fErrorDescription;
	private IRestart 		fRestartSelection;
	private String   		fRestartParameters;
	
	/**
	 * Erzeugt einen neuen Fehlerdialog.
	 * @param shell - Shell an der Dialog modal angezeigt wird.
	 * @param errorDescription - Beschreibung des Fehlers
	 * @param restarts - Verfuegbaren Restarts
	 * @throws NullPointerException - wenn restart oder shell == <code>null</code>
	 */
	public EvalFailureDialog(final Shell shell, final String errorDescription, final List<IRestart> restarts)
	{
		super(shell);
		setShellStyle(getShellStyle() | SWT.RESIZE);
		setTitleImage(LispUI.getUIImageManager().get(UIImageConstants.ICON_ERROR_DIALOG));
		
		if (restarts == null) {
			throw new NullPointerException("restarts must not be null");
		}
		
		setBlockOnOpen(true);
		
		this.fRestarts = restarts;
		this.fErrorDescription = errorDescription;
	}
	
	/**
	 * Erzeugt einen Fehlerdialog ohne Restarts.
	 * @param shell - Shell an der Dialog modal angezeigt wird.
	 * @param errorDescription - Beschreibung des Fehlers
	 * @throws NullPointerException - wenn restart oder shell == <code>null</code>
	 */
	public EvalFailureDialog(Shell shell, String errorDescription)
	{
		this(shell, errorDescription, Result.EMPTY_RESTARTS);
	}

	@Override
	protected Control createContents(Composite parent) 
	{
		Control content = super.createContents(parent);
		
		setTitle(TITLE);
		setMessage(fErrorDescription, IMessageProvider.ERROR);
		
		return content;
	}

	@Override
	protected Control createDialogArea(Composite parent) 
	{
		Composite composite = (Composite)super.createDialogArea(parent);
		createRestartSelection(composite);
		return composite;
	}
	
	private void createRestartSelection(Composite composite)
	{
		fTableViewer = new TableViewer(composite, SWT.FULL_SELECTION | SWT.V_SCROLL);
		fTableViewer.getTable().setLayoutData(new GridData(GridData.FILL_BOTH));
		fTableViewer.setContentProvider(new RestartDialogContentProvider());
		fTableViewer.setLabelProvider(new RestartDialogLabelProvider());
		
		fTableViewer.getTable().setHeaderVisible(true);
		fTableViewer.getTable().setLinesVisible(true);
		fTableViewer.addSelectionChangedListener(this);
		
		TableColumn column1 = new TableColumn(fTableViewer.getTable(), SWT.LEFT);
		column1.setText("Restart");
		column1.setWidth(200);
		
		TableColumn column2 = new TableColumn(fTableViewer.getTable(), SWT.LEFT);
		column2.setText("Description");
		column2.setWidth(300);
		fTableViewer.setInput(fRestarts);
		
		Label label = new Label(composite, SWT.LEFT);
		label.setText("Restart Parameter(s):");
		
		fText = new Text(composite, SWT.LEFT);
		fText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	}
	
	@Override
	protected void createButtonsForButtonBar(Composite parent) 
	{
		createButton(parent, IDialogConstants.ABORT_ID, IDialogConstants.ABORT_LABEL, true);
		if( !fRestarts.isEmpty() ) {
			createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
			setOKButtonEnabled(false);
			hintSelectRestart(true);
		}
	}
	
	@Override
	protected void buttonPressed(int buttonId) 
	{
		if(buttonId == IDialogConstants.ABORT_ID) {
			abort();
		}
		
		super.buttonPressed(buttonId);
	}

	@Override
	protected void handleShellCloseEvent() 
	{
		abort(); //druecken auf x wie Auwahl Abort
	}

	private void abort()
	{
		fRestartSelection = null;
		fRestartParameters = null;
		setReturnCode(CODE_ABORT);
		close();
	}
	
	@Override
	protected void okPressed() 
	{
		IStructuredSelection selection = (IStructuredSelection)fTableViewer.getSelection();
		
		if(selection.size() <= 0) {
			fRestartSelection = null;
		} else {
			fRestartSelection = (IRestart)selection.getFirstElement();
		}
		
		String restartParams = fText.getText();
		if( restartParams != null && restartParams.trim().equals("") ) {
			restartParams = null;
		}
		
		fRestartParameters = restartParams;
		
		super.okPressed();
	}
	
	private void hintSelectRestart(boolean shouldHint)
	{
		if(shouldHint) {
			setErrorMessage("Please choose a restart.");
		} else {
			setErrorMessage(null);
		}
	}
	
	private void setOKButtonEnabled(boolean b)
	{
		getButton(IDialogConstants.OK_ID).setEnabled(b);
	}
	
	/**
	 * Liefert den selektierten Restart aus dem Dialog.
	 * Gibt <code>null</code> zurueck wenn kein Restart ausgewaehlt wurde.
	 * @return
	 */
	public IRestart getSelectedRestart()
	{
		return fRestartSelection;
	}
	
	/**
	 * Liefert die eingegebenen Parameter fuer den Restart.
	 * @return
	 */
	public String getRestartParameters()
	{
		return fRestartParameters;
	}

	//ISelectionChangedListener impl
	
	/**
	 * Listener-Implementierung. Nicht aufrufen.
	 */
	public void selectionChanged(SelectionChangedEvent event) 
	{
		if ( !(event.getSelection() instanceof IStructuredSelection)) {
			setOKButtonEnabled(false);
			hintSelectRestart(true);
			return;
		}
	
		IStructuredSelection selection = (IStructuredSelection)event.getSelection();
		if( !selection.isEmpty() &&
			 selection.getFirstElement() instanceof IRestart) {
			setOKButtonEnabled(true);
			hintSelectRestart(false);
		} else {
			setOKButtonEnabled(false);
			hintSelectRestart(true);
		}
	}
}

/**
 * Der Content-Provider fuer die Restart-Tabelle.
 * @author Michael Bohn
 *
 */
class RestartDialogContentProvider
implements IStructuredContentProvider
{
	private static final Object[] EMPTY_CONTENT = new Object[0];
	private List<IRestart> fRestarts;
	
	public void dispose() 
	{
		//nichts zu entsorgen
	}

	@SuppressWarnings("unchecked")
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) 
	{
		this.fRestarts = (List<IRestart>)newInput;
	}

	public Object[] getElements(Object inputElement) 
	{
		if(fRestarts == null) {
			return EMPTY_CONTENT;
		}
		return fRestarts.toArray();
	}
}

/**
 * Der LabelProvider fuer die Restart-Tabelle.
 * @author Michael Bohn
 *
 */
class RestartDialogLabelProvider
extends LabelProvider
implements ITableLabelProvider
{
	private static final int COL_RESTART = 0;
	private static final int COL_DESCRIPTION = 1;
	
	public Image getColumnImage(Object element, int columnIndex) 
	{
		return null; //keine Images unterstuetzt
	}

	public String getColumnText(Object element, int columnIndex) 
	{
		IRestart restart = (IRestart)element;
		switch (columnIndex)
		{
			case COL_RESTART : return restart.getName();
			case COL_DESCRIPTION : return restart.getDescription();
			default : return "<unknown>";
		}
	}
}
