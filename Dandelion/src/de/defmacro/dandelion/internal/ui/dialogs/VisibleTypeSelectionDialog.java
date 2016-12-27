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

import java.util.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.preferences.ViewSettingsDialog;

import de.defmacro.dandelion.internal.core.dom.TSExpression;
import de.defmacro.dandelion.internal.ui.editor.*;

/**
 * Der Dialog fuer Auswahl eines Types in der Outline.
 * Erlaubt _nicht_ die Auswahl aller Typen.
 * @author Michael Bohn
 *
 */
public class VisibleTypeSelectionDialog 
extends ViewSettingsDialog 
{
	private CheckboxTableViewer fListViewer;
	private TSExpression[] fSelection;
	
	private final Set<TSExpression> fDefaults;
	private final Set<TSExpression> fPrevious;
	
	/**
	 * Erzeugt einen neuen Dialog fuer Auswahl der sichtbaren Typen der Outline.
	 * @param parentShell - Shell an der Dialog modal geoffnet wird
	 * @param defaults - Die Default-Auswahl
	 * @param previous - Die aktuelle Auswahl
	 */
	public VisibleTypeSelectionDialog(final Shell parentShell,
				final Set<TSExpression> defaults,
				final Set<TSExpression> previous) 
	{
		super(parentShell);
		
		this.fDefaults = defaults;
		this.fPrevious = previous;
		
		setBlockOnOpen(true);
	}

	@Override
	protected Control createDialogArea(Composite parent) 
	{
		Composite composite = (Composite)super.createDialogArea(parent);
		
		Label label = new Label(composite, SWT.LEFT);
		label.setText("Select the types to show in the view:");
		
		fListViewer = CheckboxTableViewer.newCheckList(composite, SWT.NONE);
		fListViewer.setContentProvider(SExpressionTypeContentProvider.instanceOf());
		fListViewer.setLabelProvider(SExpressionTypeLabelProvider.instanceOf());
        GridData data = new GridData(GridData.FILL_BOTH);
        fListViewer.getTable().setLayoutData(data);
        fListViewer.setInput(new Object());//workaround: sonst wird nichts angezeigt, obwohl
        								   //contentProvider Daten von sich aus bereitstellt
        setSelection(fPrevious);
		return composite;
	}

	@Override
	protected void okPressed() 
	{	
		Object[] selected = fListViewer.getCheckedElements();
		fSelection = new TSExpression[selected.length];
		for(int i=0;i<selected.length;i++) {
			fSelection[i] = (TSExpression)selected[i];
		}
		
		super.okPressed();
	}
	
	@Override
	protected void cancelPressed() 
	{
		fSelection = null;
		super.cancelPressed();
	}

	@Override
	protected void performDefaults() 
	{
		setSelection(fDefaults);
	}

	private void setSelection(Set<TSExpression> types)
	{
		fListViewer.setCheckedElements(types.toArray());
	}
	
	/**
	 * Liefert die vom Benutzer getaetigte Auswahl.
	 * Gibt <code>null</code> zurueck, wenn der Benutzer abbrechen
	 * gedrueckt hat.
	 * @return
	 */
	public Set<TSExpression> getSelection()
	{
		if( fSelection == null) {
			return null;
		}
		
		if( fSelection.length == 0) {
			return EnumSet.noneOf(TSExpression.class);
		}
		return EnumSet.copyOf(Arrays.asList(fSelection));
	}
}