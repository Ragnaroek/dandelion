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

package de.defmacro.dandelion.internal.ui.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.*;
import org.eclipse.ui.forms.HyperlinkGroup;
import org.eclipse.ui.forms.events.*;
import org.eclipse.ui.forms.widgets.Hyperlink;

import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.preferences.LispServerPreferencePage;

public class LispProjectWizardPage 
extends WizardNewProjectCreationPage 
implements SelectionListener
{
	private Button fDefaultRadio;
	private Button fChooseRadio;
	private Combo fCombo;
	private EnvironmentManager fEvalServerManager;
	private IEnvironmentManagementListener fManagmentListener;

	public LispProjectWizardPage(final String pageName) {
		super(pageName);
		fEvalServerManager = LispCore.getEnvironmentManager();
	}

	@Override
	public void createControl(final Composite parent) {
		super.createControl(parent);
		Composite composite = (Composite)getControl();
		
		Group group = new Group(composite, SWT.NONE);
		group.setText("Lisp Environment");
		group.setLayoutData(new GridData(SWT.FILL, SWT.BEGINNING, true, false));
		group.setLayout(new FormLayout());
		
		fDefaultRadio = new Button(group, SWT.RADIO);
		fDefaultRadio.setSelection(true);
		fDefaultRadio.addSelectionListener(this);
		{
			FormData data = new FormData();
			data.width = 300;
			data.top = new FormAttachment(0, 5);
			data.left = new FormAttachment(0, 10);
			fDefaultRadio.setLayoutData(data);
		}

		HyperlinkGroup hyperlinkGroup = new HyperlinkGroup(parent.getDisplay());
		Hyperlink link = new Hyperlink(group, SWT.NONE);
		hyperlinkGroup.add(link);
		link.setText("Configure...");
		{
			FormData data = new FormData();
			data.right = new FormAttachment(100, -35);
			link.setLayoutData(data);
		}
		link.addHyperlinkListener(new HyperlinkAdapter() {
			@Override
			public void linkActivated(HyperlinkEvent e) {
				PreferencesUtil.createPreferenceDialogOn(getShell(), LispServerPreferencePage.ID, null, null).open();
			}
		});

		fChooseRadio = new Button(group, SWT.RADIO);
		fChooseRadio.setText("Choose Environment:");
		fChooseRadio.addSelectionListener(this);
		{
			FormData data = new FormData();
			data.left = new FormAttachment(0, 10);
			data.top  = new FormAttachment(fDefaultRadio, 7);
			fChooseRadio.setLayoutData(data);
		}
		
		fCombo = new Combo(group, SWT.READ_ONLY);
		{
			FormData data = new FormData();
			data.left = new FormAttachment(fChooseRadio, 10);
			data.top = new FormAttachment(fDefaultRadio, 7);
			fCombo.setLayoutData(data);
		}
		
		fManagmentListener = new EnvironmentManagementAdapter() {
			@Override
			public void serverAdded(IEnvironment server) {
				updateEvalServer();
			}

			@Override
			public void serverRemoved(IEnvironment server) {
				updateEvalServer();
			}
			
			@Override
			public void defaultChanged(IEnvironment newDefault) {
				updateEvalServer();
			}
		};
		
		fEvalServerManager.addEvalServerManagementListener(fManagmentListener);
		
		setControl(composite);
		updateEvalServer();
		updateLogic();
	}
	
	@Override
	public boolean isPageComplete() {
		return super.isPageComplete() && fEvalServerManager.hasEnvironment();
	}

	public IEnvironment getSelectedEvalServer()
	{
		if( !fEvalServerManager.hasEnvironment() ) {
			return null;
		}
		
		IEnvironment selection = null;
		if( fChooseRadio.getSelection() ) {
			String text = fCombo.getText();
			for(IEnvironment server : fEvalServerManager.getEnvironments()) {
				if(server.toString().equals(text)) {
					selection = server;
				}
			}
		} else {
			selection = fEvalServerManager.getDefaultEnvironment();
		}
		
		return selection;
	}
	
	protected void updateEvalServer()
	{
		fCombo.removeAll();
		for(IEnvironment server : fEvalServerManager.getEnvironments(true)) {
			fCombo.add(server.toString());
		}	
		if(fCombo.getItemCount() > 0) {
			fCombo.select(0);
		}
		
		String text = "Use default: ";
		if( !fEvalServerManager.hasEnvironment() ) {
			text += "none";
		} else {
			text += fEvalServerManager.getDefaultEnvironment().toString();
		}
		fDefaultRadio.setText(text);
		updateLogic();
	}
	
	private void updateLogic()
	{
		if( !fEvalServerManager.hasEnvironment() ) { //kein einziger Server angeben
			fChooseRadio.setEnabled(false);  //keine Auwahl moeglich, muss erst konfiguriert werden
			fDefaultRadio.setEnabled(false);
			fCombo.setEnabled(false);
			setErrorMessage("No environments are configured, please configure a environment in the preferences");
		} else {
			fChooseRadio.setEnabled(true);
			fDefaultRadio.setEnabled(true);
			fCombo.setEnabled(fChooseRadio.getSelection());
			setErrorMessage(null);
		}
	}

	@Override
	public void dispose() {
		super.dispose();
		if(fEvalServerManager != null &&  fManagmentListener != null) {
			fEvalServerManager.removeEvalServerManagementListener(fManagmentListener);
		}
	}

	public void widgetDefaultSelected(SelectionEvent e) {
		//no-op
	}

	public void widgetSelected(SelectionEvent e) {
		updateLogic();
	}
}
