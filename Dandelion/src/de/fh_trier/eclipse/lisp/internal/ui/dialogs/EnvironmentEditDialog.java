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

package de.fh_trier.eclipse.lisp.internal.ui.dialogs;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.core.connection.IEnvironmentConfiguration.TLogSeverity;

/**
 * Der Dialog fuer das Anlegen eines neuen
 * Eval-Servers.
 * @author Michael Bohn
 *
 */
public class EnvironmentEditDialog 
extends StatusDialog 
{
	private static final int CONFIG_VINDENT = 7;
	private static final int CONFIG_INDENT = 10;
	private static final int CONFIG_INDENT_LVL2 = CONFIG_INDENT + 15;
	
	private Text fName;
	private Text fVersion;
	private Text fHost;
	private Text fPort;
	protected Text fExecFile;
	private Text fCommands;
	private Text fExeCommands;
	private Button fChooseExecFile;
	private Button fCheckExtern;
	private Button fCheckExecutable;
	private Button fCheckNonExecutable;
	private Button fCheckLogging;
	private Combo  fComboLogTypes;
	
	private String fStoredHost; //zwischengespeichert wenn von Localhost evtl. wieder zurueckgewechselt wird
	private final List<IEnvironment> fServers;
	private IEnvironment fEvalServer; //die aus diesem Dialog erstellten Objekte, evtl. null
	private IEnvironmentConfiguration fConfig;
	
	/**
	 * Erzeugt einen neuen Dialog fuer das Anlegen von Eval-Servern.
	 * @param parent - Die Shell an der Dialog modal geoeffent wird
	 * @param serverList - Liste der bereits vorhandenen Server, fuer Ueberpruefung doppeltes anlegen
	 */
	public EnvironmentEditDialog(final Shell parent, final java.util.List<IEnvironment> serverList) {
		super(parent);
		
		if (serverList == null) {
			throw new NullPointerException("serverList must not be null");
		}
		
		setShellStyle(getShellStyle() | SWT.RESIZE);
		this.fServers = serverList;
	}
	
	@Override
	protected Control createDialogArea(final Composite ancestor) 
	{
		Composite parent = (Composite)super.createDialogArea(ancestor);
		GridLayout layout = (GridLayout)parent.getLayout();
		layout.numColumns = 4;
		layout.verticalSpacing = 4;
		
		Label lblName = new Label(parent, SWT.NONE);
		lblName.setText("Name");
		lblName.setLayoutData(gridData(GridData.BEGINNING, 1));
		fName = makeText(parent);
		fName.setLayoutData(gridData(GridData.FILL_HORIZONTAL, 3));
		
		Label lblVersion = new Label(parent, SWT.NONE);
		lblVersion.setText("Version");
		lblVersion.setLayoutData(gridData(GridData.BEGINNING, 1));
		fVersion = makeText(parent);
		fVersion.setLayoutData(gridData(GridData.FILL_HORIZONTAL, 3));
		
		Label lblHost = new Label(parent, SWT.NONE);
		lblHost.setText("Host");
		lblHost.setLayoutData(gridData(GridData.BEGINNING, 1));
		fHost = makeText(parent);
		fHost.setLayoutData(gridData(GridData.FILL_HORIZONTAL, 3));
		
		Label lblPort = new Label(parent, SWT.NONE);
		lblPort.setText("Port");
		lblPort.setLayoutData(gridData(GridData.BEGINNING, 1));
		fPort = makeText(parent);
		fPort.setLayoutData(gridData(GridData.FILL_HORIZONTAL, 3));
		
		//config absetzen: law of closure
		Label lblConfig = new Label(parent, SWT.NONE);
		lblConfig.setText("Configuration:");
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.verticalIndent = 15;
			lblConfig.setLayoutData(data);
		}
		
		fCheckExtern = makeCheckableButton(parent, "Extern", SWT.RADIO);
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.horizontalIndent = CONFIG_INDENT;
			data.verticalIndent = CONFIG_VINDENT;
			fCheckExtern.setLayoutData(data);
		}
		
		fCheckExecutable = makeCheckableButton(parent, "Executable", SWT.RADIO);
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.horizontalIndent = CONFIG_INDENT;
			data.verticalIndent = CONFIG_VINDENT;
			fCheckExecutable.setLayoutData(data);
		}
		
		fExecFile = makeText(parent);
		{
			GridData data = gridData(GridData.FILL_HORIZONTAL, 3);
			data.horizontalIndent = CONFIG_INDENT+15;
			fExecFile.setLayoutData(data);
		}
		
		fChooseExecFile = new Button(parent, SWT.PUSH);
		fChooseExecFile.setLayoutData(gridData(SWT.BEGINNING, 1));
		fChooseExecFile.setText("Choose...");
		fChooseExecFile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				FileDialog dialog = new FileDialog(getShell(), SWT.OPEN);
				String selectedFile = dialog.open();
				if(selectedFile != null) {
					fExecFile.setText(selectedFile);
				}
			}
		});
		
		Label lblExeCommands = new Label(parent, SWT.NONE);
		lblExeCommands.setText("Parameters:");
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.horizontalIndent = CONFIG_INDENT_LVL2;
			lblExeCommands.setLayoutData(data);
		}
		
		fExeCommands = makeText(parent);
		{
			GridData data = gridData(GridData.FILL_HORIZONTAL, 4);
			data.horizontalIndent = CONFIG_INDENT_LVL2;
			fExeCommands.setLayoutData(data);
		}
		
		fCheckNonExecutable = makeCheckableButton(parent, "Non executable", SWT.RADIO);
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.horizontalIndent = CONFIG_INDENT;
			data.verticalIndent = CONFIG_VINDENT;
			fCheckNonExecutable.setLayoutData(data);
		}
		
		Label lblCommands = new Label(parent, SWT.NONE);
		lblCommands.setText("Commands:");
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.horizontalIndent = CONFIG_INDENT_LVL2;
			lblCommands.setLayoutData(data);
		}
		
		fCommands = makeText(parent);
		{
			GridData data = gridData(GridData.FILL_HORIZONTAL, 4);
			data.horizontalIndent = CONFIG_INDENT_LVL2;
			fCommands.setLayoutData(data);
		}
		
		fCheckLogging = makeCheckableButton(parent, "Logging", SWT.CHECK);
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.verticalIndent = 20;
			data.horizontalIndent = CONFIG_INDENT;
			fCheckLogging.setLayoutData(data);
		}
		
		fComboLogTypes = new Combo(parent, SWT.READ_ONLY);
		{
			GridData data = gridData(GridData.BEGINNING, 4);
			data.horizontalIndent = CONFIG_INDENT_LVL2;
			fComboLogTypes.setLayoutData(data);
		}
		fComboLogTypes.setText("Severity");
		addLogSeverities(fComboLogTypes);
		
		fCheckExtern.setSelection(true); //default selection
		updateEnablement();
		
		updateStatusLine();
		return parent;
	}

	private void addLogSeverities(final Combo combo)
	{
		for(TLogSeverity severity : TLogSeverity.values()) {
			if(severity != TLogSeverity.DEFAULT_SEVERITY) {
				combo.add(severity.name());
			}
		}
		combo.add(TLogSeverity.DEFAULT_SEVERITY.name(), 0);
		combo.select(0);
	}
	
	private TLogSeverity getLogSeverity(final Combo combo)
	{
		return TLogSeverity.valueOf(combo.getItem(combo.getSelectionIndex()));
	}
	
	@Override
	protected Point getInitialSize() {
		return new Point(400, 550);
	}

	private GridData gridData(final int style, final int hSpan)
	{
		GridData data = new GridData(style);
		data.horizontalSpan = hSpan;
		return data;
	}
	
	private Text makeText(final Composite parent) {
		Text text = new Text(parent, SWT.BORDER);
		text.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				updateStatusLine();
			}
		});
		return text;
	}
	
	private Button makeCheckableButton(final Composite parent, final String text, final int style)
	{
		Button check = new Button(parent, style);
		check.setText(text);
		check.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				updateStatusLine();
				updateEnablement();
			}
		});
		return check;
	}
	
	protected void updateEnablement() 
	{
		if(fCheckExecutable.getSelection()) {
			fExecFile.setEnabled(true);
			fChooseExecFile.setEnabled(true);
			fCommands.setEnabled(false);
			fExeCommands.setEnabled(true);
			enableLogging(true);
			forceLocalhost();
		} else if(fCheckExtern.getSelection()) {
			fExecFile.setEnabled(false);
			fChooseExecFile.setEnabled(false);
			fCommands.setEnabled(false);
			fExeCommands.setEnabled(false);
			enableLogging(false);
			restoreFromLocalhost();
		} else if(fCheckNonExecutable.getSelection()) {
			fExecFile.setEnabled(false);
			fChooseExecFile.setEnabled(false);
			fCommands.setEnabled(true);
			fExeCommands.setEnabled(false);
			enableLogging(true);
			forceLocalhost();
		}
		
		if(fCheckLogging.getSelection() && !fCheckExtern.getSelection()) {
			fComboLogTypes.setEnabled(true);
		} else {
			fComboLogTypes.setEnabled(false);
		}
	}
	
	private void enableLogging(final boolean enabled)
	{
		fCheckLogging.setEnabled(enabled);
		fComboLogTypes.setEnabled(enabled);
	}

	protected void updateStatusLine()
	{
		if(emptyText(fName)) {
			updateStatus(makeStatus("Enter a name for the environment"));
			return;
		}
		
		if(emptyText(fVersion)) {
			updateStatus(makeStatus("Enter the version of the environment"));
			return;
		}
		
		if(!checkNameVersionUnique()) {
			updateStatus(makeStatus("The environment name and version must be unique"));
			return;
		}
		
		if(emptyText(fHost)) {
			updateStatus(makeStatus("Enter the hostname of the environment"));
			return;
		} 
		
		if(emptyText(fPort)) {
			updateStatus(makeStatus("Enter the port number of the environment"));
			return;
		} //else
			try {
				int port = Integer.parseInt(fPort.getText());
				if(port < 0 || port > 65535) {
					updateStatus(makeStatus("Invalid port number"));
					return;
				}
			} catch (NumberFormatException e) {
				updateStatus(makeStatus("Illegal port"));
				return;
			}
		//}
		
		if(fCheckExecutable.getSelection()) {
			if(emptyText(fExecFile)) {
				updateStatus(makeStatus("Select the executable"));
				return;
			} //else {
				File file = new File(fExecFile.getText());
				if( !file.exists() ) {
					updateStatus(makeStatus("Executable file does not exist"));
					return;
				}
			//}
		}
		
		if(fCheckNonExecutable.getSelection()) {
			if(emptyText(fCommands)) {
				updateStatus(makeStatus("Enter commands to start the environment"));
				return;
			}
		}
		
		//invariante: alle eingaben ok
		
		updateStatus(Status.OK_STATUS);
	}
	
	private boolean checkNameVersionUnique()
	{
		return EnvironmentManager.isNameAndVersionUnique(fName.getText(), fVersion.getText(), fServers);
	}
	
	private boolean emptyText(final Text text)
	{
		return text.getText().trim().equals("");
	}
	
	private IStatus makeStatus(final String message)
	{
		return new Status(IStatus.ERROR, LispPluginActivator.PLUGIN_ID, 0, message, null);
	}
	
	private void forceLocalhost()
	{
		if(!fHost.getText().equalsIgnoreCase("localhost")) {
			fStoredHost = fHost.getText();
			fHost.setText("localhost");
			fHost.setEnabled(false);
		}
	}
	
	private void restoreFromLocalhost()
	{
		if(fStoredHost != null) {
			fHost.setText(fStoredHost);
			fStoredHost = null;
			fHost.setEnabled(true);
		}
	}
	
	@Override
	protected void cancelPressed() {
		fEvalServer = null;
		fConfig = null;
		super.cancelPressed();
	}

	@Override
	protected void okPressed() {
		if( getStatus().getSeverity() == IStatus.OK ) {
			fEvalServer = makeEvalServer();
			fConfig = makeConfiguration();
		} else {
			fEvalServer = null;
			fConfig = null;
		}
		super.okPressed();
	}

	private IEnvironment makeEvalServer()
	{
		try {
			return new Environment(fHost.getText().trim(), Integer.parseInt(fPort.getText().trim()), fName.getText().trim(), fVersion.getText().trim());
		} catch (NumberFormatException e) {
			return null;
		}
	}
	
	private IEnvironmentConfiguration makeConfiguration()
	{
		if(fCheckExtern.getSelection()) {
			return EnvironmentConfiguration.instanceOfExtern();
		}

		boolean logging = fCheckLogging.getSelection();
		TLogSeverity logSeverity = null;
		if(logging) {
			logSeverity = getLogSeverity(fComboLogTypes);
		}

		if(fCheckExecutable.getSelection()) {
			return EnvironmentConfiguration.instanceOfExecutable(logging, logSeverity, new File(fExecFile.getText()), splitByBlanks(fExeCommands.getText()));
		}
		//nicht executable
		return EnvironmentConfiguration.instanceofNonExecutable(logging, logSeverity, splitByBlanks(fCommands.getText()));
	}
	
	private List<String> splitByBlanks(final String text)
	{
		return Arrays.asList(text.split("\\s+"));
	}
	
	/**
	 * Liefert den angelegten Server.
	 * Gibt <code>null</code> zurueck, wenn cancel gedrueckt wurde.
	 * @return Server oder <code>null</code>
	 */
	public IEnvironment getEvalServer()
	{	
		return fEvalServer;
	}
	
	/**
	 * Liefert die angelegte Konfiguration fuer den Server.
	 * Gibt <code>null</code> zurueck, wenn cancel gedrueckt wurde.
	 * @return Konfiguration oder <code>null</code>
	 */
	public IEnvironmentConfiguration getEvalServerConfiguration()
	{
		return fConfig;
	}
}
