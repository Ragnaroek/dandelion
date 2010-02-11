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

package de.defmacro.dandelion.internal.preferences;

import java.util.*;
import java.util.List;
import java.util.Map.Entry;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.ui.LispUI;
import de.defmacro.dandelion.internal.ui.dialogs.EnvironmentEditDialog;

/**
 * Die GUI-Komponente zur Anzeige der Eval-Server
 * in den Preferences.
 * @author Michael Bohn
 *
 */
public class LispServerPreferencePage
extends PreferencePage
implements IWorkbenchPreferencePage 
{
	/**
	 * Eindeutige ID der PreferencePage aus Manifest.
	 */
	public static final String ID = "de.fh_trier.eclipse.lisp.preferences.LispServerPreferences";
	
	private final EnvironmentManager fManager = LispCore.getEnvironmentManager();
	private TableViewer  fTable;
	private Button fButtonAdd;
	private Button fButtonRemove;
	private Button fButtonDefault;
	private LispEvalServerLabelProvider fLabelProvider;
	
	private final Map<IEnvironment, IEnvironmentConfiguration> fAdded; //Map aller Server die neu hinzugefugt wurden
	private final List<IEnvironment> fRemoved; //Liste aller Server die aus dem Manager geloescht wurde
	private IEnvironment fNewDefault; //Der neue default server, null wenn nicht neu gesetzt
	
	/**
	 * Erstellt eine neue PreferencePage fuer die Anzeige 
	 * der Eval-Server.
	 */
	public LispServerPreferencePage() {
		setDescription("Installed Environments:");
		fAdded = new Hashtable<IEnvironment, IEnvironmentConfiguration>();
		fRemoved = new ArrayList<IEnvironment>();
		fNewDefault = null;
	}
	
	@Override
	protected Control createContents(final Composite ancestor) 
	{
		noDefaultAndApplyButton();
		{
			GridLayout layout = new GridLayout(1, false);
			layout.marginHeight = 0;
			layout.marginWidth = 0;
			ancestor.setLayout(layout);
		}
		
		Composite parent = new Composite(ancestor, SWT.NONE);
		parent.setLayoutData(new GridData(GridData.FILL_BOTH));
		{
			GridLayout layout = new GridLayout(2, false);
			layout.marginHeight = 0;
			layout.marginWidth  = 0;
			parent.setLayout(layout);
		}
		
		fTable = new TableViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		{
			GridData data = new GridData(GridData.FILL_BOTH);
			fTable.getTable().setLayoutData(data);
		}
		fTable.getTable().setHeaderVisible(true);
		fTable.getTable().setLinesVisible(true);
		fTable.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(final SelectionChangedEvent event) {
				updateEnablement();
			}
		});
		addColumns(fTable.getTable());
		fLabelProvider = new LispEvalServerLabelProvider(fManager.getDefaultEnvironment());
		setTableProvider(fTable, fLabelProvider);
		
		Composite buttons = new Composite(parent, SWT.NONE);
		{
			GridLayout layout = new GridLayout(1, false);
			layout.marginHeight = 0;
			layout.marginWidth = 5;
			buttons.setLayout(layout);
		}
		buttons.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));

		fButtonAdd = new Button(buttons, SWT.PUSH);
		fButtonAdd.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fButtonAdd.setText("Add");
		fButtonAdd.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				addServer();
			}
		});
		
		fButtonRemove = new Button(buttons, SWT.PUSH);
		fButtonRemove.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		fButtonRemove.setText("Remove");
		fButtonRemove.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				removeServer();
			}
		});
		
		fButtonDefault = new Button(buttons, SWT.PUSH);
		{
			GridData data = new GridData(GridData.FILL_HORIZONTAL);
			data.verticalIndent = 25;
			fButtonDefault.setLayoutData(data);
		}
		fButtonDefault.setText("Default");
		fButtonDefault.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setDefault();
			}
		});
	
		return parent;
	}
	
	private void setTableProvider(final TableViewer viewer, LispEvalServerLabelProvider labelProvider)
	{
		viewer.setContentProvider(new IStructuredContentProvider() {
			public Object[] getElements(final Object inputElement) {
				return getCurrentServerList().toArray();
			}

			public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
				//no-op
			}
			
			public void dispose() {
				//no-op
			}
		});
		viewer.setInput(new Object());
		viewer.setLabelProvider(labelProvider);
	}
	
	private void addColumns(final Table table)
	{
		TableColumn name = makeColumn(table, "Name");
		name.setWidth(175);
		
		TableColumn host = makeColumn(table, "Host");
		host.setWidth(125);
		
		TableColumn port = makeColumn(table, "Port");
		port.setWidth(75);
		
		TableColumn version = makeColumn(table, "Version");
		version.setWidth(100);
	}
	
	private TableColumn makeColumn(final Table parent, final String name)
	{
		TableColumn column = new TableColumn(parent, SWT.LEFT);
		column.setText(name);
		return column;
	}

	/**
	 * Unbenutzt.
	 */
	public void init(final IWorkbench workbench) {
		//no-op
	}
	
	private IEnvironment getCurrentDefault()
	{
		if(fNewDefault == null) {
			return fManager.getDefaultEnvironment();
		}
		return fNewDefault;
	}
	
	/**
	 * Gibt die aktuelle IEvalServer-Liste zurueck.
	 * Die Liste ist != der Liste im EvalServerManager, da die akutell hinzugefuegten
	 * Server hingzugenommen werden die noch nicht im Manager gespeichert wurden.
	 * Die Liste der Server ist sortiert.
	 * @return
	 */
	protected List<IEnvironment> getCurrentServerList()
	{
		//zurueckgegebene Liste darf veraendert werden
		List<IEnvironment> serverInManager = fManager.getEnvironments();
		serverInManager.addAll(fAdded.keySet());
		serverInManager.removeAll(fRemoved);
		Collections.sort(serverInManager);
		return serverInManager;
	}
	
	protected void addServer()
	{
		EnvironmentEditDialog dialog = new EnvironmentEditDialog(getShell(), getCurrentServerList());
		dialog.setTitle("Add Environment");
		dialog.open();
		IEnvironment server = dialog.getEvalServer();
		IEnvironmentConfiguration config = dialog.getEvalServerConfiguration();
		
		if(server != null && config != null) {
			fAdded.put(server, config);
		}
		
		fTable.refresh();
	}
	
	protected void removeServer()
	{
		IEnvironment selected = getSelectedServer();
		if(selected != null) {
			if(LispCore.getProjectManager().isUsedInProject(selected)) {
				MessageDialog.openInformation(getShell(),"Removing server" , "Cannot remove this environment, the environment is used in a project");
			} else if(selected.equals(getCurrentDefault())) {
				MessageDialog.openInformation(getShell(), "Removing server", "Cannot remove default environment");
			} else {
				if(fAdded.containsKey(selected)) { //Server wurde hinzugefuegt und sofort wieder geloescht
					fAdded.remove(selected); //aus add liste entfernen
				} else {
					fRemoved.add(selected); //ein server aus Manager -> merken fuer loeschung
				}
			}
		}
		fTable.refresh();
	}
	
	protected void setDefault()
	{
		IEnvironment newDefault = getSelectedServer();
		if(newDefault == null || newDefault.equals(getCurrentDefault())) {
			return;
		}
		fNewDefault = newDefault;
		fLabelProvider.setNewDefault(newDefault);
		fTable.refresh();
	}
	
	/**
	 * @see PreferencePage#performOk()
	 */
	@Override
	public boolean performOk() 
	{	
		for(Entry<IEnvironment, IEnvironmentConfiguration> entry : fAdded.entrySet()) {
			fManager.addEnvironment(entry.getKey(), entry.getValue());
		}
		
		for(IEnvironment removed : fRemoved) {
			try {
				fManager.removeEnvironment(removed);
			} catch (ManagementException e) {
				LispPluginActivator.logBrokenInvariant("Error trying to remove a used environment", e);
			} catch (ConnectionException e) {
				LispUI.showErrorDialog(getShell(), "Error disconnecting environment", e);
			}
		}
		
		if(fNewDefault != null) {
			fManager.setDefaultEnvironment(fNewDefault);
			LispPreferences.storeDefaultServer(fNewDefault.hashCode());
		}
		
		//invariante: Manager enthaelt korrekten Zustand
		
		Map<IEnvironment, IEnvironmentConfiguration> serversToStore = new Hashtable<IEnvironment, IEnvironmentConfiguration>();
		for(IEnvironment server : fManager.getEnvironments()) {
			if(!fManager.isContributedViaPlugin(server)) {
				try {
					serversToStore.put(server, fManager.getConfigurationFor(server));
				} catch (ManagementException e) {
					//ignore
				}
			}
		}
		LispPreferences.storeEvalServer(serversToStore);
	
		return super.performOk();
	}

	protected void updateEnablement()
	{
		IEnvironment selected = getSelectedServer();
		if(selected != null) {
			fButtonRemove.setEnabled(!fManager.isContributedViaPlugin(selected));
		}
	}
	
	private IEnvironment getSelectedServer()
	{
		IStructuredSelection selection = (IStructuredSelection)fTable.getSelection();
		if(selection.isEmpty()) {
			return null;
		}
		return (IEnvironment)selection.getFirstElement();
	}
}