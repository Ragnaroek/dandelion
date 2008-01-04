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

package de.fh_trier.eclipse.lisp.internal.ui.views.apropos;

import java.util.*;
import java.util.regex.Pattern;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.part.ViewPart;

import de.fh_trier.eclipse.lisp.internal.core.LispCore;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.core.meta.*;
import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.views.apropos.AproposQueryResultSorter.TSortBy;

public class AproposView 
extends ViewPart
implements SelectionListener, IEnvironmentManagementListener
{
	public static final String ID = "de.fh_trier.eclipse.lisp.views.aproposView";
	
	private static final String ALL_PACKAGES = "All Packages";
	private static final String TEXT_MODE_AND = "and";
	private static final String TEXT_MODE_OR = "or";
	private static final String TEXT_MODE_NOT = "not";
	private static final SortedSet<IMetaSymbol> EMPTY_INPUT = Collections.unmodifiableSortedSet(new TreeSet<IMetaSymbol>());
	
	private static final int COMBO_WIDTH_HINT = 200;
	private static final int CONNECT_STATE_WIDTH_HINT = 20;
	private static final Pattern SPLIT_PATTERN = Pattern.compile("\\s+");

	private static class ColumnSortListener
	extends SelectionAdapter
	{
		private TableViewer fTableViewer;
		private TSortBy     fSortBy;
		
		public ColumnSortListener(final TableViewer tableViewer, final TSortBy sortBy)
		{
			this.fTableViewer = tableViewer;
			this.fSortBy = sortBy;
		}
		
		@Override
		public void widgetSelected(final SelectionEvent e) {
			fTableViewer.setSorter(new AproposQueryResultSorter(fSortBy));
		}
	}
	
	private TableViewer fViewer;
	
	private EnvironmentManager fEvalServerManager;
	private Combo fComboPackages;
	private Combo fComboServer;
	private Combo fComboMode;
	private Label fConnectionState;
	private Label fLabelSymbolInPackages;
	private Text fQueryText;
	
	public AproposView()
	{
		this.fEvalServerManager = LispCore.getEvalServerManager();
	}

	@Override
	public void createPartControl(final Composite parent) {
		
		GridLayout parentLayout = new GridLayout(2, false);
		parentLayout.verticalSpacing = 0;
		parent.setLayout(parentLayout);
		
		fComboMode = new Combo(parent, SWT.READ_ONLY);
		fComboMode.add(TEXT_MODE_AND);
		fComboMode.add(TEXT_MODE_OR);
		fComboMode.add(TEXT_MODE_NOT);
		fComboMode.select(0);
		
		Composite input = new Composite(parent, SWT.NONE);
		input.setLayout(getElementLayout());
		input.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		fQueryText = new Text(input, SWT.BORDER);
		fQueryText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Button button = new Button(input, SWT.PUSH);
		button.setText("Search");
		button.addSelectionListener(this);
		getSite().getShell().setDefaultButton(button); //als Default-Button registrieren
		
		Label lblConnection = new Label(parent, SWT.NONE);
		lblConnection.setText("Environment:");
		
		Composite serverAndState = new Composite(parent, SWT.NONE);
		serverAndState.setLayout(getElementLayout());		
		
		fComboServer = new Combo(serverAndState, SWT.READ_ONLY);
		fComboServer.addSelectionListener(this);
		{
			GridData data = new GridData();
			data.widthHint = COMBO_WIDTH_HINT;
			fComboServer.setLayoutData(data);
		}
		
		fConnectionState = new Label(serverAndState, SWT.NONE);
		{
			GridData data = new GridData();
			data.widthHint = CONNECT_STATE_WIDTH_HINT;
			fConnectionState.setLayoutData(data);
		}
		
		Label lblPackage = new Label(parent, SWT.NONE);
		lblPackage.setText("Package:");
		
		Composite packageAndInfo = new Composite(parent, SWT.NONE);
		packageAndInfo.setLayout(getElementLayout());
		{
			GridData data = new GridData(GridData.FILL_HORIZONTAL);
			packageAndInfo.setLayoutData(data);
		}
		
		fComboPackages = new Combo(packageAndInfo, SWT.READ_ONLY);
		{
			GridData data = new GridData();
			data.widthHint = COMBO_WIDTH_HINT;
			fComboPackages.setLayoutData(data);
		}
		fComboPackages.addSelectionListener(this);
		fLabelSymbolInPackages = new Label(packageAndInfo, SWT.NONE);
		{
			GridData data = new GridData(GridData.FILL_HORIZONTAL);
			data.grabExcessHorizontalSpace = true;
			fLabelSymbolInPackages.setLayoutData(data);
		}
		
		doUpdateServerList();
		/*
		if( fEvalServerManager.hasEvalServer() ) {
			fillKnownEvalServer(fComboServer);
			IEvalServer defaultServer = fEvalServerManager.getDefaultEvalServer();
			updatePackageCombo(defaultServer);
			updateSymbolCount(defaultServer);
		}*/
		
		//TableViewer
		
		fViewer = new TableViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		{
			GridData data = new GridData(GridData.FILL_BOTH);
			data.horizontalSpan = 2;
			fViewer.getControl().setLayoutData(data);
		}
		
		fViewer.setContentProvider(new AproposQueryResultContentProvider());
		fViewer.setLabelProvider(new AproposQueryResultLabelProvider());
		fViewer.setSorter(new AproposQueryResultSorter(AproposQueryResultSorter.TSortBy.DEFAULT));
		fViewer.setInput(getViewSite());
		fViewer.getTable().setHeaderVisible(true);
		fViewer.getTable().setLinesVisible(true);
		
		addColumns(fViewer);
		fEvalServerManager.addEvalServerManagementListener(this);
	}

	private void doUpdateServerList() {
		fComboServer.removeAll();
		if( fEvalServerManager.hasEvalServer() ) {
			fillKnownEvalServer(fComboServer);
			IEnvironment defaultServer = fEvalServerManager.getDefaultEvalServer();
			updatePackageCombo(defaultServer);
			updateSymbolCount(defaultServer);
			updateConnectStatus(defaultServer);
		}
	}
	
	private void addColumns(final TableViewer viewer)
	{
		TableColumn column1 = makeColumn(viewer, SWT.CENTER, TSortBy.TYP);
		column1.setWidth(22);
		
		TableColumn column5 = makeColumn(viewer, SWT.LEFT, TSortBy.PACKAGE);
		column5.setText("Package");
		column5.setWidth(200);
		
		TableColumn column2 = makeColumn(viewer, SWT.LEFT, TSortBy.SYMBOL_NAME);
		column2.setText("Symbol");
		column2.setWidth(200);
		
		TableColumn column3 = makeColumn(viewer, SWT.LEFT, TSortBy.TYP);
		column3.setText("Type");
		column3.setWidth(75);
		
		TableColumn column4 = makeColumn(viewer, SWT.LEFT, TSortBy.ARGUMENTS);
		column4.setText("Arguments");
		column4.setWidth(250);
		
		TableColumn column6 = makeColumn(viewer, SWT.LEFT, TSortBy.DOCUMENTATION);
		column6.setText("Documentation");
		column6.setWidth(250);
	}
	
	private TableColumn makeColumn(final TableViewer viewer, final int style, final TSortBy sortBy)
	{
		TableColumn column = new TableColumn(viewer.getTable(), style);
		column.addSelectionListener(new ColumnSortListener(viewer, sortBy));
		return column;
	}
	
	private GridLayout getElementLayout()
	{
		GridLayout layout = new GridLayout(2, false);
		layout.verticalSpacing = 0;
		layout.marginHeight = 3;
		return layout;
	}
	
	private void fillKnownEvalServer(final Combo combo) 
	{
		if(fEvalServerManager.hasEvalServer()) {
			for(IEnvironment server : fEvalServerManager.getEvalServer(true)) {
				combo.add(server.toString());
			}

			IEnvironment defaultServer = fEvalServerManager.getDefaultEvalServer();
			combo.setText(defaultServer.toString());
			updateConnectStatus(defaultServer);
		}
	}
	
	private void updateConnectStatus(final IEnvironment server)
	{
		if(server != null) {
			if(fEvalServerManager.isConnected(server)) {
				fConnectionState.setImage(LispUI.getUIImageManager().get(UIImageConstants.ICON_CONNECT));
			} else {
				fConnectionState.setImage(LispUI.getUIImageManager().get(UIImageConstants.ICON_DISCONNECT));
			}
		}
	}
	
	private void initPackageCombo()
	{
		fComboPackages.removeAll();
		fComboPackages.add(ALL_PACKAGES);
	}
	
	private void updatePackageCombo(final IEnvironment server)
	{
		ISymbolStore store = fEvalServerManager.getSymbolStoreFor(server);
		initPackageCombo();
		for(String pack : store.getPackages()) {
			fComboPackages.add(pack);
		}
		
		fComboPackages.select(0);
	}
	
	private void updateSymbolCount(final IEnvironment server)
	{
		ISymbolStore store = fEvalServerManager.getSymbolStoreFor(server);
		String pack = fComboPackages.getText();
		int count = 0;
		String text;
		if(pack.equals(ALL_PACKAGES)) {
			count = store.getSymbolCount();
			text = " symbols in " + ALL_PACKAGES;
		} else {
			count = store.getSymbolCount(pack);
			text = " symbols in package " + pack;
		}
		fLabelSymbolInPackages.setText(count + text);
	}
	
	private IEnvironment findEvalServer(final String selectedText)
	{
		for(IEnvironment server : fEvalServerManager.getEvalServer()) {
			if(selectedText.equals(server.toString())) {
				return server;
			}
		}
		return null;
	}
	
	
	
	//----- Interface SelectionListener
	public void widgetDefaultSelected(final SelectionEvent e) {
		//no-op
	}

	public void widgetSelected(final SelectionEvent e) 
	{
		if(e.getSource() instanceof Combo) {
			comboSelection(e);
		} else {
			startSearch();
		}
	}
	//----- Ende Interface SelectionListener
	
	private void startSearch() 
	{
		IEnvironment selectedServer = findEvalServer(fComboServer.getText());
		if(selectedServer == null) {
			return;
		}
		
		String queryString = fQueryText.getText();
		String pack = fComboPackages.getText();
		String[] words;
		if(queryString == null || queryString.trim().equals("")) { //nichts eingegeben
			words = new String[] {""}; //alles als Treffer liefern
		} else {
			words = SPLIT_PATTERN.split(queryString);
		}
		
		ISymbolStore store = fEvalServerManager.getSymbolStoreFor(selectedServer);
		SortedSet<IMetaSymbol> result;
		if(ALL_PACKAGES.equals(pack)) {
			result = store.fullTextQuery(Arrays.asList(words), getSelectedSearchMode());
		} else {
			result = store.fullTextQuery(pack, Arrays.asList(words), getSelectedSearchMode());
		}
		
		fViewer.setInput(result);
	}
	
	private TSearchMode getSelectedSearchMode()
	{
		String text = fComboMode.getText();
		if(TEXT_MODE_AND.equals(text)) {
			return TSearchMode.MODE_AND;
		} else if(TEXT_MODE_OR.equals(text)) {
			return TSearchMode.MODE_OR;
		} else {
			return TSearchMode.MODE_NOT;
		}
	}
	
	private void comboSelection(final SelectionEvent e) 
	{
		Combo combo = (Combo)e.getSource();
		
		String selectedText = fComboServer.getItem(fComboServer.getSelectionIndex());
		IEnvironment server = findEvalServer(selectedText);
		if(server == null) {
			return;
		}
		
		if(combo == fComboServer) {
			fViewer.setInput(EMPTY_INPUT);
			updateConnectStatus(server);
			updatePackageCombo(server);
			updateSymbolCount(server);
		} else if(combo == fComboPackages) {
			updateSymbolCount(server);
		}
	}
	
	//----- Interface IEvalServerManagementListener
	public void connect(final IEnvironment server) {
		if(server.toString().equals(fComboServer.getText())) {
			updateConnectStatus(server);
		}
 	}

	public void disconnect(final IEnvironment server) {
		doUpateServerState(server);
	}
	
	public void initialized(final IEnvironment server) {
		doUpateServerState(server);
	}
	
	public void startup(final IEnvironment server) {
		//no-op
	}
	
	public void serverAdded(final IEnvironment server) {
		doUpdateServerList();
	}

	public void serverRemoved(final IEnvironment server) {
		doUpdateServerList();
	}

	public void defaultChanged(IEnvironment newDefault) {
		//no-op
	}

	private void doUpateServerState(final IEnvironment server) {
		if(server.toString().equals(fComboServer.getText())) {
			updateConnectStatus(server);
			updatePackageCombo(server);
			updateSymbolCount(server);
		}
	}

	//----- Ende Interface IEvalServerManagementListener
	
	@Override
	public void setFocus() {
		fViewer.getControl().setFocus();
	}

	@Override
	public void dispose() {
		if(fEvalServerManager != null) {
			fEvalServerManager.removeEvalServerManagementListener(this);
		}
	}
}
