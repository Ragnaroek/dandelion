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

package de.fh_trier.eclipse.lisp.internal.core.connection;

import java.util.List;
import org.eclipse.core.runtime.*;
import de.fh_trier.eclipse.lisp.internal.core.connection.protocol.*;
import de.fh_trier.eclipse.lisp.internal.core.meta.*;

/**
 * Ein Job der den {@link ISymbolStore} einer Verbindung initialisiert.
 * @author Michael Bohn
 */
public class InitializationJob 
extends AbstractJob
{
	/**
	 * Der Name des Jobs.
	 */
	private static final String JOB_NAME = "Initialising Connection";
	
	private IConnection fConnection;
	private ISymbolStore fStore;
	
	/**
	 * Erstellt einen neuen Initialisierungsjob.
	 * @param connection - Die Verbindung von der die Symbole gelesen werden sollen.
	 * @param store - Der zu initialisierende Symbolspeicher.
	 * @throws NullPointerException - wenn connection oder store <code>null</code>
	 */
	public InitializationJob(final IConnection connection, final ISymbolStore store)
	{
		super(JOB_NAME, connection.getEvalServer());

		if (store == null) {
			throw new NullPointerException("store must not be null");
		}

		this.fConnection = connection;
		this.fStore = store;
	}

	/**
	 * @see AbstractJob#getType()
	 */
	@Override
	public JobType getType() {
		return JobType.INIT_JOB;
	}

	/**
	 * Initialisiert den Symbolspeicher.
	 * Alle Paket-, Makro- und Funktionssymbole werden eingelesen und zum
	 * Symbolspeicher hinzugefuegt.
	 * @param monitor - Der ProgressMonitor
	 */
	@Override
	protected IStatus run(final IProgressMonitor monitor) 
	{
		if(monitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}
		
		try {
			ensureConnected(fConnection);
			
			//eigentlich nicht noetig, aber die ausführung zweier init-jobs
			//zur selben zeit wird so verhindert
			fConnection.getLock().acquire(); //Verbindung blockieren
				
			IProtocolReader reader = fConnection.getProtocolReader();
			IProtocolWriter writer = fConnection.getProtocolWriter();
			
			writer.writePackageRequest();
			List<String> packages = reader.readPackageList();
			
			monitor.beginTask("Reading package symbols", packages.size() * 1000 * 2);
			
			for(String pack : packages) {
				monitor.subTask("Package " + pack);
				
				writer.writeFunctionRequest(pack);
				List<IMetaSymbol> functions = reader.readFunctionSymbols(pack, TMetaType.FUNCTION);
				fStore.internFunctionSymbol(pack, functions);
				monitor.worked(1000);
				if(monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
				
				writer.writeMacroRequest(pack);
				List<IMetaSymbol> macros = reader.readFunctionSymbols(pack, TMetaType.MACRO);
				fStore.internMacroSymbol(pack, macros);
				monitor.worked(1000);
				
				if(monitor.isCanceled()) {
					return Status.CANCEL_STATUS;
				}
			}
			
			//Die Initialisierung wurde nicht abgebrochen und alle Package-Symbole wurden vom Server gelesen
			fStore.setInitialized(true);
			
		} catch (ConnectionException e) {
			return error("Error initialising connection", e);
		} finally {
			//System.out.println("releasing init job lock");
			fConnection.getLock().release();
			monitor.done();
		}
		
		return Status.OK_STATUS;
	}
}
