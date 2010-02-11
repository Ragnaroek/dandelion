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

package de.defmacro.dandelion.internal.core.connection;

import java.util.*;
import java.util.Map.Entry;

import net.jcip.annotations.ThreadSafe;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.jobs.*;
import org.eclipse.ui.*;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.core.LispCore;
import de.defmacro.dandelion.internal.core.connection.AbstractJob.JobType;
import de.defmacro.dandelion.internal.core.meta.*;
import de.defmacro.dandelion.internal.preferences.*;
import de.defmacro.dandelion.internal.ui.views.listener.*;
import edu.umd.cs.findbugs.annotations.*;

/**
 * Die zentrale Verwaltungsklasse fuer die Lisp-Umgebungen.
 * Ein Zugriff auf die Singleton-Instanz ist ueber {@link LispCore#getEnvironmentManager()}
 * gewaehrleistet. Diese Klasse ist nicht zur Instanzierung vorgesehen.
 * 
 * Diese Klasse ist Thread-Safe.
 * @author Michael Bohn
 *
 */
@ThreadSafe
public class EnvironmentManager 
implements IDisposable, IJobHandler
{	
	private List<IEnvironmentManagementListener> fListener;
	private List<IEnvironment> fEvalServer;
	private Map<IEnvironment, ConfigurationEntry> fConfigurations;
	private IEnvironment fDefaultEvalServer;
	//private Map<IProject, IEnvironment> fProjectAssociation;
	private Map<IEnvironment, IConnection> fOpenConnections;
	private Map<IEnvironment, ISymbolStore> fSymbolStores;
	private List<AbstractJob> fPendingJobs; //alle zur Zeit laufenden Jobs
	private ServerProcessManager fProcessManager;
	private boolean fIsInitialized = false;
	
	private static final class ConfigurationEntry
	{
		private final boolean fContributedViaPlugin;
		private final IEnvironmentConfiguration fConfiguration;
		
		public ConfigurationEntry(final boolean contributedViaPlugin, final IEnvironmentConfiguration configuration) {
			fContributedViaPlugin = contributedViaPlugin;
			fConfiguration = configuration;
		}
		
		public boolean isContributedViaPlugin()
		{
			return fContributedViaPlugin;
		}
		
		public IEnvironmentConfiguration getConfiguration()
		{
			return fConfiguration;
		}
	}
	
	/**
	 * Die Singleton-Instanz ist ueber {@link LispCore#getEnvironmentManager()} zu erreichen.
	 * Dieses Objekt darf nicht instantiert werden.
	 */
	public EnvironmentManager()
	{
		fSymbolStores = new Hashtable<IEnvironment, ISymbolStore>();
		
		//fProjectAssociation = new Hashtable<IProject, IEnvironment>();
		fOpenConnections = new Hashtable<IEnvironment, IConnection>();
		fEvalServer = new ArrayList<IEnvironment>();
		fListener = new ArrayList<IEnvironmentManagementListener>();
		fPendingJobs = new LinkedList<AbstractJob>();
		fProcessManager = new ServerProcessManager(this);
		fConfigurations = new Hashtable<IEnvironment, ConfigurationEntry>();
	}
	
	/**
	 * Fuegt einen neuen {@link IEnvironmentManagementListener} hinzu.
	 * @param listener
	 */
	public synchronized void addEvalServerManagementListener(final IEnvironmentManagementListener listener)
	{
		fListener.add(listener);
	}
	
	/**
	 * Entfernt einen vorhanden {@link IEnvironmentManagementListener}.
	 * Ohne Wirkung wenn der Listener nicht angemeldet war.
	 * @param listener
	 */
	public synchronized void removeEvalServerManagementListener(final IEnvironmentManagementListener listener)
	{
		fListener.remove(listener);
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber ein disconnect des uebergebenen Servers.
	 * @param server
	 */
	protected synchronized void fireDisconnect(final IEnvironment server)
	{
		for(IEnvironmentManagementListener listener : fListener) {
			listener.disconnect(server);
		}
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber ein connect des uebergebenen Servers.
	 * @param server
	 */
	protected synchronized void fireConnect(final IEnvironment server) 
	{
		for(IEnvironmentManagementListener listener : fListener) {
			listener.connect(server);
		}
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber den Start eines Servers.
	 * @param server
	 */
	protected synchronized void fireStartup(final IEnvironment server) 
	{
		for(IEnvironmentManagementListener listener : fListener) {
			listener.startup(server);
		}
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber eine Initialisierung des Servers.
	 * @param server
	 */
	protected synchronized void fireInitialized(final IEnvironment server) {
		for(IEnvironmentManagementListener listener : fListener) {
			listener.initialized(server);
		}
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber das Hinzufuegen eines neuen Servers.
	 * @param server
	 */
	protected synchronized void fireServerAdded(final IEnvironment server) {
		for(IEnvironmentManagementListener listener : fListener) {
			listener.serverAdded(server);
		}
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber das Entfernen eines vorhandenen Servers.
	 * @param server
	 */
	protected synchronized void fireServerRemoved(final IEnvironment server) {
		for(IEnvironmentManagementListener listener : fListener) {
			listener.serverRemoved(server);
		}
	}
	
	/**
	 * Benachrichtigt alle angemeldeten Listener ueber den Wechsel des Default-Servers.
	 * @param server
	 */
	protected synchronized void fireDefaultChanged(IEnvironment server) {
		for(IEnvironmentManagementListener listener : fListener) {
			listener.defaultChanged(server);
		}
	}
	
	/**
	 * Wird bei Aktivierung des Plugins aufgerufen.
	 * Diese Methode wird vom PluginActivator aufgerufen. Darf sonst nicht
	 * aufgerufen werden.
	 * @param workspace
	 */
	public synchronized void initialize()
	{	
		Assert.isTrue(!fIsInitialized);
		
		readKnownEvalServers();
		
		fIsInitialized = true;
	}

	private void readKnownEvalServers() 
	{
		readUserDefinedServers();
		readExtensionServers();
		
		Integer defaultHash = LispPreferences.loadDefaultServer();
		
		if(defaultHash == null) { //kein default, erster server als default setzen
			if( !fEvalServer.isEmpty() ) {
				fDefaultEvalServer = fEvalServer.get(0);
			}
		} else {
			for(IEnvironment server : fEvalServer) {
				if(server.hashCode() == defaultHash.intValue()) {
					fDefaultEvalServer = server;
				}
			}
			//Default nicht gefunden, ersten server nehmen
			if(fDefaultEvalServer == null && !fEvalServer.isEmpty()) {
				fDefaultEvalServer = fEvalServer.get(0);
			}
		}
	}
	
	private void readExtensionServers()
	{
		EnvironmentExtensionReader reader = new EnvironmentExtensionReader();
		Map<IEnvironment, IEnvironmentConfiguration> result = reader.readExtensions();
		
		for(Entry<IEnvironment, IEnvironmentConfiguration> entry : result.entrySet()) {
			if(isNameAndVersionUnique(entry.getKey())) { //nur hinzufuegen wenn name und version noch nicht existieren
				internalAddEvalServer(entry.getKey(), entry.getValue(), true);
			} else {
				LispPluginActivator.logError("double defined eval server, name and version must be unique", null);
			}
		}
	}
	
	private boolean isNameAndVersionUnique(final IEnvironment server)
	{
		return isNameAndVersionUnique(server.getName(), server.getVersion(), fEvalServer);
	}
	
	private void readUserDefinedServers()
	{
		Map<IEnvironment, IEnvironmentConfiguration> servers = LispPreferences.loadEvalServer();
		for(Entry<IEnvironment, IEnvironmentConfiguration> entry : servers.entrySet()) {
			internalAddEvalServer(entry.getKey(), entry.getValue(), false);
		}
	}
	
	/**
	 * Fuegt eine neue Lisp-Umgebung und deren Konfiguration zur Liste der bekannten Umgebungen hinzu.
	 * Benachrichtigt alle angemeldeten Listener.
	 * Ein Aufruf dieser Methode darf nur aus dem UI-Thread erfolgen.
	 * @param server
	 * @param config
	 */
	public synchronized void addEnvironment(final IEnvironment server, final IEnvironmentConfiguration config)
	{
		//vor internalAdd.. default setzten, da listener dort benachrichtigt werden und evtl. default
		//abfragen
		if(fEvalServer.size() == 0) { //der erste Server der hinzugefuegt wurde
			setDefaultEnvironment(server); //als default setzen
		}
		internalAddEvalServer(server, config, false);
	}
	
	/**
	 * Entfernt eine Lisp-Umgebung.
	 * Eine evtl. bestehende Verbindung und ein evtl. geoffneter Listener zu Verbindung
	 * wird geschlossen.
	 * Benachrichtigt alle angemeldeten Listener.
	 * Ein Aufruf dieser Methode darf nur aus dem UI-Thread erfolgen.
	 * @param server
	 * @throws ManagementException - wenn server noch in einem Projekt verwendet wird
	 * @throws ConnectionException 
	 */
	public synchronized void removeEnvironment(final IEnvironment server)
	throws ManagementException, ConnectionException
	{
		if(LispCore.getProjectManager().isUsedInProject(server)) {
			throw new ManagementException("server is used in a project");
		}
		
		//invariante: server kann geloescht werden
		closeListenerFor(server);
		closeConnection(server);
		fEvalServer.remove(server);
		fConfigurations.remove(server);
		
		fireServerRemoved(server);
	}
	
	private void internalAddEvalServer(final IEnvironment server, final IEnvironmentConfiguration config, final boolean pluginContributed)
	{
		fEvalServer.add(server);
		fConfigurations.put(server, new ConfigurationEntry(pluginContributed, config));
		fireServerAdded(server);
	}
	
	/**
	 * Uberprueft ob eine Verbidung zur uebergebenen Lisp-Umgebung besteht.
	 * @param  - Umgebung die ueberprueft werden soll
	 * @return <code>true</code> wenn Verbindung zur Umgebung besteht, sonst <code>false</code>
	 */
	public synchronized boolean isConnected(final IEnvironment server) 
	{
		IConnection connection = fOpenConnections.get(server);
		if(connection == null) {
			return false;
		}
		return connection.isConnected();
	}
	
	/**
	 * Liefert die Konfiguration fuer eine bestimmte Lisp-Umgebung.
	 * @param server - die Umgebung fuer die die Konfiguration geliefert werden soll.
	 * @return - die {@link IEnvironmentConfiguration} fuer die Umgebung
	 * @throws ManagementException - wenn IEvalServer nicht bekannt (nicht aus Liste {@link EnvironmentManager#getEnvironments()}
	 */
	@NonNull
	public synchronized IEnvironmentConfiguration getConfigurationFor(final IEnvironment server)
	throws ManagementException
	{	
		ConfigurationEntry entry = fConfigurations.get(server);
		if(entry == null) {
			throw new ManagementException();
		}
		
		//invariante: entry != null
		return entry.getConfiguration();
	}
	
	/**
	 * Liefert <code>true</code> wenn die Umgebung ueber einen Erweiterungspunkt hinzugefuegt wurde.
	 * @param server - Umgebung die ueberprueft werden soll.
	 * @return <code>false</code> - wenn server nicht aus Liste der Server oder nicht ueber ein Plugin beigetragen
	 */
	public synchronized boolean isContributedViaPlugin(final IEnvironment server)
	{
		ConfigurationEntry config = fConfigurations.get(server);
		if(config == null) {
			return false;
		}
		return config.isContributedViaPlugin();
	}
	
	/**
	 * Liefert das Verbindungsobjekt fuer das uebergebene Projekt.
	 * @param project - Das Lisp-Projekt fuer das die Verbindung zurueckgegeben werden soll.
	 * @return Verbindungsobjekt.
	 * @throws ConnectionException
	 * @throws ManagementException - wenn fuer das Projekt kein Eval Server konfiguriert wurde, das Projekt geschlossen ist, 
	 *                               das Projekt nicht mehr existiert oder das Projekt nicht die LispNature besitzt.
	 * @throws NullPointerException - wenn project == <code>null</code>
	 * @see EnvironmentManager#getConnectionFor(IEnvironment)
	 */
	/*
	@NonNull
	public synchronized IConnection getConnectionFor(final IProject project) 
	throws ManagementException
	{
		checkNature(project);
		
		IEnvironment server = getEnvironmentFor(project);
		if(server == null) {
			throw new ManagementException("Eval server for this project not found");
		}
		
		return getConnectionFor(server);
	}*/
	
	/**
	 * Gibt das IConnection-Objekt fuer den uebergebene EvalServer zurueck.
	 * Der Aufrufer ist fuer den Aufruf von connect() der Verbindung
	 * zustaending. Beim allerersten Zugriff oder
	 * wenn der Server zwischenzeitlich beendet wurde wird der
	 * Server versucht zu starten (falls Autostart fuer Server gewuenscht)
	 * und der InitJob fuer die Verbindung wird gestartet.
	 * @return
	 * @throws ConnectionException
	 * @throws {@link NullPointerException} - wenn evalServer == <code>null</code>
	 */
	public synchronized IConnection getConnectionFor(final IEnvironment evalServer)
	throws ManagementException
	{	
		if(evalServer == null) {
			throw new NullPointerException();
		}
		
		IConnection connection = fOpenConnections.get(evalServer);
		if(connection == null) {
			connection = new LispConnection(evalServer);
			IEnvironmentConfiguration config = getConfigurationFor(evalServer);
			fOpenConnections.put(evalServer, connection);
			if( !config.isExtern() && !fProcessManager.isProcessRunning(evalServer)) {
				fProcessManager.startProcess(evalServer, config); //den Server starten
			}
			doInit(connection); //Verbindung initialisieren	
		}
	 			
		return connection;
	}

	/**
	 * @see IJobHandler#startJob(AbstractJob)
	 */
	public synchronized void startJob(final AbstractJob job)
	{
		fPendingJobs.add(job);
		job.addJobChangeListener(this);
		job.schedule();
	}
	
	/**
	 * @see IJobHandler#removeJob(AbstractJob)
	 */
	public synchronized void removeJob(final AbstractJob job)
	{
		fPendingJobs.remove(job);
		job.removeJobChangeListener(this);
	}
	
	/**
	 * @see IJobHandler#jobPending(JobType, IEnvironment)
	 */
	public synchronized boolean jobPending(final JobType type, final IEnvironment server)
	{
		for(AbstractJob job : fPendingJobs) {
			if(job.getType() == type && job.getEvalServer().equals(server)) {
				return true;
			}
		}
		return false;
	}
	
	private void doInit(final IConnection connection) 
	{
		ISymbolStore store = getSymbolStoreFor(connection.getEvalServer());	
		InitializationJob initJob = new InitializationJob(connection, store);
		startJob(initJob);
	}
	
	/**
	 * Liefert das {@link ISymbolStore}-Objekt fuer die die Umgebung aus dem Projekt.
	 * @see EnvironmentManager#getSymbolStoreFor(IEnvironment)
	 * @param project - Das Lisp-Projekt fuer das der Symbolspeicher geliefert werden soll.
	 * @return Der Symbolspeicher fuer die Umgebung.
	 */
	/*
	public synchronized ISymbolStore getSymbolStoreFor(final IProject project) 
	{
		return getSymbolStoreFor(getEnvironmentFor(project));
	}*/
	
	/**
	 * Liefert das {@link ISymbolStore}-Objekt fuer Umgebung.
	 * @param evalServer
	 * @return Der Symbolspeicher, nie <code>null</code>
	 */
	public synchronized ISymbolStore getSymbolStoreFor(final IEnvironment evalServer) 
	{
		ISymbolStore store = fSymbolStores.get(evalServer);
		if(store == null) {
			store = new SymbolStore();
			fSymbolStores.put(evalServer, store);
		}
		return store;
	}
	
	/*
	private void checkNature(final IProject project)
	throws ManagementException
	{
		try {
			if( !hasLispNature(project) ) {
				throw new ManagementException("Illegal project: " + project.getName());
			}
		} catch (CoreException e) {
			throw new ManagementException(e);
		}
	}*/
	

	
	/**
	 * Speichert eine Zuordnung einer Lisp-Umgebung zu einem Projekt.
	 * @param project - Lisp-Projekt
	 * @param server - Lisp-Umgebung
	 * @return IEvalServer - der vorher eingetragene Eval-Server fuer dieses Project oder <code>null</code>
	 * @throws ConnectionException - wenn ein schliessen der Verbindung zur vorherigen Umgebung des Projektes fehlschlaegt
	 */
	/*
	public synchronized IEnvironment setEnvironmentFor(final IProject project, final IEnvironment server) 
	throws ConnectionException
	{	
		IEnvironment old = fProjectAssociation.get(project);
		if( old != null && server != null) {
			if( old.equals(server) ) { //zum selben server verbinden -> nichts machen
				return server;
			}
		}
		
		//invariante: Server fuer Projekt hat sich tatsaechlich geaendert
		
		if(old != null) { //projekt wird nicht neu angelegt, initialisiert
			returnConnection(project); //alte Verbindung loesen, disconnect wenn kein weiteres Projekt Verbindung benutzt
		}
			
		fProjectAssociation.put(project, server);
		return old;
	}
	*/
	/**
	 * Liefert die Lisp-Umgebung fuer das uebergebene Projekt.
	 * @param project - Ein Projekt-Objekt
	 * @return Die Lisp-Umgebung des Projekts, <code>null</code> wenn Projekt keine Umgebung besitzt oder kein
	 * Lisp-Projekt ist.
	 */
	/*
	public synchronized IEnvironment getEnvironmentFor(final IProject project)
	{
		return fProjectAssociation.get(project);
	}*/
	
	/**
	 * Oeffnet die Listener-Ansicht zur Lisp-Umgebung.
	 * Ist die Ansicht bereits geoeffnet ist ein Aufruf dieser Methode ohne Wirkung.
	 * @param project
	 * @return Referenz auf die geoffnete oder bereits offene {@link IListenerView}
	 * @throws ManagementException - wenn fuer diese Projekt kein Eval-Server existiert
	 * @throws PartInitException - wenn ein oeffnen der Ansicht fehlschlaegt.
	 */
	/*
	public synchronized IListenerView openListenerFor(final IProject project)
	throws ManagementException, PartInitException
	{
		IEnvironment evalServer = getEnvironmentFor(project);
		if(evalServer == null) {
			throw new ManagementException("Eval server for this project not found");
		}
		
		return openListenerFor(evalServer);
	}
	*/
	/**
	 * Oeffnet den zugehoerigen Listener fuer die Umgebung.
	 * @param evalServer - Die Lisp-Umgebung
	 * @return Referenz auf die geoffnete oder bereits offene {@link IListenerView} 
	 * @throws PartInitException
	 * @see {@link EnvironmentManager#openListenerFor(IProject)}
	 */
	public synchronized IListenerView openListenerFor(final IEnvironment evalServer) 
	throws PartInitException
	{
		if (evalServer == null) {
			throw new NullPointerException("server must not be null");
		}
		
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		//invariante: verbunden
		//IConnection connection = entry.getConnection();
		//secondaryID der View wird der hashCode() des EvalServers da dieser auch ueber Instanzen erhalten bleibt!
		//wichtig wenn nach workbench restart die View wieder angezeigt wird
		IViewPart part = window.getActivePage().showView(IListenerView.ID, Integer.toString(evalServer.hashCode()), 
				IWorkbenchPage.VIEW_VISIBLE);
		IListenerView listener = (IListenerView)part;
		listener.setEvalServer(evalServer, isConnected(evalServer));
		return listener;
 	}
	
	/**
	 * Schliesst einen Listener zur Umgebung.
	 * Ohne Wirkung wenn Listener nicht geoffnet.
	 * @param server - Die Lisp-Umgebung
	 */
	public synchronized void closeListenerFor(final IEnvironment server) 
	{
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		IViewReference[] refs = window.getActivePage().getViewReferences();
		
		for(IViewReference ref : refs) {
			if(ref.getId().equals(IListenerView.ID) && ref.getSecondaryId() != null 
					&& ref.getSecondaryId().equals(Integer.toString(server.hashCode()))) {
				IViewPart part = ref.getView(false);
				if(part != null) {
					window.getActivePage().hideView(part);
				}
			}
		}
	}
	
	/**
	 * Die Verbindung des Projektes wird zurueckgegeben. Das kann dazu fuehren
	 * das die Verbindung geschlossen wird, wenn die Verbindungs sonst nirgendwo
	 * mehr verwendet wird.
	 * Ein Aufruf dieser Methode darf nur aus dem UI-Thread erfolgen.
	 * @param project - das Projekt fuer das die Verbindung zurueckgegeben wird
	 * @return boolean - <code>true</code> wenn die Verbindung geschlossen wurde
	 * @throws ConnectionException - bei Fehler beim schliessen der Verbindung
	 */
	/*
	public synchronized boolean returnConnection(final IProject project) 
	throws ConnectionException
	{
		IEnvironment removed = fProjectAssociation.remove(project);
		if(removed == null) return false;
		
		boolean disconnect = true;
		for(Entry<IProject, IEnvironment> entry : fProjectAssociation.entrySet()) {
			if(entry.getValue().equals(removed)) {
				disconnect = false; //es gibt noch ein Projekt welches den EvalServer auch verwendet
			}
		}
		
		if(disconnect) {
			closeConnection(removed);
		}
		return disconnect;
	}
	*/
	/**
	 * Schliesst die Verbindung zum EvalServer (auf jeden Fall). Der zu dieser
	 * Verbindung gehoerende Listener wird disconnected.
	 * Ein Aufruf dieser Methode darf nur aus dem UI-Thread erfolgen.
	 * @param server
	 * @throws ConnectionException 
	 */
	public synchronized void closeConnection(final IEnvironment server) 
	throws ConnectionException
	{
		IConnection connection = fOpenConnections.get(server);
		if(connection != null) {
			fOpenConnections.remove(server);
			doDisconnect(server, connection);
		}
	}
	
	private void doDisconnect(final IEnvironment server, final IConnection connection) 
	throws ConnectionException
	{
		if(connection != null && connection.isConnected()) {
			connection.disconnect();
			fireDisconnect(server);
		}
	}
	
	
	/**
	 * Schliesst alle bestehenden Verbindungen zu Lisp-Umgebungen.
	 * Ein Aufruf dieser Methode darf nur aus dem UI-Thread erfolgen.
	 * Bei einem Fehler beim Schliessen der Verbindung wird die letzte Fehlermeldung
	 * zurueckgeliefert (bei mehrfachen Fehlern)
	 * @throws ConnectionException - letzte Fehlermeldung bei schliessen der Verbindung
	 */
	public synchronized void closeAllConnections()
	throws ConnectionException
	{
		try {
			ConnectionException exceptionOccured = null;
			for(Iterator<Entry<IEnvironment, IConnection>> iter = fOpenConnections.entrySet().iterator(); iter.hasNext(); ) {
				Entry<IEnvironment, IConnection> entry = iter.next();
				if(entry != null) {
					try {
						doDisconnect(entry.getKey(), entry.getValue());
						//kein iter.remove(); am ende clear()
					} catch (ConnectionException e) {
						LispPluginActivator.logError("Disconnect eval server failed", e);
						exceptionOccured = e;
					}
				}
			}
			
			if(exceptionOccured != null) {
				throw exceptionOccured; //letzte Fehlermeldung an aufrufer weitergeben
			}
		} finally {
			fOpenConnections.clear();
		}
	}
	
	/**
	 * Liefert eine Liste aller bekannten Lisp-Umgebung.
	 * Die zurueckgegebene Liste darf veraendert werden.
	 * @return Liste der Umgebungen, nie <code>null</code>
	 */
	public synchronized List<IEnvironment> getEnvironments()
	{
		return new ArrayList<IEnvironment>(fEvalServer);
	}
	
	/**
	 * Wie {@link EnvironmentManager#getEnvironments()} nur sind die Lisp-Umgebung nach Name sortiert.
	 * @param sort - <code>true</code> wenn Umgebungen nach Namen sortiert werden sollen.
	 * @return (Sortierte) Liste der Umgebungen, nie <code>null</code>
	 */
	public synchronized List<IEnvironment> getEnvironments(boolean sort)
	{
		List<IEnvironment> server = getEnvironments();
		if(sort) {
			Collections.sort(server);
		}
		return server;
	}
	
	/**
	 * Liefert den eingestellten Default-Server.
	 * Garantiert nicht <code>null</code> wenn {@link EnvironmentManager#hasEnvironment()} <code>true</code> liefert.
	 * @return Die Default Lisp-Umgebung.
	 * @see EnvironmentManager#hasEnvironment()
	 */
	public synchronized IEnvironment getDefaultEnvironment()
	{
		return fDefaultEvalServer;
	}
	
	/**
	 * Setzt einen neuen Default-Server.
	 * Alle angemeldeten Listener werden benachrichtigt.
	 * Ein Aufruf dieser Methode darf nur aus dem UI-Thread erfolgen.
	 * @param server
	 */
	public synchronized void setDefaultEnvironment(final IEnvironment server)
	{
		if(fDefaultEvalServer == null || !fDefaultEvalServer.equals(server)) {
			fDefaultEvalServer = server;
			fireDefaultChanged(server);
		}
	}

	/**
	 * Praedikat welches angibt ob Eval-Server vorhanden sind.
	 * Wird hier <code>false</code> zurueckgeben liefert ein Aufruf
	 * von {@link EnvironmentManager#getDefaultEnvironment()} <code>null</code> und
	 * {@link EnvironmentManager#getEnvironments()} eine leere Liste.
	 * @return
	 * @see EnvironmentManager#getEnvironments()
	 * @see EnvironmentManager#getDefaultEnvironment()
	 */
	public synchronized boolean hasEnvironment()
	{
		return fEvalServer != null && !fEvalServer.isEmpty();
	}
	
	/**
	 * Darf nicht aufgerufen werden.
	 * @see IJobChangeListener#done(IJobChangeEvent)
	 */
	public synchronized void done(final IJobChangeEvent event) 
	{	
		if(event.getJob() == null || !(event.getJob() instanceof AbstractJob)) {
			return;
		}
		
		final AbstractJob job = (AbstractJob)event.getJob();
		removeJob(job);
		IStatus result = event.getResult();
		
		if(result != null && result.isOK()) {
			/*
			Display display = PlatformUI.getWorkbench().getDisplay();
			if(display == null || display.isDisposed()) {
				return;
			}
			display.asyncExec(new ListenerNotifier(job.getEvalServer(), job.getType(), job.hasConnected())); */
			
			SaveListenerNotifier notifier = new SaveListenerNotifier(new SaveRunnable() {
				@Override
				public void run0() {
					IEnvironment server = job.getEvalServer();
					
					if(job.hasConnected()) {
						fireConnect(server);	
					}
					
					JobType type = job.getType();
					switch(type) {
					case INIT_JOB    : fireInitialized(server);
					                   break;
					case START_JOB   : fireStartup(server);
						               break;
					default : throw new IllegalArgumentException("Cannot handle " + type);
					}
				}
			});
			notifier.execInUIDisplay();
		} else { //job wurde mit Fehler beendet
			//nachfolgende Jobs abbrechen
			cancelFollowingJobs(job.getEvalServer(), job.getType());
			if(job.getType() == JobType.START_JOB) { //Server-Start Job nicht erfolgreich, Connection Objekt loeschen
				                                     //damit beim naechsten Aufruf versucht wird Server neu zu starten
				fOpenConnections.remove(job.getEvalServer());
			}
		}
	}
	
	private void cancelFollowingJobs(final IEnvironment server, final JobType type)
	{
		for(Iterator<AbstractJob> iter = fPendingJobs.iterator(); iter.hasNext(); ) {
			AbstractJob job = iter.next();
			if(job.getEvalServer().equals(server) && job.getType().getPriority() > type.getPriority()) {
				iter.remove();
				job.cancel();
			}	
		}
	}
	
	/**
	 * Darf nicht aufgerufen werden.
	 * @see IJobChangeListener#aboutToRun(IJobChangeEvent)
	 */
	public synchronized void aboutToRun(final IJobChangeEvent event) {
		//no-op
	}

	/**
	 * Darf nicht aufgerufen werden.
	 * @see IJobChangeListener#awake(IJobChangeEvent)
	 */
	public synchronized void awake(final IJobChangeEvent event) {
		//no-op	
	}
	
	/**
	 * Darf nicht aufgerufen werden.
	 * @see IJobChangeListener#running(IJobChangeEvent)
	 */
	public synchronized void running(final IJobChangeEvent event) {
		//no-op	
	}

	/**
	 * Darf nicht aufgerufen werden.
	 * @see IJobChangeListener#scheduled(IJobChangeEvent)
	 */
	public synchronized void scheduled(final IJobChangeEvent event) {
		//no-op
	}

	/**
	 * Darf nicht aufgerufen werden.
	 * @see IJobChangeListener#sleeping(IJobChangeEvent)
	 */
	public synchronized void sleeping(final IJobChangeEvent event) {
		//no-op
	}

	/**
	 * Fahert den Manager herunter. Wird vom {@link LispPluginActivator} aufgerufen.
	 */
	public synchronized void dispose() {
		try {
			closeAllConnections();
		} catch (ConnectionException e) {
			//no-op, in closeAllConnections wird geloggt
		}
	}
	
	/**
	 * Testet ob der Name und die Version der Lisp-Umgebung eindeutig ist.
	 * @param name - Name der Lisp-Umgebung
	 * @param version - Version der Lisp-Umgebung
	 * @param fServerList - Liste aller bekannten Server
	 * @return <code>true</code> wenn name und version noch nicht vergeben.
	 */
	public static boolean isNameAndVersionUnique(final String name, final String version, final List<IEnvironment> fServerList) 
	{
		for(IEnvironment server : fServerList) {
			if(server.getName().equalsIgnoreCase(name.trim())
					&& server.getVersion().equalsIgnoreCase(version.trim())) {
				return false;
			}
		}
		return true;
	}
}

