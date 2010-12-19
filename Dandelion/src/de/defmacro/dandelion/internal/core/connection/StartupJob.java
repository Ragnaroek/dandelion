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

import java.io.*;
import java.net.*;
import java.util.*;
import org.eclipse.core.runtime.*;

import de.defmacro.dandelion.internal.LispPluginActivator;
import edu.umd.cs.findbugs.annotations.Nullable;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Job zum Start einer Umgebung.
 * @author Michael Bohn
 */
public class StartupJob 
extends AbstractJob 
{
	private static final String JOB_NAME = "Starting Lisp Environment";
	private static final String STARTUP_TOKEN = "SERVER_UP";
	
	private Process fCreatedProcess;
	private IEnvironmentConfiguration fConfiguration;
	
	public StartupJob(final IEnvironment server, final IEnvironmentConfiguration config)
	{
		super(JOB_NAME, server);
		
		if (config == null) {
			throw new NullPointerException("config must not be null");
		}
		
		this.fConfiguration = config;
	}
	
	/**
	 * @see AbstractJob#getType()
	 */
	@Override
	public JobType getType() {
		return JobType.START_JOB;
	}

	/**
	 * Startet den Prozess der Umgebung.
	 */
	@Override
	@SuppressWarnings("OS") //Stream darf nicht geschlossen werden
	protected IStatus run(final IProgressMonitor monitor) 
	{	
		if(monitor.isCanceled()) {
			return Status.CANCEL_STATUS;
		}
		
		BufferedReader reader = null; //nur wegen Fehlerbehandlung ausserhalb definiert!
		try {
			ProcessBuilder builder = new ProcessBuilder();
			List<String> command = new ArrayList<String>();
			addServerStartCommands(command, fConfiguration);
			addServerParameters(command, fConfiguration, getEvalServer());
			
			builder.command(command);
			fCreatedProcess = builder.start();
			
			reader = new BufferedReader(new InputStreamReader(fCreatedProcess.getInputStream()));
			String line = reader.readLine();
			if(line == null || !line.equalsIgnoreCase(STARTUP_TOKEN)) {
				Status status = new Status(IStatus.WARNING, LispPluginActivator.ID, 0, "Unexpeced startup token read: " + line, null);
				//LispPluginActivator.log(status);
				return status; //mit warning beenden
			}
		} catch (IOException e) {
			//Im Fehlerfall Stream schliessen.
			if(reader != null) {
				try {
					reader.close();
				} catch(IOException nestedErr) {/*nichts*/}
				
			}
			return error("Starting eval server failed", e);
		} catch (URISyntaxException e) {
			return error("Adding server parameters failed", e);
		}
		
		//reader nicht schliessen, da der Stream noch im ProcessManager benötigt wird.
		
		//alles ok bei start
		return Status.OK_STATUS;
	}
	
	/**
	 * Gibt den von diesem Job erstellten Prozess zurueck.
	 * Gibt <code>null</code> zurueck wenn der Job nicht mit IStatus.OK
	 * beendet wurde.
	 * @return
	 */
	@Nullable
	public Process getProcess()
	{
		return fCreatedProcess;
	}
	
	private void addServerStartCommands(final List<String> commands, final IEnvironmentConfiguration config)
	{
		if(config.isExecutable()) {
			commands.add(config.getEvalServerExecutable().toString());
			commands.addAll(config.getCommands());
		} else {
			commands.addAll(config.getCommands());
		}
	}
	
	private void addServerParameters(final List<String> commands, final IEnvironmentConfiguration config, final IEnvironment server)
	throws URISyntaxException
	{
		commands.add(Integer.toString(server.getPort()));
		if(config.keepAlive()) {
			commands.add("T"); //server forever
		} else {
			commands.add("NIL");
		}
		
		if(config.isLoggingEnabled()) {
			commands.add("T");
			commands.add(config.getLogSeverity().name());
			if(config.isExecutable()) {
				commands.add(makeLogFile(config.getEvalServerExecutable().getParent(), server));
			} else {
				commands.add(makeLogFile(new File(new URI(Platform.getInstallLocation().getURL().toString())).toString(), server));
			}
		}
	}
	
	private String normalizePath(final String path)
	{
		String separator = System.getProperty("file.separator");
		if(!path.endsWith(separator)) {
			return path + separator;
		}
		return path;
	}
	
	private String makeLogFile(final String path, final IEnvironment server)
	{	
		return normalizePath(path) + server.getName() +"_"+server.getVersion()+".html";
	}
}
