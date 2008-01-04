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

import java.io.File;
import java.util.*;
import org.eclipse.core.runtime.*;
import de.fh_trier.eclipse.lisp.core.connection.IConfigurationFactory;
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.core.connection.IEnvironmentConfiguration.TLogSeverity;
import edu.umd.cs.findbugs.annotations.NonNull;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

public class EnvironmentExtensionReader 
{
	private static final String SERVER_EXTENSION_POINT = "de.fh_trier.eclipse.lisp.servers";
	private Map<IEnvironment, IEnvironmentConfiguration> fResultMap;
	
	/**
	 * Erzeugt einen neuen Extension-Reader.
	 */
	public EnvironmentExtensionReader()
	{
		this.fResultMap = new Hashtable<IEnvironment, IEnvironmentConfiguration>();
	}
	
	/**
	 * Liest alle vorhandenen Lisp-Umgebung sowie deren Konfiguration ein.
	 * Die Umgebungen und Konfigurationen werden aus der Extension-Registry gelesen.
	 * @return Die eingelesenen Umgebungen und jeweiligen Konfigurationen
	 */
	@NonNull
	public Map<IEnvironment, IEnvironmentConfiguration> readExtensions()
	{
		IConfigurationElement[] servers = Platform.getExtensionRegistry()
		     .getConfigurationElementsFor(SERVER_EXTENSION_POINT);
		
		for(IConfigurationElement server : servers) {
			IEnvironment evalServer = createServerFromElement(server);
			if(evalServer == null) {
				continue;
			}
			
			IEnvironmentConfiguration config = createConfigurationFromElement(server);
			fResultMap.put(evalServer, config);
		}
		
		return fResultMap;
	}
	
	private IEnvironment createServerFromElement(final IConfigurationElement server) 
	{
		String name = server.getAttribute("name"); //Server name
		String host = server.getAttribute("host");
		String portString = server.getAttribute("port");
		String version = server.getAttribute("version");
		
		int port = 0;
		try {
			port = Integer.parseInt(portString);
		} catch (NumberFormatException e) {
			error("Illegal port number " + portString, e);
			return null;
		}
		
		if(port < 0 || port > 65535) {
			error("Illegal port range " + port, null);
			return null;
		}
		
		IEnvironment evalServer = new Environment(host, port, name, version);
		return evalServer;
	}
	
	@NonNull
	@SuppressWarnings("Dm")
	private IEnvironmentConfiguration createConfigurationFromElement(final IConfigurationElement server)
	{
		IConfigurationElement[] configChilds = server.getChildren("configuration");
		if(configChilds.length < 1) { //kein Configuration angegeben, default extern
			return EnvironmentConfiguration.instanceOfExtern();
		}
		
		IConfigurationElement config = configChilds[0];
		boolean logging = Boolean.parseBoolean(config.getAttribute("logging"));
		String logString = config.getAttribute("logseverity");
		TLogSeverity severity = null;
		
		if(logString == null) {
			severity = TLogSeverity.DEFAULT_SEVERITY;
		} else {
			try {
				
				severity = TLogSeverity.valueOf(logString.toUpperCase());
			} catch (IllegalArgumentException e) {
				error("Illegal log severity supplied", e);
				severity = TLogSeverity.DEFAULT_SEVERITY;
			}
		}
		
		try {
			Object o = config.createExecutableExtension("class");
			if(o instanceof IConfigurationFactory) {
				IConfigurationFactory factory = (IConfigurationFactory)o;
				File executable = factory.getExecutableFile();
				List<String> cmd = factory.getCommands();
				if(executable != null) { //executable angegeben
					return EnvironmentConfiguration.instanceOfExecutable(logging, severity, executable, cmd);
				}  
				//kein executable
				return EnvironmentConfiguration.instanceofNonExecutable(logging, severity, cmd);
	
				//error("illegal return value from factory, at least one method must return a value", null);
				//return EnvironmentConfiguration.instanceOfExtern();				
			} 
			//invariante: objekt nicht instanz von IConfigurationFactory
			error("illegal class argument", null);
		} catch (CoreException e) {
			error("error creating executable extension", e);
			return EnvironmentConfiguration.instanceOfExtern();
		}
		
		return EnvironmentConfiguration.instanceOfExtern();
	}
	
	private void error(final String message, final Throwable e)
	{
		LispPluginActivator.logError("Extension error: " + message, e);
	}
}
