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

/**
 * Standardimplementierung des {@link IEnvironmentConfiguration}-Interfaces. Eine Instanz dieser Klasse
 * kann nur ueber Factory-Methode erzeugt werden.
 * @author Michael Bohn
 * pattern: immutable
 */
public class EnvironmentConfiguration 
implements IEnvironmentConfiguration 
{
	private static EnvironmentConfiguration externInstance;
	
	private boolean fExtern;
	private boolean fExecutable;
	private File fExecutableFile;
	private List<String> fCommands;
	private boolean fLoggingEnabled;
	private TLogSeverity fLogSeverity;
	private boolean fKeepAlive = false; //noch nicht genutzt
	
	/**
	 * Privater Konstruktor, Instanzerzeugung nur ueber Factory-Methoden erlaubt
	 * @param loggingEnabled
	 * @param severity
	 */
	private EnvironmentConfiguration(final boolean loggingEnabled, final TLogSeverity severity)
	{
		this.fLoggingEnabled = loggingEnabled;
		this.fLogSeverity = severity;
	}
	
	/**
	 * Liefert die (Singleton)-Instanz fuer eine Extern-Konfiguration.
	 * @return Das {@link IEnvironmentConfiguration}-Objekt fuer externe Konfiguration
	 */
	public static IEnvironmentConfiguration instanceOfExtern()
	{
		if(externInstance == null) {
			externInstance = new EnvironmentConfiguration(false, null);
			externInstance.fExtern = true;
		}
		return externInstance;
	}
	
	/**
	 * Erzeugt eine Instanz fuer die Konfiguration ueber eine ausfuherbare Datei.
	 * @param logging - Logging ein oder ausgeschaltet
	 * @param severity - Schweregrad der Meldungen die geloggt werden soll, <code>null</code> wenn logging = <code>false</code>
	 * @param executableFile - Die ausfuehrbare Datei, als {@link File}-Objekt
	 * @return Das {@link IEnvironmentConfiguration}-Objekt fuer Konfiguration ueber ausfuehrbare Datei
	 */
	public static IEnvironmentConfiguration instanceOfExecutable(final boolean logging, final TLogSeverity severity, final File executableFile)
	{
		return instanceOfExecutable(logging, severity, executableFile, null);
	}
	
	/**
	 * Erzeugt eine Instanz fuer die Konfiguration ueber eine ausfuherbare Datei.
	 * @param logging - Logging ein oder ausgeschaltet
	 * @param severity - Schweregrad der Meldungen die geloggt werden soll, <code>null</code> wenn logging = <code>false</code>
	 * @param executableFile - Die ausfuehrbare Datei, als {@link File}-Objekt
	 * @param params - Optionale Parameter fuer die Executable
	 * @return Das {@link IEnvironmentConfiguration}-Objekt fuer Konfiguration ueber ausfuehrbare Datei
	 */
	public static IEnvironmentConfiguration instanceOfExecutable(final boolean logging, final TLogSeverity severity, final File executableFile, final List<String> params)
	{
		EnvironmentConfiguration config = new EnvironmentConfiguration(logging, severity);
		config.fExecutableFile = executableFile;
		config.fExecutable = true;
		if(params == null) {
			config.fCommands = Collections.emptyList();
		} else {
			config.fCommands = Collections.unmodifiableList( new ArrayList<String>(params));
		}
		
		return config;
	}
	
	/**
	 * Erzeugt eine Instanz fuer Konfiguration ueber Kommadozeilenparameter.
	 * @param logging - Logging ein oder ausgeschaltet
	 * @param severity - Schweregrad der Meldungen die geloggt werden soll, <code>null</code> wenn logging = <code>false</code>
	 * @param commands - Liste von Kommandos die die Umgebung starten.
	 * @return Das {@link IEnvironmentConfiguration}-Objekt fuer Kommandozeilenkonfiguration
	 * @throws NullPointerException - wenn commandos == <code>null</code>
	 */
	public static IEnvironmentConfiguration instanceofNonExecutable(final boolean logging, final TLogSeverity severity, final List<String> commands)
	{
		EnvironmentConfiguration config = new EnvironmentConfiguration(logging, severity);
		config.fCommands = Collections.unmodifiableList( new ArrayList<String>(commands) );
		config.fExecutable = false;
		return config;
	}
	
	/**
	 * @see IEnvironmentConfiguration
	 */
	public boolean isExtern() {
		return fExtern;
	}
	
	/**
	 * @see IEnvironmentConfiguration
	 */
	public boolean isExecutable() {
		return fExecutable;
	}
	
	/**
	 * @see IEnvironmentConfiguration
	 */
	public File getEvalServerExecutable() {
		return fExecutableFile;
	}
	
	/**
	 * @see IEnvironmentConfiguration
	 */
	public List<String> getCommands() {
		return fCommands;
	}
	
	/**
	 * @see IEnvironmentConfiguration
	 */
	public boolean isLoggingEnabled() {
		return fLoggingEnabled;
	}

	/**
	 * @see IEnvironmentConfiguration
	 */
	public TLogSeverity getLogSeverity() {
		return fLogSeverity;
	}

	/**
	 * @see IEnvironmentConfiguration
	 */
	public boolean keepAlive() {
		return fKeepAlive;
	}
}
