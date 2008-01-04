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

package de.fh_trier.eclipse.lisp.internal.preferences;

import java.io.File;
import java.util.*;
import java.util.Map.Entry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.preference.IPreferenceStore;
import de.fh_trier.eclipse.lisp.internal.LispPluginActivator;
import de.fh_trier.eclipse.lisp.internal.core.connection.*;
import de.fh_trier.eclipse.lisp.internal.core.connection.IEnvironmentConfiguration.TLogSeverity;
import de.fh_trier.eclipse.lisp.internal.core.dom.TSExpression;
import static de.fh_trier.eclipse.lisp.internal.LispPluginActivator.PLUGIN_NS;

/**
 * Bietet statischen Zugriff auf Hilfsmethoden zur Speicherung
 * in den Preferences.
 * @author Michael Bohn
 * @since 1.0
 * @version 1.0.5
 */
public class LispPreferences 
{
	private LispPreferences()
	{ /* keine Instanzierung */}
	
	//Preference Konstanten
	
	/**
	 * Boolean-Attribut. In der Outline-View sollen nur Top-Level Forms angezeigt werden.
	 */
	public static final String OUTLINE_HIDE_SUBLEVEL_FORMS = "de.fh_trier.eclipse.lisp.preferences.hideSubLevel";
	
	/**
	 * Boolean-Attribut. Sortierung der angezeigten Typen nach Namen.
	 */
	public static final String OUTLINE_SORT_BY_NAME = "de.fh_trier.eclipse.lisp.preferences.sortByName";
	
	/**
	 * Boolean-Attribut. Sortierung der angezeigten Typen nach Typ.
	 */
	public static final String OUTLINE_SORT_BY_TYPE = "de.fh_trier.eclipse.lisp.preferences.sortByType";
	
	/**
	 * String[].
	 * Diese Typen sollen in der Outline-View im Top-Level angezeigt werden.
	 * @see TSExpression
	 * @see #decodeType
	 * @see #encodeType 
	 */
	public static final String OUTLINE_SHOW_TYPES_ON_TOPLEVEL = "de.fh_trier.eclipse.lisp.preferences.show_types_toplevel";
	
	/**
	 * String[].
	 * Diese Typen sollen in der Outline-View im Sub-Level angezeigt werden.
	 * @see TSExpression
	 * @see #decodeType
	 * @see #encodeType 
	 */
	public static final String OUTLINE_SHOW_TYPES_ON_SUBLEVEL = "de.fh_trier.eclipse.lisp.preferences.show_types_sublevel";
	
	/**
	 * Preference-Key fuer Einstellung ob Completion Proposals in Upper oder Lower-Case vorgenommen
	 * werden.
	 */
	public static final String P_UPPER_CASE_PROPOSAL = PLUGIN_NS + ".preferences.upper_case_proposal";
	
	private static final String P_EVAL_SERVER_COUNT = PLUGIN_NS + ".preferences.server.count";
	private static final String P_EVAL_SERVER_NAME = PLUGIN_NS + ".preferences.server.name";
	private static final String P_EVAL_SERVER_HOST = PLUGIN_NS + ".preferences.server.host";
	private static final String P_EVAL_SERVER_PORT = PLUGIN_NS + ".preferences.server.port";
	private static final String P_EVAL_SERVER_VERSION = PLUGIN_NS + ".preferences.server.version";
	
	private static final String P_EVAL_SERVER_DEFAULT = PLUGIN_NS + ".preferences.server.default";
	
	private static final String P_CONFIG_EXTERN = PLUGIN_NS + ".preferences.config.extern";
	private static final String P_CONFIG_EXECUTABLE = PLUGIN_NS + ".preferences.config.executable";
	private static final String P_CONFIG_LOGGING = PLUGIN_NS + ".preferences.config.logging";
	private static final String P_CONFIG_LOGGING_SEVERITY = PLUGIN_NS + ".preferences.config.logseverity";
	private static final String P_CONFIG_EXECUTABLE_FILE = PLUGIN_NS + ".preferences.config.executable.file";
	
	/**
	 * Unpassender Wert, sollte besser .preferences.config.commands heissen (seit Version 1.0.5 Verhalten geaendert)
	 */
	private static final String P_COMMANDS = PLUGIN_NS + ".preferences.config.nonexecutable.commands";
	
	
	/**
	 * Liefert <code>true</code> wenn die Property die Outline
	 * betrifft.
	 * @param property
	 * @return
	 */
	public static boolean affectsOutline(final String property)
	{
		return OUTLINE_HIDE_SUBLEVEL_FORMS.equals(property) ||
			   OUTLINE_SHOW_TYPES_ON_TOPLEVEL.equals(property) ||
			   OUTLINE_SHOW_TYPES_ON_SUBLEVEL.equals(property) ||
			   OUTLINE_SORT_BY_NAME.equals(property) ||
			   OUTLINE_SORT_BY_TYPE.equals(property);
	}
	
	/**
	 * Initialisiert den PreferenceStore mit den Default-Werten.
	 * @param store
	 */
	public static void initDefaultValues(final IPreferenceStore store)
	{
		store.setDefault(OUTLINE_HIDE_SUBLEVEL_FORMS, false);
		store.setDefault(OUTLINE_SORT_BY_NAME, false);
		store.setDefault(OUTLINE_SORT_BY_TYPE, false);
		
		Set<TSExpression> defaultVisibleTypesTopLevel = 
			EnumSet.allOf(TSExpression.class);
		store.setDefault(OUTLINE_SHOW_TYPES_ON_TOPLEVEL, encodeOutlineVisibleTypes(defaultVisibleTypesTopLevel));
		
		Set<TSExpression> defaultVisibleTypeSubLevel = EnumSet.of(
				TSExpression.DEFMACRO,
				TSExpression.DEFUN,
				TSExpression.INPACKAGE,
				TSExpression.LAMBDA,
				TSExpression.DEFPACKAGE);
		store.setDefault(OUTLINE_SHOW_TYPES_ON_SUBLEVEL, encodeOutlineVisibleTypes(defaultVisibleTypeSubLevel));
		
		store.setDefault(P_UPPER_CASE_PROPOSAL, false);
	}
	
	/**
	 * Erzeugt einen String aus den Sichtbaren Typen der Outline der in den Preferences abgespeichert werden
	 * kann.
	 * @param types - Menge von Typen die angezeigt werden.s
	 * @return Der kodierte String
	 * @see LispPreferences#decodeOutlineVisibleTypes(String)
	 * testcase
	 */
	public static String encodeOutlineVisibleTypes(final Set<TSExpression> types)
	{
		boolean first = true;
		StringBuilder encoded = new StringBuilder();
		for(TSExpression typ : types) {
			if( first ) {
				first = false;
			} else {
				encoded.append(";");
			}
			
			encoded.append(typ.name());
		}
		return encoded.toString();
	}
	
	/**
	 * Dekodiert einen abgespeicherten String von sichtbaren Typen zurueck
	 * in die Typmenge.
	 * @param encodedTypes - Der kodierte String
	 * @return Menge von Typen, aus String dekodiert
	 * @see LispPreferences#encodeOutlineVisibleTypes(Set)
	 * testcase
	 */
	public static Set<TSExpression> decodeOutlineVisibleTypes(final String encodedTypes)
	{
		StringTokenizer tokenizer = new StringTokenizer(encodedTypes, ";");
		Set<TSExpression> decodedTypes = EnumSet.noneOf(TSExpression.class);

		while( tokenizer.hasMoreElements() ) {
			String entry = tokenizer.nextToken();
			try { 
				TSExpression typ = TSExpression.valueOf(entry);
				decodedTypes.add(typ);
			} catch (IllegalArgumentException e) { //ungueltige Eintraege in Preference ignorieren
				LispPluginActivator.log(IStatus.WARNING, "Unkown entry in outline visible types preference, ignoring", e);
			}
		}
		
		return decodedTypes;
	}
	
	/**
	 * Speichert die uebergebene Liste der EvalServer in den Preferences ab.
	 * Es werden keine Listener ueber die Aenderungen in den Preferences benachrichtigt.
	 * @param servers
	 * @see LispPreferences#loadEvalServer()
	 */
	public static void storeEvalServer(final Map<IEnvironment, IEnvironmentConfiguration> servers)
	{
		IPreferenceStore store = LispPluginActivator.getDefault().getPreferenceStore();
		store.putValue(P_EVAL_SERVER_COUNT, Integer.toString(servers.size()));
		
		int cnt = 0;
		for(Entry<IEnvironment, IEnvironmentConfiguration> entry : servers.entrySet()) {
			storeServerInPreferences(cnt, entry.getKey(), store);
			storeConfigInPreferences(entry.getKey(), entry.getValue(), store);
			cnt++;
		}
	}
	
	private static void storeConfigInPreferences(final IEnvironment server, final IEnvironmentConfiguration config, final IPreferenceStore store)
	{
		String serverID = ".."+Integer.toString(server.hashCode());
		store.putValue(P_CONFIG_EXTERN+serverID, Boolean.toString(config.isExtern()));
		if(!config.isExtern()) {
			store.putValue(P_CONFIG_EXECUTABLE+serverID, Boolean.toString(config.isExecutable()));
			store.putValue(P_CONFIG_LOGGING+serverID, Boolean.toString(config.isLoggingEnabled()));
			if(config.isLoggingEnabled()) {
				store.putValue(P_CONFIG_LOGGING_SEVERITY+serverID, config.getLogSeverity().name());
			}
			if(config.isExecutable()) {
				store.putValue(P_CONFIG_EXECUTABLE_FILE+serverID, config.getEvalServerExecutable().toString());
			}
			//in jedem Fall Kommands abspeichern (falls vorhanden)
			store.putValue(P_COMMANDS+serverID, flattenList(config.getCommands()));
			/*} else {
				store.putValue(P_CONFIG_NONEXECUTABLE_COMMANDS+serverID, flattenList(config.getNonExecutableCommands()));
			}*/
		}
	}
	
	/**
	 * Speichert den Hashwert des Default-Servers in den Preferences.
	 * @param serverHash
	 */
	public static void storeDefaultServer(final Integer serverHash)
	{
		IPreferenceStore store = LispPluginActivator.getDefault().getPreferenceStore();
		store.putValue(P_EVAL_SERVER_DEFAULT, Integer.toString(serverHash));
	}
	
	private static String flattenList(final List<String> list)
	{
		StringBuilder builder = new StringBuilder();
		for(String string : list) {
			builder.append(string);
			builder.append(" ");
		}
		return builder.toString();
	}
	
	private static void storeServerInPreferences(final int item, final IEnvironment server, final IPreferenceStore store)
	{
		String id = ".."+item;
		store.putValue(P_EVAL_SERVER_HOST+id, server.getHost());
		store.putValue(P_EVAL_SERVER_NAME+id, server.getName());
		store.putValue(P_EVAL_SERVER_PORT+id, Integer.toString(server.getPort()));
		store.putValue(P_EVAL_SERVER_VERSION+id, server.getVersion());
	}
	
	/**
	 * Laedt die Eval-Server und deren Konfiguration aus den Preferences.
	 * @return Die geladenen Server und Konfigurationen.
	 */
	public static Map<IEnvironment, IEnvironmentConfiguration> loadEvalServer()
	{
		IPreferenceStore store = LispPluginActivator.getDefault().getPreferenceStore();
		int cnt = store.getInt(P_EVAL_SERVER_COUNT);
		Map<IEnvironment, IEnvironmentConfiguration> servers = new Hashtable<IEnvironment, IEnvironmentConfiguration>();
		
		for(int i=0;i<cnt;i++) {
			IEnvironment server = loadServerFromPreferences(i, store);
			IEnvironmentConfiguration config = loadConfigFromPreferences(server, store);
			servers.put(server, config);
		}
		
		return servers;
	}
	
	/**
	 * Laedt den Hash-Wert des Default-Server.
	 * @return Hash-Wert Default-Server
	 */
	public static Integer loadDefaultServer()
	{
		IPreferenceStore store = LispPluginActivator.getDefault().getPreferenceStore();
		if( !store.contains(P_EVAL_SERVER_DEFAULT) ) {
			return null;
		}
		return store.getInt(P_EVAL_SERVER_DEFAULT);
	}
	
	private static IEnvironment loadServerFromPreferences(final int item, final IPreferenceStore store)
	{
		String id = ".."+item;
		String name = store.getString(P_EVAL_SERVER_NAME+id);
		String host = store.getString(P_EVAL_SERVER_HOST+id);
		int port = store.getInt(P_EVAL_SERVER_PORT+id);
		String version = store.getString(P_EVAL_SERVER_VERSION+id);
		
		return new Environment(host, port, name, version);
	}
	
	private static IEnvironmentConfiguration loadConfigFromPreferences(final IEnvironment server, final IPreferenceStore store)
	{
		String id = ".." + server.hashCode();
		boolean extern = store.getBoolean(P_CONFIG_EXTERN+id);
		if(extern) {
			return EnvironmentConfiguration.instanceOfExtern();
		}
		
		//invariante: nicht extern
		
		boolean logging = store.getBoolean(P_CONFIG_LOGGING+id);
		boolean executable = store.getBoolean(P_CONFIG_EXECUTABLE+id);
		TLogSeverity severity = null;
		if(logging) {
			severity = TLogSeverity.valueOf(store.getString(P_CONFIG_LOGGING_SEVERITY+id));
		}
		
		List<String> commands = deflatenList(store.getString(P_COMMANDS+id));
		if(executable) {
			File execFile = new File(store.getString(P_CONFIG_EXECUTABLE_FILE+id));
			return EnvironmentConfiguration.instanceOfExecutable(logging, severity, execFile, commands);
		} 

		//invariante: nicht executable
		return EnvironmentConfiguration.instanceofNonExecutable(logging, severity, commands);
	}
	
	private static List<String> deflatenList(final String flattenedString)
	{
		return Arrays.asList(flattenedString.split("\\s+"));
	}
}
