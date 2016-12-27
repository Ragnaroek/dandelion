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

import java.io.File;
import java.util.List;

/**
 * Die Konfiguration einer Lisp-Umgebung.
 * @author Michael Bohn
 * @since 1.0
 * @version 1.0.5
 */
public interface IEnvironmentConfiguration
{
	/**
	 * Schweregrad der Log-Meldungen.
	 * @author Michael Bohn
	 */
	public enum TLogSeverity
	{
		DEBUG,
		INFO,
		WARNING,
		ERROR,
		FATAL;
		
		/**
		 * Der Default-Logschweregrad.
		 * value= {@link TLogSeverity#ERROR}
		 */
		public static final TLogSeverity DEFAULT_SEVERITY = TLogSeverity.ERROR;
	}
	
	/** 
	 * Gibt <code>true</code> zurueck wenn der
	 * Eval-Server als ausfuehrbare Datei vorliegt.
	 * @return <code>true</code> wenn Umgebung als executable vorliegts
	 */
	public boolean isExecutable();
	
	/**
	 * Liefert die ausfuehrbare Datei die gestartet werden soll.
	 * <code>null</code> wenn {@link IEnvironmentConfiguration#isExecutable()} <code>false</code> liefert.
 	 * @return Die ausuehrbare Datei oder <code>null</code>
	 */
	public File getEvalServerExecutable();	
	
	/**
	 * Liefert eine Liste der Parameter zum Start der Lisp-Umgebung.
	 * Liegt der Eval-Server als ausfuehrbare Datei
	 * vor, werden die Parameter der ausfuehrbaren Datei uebergeben (vor den Environment Parametern)
	 * 
	 * @return Liste von Parameter oder <code>null</code> wenn keine Parameter vorhanden
	 */
	public List<String> getCommands();
	
    /**
     * Externe Konfiguration des Servers. 
     * Bedeutet das der Server  extern konfiguriert wurde und bereits laueft. Wird hier <code>true</code>
     * zurueckgegeben geben alle anderen get... - Methode <code>null</code> zrueck. 
     * @return <code>true</code> wenn Server extern konfiguriert.
     */
	public boolean isExtern();
	
	/**
	 * Logging fuer den Server eingeschaltet.
	 * @return <code>true</code> wenn Logging im Server eingeschaltet ist.
	 */
	public boolean isLoggingEnabled();
	
	/**
	 * Liefert den Schweregrad der Log-Meldung.
	 * @return Schweregrad der Log-Meldung.
	 * @see TLogSeverity
	 */
	public TLogSeverity getLogSeverity();
	
	/**
	 * Server beim Beenden von Eclipse nicht beenden.
	 * Methode fuer spaetere Verwendung, liefert bisher immer <code>false</code>.
	 * @return <code>true</code> wenn Server nicht beendet werden soll.
	 */
	public boolean keepAlive();
}
