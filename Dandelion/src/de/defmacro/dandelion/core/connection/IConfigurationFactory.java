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

package de.defmacro.dandelion.core.connection;

import java.io.File;
import java.util.List;

/**
 * Klassen die dieses Interface implementieren 
 * stellen die Konfiguratation für die Verwendung am Erweiterungspunkt <code>de.fh_trier.eclipse.lisp.servers</code>
 * bereit.
 * 
 * Seit Version 1.1 veraendertes Verhalten!
 * @author Michael Bohn
 * @since 1.0
 * @version 1.0.5
 */
public interface IConfigurationFactory 
{		
	/**
	 * Die Umgebung wird ueber die zurueckgelieferte ausfuehrbare Datei gestartet.
	 * Es wird zuerst bei der Factory nachgefragt ob eine
	 * Executable-Datei bereitgestellt wird. Wird hier
	 * <code>null</code> zurueckgegeben, wird die Factory ueber die getCommands()-Methode
	 * aufgefordert eine Liste von Kommadoparmetern zu erzeugen um den 
	 * Server zu starten.
	 * <br />
	 * startParam sind Parameter zwischen Executable und Environment-Parametern:<br />
	 * &lt;exectuable&gt;&lt;startParam&gt;&lt;env-param&gt;
	 * @param startParams - zusaetzliche Startparameter fuer Executable, Parameter zwischen Dateiname und Environment-Paramentern
	 * @return Ein {@link File}-Objekt zur ausfuehrbaren Datei oder <code>null</code> wenn nur Konsolenparameter verwendet werden
	 * @see IConfigurationFactory#getCommands()
	 */
	public File getExecutableFile();
	
	/**
	 * Parameter fuer die Executable (vor Enivronment Parametern) oder alleinige Parameter
	 * um Server zu starten (z.B. Konsolenparameter: sbcl --noinform --image /path/to/image.core).
	 * @return Eine Liste von Kommandos
	 * @see IConfigurationFactory#getExecutableFile()
	 */
	public List<String> getCommands();
}
