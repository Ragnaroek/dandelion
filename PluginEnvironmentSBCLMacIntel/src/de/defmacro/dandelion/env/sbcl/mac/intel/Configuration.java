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

package de.defmacro.dandelion.env.sbcl.mac.intel;

import java.io.*;
import java.util.List;

import de.defmacro.dandelion.core.connection.*;
import de.defmacro.dandelion.env.sbcl.mac.intel.Activator;

public class Configuration 
extends AbstractConfigurationFactory
{
	/**
	 * Keine Kommandos.
	 */
	public List<String> getCommands() {
		return null;
	}

	/**
	 * Liefert den Pfad zur Binaerdatei.
	 */
	public File getExecutableFile() 
	{
		return loadFromProject(Activator.getDefault(), Activator.PLUGIN_ID, "/binary/environment_sbcl_1.0.33");
	}
}
