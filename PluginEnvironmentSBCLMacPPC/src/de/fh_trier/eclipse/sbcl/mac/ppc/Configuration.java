package de.fh_trier.eclipse.sbcl.mac.ppc;

import java.io.File;
import java.util.*;

import de.fh_trier.eclipse.lisp.core.connection.AbstractConfigurationFactory;

public class Configuration 
extends AbstractConfigurationFactory 
{
	public List<String> getCommands() {
		List<String> commands = new ArrayList<String>();
		commands.add("--noinform");
		return commands;
	}

	public File getExecutableFile() {
		return loadFromProject(Activator.getDefault(), Activator.PLUGIN_ID, "/lib/eval-server_1.0.2_05");
	}
}
