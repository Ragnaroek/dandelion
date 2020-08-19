package de.defmacro.dandelion.env.sbcl.linux.x86;

import java.io.File;
import java.util.*;

import de.defmacro.dandelion.core.connection.AbstractConfigurationFactory;

public class Configuration 
extends AbstractConfigurationFactory 
{
	public List<String> getCommands() 
	{
		List<String> list = new ArrayList<String>(1);
		list.add("--noinform");
		list.add("--dynamic-space-size"); //since 1.0.40 we need more heap
		list.add("768");
		return list;
	}

	public File getExecutableFile() {
		return loadFromProject(Activator.getDefault(), Activator.PLUGIN_ID, "/binary/environment_sbcl_2.0.1");
	}
}
