package de.defmacro.dandelion.env.sbcl.win32.x86;

import java.io.File;
import java.util.*;

import de.defmacro.dandelion.core.connection.*;

public class Configuration 
extends AbstractConfigurationFactory
{
	public List<String> getCommands() 
	{
		List<String> list = new ArrayList<String>(1);
		list.add("--noinform");
		return list;
	}

	public File getExecutableFile() 
	{	
		return loadFromProject(Activator.getDefault(), Activator.PLUGIN_ID, "/binary/environment_sbcl_2.0.0.exe");
	}
}
