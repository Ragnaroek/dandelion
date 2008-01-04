package de.fh_trier.eclipse.sbcl.win32.x86;

import java.io.File;
import java.util.*;
import de.fh_trier.eclipse.lisp.core.connection.*;

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
		return loadFromProject(Activator.getDefault(), Activator.PLUGIN_ID, "/lib/eval-server.exe");
	}
}
