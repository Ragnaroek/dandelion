package de.defmacro.dandelion.core.connection;

import java.io.*;
import java.net.URL;

import org.eclipse.core.runtime.*;

public abstract class AbstractConfigurationFactory 
implements IConfigurationFactory 
{
	protected File loadFromProject(final Plugin plugin, final String pluginID, final String filespec)
	{
		URL url = FileLocator.find(plugin.getBundle(), new Path(filespec), null);
		if(url == null) {
			plugin.getLog().log(new Status(IStatus.ERROR, pluginID, 0, "File " + filespec + " could not be found", null));
			throw new RuntimeException();
		}
		try {
			url = FileLocator.toFileURL(url);
		} catch (IOException e) {
			plugin.getLog().log(new Status(IStatus.ERROR, pluginID, 0, "Error creating executable file", e));
			throw new RuntimeException(e);
		}
		File file = new File(url.getFile());
		return file.getAbsoluteFile();
	}
}
