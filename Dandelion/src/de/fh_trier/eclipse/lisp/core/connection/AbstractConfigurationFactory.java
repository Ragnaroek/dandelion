package de.fh_trier.eclipse.lisp.core.connection;

import java.io.*;
import java.net.URL;

import org.eclipse.core.runtime.*;

public abstract class AbstractConfigurationFactory 
implements IConfigurationFactory 
{
	protected File loadFromProject(final Plugin plugin, final String pluginID, final String filespec)
	{
		URL url = FileLocator.find(plugin.getBundle(), new Path(filespec), null);
		try {
			url = FileLocator.toFileURL(url);
		} catch (IOException e) {
			plugin.getLog().log(new Status(IStatus.ERROR, pluginID, 0, "Error creating executable file", e));
		}
		File file = new File(url.getFile());
		return file;
	}
}
