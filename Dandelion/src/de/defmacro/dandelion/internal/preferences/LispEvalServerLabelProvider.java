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

package de.defmacro.dandelion.internal.preferences;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;

import de.defmacro.dandelion.internal.core.connection.*;

/**
 * Der {@link LabelProvider} fuer die Darstellung
 * der Eval-Server in der Preference-Liste.
 * @author Michael Bohn
 */
public class LispEvalServerLabelProvider 
extends LabelProvider
implements ITableLabelProvider
{
	private static final String DEFAULT = "(default)";
	
	private IEnvironment fDefaultServer;
	
	/**
	 * Erstellt einen neuen LableProvider fuer die Eval-Server Liste.
	 * @param defaultServer - Server der als Default angezeigt werden soll
	 */
	public LispEvalServerLabelProvider(final IEnvironment defaultServer)
	{
		this.fDefaultServer = defaultServer; 
	}
	
	/**
	 * @see ITableLabelProvider#getColumnImage(Object, int)
	 */
	public Image getColumnImage(final Object element, final int columnIndex) 
	{
		return null;
	}

	/**
	 * @see ITableLabelProvider#getColumnText(Object, int)
	 */
	public String getColumnText(final Object element, final int index) 
	{
		if( !(element instanceof IEnvironment) ) {
			return null;
		}
		
		IEnvironment server = (IEnvironment)element;
		switch(index) {
		case 0 : return serverName(server);
		case 1 : return server.getHost();
		case 2 : return Integer.toString(server.getPort());
		case 3 : return server.getVersion();
		}
		return null;
	}
	
	private String serverName(final IEnvironment server) 
	{
		//defaultServer kann null sein
		if(server.equals(fDefaultServer)) {
			return server.getName() + " " + DEFAULT;
		}
		return server.getName();
	}
	
	/**
	 * Setzt einen neuen Server der als Default angezeigt wird.
	 * Der Aufrufer ist fuer ein Refresh der Tabelle verantwortlich damit 
	 * die Aenderung angezeigt wird.
	 * @param newDefault
	 */
	public void setNewDefault(final IEnvironment newDefault)
	{
		this.fDefaultServer = newDefault;
	}
}
