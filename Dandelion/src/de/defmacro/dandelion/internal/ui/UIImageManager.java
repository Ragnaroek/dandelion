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

package de.defmacro.dandelion.internal.ui;

import org.eclipse.jface.resource.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

import de.defmacro.dandelion.internal.*;
import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.core.meta.TMetaType;

/**
 * Cache fuer Bilder. Bietet ausserdem Hilfmethoden und Hilfsobjekte
 * fuer die Verwendung von Bildern im Plugin.
 * @author Michael Bohn
 *
 */
public class UIImageManager
implements IDisposable
{	
	private static Case<Image> fImageCase = null;
	
	//subclassing nicht empfohlen fuer ImageRegistry
	private ImageRegistry fRegistry;
	private String        fBase;
	
	/**
	 * Erzeugt einen neuen Bildmanager.
	 * Die Dateipfad in der {@link UIImageManager#get(String)}-Methode sind relativ
	 * zur uebergeben Base-Angabe.
	 * @param base - Basisordner aus dem die Dateien geladen werden.
	 * @throws NullPointerException - wenn base == <code>null</code>
	 */
	public UIImageManager(String base)
	{	
		if( !base.endsWith("/") ) {
			base = base + "/";
		}	
		
		this.fRegistry = new ImageRegistry(Display.getCurrent());
		this.fBase = base;
	}
	
	/**
	 * Liefert das Bildobjekt fuer die angegebene Datei.
	 * Die Datei wird in den Speicher geladen wenn sie sich noch
	 * nicht im Cache befindet.
	 * @param file - Bilddatei
	 * @return - Das Bildobjekt der Datei
	 */
	public Image get(final String file) 
	{	
		Image img = fRegistry.get(file);
		
		if(img == null) {
			getDescriptor(file);
			img = fRegistry.get(file);
			System.out.println("img="+img);
			assert(img != null);
		}
		
		return img;
	}
	
	/**
	 * Liefert einen {@link ImageDescriptor} fuer die angegebene Datei.
	 * @param file - Bilddatei
	 * @return Den {@link ImageDescriptor} der Datei
	 */
	public ImageDescriptor getDescriptor(final String file) 
	{
		ImageDescriptor descriptor = fRegistry.getDescriptor(file);
		
		if(descriptor == null) {
			descriptor = getImageDescriptor(file);
			fRegistry.put(file, descriptor);
		}
		
		return descriptor;
	}

	private ImageDescriptor getImageDescriptor(final String file)
	{
		return LispPluginActivator.getImageDescriptor(fBase + file);
	}
	
	/**
	 * Liefert die Singleton-Instanz de {@link SExpressionImageCase}.
	 * @return Die Fallunterscheidnung fuer Bild der Typen.
	 */
	public Case<Image> getSExpressionImageCase()
	{
		if( fImageCase == null) {
			fImageCase = new SExpressionImageCase(this);
		}
		
		return fImageCase;
	}
	
	/**
	 * Liefert das Bild fuer einen bestimmten {@link TMetaType}.
	 * @param type - Der Meta-Typ
	 * @return Das Bildobjekt fuer diesen Meta-Typ
	 */
	public Image getImageForMetaType(final TMetaType type) {
		switch(type) {
			case FUNCTION     : return get(UIImageConstants.ICON_DEFUN);
			case MACRO 		  : return get(UIImageConstants.ICON_DEFMACRO);
			case LOCAL_SYMBOL : return get(UIImageConstants.ICON_SYMBOL_PRIVATE);
			case PACKAGE      : return get(UIImageConstants.ICON_PACKAGE);
			default : return null;
		}
	}
	
	/**
	 * Entsorgt den Bildmanager.
	 */
	@Deprecated
	public void dispose()
	{
		//wird automatisch an der registry aufgerufen wenn das Eclipse UI runterfaehrt
		//fRegistry.dispose();
	}
}
