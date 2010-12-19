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

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IDecoratorManager;

import de.defmacro.dandelion.internal.LispPluginActivator;

/**
 * Ein Dekorator der Bilder im Warnungs- oder
 * Fehlerymbolen dekoriert.
 * @author Michael Bohn
 *
 */
public class LispResourceDecorator 
extends LabelProvider 
implements ILabelDecorator 
{
	/**
	 * Eindeutige ID des Decorators aus Manifest.
	 */
	public static final String ID = LispPluginActivator.NS + ".decorators.resourceDecorator";
	
	/**
	 * Der Schluessel fuer die Markierung einer Ressource.
	 * Wird gesetzt um Fehler in der Ressource anzuzeigen. Der Decorator legt dann ein enstprechendes Bild
	 * ueber das Ressourcen-Bild.
	 */
	public static final QualifiedName MALFORMATION_MARK = new QualifiedName(LispPluginActivator.NS,"__malformation");
	
	/**
	 * Wert fuer Markierung Ressource um Anzuzeigen das sie Fehler enthaelt.
	 */
	public static final String ERROR = "__error";
	
	/**
	 * Wert fuer Markierung Ressource um Anzuzeigen das sie Warnnungen enthaelt.
	 */
	public static final String WARNING = "__warning";
	
	/**
	 * Legt das entsprechende Bild ueber das Ressourcen-Bild.
	 * Das dekorierende Bild ist abhaengig vom Wert der Schluessel MALFORMATION_MARK in den
	 * Property der Ressource. Wird kein Ressourcen-Objekt uebergeben tut diese Methode nichts.
	 */
	public Image decorateImage(Image image, Object element) 
	{
		if( !(element instanceof IResource) ) {
			return null;
		}
	
		IResource resource = (IResource)element;
		if( !resource.exists() ) {
			return null;
		}
		
		String malformationMark = null;
		try {
			if(resource instanceof IProject) {
				IProject project = (IProject)resource;
				//keine Markierung fuer geschlossene, getSessionProperty wirft Exception fuer geschlossene Projekte
				if( !project.isOpen() ) {
					return null;
				}
			}
			//invariante : instanceof IResource && Ressource existiert && IProject ist offen
			malformationMark = (String)resource.getSessionProperty(MALFORMATION_MARK);
		} catch (CoreException e) {
			LispPluginActivator.logError("error retrieving malformation mark", e);
			return null;
		}
		
		//TODO Bilder cachen
		if(ERROR.equals(malformationMark)) {
			return new OverlayImage(image, UIImageConstants.OVERLAY_ERROR).getImage();
		} else if(WARNING.equals(malformationMark)) {
			return new OverlayImage(image, UIImageConstants.OVERLAY_WARNING).getImage();
		} else {
			return null;
		}
	}

	/**
	 * Ungenutzt.
	 */
	public String decorateText(String text, Object element) {
		//no-op
		return null;
	}
	
	/**
	 * Liefert die Singleton-Instanz de Decorators aus dem DecoratorManager der Workbench.
	 * @return Der Dekorator fuer die Ressourcen
	 */
	public static LispResourceDecorator getDecorator()
	{
		IDecoratorManager decoratorManager = LispPluginActivator.getDefault().getWorkbench().getDecoratorManager();
		if ( decoratorManager.getEnabled(ID) ) {
			return (LispResourceDecorator) decoratorManager.getLabelDecorator(ID);
		}
		return null;
	}
	
	/**
	 * Aktualisiert die Bild der uebergeben Ressourcen.
	 * @param resources
	 */
	public void refresh(final IResource[] resources)
	{		
		fireLabelProviderChanged(new LabelProviderChangedEvent(this, resources));
	}
}
