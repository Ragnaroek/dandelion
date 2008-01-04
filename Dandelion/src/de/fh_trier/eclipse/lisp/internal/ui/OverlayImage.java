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

package de.fh_trier.eclipse.lisp.internal.ui;

import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.swt.graphics.*;

/**
 * Legt ein weiteres Bild ueber ein bereits
 * vorhandenes.
 * @author Michael Bohn
 *
 */
public class OverlayImage 
extends CompositeImageDescriptor 
{
	private Image fBaseImage;
	private String fOverlayImageID;
	private Point  fSize;
	
	/**
	 * Erzeugt ein neues Bild.
	 * @param baseImage - Das Bild ueber das ein weiteres gelegt werden soll
	 * @param overlayImageID - Der Key des Bildes auf dem {@link UIImageManager} das ueber das Bild gelegt werden soll.
	 */
	public OverlayImage(final Image baseImage, final String overlayImageID)
	{
		if (baseImage == null) {
			throw new NullPointerException("baseImage must not be null");
		}
		
		if (overlayImageID == null) {
			throw new NullPointerException("overlayImageID must not be null");
		}

		this.fBaseImage = baseImage;
		this.fOverlayImageID = overlayImageID;
		this.fSize = new Point(fBaseImage.getBounds().width,
					           fBaseImage.getBounds().height);
	}
	
	@Override
	protected void drawCompositeImage(int width, int height) 
	{
		drawImage(fBaseImage.getImageData(), 0, 0); 
		
		Image overlayImage = LispUI.getUIImageManager().get(fOverlayImageID);
		ImageData imageData = overlayImage.getImageData();
		
		//unten links
		drawImage(imageData, 0, fSize.y - imageData.height);
	}

	@Override
	protected Point getSize() {
		return fSize;
	}
	
	/**
	 * Liefert das neue Bild.
	 * Der Aufrufer ist fuer die Entsorgung des erzeugten Bildes verantwortlich.
	 * @return Das neue Bild
	 */
	public Image getImage()
	{
		return createImage();
	}
}
