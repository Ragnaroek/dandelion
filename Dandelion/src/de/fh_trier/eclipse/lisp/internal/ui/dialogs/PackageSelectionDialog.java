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

package de.fh_trier.eclipse.lisp.internal.ui.dialogs;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.*;

import de.fh_trier.eclipse.lisp.internal.ui.*;

/**
 * Der Dialog fuer Auswahl eines Paketes.
 * @author Michael Bohn
 *
 */
public class PackageSelectionDialog 
extends ElementListSelectionDialog
{
	private static class PackageSelectionDialogLabelProvider
	extends LabelProvider
	{
		private Image fImage;
		
		public PackageSelectionDialogLabelProvider() {
			this.fImage = LispUI.getUIImageManager().get(UIImageConstants.ICON_PACKAGE);
		}
		
		@Override
		public Image getImage(Object element) {
			return fImage;
		}
	}
	
	/**
	 * Erzeugt einen neuen Dialog fuer die Auswahl eines Paketes.
	 * @param shell - Shell an der Dialog modal angezeigt wird
	 */
	public PackageSelectionDialog(final Shell shell)
	{
		super(shell, new PackageSelectionDialogLabelProvider());
		this.setBlockOnOpen(true);
		this.setMultipleSelection(false);
		this.setMessage("Choose a package:");
		this.setEmptyListMessage("No package available");
		this.setEmptySelectionMessage("No package selected");
		this.setSize(100, 25); //das sind keine pixel angaben
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Control superControl = super.createDialogArea(parent);
		validateCurrentSelection();
		return superControl;
	}
}
