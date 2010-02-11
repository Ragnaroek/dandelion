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

package de.defmacro.dandelion.internal.ui.dialogs;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

import de.defmacro.dandelion.internal.ui.*;
import de.defmacro.dandelion.internal.ui.text.TextUtilities;
import de.defmacro.dandelion.internal.ui.views.listener.EvalHistory.HistoryEntry;

/**
 * Der Dialog fuer Auswahl der Historieneintraege
 * des Listeners.
 * @author Michael Bohn
 *
 */
public class EvalHistorySelectionDialog 
extends ElementListSelectionDialog 
{
	private static class HistoryLabelProvider
	extends LabelProvider
	{
		@Override
		public Image getImage(final Object element) 
		{
			return LispUI.getUIImageManager().get(UIImageConstants.ICON_HISTORY);
		}

		@Override
		public String getText(final Object element) 
		{
			HistoryEntry entry = (HistoryEntry)element;
			return TextUtilities.getTimePresentation(entry.getTimestamp()) + " - " +
				entry.getShortedForm(100, true);
		}
	}
	
	/**
	 * Erzeugt einen neuen Dialog fuer die Auswahl von Historieneintraegen des Listeners.
	 * @param shell - Shell an der Dialog model geoffnet wird
	 */
	public EvalHistorySelectionDialog(final Shell shell)
	{
		super(shell, new HistoryLabelProvider());
		this.setMultipleSelection(false);
		this.setBlockOnOpen(true);
		this.setMessage("Choose a history entry:");
		this.setEmptyListMessage("No history entry");
		this.setEmptySelectionMessage("Select a history entry");
		setSize(90, 25);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Control superControl = super.createDialogArea(parent);
		validateCurrentSelection();
		return superControl;
	}
}
