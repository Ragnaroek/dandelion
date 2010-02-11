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

package de.defmacro.dandelion.internal.ui.text.presentation;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPartitioningException;
import org.eclipse.jface.text.DefaultTextDoubleClickStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.swt.graphics.Point;

import de.defmacro.dandelion.internal.LispPluginActivator;
import de.defmacro.dandelion.internal.ui.text.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Selektiert einen Bereich wenn eine Klammer doppelgeklickt
 * wurde.
 * @author Michael Bohn
 *
 */
/*package*/ class LispDoubleClickStrategy 
implements ITextDoubleClickStrategy 
{
	private static LispDoubleClickStrategy instance;
	@SuppressWarnings("UwF") //wird mit instance initialsiert
	private static ITextDoubleClickStrategy defaultStrategy;
	
	private LispDoubleClickStrategy()
	{
		//direkte Objekterstellung nicht erlaubt
	}
	
	/**
	 * Im Dokument wurd doppelgeklickt.
	 * Es wird der entsprechende Klammerbereich im Viewer selektiert.
	 */
	public void doubleClicked(ITextViewer viewer) 
	{
		if(viewer == null) return;
		
		Point offset 	= viewer.getSelectedRange();
		IDocument doc 	= viewer.getDocument();
		
		char beforeCaret = 0;
		char atCaret = 0;
		
		try {
			if(offset.x-1 >= 0) {
				beforeCaret = doc.getChar(offset.x-1);
			}
			if(offset.x < doc.getLength()) {
				atCaret = doc.getChar(offset.x);
			}
			
			Boolean direction = SourceUtilities.resolveSearchDirection(beforeCaret, atCaret);
			if(direction == null) { //unentscheidbar
				delegateToDefault(viewer);
				return;
			}
			
			int pos = SourceUtilities.translateOffset(beforeCaret, atCaret, offset.x);
			IRegion region = SourceUtilities.findMatchingParenthesis(viewer.getDocument(), 
					pos, direction);
			if(region == null) {//kein passender char gefunden
				delegateToDefault(viewer);
				return;
			}
			viewer.setSelectedRange(region.getOffset(), region.getLength());
			
		} catch (BadLocationException e) {
			LispPluginActivator.logError("broken invariant: Character not available in Document", e);
			return;
		} catch (BadPartitioningException e) {
			LispPluginActivator.logError("Error while retrieving Partitiontype", e);
			return;
		}	
	}
	
	private void delegateToDefault(ITextViewer viewer)
	{
		defaultStrategy.doubleClicked(viewer);
	}
	
	/**
	 * Factory-Methode fuer LispDoubleClickStrategy.
	 * @return LispDoubleClickStrategy - Singleton-Instanz
	 */
	public static ITextDoubleClickStrategy instanceOf()
	{
		if(instance == null) {
			instance = new LispDoubleClickStrategy();
			defaultStrategy = new DefaultTextDoubleClickStrategy();
		}
		return instance;
	}
}
