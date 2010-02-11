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

package de.defmacro.dandelion.internal.ui.views.macroexpand;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.core.connection.IResult.TResult;
import de.defmacro.dandelion.internal.ui.LispUI;
import de.defmacro.dandelion.internal.ui.text.presentation.LispSourcePresentationManager;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

@SuppressWarnings("UwF")
public class MacroexpandView 
extends ViewPart
implements IBackgroundEvaluationListener
{
	public static final String ID = "de.fh_trier.eclipse.lisp.views.macroexpandView";
	
	private static final Color BACKGROUND_COLOR = LispUI.getUIColorManager().getColor(new RGB(255, 255, 225));
	
	private SourceViewer fSourceViewer;
	private IDocument fDocument;
	
	@Override
	public void createPartControl(final Composite parent) {
		fSourceViewer = new SourceViewer(parent, null, SWT.V_SCROLL);
		fDocument = new Document();
		
		fSourceViewer.getTextWidget().setBackground(BACKGROUND_COLOR);
		LispSourcePresentationManager.installMatchingCharacterPainter(fSourceViewer);
		LispSourcePresentationManager.installSourceViewSupport(fSourceViewer, fDocument);
	}

	@Override
	public void setFocus() {
		fSourceViewer.getControl().setFocus();
	}

	public void finishEval() {
		//no-op
	}

	public IRestartSelection formEvaluated(final IResult result, final boolean more) {
		if(result.getTyp() != TResult.SUCCESS) {
			return RestartSelection.ABORT_SELECTION;
		}
		fDocument.set(result.getResult());
		return null;
	}

	public void handleOutput(final String output) {
		//no-op
		
	}

	public void prepareEval(final IConnection connectin, boolean bulk) {
		//no-op
	}

	public Display syncExecOnDisplay() {
		return PlatformUI.getWorkbench().getDisplay();
	}
}
