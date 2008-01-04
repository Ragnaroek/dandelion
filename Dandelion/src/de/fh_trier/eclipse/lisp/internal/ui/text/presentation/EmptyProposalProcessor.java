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

package de.fh_trier.eclipse.lisp.internal.ui.text.presentation;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.*;

/**
 * Der Processor macht nichts.
 * Implementierung fuer Umgehung Bug in Eclipse.
 * @author Michael Bohn
 *
 */
public class EmptyProposalProcessor 
implements IContentAssistProcessor 
{
	private static ICompletionProposal[] EMPTY_PROPOSAL = new ICompletionProposal[0];
	private static IContextInformation[] EMPTY_CONTEXT = new IContextInformation[0];
	private static char[] EMPTY_CHAR = new char[0];

	public ICompletionProposal[] computeCompletionProposals(final ITextViewer viewer, final int offset)
	{
		return EMPTY_PROPOSAL;
	}

	public IContextInformation[] computeContextInformation(final ITextViewer viewer, final int offset) 
	{
		return EMPTY_CONTEXT;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		return EMPTY_CHAR;
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return EMPTY_CHAR;
	}

	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	public String getErrorMessage() {
		return null;
	}
}
