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

package de.defmacro.dandelion.internal.ui.views.listener;

import de.defmacro.dandelion.internal.ui.views.listener.EvalHistory.HistoryEntry;

public interface IEvalHistory 
extends Iterable<EvalHistory.HistoryEntry>
{
	public abstract void addEntry(String form);

	public abstract void addEntry(String form, long timestamp);

	public abstract HistoryEntry getEntry(int pos);

	public abstract long getTimestamp(int pos);

	public abstract String getForm(int pos);

	public abstract int getSize();
	
	public String getShortedForm(final int pos, final int maxChars, final boolean discardNewlines);
	
	/**
	 * Gibt alle Forms als String-Array zurueck.
	 * Das zurueckgegebene Array darf veraendert werden.
	 * @return String[] - Forms als Array
	 */
	public HistoryEntry[] toArray();
}