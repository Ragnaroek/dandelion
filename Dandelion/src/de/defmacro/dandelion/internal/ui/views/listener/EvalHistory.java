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

import java.util.*;

import de.defmacro.dandelion.internal.ui.text.TextUtilities;

/**
 * Historie eines Listeners.
 * @author Michael Bohn
 *
 */
public class EvalHistory 
implements IEvalHistory
{
	private List<HistoryEntry> fQueue;
	private int fMaxEntrys;
	
	public static class HistoryEntry
	implements Comparable<HistoryEntry>
	{
		private final long fTimestamp;
		private final String fForm;
		
		private HistoryEntry(final String text, final long timestamp)
		{
			this.fForm = text;
			this.fTimestamp = timestamp;
		}
		
		public String getForm()
		{
			return fForm;
		}
		
		public String getShortedForm(final int maxChars, final boolean discardNewlines)
		{
			if(maxChars < 3) {
				throw new IllegalArgumentException("maxChars must be >= 3");
			}
			
			String form = getForm();
			if(discardNewlines) {
				form = form.replace(System.getProperty("line.separator"), "");
			}
			
			if(form.length() == maxChars) {
				return form;
			} else if (form.length() > maxChars-3) {
				return form.subSequence(0, maxChars-3) + "...";
			}
			return form;
		}
		
		public long getTimestamp()
		{
			return fTimestamp;
		}

		@Override
		public String toString() {
			return TextUtilities.getTimePresentation(getTimestamp()) +
			" - " + getShortedForm(33, true);
		}

		/**
		 * Vergleich anhand Zeitstempel.<p />
		 * <strong>Note:</strong> this class has a natural ordering that is
         * inconsistent with equals.
         * @throws NullPointerException - wenn der uebergebene IEvalServer <code>null</code>
		 */
		public int compareTo(final HistoryEntry other) 
		{
			if(fTimestamp == other.fTimestamp) {
				return 0;
			} else if(fTimestamp < other.fTimestamp) {
				return -1;
			} else {
				return 1;
			}
		}

		@Override
		public int hashCode() {
			final int PRIME = 31;
			int result = 1;
			result = PRIME * result + ((fForm == null) ? 0 : fForm.hashCode());
			result = PRIME * result + (int) (fTimestamp ^ (fTimestamp >>> 32));
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			final HistoryEntry other = (HistoryEntry) obj;
			if (fForm == null) {
				if (other.fForm != null)
					return false;
			} else if (!fForm.equals(other.fForm))
				return false;
			if (fTimestamp != other.fTimestamp)
				return false;
			return true;
		}
	}
	
	public EvalHistory(final int maxEntrys)
	{
		if(maxEntrys <= 0) {
			throw new IllegalArgumentException("maxEntrys must be greater or equal zero");
		}
		
		fQueue = new ArrayList<HistoryEntry>();
		this.fMaxEntrys = maxEntrys;
	}
	
	public void addEntry(final String form)
	{
		addEntry(form, System.currentTimeMillis());
	}
	
	public void addEntry(final String form, final long timestamp)
	{
		//neueste Eintraege stehen oben
		
		if(fQueue.size() >= fMaxEntrys) {
			fQueue.remove(0); //letzten Eintrag entfernen
		}
		
		fQueue.add(new HistoryEntry(form, timestamp));
	}
	
	public HistoryEntry getEntry(final int pos)
	{
		return fQueue.get(pos);
	}
	
	public long getTimestamp(final int pos)
	{
		return getEntry(pos).getTimestamp();
	}
	
	public String getForm(final int pos)
	{
		return getEntry(pos).getForm();
	}
	
	public String getShortedForm(final int pos, final int maxChars, final boolean discardNewlines)
	{
		return getEntry(pos).getShortedForm(maxChars, discardNewlines);
	}
	
	public int getSize()
	{
		return fQueue.size();
	}

	public HistoryEntry[] toArray() 
	{
		return fQueue.toArray(new HistoryEntry[fQueue.size()]);
		/*
		String[] forms = new String[fQueue.size()];
		for(int i=0; i<forms.length; i++) {
			forms[i] = fQueue.get(i).getForm();
		}
		return forms; */
	}

	public Iterator<HistoryEntry> iterator() {
		return fQueue.iterator();
	}
}
