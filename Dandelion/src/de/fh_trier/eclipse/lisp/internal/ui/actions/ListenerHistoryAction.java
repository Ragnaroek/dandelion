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

package de.fh_trier.eclipse.lisp.internal.ui.actions;

import org.eclipse.jface.action.*;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;
import de.fh_trier.eclipse.lisp.internal.ui.*;
import de.fh_trier.eclipse.lisp.internal.ui.dialogs.EvalHistorySelectionDialog;
import de.fh_trier.eclipse.lisp.internal.ui.views.listener.*;
import de.fh_trier.eclipse.lisp.internal.ui.views.listener.EvalHistory.HistoryEntry;

/**
 * Die Aktion fuer Aufruf der History eines Listeners.
 * @author Michael Bohn
 *
 */
public class ListenerHistoryAction
extends Action 
implements SelectionListener
{	
	private static final int MAX_RECENT_HISTORY = 10;
	
	private IMenuCreator fMenuCreator = new IMenuCreator()
	{
		private Menu fLastMenu;
		
		public void dispose() {
			disposeOldMenu();
		}

		public Menu getMenu(Control parent) {
			
			disposeOldMenu();
			
			Menu menu = new Menu(parent);
			
			int max = Math.min(MAX_RECENT_HISTORY, fHistory.getSize());
			
			for(int i=0;i<max; i++) {
				MenuItem item = new MenuItem(menu, SWT.NONE);
				item.addSelectionListener(ListenerHistoryAction.this);
				item.setText(fHistory.getEntry(i).toString());
				item.setData(fHistory.getForm(i));
			}
			
			fLastMenu = menu;
			return menu;
		}

		public Menu getMenu(Menu parent) {
			return null;
		}
		
		private void disposeOldMenu()
		{
			if(fLastMenu != null) {
				fLastMenu.dispose();
			}
		}
	};
	
	protected IEvalHistory fHistory; //protected fuer closure
	private ListenerView fView;
	
	/**
	 * Erzeugt eine neuen Historienauwahlaktion.
	 * @param history - Die Historie des Listeners
	 * @param view - Der Listener
	 */
	public ListenerHistoryAction(final IEvalHistory history, final ListenerView view)
	{
		super("", IAction.AS_DROP_DOWN_MENU);
		
		if (history == null) {
			throw new NullPointerException("IEvalHistory must not be null");
		}
		
		if (view == null) {
			throw new NullPointerException("ListenerView must not be null");
		}
		
		this.fHistory = history;
		this.fView = view;
		
		this.setToolTipText("Choose previous evaluated Form");
		this.setText("Eval Historie");
		this.setImageDescriptor(LispUI.getUIImageManager().getDescriptor(UIImageConstants.ICON_HISTORY));
	}

	/**
	 * Zeigt den Historienauswahldialog an.
	 */
	@Override
	public void run() 
	{
		EvalHistorySelectionDialog dialog = new EvalHistorySelectionDialog(fView.getSite().getShell());
		HistoryEntry[] entrys = fHistory.toArray();
		dialog.setElements(entrys);
		
		int result = dialog.open();
		if(result == Window.OK) {
			Object o = dialog.getResult()[0];
			HistoryEntry entry = (HistoryEntry)o;
			reeval(entry.getForm());
		}
	}

	/**
	 * @see Action#getMenuCreator()
	 */
	@Override
	public IMenuCreator getMenuCreator() {
		return fMenuCreator;
	}

	private void reeval(String form)
	{
		fView.addText(form);
	}
	
	//SelectionListener Implementierung
	
	/**
	 * Listener-Implementierung. Nicht aufrufen.
	 */
	public void widgetDefaultSelected(SelectionEvent e) 
	{
		//no-op
	}

	/**
	 * Listener-Implementierung. Nicht aufrufen.
	 */
	public void widgetSelected(SelectionEvent e) 
	{
		MenuItem menu = (MenuItem)e.getSource();
		reeval((String)menu.getData());
	}
}
