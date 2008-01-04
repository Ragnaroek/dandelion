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

package de.fh_trier.eclipse.lisp.internal.core.dom;

import java.util.*;

import edu.umd.cs.findbugs.annotations.*;

/**
 * Knoten im Baum der Lisp-Ausdruecke.
 * @author Michael Bohn
 */
public abstract class SExpressionNode
{
	/**
	 * Besuch eines Visitors.
	 * @param visitor
	 */
	public abstract void accept(ISexpDOMVisitor visitor);
	
	/**
	 * Fuegt eine S-Expression an Position 0 der Child-Liste an.
	 * Default-Verhalten. Kann in abgeleiteten Klassen ueberschrieben werden.
	 * @param child
	 */
	protected abstract void addFirstChild(final SExpression child);
	
	/**
	 * Fuegt eine S-Expression an die n-te Position an.
	 * @param n
	 * @param child
	 */
	protected abstract void addNth(final int n, final SExpression child);
	
	/**
	 * Fuegt einen neuen Kindknoten an diesen Knoten
	 * an. Nur fuer Testzwecke public.
	 * @param node - Anzufuegender Kindknoten
	 * @throws NullPointerException - wenn child <code>null</code>
	 */
	public abstract void appendChild(final SExpression child);
	
	/**
	 * Fuegt eine Liste von Kindknoten an diesen Knoten an.
	 * Nur fuer Testzwecke public.
	 * @param nodes
	 */
	public abstract void appendChilds(final List<SExpression> nodes);

	/**
	 * Liste aller Kindknoten, geordnet.
	 * Kann <code>null</code> zurueckliefern, wenn {@link ISexpNode#hasChildren()} <code>false</code>
	 * zurueck gibt.
	 * @return
	 */
	@Nullable
	public abstract List<SExpression> getChildren();
	
	/**
	 * Erstellt falls notwendig die Child-Liste.
	 * Achtung: lazy-init wird umgangen.
	 * @return
	 */
	@NonNull
	protected abstract List<SExpression> getChildrenCreateIfNeccessary();
	
	/**
	 * Liefert den n-ten Kindknoten.
	 * @param pos
	 * @return
	 */
	public abstract SExpression getChild(final int pos);

	/**
	 * Der Knoten besitzt Kindknoten.
	 * @return boolean - <code>true</code> wenn Knoten Kindknoten besitzt
	 */
	public abstract boolean hasChildren();
	
	/**
	 * Wird bei appendChild gesetzt.
	 * @param parent
	 */
	protected abstract void setParent(final SExpression parent);
	
	/**
	 * Liefert den Parent-Knoten.
	 * @return
	 */
	public abstract SExpression getParent();
	
	protected abstract void setRoot(final SExpression root);
	
	/**
	 * Liefert den Wurzelknoten.
	 * @return
	 */
	public abstract SExpression getRoot();
}
