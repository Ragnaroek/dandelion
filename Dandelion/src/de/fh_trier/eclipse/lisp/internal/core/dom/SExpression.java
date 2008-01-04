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

import org.eclipse.jface.text.Position;

import edu.umd.cs.findbugs.annotations.*;

/**
 * Basisklass fuer alle Lisp-Ausdruecke.
 * @author Michael Bohn
 */
public class SExpression
extends SExpressionNode
implements IHierarchicalMalformationProvider
{
	/**
	 * Comparator fuer zwei {@link SExpression}-Objekte.
	 * SExpression ohne Position werden als am kleinsten angesehen, und stehen
	 * nach der Sortierung am Ende der Liste.
	 */
	public static final Comparator<SExpression> POSITION_COMPARATOR = new Comparator<SExpression>() {
		public int compare(final SExpression o1, final SExpression o2) {
			
			if(o1.getPosition() == null && o2.getPosition() == null) {
				return 0;
			}
			
			if(o1.getPosition() == null) {
				return 1;
			}
			if(o2.getPosition() == null) {
				return -1;
			}
			
			return o1.getPosition().getOffset() - o2.getPosition().getOffset();
		}
	};
	
	private Position fPosition;
	
	/**
	 * Typ des Ausdrucks.
	 */
	@NonNull
	protected TSExpression fTyp = TSExpression.SEXPRESSION;
	
	/**
	 * Der Fehlerspeicher. 
	 */
	//lazy-init
	protected IMalformationProvider fMalformationStore;
	
	/**
	 * Die Liste der Kindknoten. Wird lazy-initialisert, da nicht jeder Knoten
	 * Kinder besitzt. Kann daher als immer <code>null</code> sein.
	 */
	@Nullable
	protected List<SExpression> fChildren;
	private SExpression fParent;
	private SExpression fRoot;
	
	/**
	 * Erstellt eine neue Sexpression, ohne Positionsangabe und Kindknoten.
	 */
	public SExpression() 
	{
		this(null, null);
	}
	
	/**
	 * Erstellte ein neue Sexpression, ohne Positionsangabe.
	 * @param childs - Die Kindknoten des Ausdrucks
	 */
	public SExpression(final List<SExpression> childs)
	{
		this(childs, null);
	}
	
	/**
	 * Erstellt eine neue Sexpression.
	 * @param childs - Die Kindknoten
	 * @param position - Die Positionsangabe im Dokument
	 */
	public SExpression(final List<SExpression> childs, final Position position)
	{
		this.fPosition = position;
		
		if(childs != null) {
			appendChilds(childs);
		}
	}
	
	@Override
	protected void addFirstChild(final SExpression child) 
	{
		addNth(0, child);
	}
	
	@Override
	protected void addNth(final int n, final SExpression child)
	{
		//lazy-init
		List<SExpression> childs = getChildrenCreateIfNeccessary();
		child.setParent(this);
		childs.add(n, child);
	}
	
	/**
	 * Fuegt einen neuen Kindknoten an diesen Knoten
	 * an. Nur fuer Testzwecke public.
	 * @param node - Anzufuegender Kindknoten
	 * @throws NullPointerException - wenn child <code>null</code>
	 */
	@Override
	public void appendChild(final SExpression child) 
	{
		//lazy-init
		List<SExpression> childs = getChildrenCreateIfNeccessary();
		child.setParent(this);
		childs.add(child); //append ans ende der liste
	}
	
	/**
	 * Fuegt eine Liste von Kindknoten an diesen Knoten an.
	 * Nur fuer Testzwecke public.
	 * @param nodes
	 */
	@Override
	public void appendChilds(final List<SExpression> nodes) 
	{
		if(nodes == null) {
			return;
		}
		
		//invariante: Node-Liste != null
		
		for( SExpression node : nodes ) {
			appendChild(node);
		}
	}

	/**
	 * Liefert eine Liste aller Kindknoten, geordnet.
	 * Kann <code>null</code> zurueckliefern, wenn {@link ISexpNode#hasChildren()} <code>false</code>
	 * zurueck gibt.
	 * @return
	 */
	@Nullable
	@Override
	public List<SExpression> getChildren() 
	{
		return fChildren;
	}
	
	/**
	 * Erstellt falls notwendig die Child-Liste.
	 * Achtung: lazy-init wird umgangen.
	 * @return
	 */
	@NonNull
	@Override
	protected List<SExpression> getChildrenCreateIfNeccessary()
	{
		if( fChildren == null ) {
			fChildren = new ArrayList<SExpression>();
		}
		return fChildren;
	}
	
	/**
	 * Liefert den n-ten Kindknoten.
	 */
	@Override
	public SExpression getChild(final int pos) {
		return getChildren().get(pos);
	}

	/**
	 * Der Knoten besitzt Kindknoten.
	 * @return boolean - <code>true</code> wenn Knoten Kindknoten besitzt
	 */
	@Override
	public boolean hasChildren() 
	{
		return fChildren != null && !fChildren.isEmpty();
	}
	
	/**
	 * Wird bei appendChild gesetzt.
	 * @param parent
	 */
	@Override
	protected void setParent(final SExpression parent)
	{
		this.fParent = parent;
	}
	
	/**
	 * Liefert den Parent-Knoten.
	 * <code>null</code> wenn Wurzelknoten.
	 */
	@Override
	public SExpression getParent()
	{
		return fParent;
	}
	
	@Override
	protected void setRoot(final SExpression root)
	{
		this.fRoot = root;
	}
	
	/**
	 * Liefert den Wurzelknoten des Baumes.
	 */
	@Override
	public SExpression getRoot()
	{
		return fRoot;
	}
	
	/**
	 * Besuch eines Visitor. Muss fuer jede Klasse separat
	 * ueberschrieben werden, damit korrekte Methode
	 * in Visitor aufgerufen wird.
	 * <p>
	 * Template:
	 * <pre>
	 * boolean visitChilds = visitor.visit(this);
	 * if( !hasChildren() ) return;
	 * if( visitChilds ) {		
	 *   for(SExpression child : getChildren()) {
	 *     child.accept(visitor);
	 *   }
	 * }
	 *	</pre></p>
	 * @param visitor
	 */
	@Override
	public void accept(final ISexpDOMVisitor visitor) 
	{
		boolean visitChilds = visitor.visit(this);
		if( !hasChildren() ) return;
		
		if( visitChilds ) {		
			for(SExpression child : getChildren()) {
				child.accept(visitor);
			}
		}
	}
	
	/**
	 * TypeSwitch an der Sexpression.
	 * @param <T>
	 * @param c
	 * @return
	 */
	public <T> T typeSwitch(final Case<T> c)
	{
		return c.typeCase(this);
	}

	protected IMalformationProvider getMalformationProvider()
	{
		if(fMalformationStore == null) {
			fMalformationStore = new DefaultMalformationProvider();
		}
		
		return fMalformationStore;
	}
	
	/**
	 * Test ob Ausdruck auf Toplevel.
	 * @return
	 */
	public boolean isToplevel()
	{
		return getRoot() == getParent();
	}
	
	/**
	 * Test ob Ausdruck NIL. NIL wenn keine Kindknoten.
	 * @return
	 */
	public boolean isNil()
	{
		return getTyp() == TSExpression.SEXPRESSION && !hasChildren() || 
			getTyp() == TSExpression.SYMBOL && Symbol.SYM_NIL.equalsIgnoreCase(((Symbol)this).getSymbolName());
	}
	
	/**
	 * Liefert die Position des Ausdrucks im Dokument.
	 * @return
	 */
	public Position getPosition()
	{
		return fPosition;
	}
	
	/*package*/ void setPosition(final Position position)
	{
		this.fPosition = position;
	}
	
	/**
	 * Liefert den Typ des Ausdrucks.
	 * Muss von abgeleiteten Klassen passende ueberschrieben werden.
	 * @return
	 */
	public TSExpression getTyp() {
		return fTyp;
	}
	
	/*package*/ void setTyp(final TSExpression typ)
	{
		this.fTyp = typ;
	}

	/**
	 * @see IMalformationProvider#getMalformations()
	 */
	@NonNull
	public List<ISyntacticalMalformation> getMalformations() 
	{
		return getMalformationProvider().getMalformations();
	}

	/**
	 * @see IMalformationProvider#addMalformation(ISyntacticalMalformation)
	 */
	public void addMalformation(final ISyntacticalMalformation malformation)
	{
		getMalformationProvider().addMalformation(malformation);
	}
	
	/**
	 * @see IMalformationProvider#clearMalformations()
	 */
	public void clearMalformations() {
		if(fMalformationStore == null) {
			return;
		}
		getMalformationProvider().clearMalformations();
	}

	/**
	 * @see IMalformationProvider#addAll(Collection)
	 */
	public void addAll(Collection<ISyntacticalMalformation> malformations) 
	{
		getMalformationProvider().addAll(malformations);
	}

	/**
	 * @see IMalformationProvider#hasMalformation()
	 */
	public boolean hasMalformation() 
	{
		return !(fMalformationStore == null || !getMalformationProvider().hasMalformation());
	}

	/**
	 * @see IMalformationProvider#hasMalformation(TSeverity)
	 */
	public boolean hasMalformation(TSeverity severity) 
	{
		if(!hasMalformation()) {
			return false;
		}
		
		//invariante: nicht-leere Malformation Liste
		return getMalformationProvider().hasMalformation(severity);
	}

	/**
	 * @see IHierarchicalMalformationProvider#hasMalformation(boolean, TSeverity)
	 */
	public boolean hasMalformation(boolean recursive, TSeverity severity) 
	{
		if( hasMalformation(severity) ) {
			return true;
		}
		
		if( recursive && hasChildren() ) {
			for(SExpression child : getChildren() ) {
				if( child.hasMalformation(true, severity) ) {
					return true;
				}
			}
		}
		
		return false;
	}

	/**
	 * @see IHierarchicalMalformationProvider#hasMalformation(boolean)
	 */
	public boolean hasMalformation(boolean recursive) 
	{
		if( hasMalformation() ) {
			return true;
		}
		
		if( recursive && hasChildren() ) {
			for(SExpression child : getChildren() ) {
				if( child.hasMalformation(true) ) {
					return true;
				}
			}
		}
		
		return false;
	}
}
