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

import java.util.*;

import org.eclipse.jface.text.Position;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.core.meta.*;
import edu.umd.cs.findbugs.annotations.NonNull;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

/**
 * Abstrakte Basisklasse fuer Lisp-Prosposal.
 * @author Michael Bohn
 *
 */
@SuppressWarnings("UwF")
public abstract class AbstractProposalCollector 
extends SexpressionDOMVisitorAdapter 
{
	private SortedSet<IMetaSymbol> fResult;
	private ISexpModel fModel;
	protected String fPrefix;
	
	/**
	 * Erzeugt einen neuen PropsoalCollector.
	 * @param prefix - Praefix mit dem die Symbole beginnen um aufgenommen zu werden
	 * @throws NullPointerException - wenn prefix == <code>null</code>
	 */
	public AbstractProposalCollector(final String prefix)
	{
		if (prefix == null) {
			throw new NullPointerException("prefix must not be null");
		}
		
		this.fPrefix = prefix;
		fResult = new TreeSet<IMetaSymbol>();
	}
	
	/**
	 * @see ISexpDOMVisitor#preVisit(ISexpModel)
	 */
	@Override
	public void preVisit(final ISexpModel model) {
		fModel = model;
	}
	
	protected void cons(final IMetaSymbol sym) {
		fResult.add(sym);
	}
	
	/**
	 * Ueberprueft ob der Symbolename des Symols mit
	 * dem Praefix beginnt. Case-Insensitive.
	 * @param sym
	 * @return
	 */
	protected boolean checkPrefix(final Symbol sym)
	{
		String symName = sym.getSymbolName().toLowerCase();
		String prefix = fPrefix.toLowerCase();
		return symName.startsWith(prefix) && !symName.equals(prefix);
	}
	
	/**
	 * Erstellt eine MetaSymbol aus einer FunctionDefiningForm. Der definierte Name der
	 * Form wird MetaSymbol-Name und ein eventuell vorhandener Qualifier wird Package-Name.
	 * @param form
	 * @param type
	 * @return
	 */
	protected IMetaSymbol constructMetaSymbol(final FunctionDefiningForm form, final TMetaType type)
	{
		//String pack = fModel.getInpackage(form.getPosition()).getPackage().getSymbolName();
		String pack = getPackage(form.getDefinedName(), form.getPosition());
		String name = form.getDefinedName().getSymbolName();
		List<String> args = constructArgumentList(form, type);
		return new FunctionMetaSymbol(pack, name, null, args, type);
	}
	
	protected IMetaSymbol constructMetaSymbol(final Symbol sym) {
		//String pack = fModel.getInpackage(sym.getPosition()).getPackage().getSymbolName();
		String pack = getPackage(sym, sym.getPosition());
		String name = sym.getSymbolName();
		return new SymbolMetaSymbol(pack, name, TMetaType.LOCAL_SYMBOL);
	}
	
	/**
	 * Gibt das Paket fuer das Symbol zurueck. Ist das Symbol qualified wird
	 * der Qualifier zurueckgegeben. Ansonsten wird die in-package deklaration
	 * fuer die uebergegebene Position berechnet.
	 * @param sym
	 * @param calcPosition
	 * @return
	 */
	private String getPackage(final Symbol sym, final Position calcPosition)
	{
		if(sym.isQualified()) {
			return sym.getQualifier();
		}
		
		return fModel.getInpackage(calcPosition).getPackage().getSymbolName();
	}
	
	private List<String> constructArgumentList(final FunctionDefiningForm form, final TMetaType type) 
	{
		OrdinaryLambdaList lambdaList = form.getLambdaList();
		if(lambdaList.hasMalformation() || lambdaList.isNil()) {
			return new ArrayList<String>(0);
		}
		
		List<String> result = new ArrayList<String>();
		
		if(type == TMetaType.MACRO) {
			MacroLambdaList macroLambda = (MacroLambdaList)form.getLambdaList();
			if(macroLambda.hasWholeParameter()) {
				result.add("&whole");
				result.add(macroLambda.getWholeParameter().getSymbolName());
			}
			if(macroLambda.hasEnvironmentParameter()) {
				result.add("&environment");
				result.add(macroLambda.getEnvironmentParameter().getSymbolName());
			}
		}
		
		if(lambdaList.hasRequiredParameters()) {
			consSymbolNames(extractArgNameSymbolsRequired(lambdaList.getRequiredParameters()), result);
		}
		if(lambdaList.hasOptionalParameters()) {
			consParams("&optional", lambdaList.getOptionalParameters(), result);
		}
		if(lambdaList.hasRestParameter()) {
			result.add("&rest");
			result.add(lambdaList.getRestParameter().getSymbolName());
		}
		
		if(type == TMetaType.MACRO) {
			MacroLambdaList macro = (MacroLambdaList)lambdaList;
			if(macro.hasBodyParameter()) {
				result.add("&body");
				result.add(macro.getBodyParameter().getSymbolName());
			}
		}
		
		if(lambdaList.hasKeywordParameters()) {
			consParams("&key", lambdaList.getKeywordParameters(), result);
		}
		if(lambdaList.hasAllowOtherKeys()) {
			result.add("&allow-other-keys");
		}
		if(lambdaList.hasAuxParameters()) {
			consParams("&aux", lambdaList.getAuxParameters(), result);
		}
		
		return result;
	}

	protected List<Symbol> getArgumentNames(final OrdinaryLambdaList lambdaList) 
	{
		boolean isMacro = false;
		List<Symbol> result = new ArrayList<Symbol>();
		if(lambdaList instanceof MacroLambdaList) {
			isMacro = true;
			MacroLambdaList macroLambda = (MacroLambdaList)lambdaList;
			if(macroLambda.hasWholeParameter()) {
				result.add(macroLambda.getWholeParameter());
			}
			if(macroLambda.hasEnvironmentParameter()) {
				result.add(macroLambda.getEnvironmentParameter());
			}
		}
		
		if(lambdaList.hasRequiredParameters()) {
			result.addAll(extractArgNameSymbolsRequired(lambdaList.getRequiredParameters()));
		}
		if(lambdaList.hasOptionalParameters()) {
			result.addAll(extractArgNameSymbols(lambdaList.getOptionalParameters()));
		}
		if(lambdaList.hasRestParameter()) {
			result.add(lambdaList.getRestParameter());
		}
		if(isMacro) {
			MacroLambdaList macro = (MacroLambdaList)lambdaList;
			if(macro.hasBodyParameter()) {
				result.add(macro.getBodyParameter());
			}
		}
		
		if(lambdaList.hasKeywordParameters()) {
			result.addAll(extractArgNameSymbols(lambdaList.getKeywordParameters()));
		}
		if(lambdaList.hasAuxParameters()) {
			result.addAll(extractArgNameSymbols(lambdaList.getAuxParameters()));
		}
		
		return result;
	}
	
	private void consSymbolNames(final List<Symbol> syms, final List<String> list) {
		for(Symbol sym : syms) {
			list.add(sym.getSymbolName());
		}
	}
	
	private void consParams(final String key, final List<ParameterDefinition> params, final List<String> list) {
		list.add(key);
		for(ParameterDefinition param : params) {
			list.add(param.getParameterSymbol().getSymbolName());
		}
	}
	
	private List<Symbol> extractArgNameSymbols(final List<ParameterDefinition> paramList) {
		List<Symbol> result = new ArrayList<Symbol>(paramList.size());
		for(ParameterDefinition param : paramList) {
			result.add(param.getParameterSymbol());
		}
		return result;
	}
	
	private List<Symbol> extractArgNameSymbolsRequired(final List<RequiredParameterDefinition> paramList) {
		List<Symbol> result = new ArrayList<Symbol>(paramList.size());
		for(RequiredParameterDefinition param : paramList) {
			if(param.isDestructuring()) {
				result.addAll(extractDestructuring(param.getDestructuringParameter()));
			} else {
				result.add(param.getParameterSymbol());
			}
		}
		return result;
	}
	
	private List<Symbol> extractDestructuring(final SExpression destruct)
	{
		List<Symbol> result = new ArrayList<Symbol>();
		if(destruct.hasChildren()) {
			result.add(new Symbol("("));
			for(SExpression child : destruct.getChildren()) {
				if(child.getTyp() == TSExpression.SYMBOL) {
					result.add((Symbol)child);
				} else {
					result.addAll(extractDestructuring(child));
				}
			}
			result.add(new Symbol(")"));
		}
		return result;
	}
	
	/**
	 * Liefert alle eingesammelten Symbole als Set.
	 * @return Menge der eingesammelten Symbole
	 */
	@NonNull
	public SortedSet<IMetaSymbol> getResult()
	{
		return fResult;
	}
}
