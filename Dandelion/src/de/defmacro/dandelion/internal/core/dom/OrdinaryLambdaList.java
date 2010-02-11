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

package de.defmacro.dandelion.internal.core.dom;

import java.util.*;

import edu.umd.cs.findbugs.annotations.*;

/**
 * Eine Ordinary-Lambda-Liste.
 * @author Michael Bohn
 */
public class OrdinaryLambdaList
extends DefaultMalformationProvider
{
	/**
	 * Die NIL (leere) Lambda-Liste.
	 */
	public static final OrdinaryLambdaList NIL = new OrdinaryLambdaList(null, null, null, null, false, null);
	
	//NICHT DIREKT ZUGREIFEN!!!!
	private static List<ParameterDefinition> empty_parameter_definition;
	private static List<RequiredParameterDefinition> empty_required;
	
	private List<RequiredParameterDefinition> fRequired;
	private List<ParameterDefinition> fOptional;
	private Symbol fRest;
	
	private List<ParameterDefinition> fKeyword;
	private boolean fAllowOtherKeys;
	private List<ParameterDefinition> fAux;
	
	/**
	 * Erstellt eine neue Ordinary-Lambda-List.
	 * Fuer jeden Parameter kann <code>null</code> uebergeben werden.
	 * @param required
	 * @param optional
	 * @param rest
	 * @param body
	 * @param keyword
	 */
	public OrdinaryLambdaList(final List<RequiredParameterDefinition> required,
				      final List<ParameterDefinition> optional,
				      final Symbol rest,
				      final List<ParameterDefinition> keyword,
				      final boolean allowOtherKeys,
				      final List<ParameterDefinition> aux
	)
	{
		if(required == null) {
			fRequired = getEmptyRequired(); //nicht direkt auf werte zugreifen, oder in static-Block initialisieren
			                                //da im static-Block auch eine Instanz von LambdaList angelegt wird!!!!!!!!!!!!!!
		} else {
			fRequired = Collections.unmodifiableList(required);
		}
		
		if(optional == null) {
			fOptional = getEmptyParameterDefinition();
		} else {
			fOptional = Collections.unmodifiableList(optional);
		}
		
		this.fRest = rest;
		
		if(keyword == null) {
			fKeyword = getEmptyParameterDefinition();
		} else {
			fKeyword = Collections.unmodifiableList(keyword);
		}
		
		fAllowOtherKeys = allowOtherKeys;
		
		if(aux == null) {
			fAux = getEmptyParameterDefinition();
		} else {
			fAux = Collections.unmodifiableList(aux);
		}
	}
	
	/**
	 * Liefert die required-Parameter der Lambda-Liste.
	 * @return Required-Parameter
	 */
	@NonNull
	public List<RequiredParameterDefinition> getRequiredParameters()
	{
		return fRequired;
	}
	
	/**
	 * Liefert die &ampoptional-Parameter.
	 * @return optional-Parameter
	 */
	@NonNull
	public List<ParameterDefinition> getOptionalParameters()
	{
		return fOptional;
	}
	
	/**
	 * Liefert das &amprest Symbol.
	 * @return rest-parameter
	 */
	@Nullable
	public Symbol getRestParameter()
	{
		return fRest;
	}
	
	/**
	 * Liefert die &ampkey-Parameter.
	 * @return key-Parameter
	 */
	@NonNull
	public List<ParameterDefinition> getKeywordParameters()
	{
		return fKeyword;
	}
	
	/**
	 * Liefert die &ampaux-Parameter.
	 * @return aux-Parameter
	 */
	@NonNull
	public List<ParameterDefinition> getAuxParameters()
	{
		return fAux;
	}
	
	/**
	 * Test ob required-Parameter angegeben wurden.
	 * @return
	 */
	public boolean hasRequiredParameters()
	{
		return !getRequiredParameters().isEmpty();
	}
	
	/**
	 * Test ob optional-Parameter angegeben wurden.
	 * @return
	 */
	public boolean hasOptionalParameters()
	{
		return !getOptionalParameters().isEmpty();
	}
	
	/**
	 * Test ob rest-Parameter angebeben wurde.
	 * @return
	 */
	public boolean hasRestParameter()
	{
		return getRestParameter() != null;
	}
	
	/**
	 * Test ob key-Parameter angegeben wurde
	 * @return
	 */
	public boolean hasKeywordParameters()
	{
		return !getKeywordParameters().isEmpty();
	}
	
	/**
	 * Test ob &ampallow-other-key angegeben wurde.
	 * @return
	 */
	public boolean hasAllowOtherKeys()
	{
		return fAllowOtherKeys;
	}
	
	/**
	 * Test ob aux-Parameter angegeben wurden.
	 * @return
	 */
	public boolean hasAuxParameters()
	{
		return !getAuxParameters().isEmpty();
	}
	
	/**
	 * Test ob Lambda-List NIL.
	 * @return
	 */
	public boolean isNil()
	{
		return !hasRequiredParameters() && !hasOptionalParameters() && !hasRestParameter() && !hasKeywordParameters()
			&& !hasAllowOtherKeys() && !hasAuxParameters();
	}
	
	private List<ParameterDefinition> getEmptyParameterDefinition()
	{
		if(empty_parameter_definition == null) {
			empty_parameter_definition = Collections.unmodifiableList(new ArrayList<ParameterDefinition>(0));
		}
		return empty_parameter_definition;
	}
	
	private List<RequiredParameterDefinition> getEmptyRequired()
	{
		if(empty_required == null) {
			empty_required = Collections.unmodifiableList(new ArrayList<RequiredParameterDefinition>(0));
		}
		return empty_required;
	}
}
