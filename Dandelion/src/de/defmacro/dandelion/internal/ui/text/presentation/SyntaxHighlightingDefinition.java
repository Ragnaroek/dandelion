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

import java.util.HashMap;
import java.util.Map;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

import de.defmacro.dandelion.internal.ui.UIColorManager;

/**
 * Liefert die Einstellung fuer die Syntaxhervorhebung.
 * @author Michael Bohn
 */
public class SyntaxHighlightingDefinition 
{
	public enum SyntaxType
	{
		STRING,
		COMMENT,
		FUNCTION_DECLARATION,
		VARIABLE_DECLARATION,
		TYP_DECLARATION,
		LOCAL_VARIABLE_DECLARATION,
		KEYWORD_SYMBOL,
		CHARACTER,
	}
	
	private Map<SyntaxType, Color> syntaxColors = new HashMap<SyntaxType, Color>();
	
	private UIColorManager colorManager;
	
	public SyntaxHighlightingDefinition(final UIColorManager colorManager)
	{
		this.colorManager = colorManager;
		
		syntaxColors.putAll(getDefaults());
	}
	
	public Color getHighlighteColor(final SyntaxType type)
	{
		Assert.isNotNull(syntaxColors);
		
		return syntaxColors.get(type);
	}
	
	public TextAttribute getTextAttribute(final SyntaxType type)
	{
		Assert.isNotNull(syntaxColors);
		
		int style = SWT.NORMAL;
		if(type == SyntaxType.FUNCTION_DECLARATION
				|| type == SyntaxType.VARIABLE_DECLARATION
				|| type == SyntaxType.TYP_DECLARATION
				|| type == SyntaxType.LOCAL_VARIABLE_DECLARATION
				|| type == SyntaxType.KEYWORD_SYMBOL) {
			style = SWT.BOLD;
		}
		   
		return new TextAttribute(syntaxColors.get(type), null, style);
	}
	
	public Map<SyntaxType, Color> getDefaults()
	{
		assert(colorManager != null);
		
		Map<SyntaxType, Color> defaults = new HashMap<SyntaxType, Color>();
		
		defaults.put(SyntaxType.COMMENT, colorManager.getColor(new RGB(63, 127, 95)));
		defaults.put(SyntaxType.STRING, colorManager.getColor(new RGB(42, 0, 255)));
		defaults.put(SyntaxType.FUNCTION_DECLARATION, colorManager.getColor(new RGB(127, 0 ,85)));
		defaults.put(SyntaxType.VARIABLE_DECLARATION, colorManager.getColor(new RGB(20, 105, 171)));
		defaults.put(SyntaxType.TYP_DECLARATION, colorManager.getColor(new RGB(29, 140, 86)));
		defaults.put(SyntaxType.LOCAL_VARIABLE_DECLARATION, colorManager.getColor(new RGB(0, 15, 111)));
		defaults.put(SyntaxType.KEYWORD_SYMBOL, colorManager.getColor(new RGB(148, 101, 48)));
		defaults.put(SyntaxType.CHARACTER, defaults.get(SyntaxType.STRING));
		
		return defaults;
	}
}
