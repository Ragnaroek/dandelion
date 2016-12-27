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

package de.defmacro.dandelion.internal.ui;

/**
 * Konstanten aller im Plugin vorhandenen Bilder.
 * Die Konstanten dienen gleichzeitig als
 * Schluessel fuer den {@link UIImageManager}.
 * @author Michael Bohn
 *
 */
public class UIImageConstants 
{
	private UIImageConstants()
	{ /* keine Instanz */ }
	
	public static final String ICON_ERROR = "error.gif";
	public static final String ICON_CLEAR = "clear.gif";
	public static final String ICON_PACKAGE = "package.gif";
	public static final String ICON_HISTORY = "history.gif";
	public static final String ICON_ERROR_DIALOG = "error_dialog.png";
	public static final String ICON_IN_PACKAGE = "in-package.gif";
	public static final String ICON_DEFUN = "defun.gif";
	public static final String ICON_SYMBOL_PRIVATE = "symbol_private.gif";
	public static final String ICON_SYMBOL_PUBLIC = "symbol_public.gif";
	public static final String ICON_SYMBOL_UNINTERNED = "symbol_uninterned.gif";
	public static final String ICON_SYMBOL_READER = "symbol_reader.gif";
	public static final String ICON_TOPLEVEL = "toplevel.gif";
	public static final String ICON_FORM = "form.gif";
	public static final String ICON_SEXPRESSION = "sexp.gif";
	public static final String ICON_DEFMACRO = "defmacro.gif";
	public static final String ICON_LAMBDA_FORM = "lambda-form.gif";
	public static final String ICON_DEFPACKAGE = "defpackage.gif";
	public static final String ICON_EVALUATE_SELECTION = "evaluate_selection.gif";
	public static final String ICON_EVALUATE_FILE = "evaluate_file.gif";
	public static final String ICON_EVALUATE_TOPLEVEL = "evaluate_toplevel.gif";
	public static final String ICON_MACROEXPAND = "macroexpand.png";
	public static final String ICON_MACROEXPAND_ONE = "macroexpand-1.png";
	public static final String ICON_CONNECT = "connect.png";
	public static final String ICON_DISCONNECT = "disconnect.png";
	
	/**
	 * Dekorator-Image fuer ein Warnung.
	 */
	public static final String OVERLAY_WARNING = "warning_ovr.gif";
	
	/**
	 * Dekorator-Image fuer einen Fehler.
	 */
	public static final String OVERLAY_ERROR = "error_ovr.gif";
}
