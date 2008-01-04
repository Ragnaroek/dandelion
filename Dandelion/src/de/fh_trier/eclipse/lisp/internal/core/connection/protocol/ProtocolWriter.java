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

package de.fh_trier.eclipse.lisp.internal.core.connection.protocol;

import java.io.*;

import de.fh_trier.eclipse.lisp.internal.core.connection.*;

import static de.fh_trier.eclipse.lisp.internal.core.connection.protocol.ProtocolUtilities.*;
import static de.fh_trier.eclipse.lisp.internal.core.connection.protocol.ProtocolConstants.*;

/**
 * Diese Klasse implementiert das Protokoll fuer
 * die Kommunikation mit dem Eval-Server.
 * @author Michael Bohn
 * testcase
 */
public class ProtocolWriter 
implements IProtocolWriter 
{
	private Writer fWriter;
	
	/**
	 * Erstellt einen neuen ProtocolWriter fuer
	 * die Verbindung.
	 * @param connection
	 * @throws ConnectionException
	 */
	public ProtocolWriter(final IConnection connection)
	throws ConnectionException
	{
		if (connection == null) {
			throw new NullPointerException("IConnection must not be null");
		}
		
		this.fWriter = new OutputStreamWriter(connection.getOutputStream());
	}

	/**
	 * @see IProtocolWriter#writeConnect(String, int)
	 */
	public void writeConnect(final String ioHost, final int ioPort)
	throws ProtocolException 
	{
		if (ioHost == null) {
			throw new NullPointerException("ioHost must not be null");
		}
		
		if(ioPort < 0 || ioPort > 65535) {
			throw new IllegalArgumentException("invalid ioPort: " + ioPort);
		}
		
		StringBuilder cmd = new StringBuilder();
		cmd.append(TOKEN_CONNECT);
		cmd.append(TOKEN_BLANK);
		cmd.append(ioHost);
		cmd.append(TOKEN_BLANK);
		cmd.append(ioPort);
		
		doWrite(cmd.toString());
	}

	/**
	 * @see IProtocolWriter#writeDisconnect()
	 */
	public void writeDisconnect() throws ProtocolException 
	{
		doWrite(TOKEN_DISCONNECT);
	}

	/**
	 * @see IProtocolWriter#writeEval(String, String)
	 */
	public void writeEval(final String pack, final String form) throws ProtocolException 
	{
		if (pack == null) {
			throw new NullPointerException("package must not be null");
		}
		
		if (form == null) {
			throw new NullPointerException("form must not be null");
		}
		
		StringBuilder cmd = new StringBuilder();
		cmd.append(TOKEN_EVAL);
		cmd.append(TOKEN_BLANK);
		cmd.append(pack);
		cmd.append(TOKEN_BLANK);
		cmd.append(encodeBase64(form));

		doWrite(cmd.toString());
	}

	/**
	 * @see IProtocolWriter#writeInvokeRestart(IRestart, String)
	 */
	public void writeInvokeRestart(IRestart restart, String restartArgs) throws ProtocolException 
	{
		if (restart == null) {
			throw new NullPointerException("restart must not be null");
		}

		StringBuilder cmd = new StringBuilder();
		cmd.append(TOKEN_INVOKE_RESTART);
		cmd.append(TOKEN_BLANK);
		cmd.append(restart.getName());
		
		if(restartArgs != null) {
			cmd.append(TOKEN_BLANK);
			cmd.append(encodeBase64(restartArgs));
		}
		
		doWrite(cmd.toString());
	}

	/**
	 * @see IProtocolWriter#writeAbortRestart()
	 */
	public void writeAbortRestart() throws ConnectionException 
	{
		doWrite(TOKEN_ABORT);
	}

	/**
	 * @see IProtocolWriter#writePackageRequest()
	 */
	public void writePackageRequest() throws ConnectionException
	{
		doWrite(TOKEN_PACKAGES);
	}
	
	/**
	 * @see IProtocolWriter#writeFunctionRequest(String)
	 */
	public void writeFunctionRequest(final String pack) throws ConnectionException 
	{
		doWrite(TOKEN_FUNCTIONS + TOKEN_BLANK + pack);
	}

	/**
	 * @see IProtocolWriter#writeMacroRequest(String)
	 */
	public void writeMacroRequest(final String pack) throws ConnectionException 
	{
		doWrite(TOKEN_MACROS + TOKEN_BLANK + pack);
	}

	private void doWrite(final String string)
	throws ProtocolException
	{
		try {
			fWriter.write(string + TOKEN_TERMINATION);
			fWriter.flush();
		} catch (IOException e) {
			//Error Translation
			throw new ProtocolException("Error while writing", e);
		}
	}
}
