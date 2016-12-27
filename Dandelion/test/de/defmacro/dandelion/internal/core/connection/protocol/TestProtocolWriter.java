package de.defmacro.dandelion.internal.core.connection.protocol;

import java.io.*;
import org.junit.*;
import de.defmacro.dandelion.internal.core.connection.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

import static org.junit.Assert.*;

@SuppressWarnings("SIC")
public class TestProtocolWriter 
{
	@SuppressWarnings("UwF")
	private BufferedReader reader;
	@SuppressWarnings("UwF")
	private IProtocolWriter protocolWriter;
	
	@Before
	public void setUp()
	throws Exception
	{
		final PipedInputStream pipeInput = new PipedInputStream();
		final PipedOutputStream pipeOutput = new PipedOutputStream(pipeInput);
		reader = new BufferedReader(new InputStreamReader(pipeInput));
		
		//dummy Implementierung IConnection
		IEnvironment dummyServer = new Environment("localhost", 31337, "dummy", "xxx");
		IConnection dummyConnection = new LispConnection(dummyServer) {
			@Override
			public OutputStream getOutputStream() throws ConnectionException {
				return pipeOutput;
			}
		};
		
		protocolWriter = new ProtocolWriter(dummyConnection);
	}
	
	@After
	public void tearDown()
	{
		reader = null;
		protocolWriter = null;
	}
	
	//Testfaelle
	@Test
	public void testWriteConnect()
	throws Exception
	{
		protocolWriter.writeConnect("localhost", 31337);
		assertEquals("CONNECT localhost 31337", doRead());
		
		protocolWriter.writeConnect("192.168.0.1", 54785);
		assertEquals("CONNECT 192.168.0.1 54785", doRead());
		
		try {
			protocolWriter.writeConnect(null, 7845);
			fail("NullPointerException expected");
		} catch (NullPointerException e) {/*nichts*/}
		
		try {
			protocolWriter.writeConnect("host", -1);
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) {/*nichts*/}
		
		try {
			protocolWriter.writeConnect("host", 65536);
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) {/*nichts*/}
	}
	
	@Test
	public void testWriteDisconnect()
	throws Exception
	{
		protocolWriter.writeDisconnect();
		assertEquals("DISCONNECT", doRead());
	}
	
	@Test
	public void testWriteEval()
	throws Exception
	{
		protocolWriter.writeEval(":my-package", "(defun my-function () \nnil)");
		assertEquals("EVAL :my-package KGRlZnVuIG15LWZ1bmN0aW9uICgpIApuaWwp",
				doRead());
		
		protocolWriter.writeEval("some-package", "nil");
		assertEquals("EVAL some-package bmls", doRead());
		
		try {
			protocolWriter.writeEval(null, "()");
			fail("NullPointerException expected");
		} catch (NullPointerException e) {/*nichts*/}
	
		try {
			protocolWriter.writeEval("some-package", null);
			fail("NullPointerException expected");
		} catch (NullPointerException e) {/*nichts*/}
	}
	
	@Test
	public void testWriteInvokeRestart() 
	throws Exception 
	{
		IRestart restart = new Restart("restartSymbol", "description");
		protocolWriter.writeInvokeRestart(restart, null);
		assertEquals("INVOKE-RESTART restartSymbol", doRead());
		
		restart = new Restart("restart2", "description");
		protocolWriter.writeInvokeRestart(restart, "args");
		assertEquals("INVOKE-RESTART restart2 YXJncw==", doRead());
		
		restart = new Restart("1", "spielt keine Rolle");
		protocolWriter.writeInvokeRestart(restart, "a 'b c 3");
		assertEquals("INVOKE-RESTART 1 YSAnYiBjIDM=", doRead());
		
		try {
			protocolWriter.writeInvokeRestart(null, null);
			fail("NullPointerException expected");
		} catch (NullPointerException e) {/*nichts*/
		}
	}
	
	@Test
	public void testWriteAbortRestart()
	throws Exception
	{
		protocolWriter.writeAbortRestart();
		assertEquals("ABORT", doRead());
	}
	
	@Test
	public void testWritePackageRequest()
	throws Exception
	{
		protocolWriter.writePackageRequest();
		assertEquals("PACKAGES", doRead());
	}
	
	@Test
	public void testWriteFunctionRequest()
	throws Exception
	{
		protocolWriter.writeFunctionRequest("the-package");
		assertEquals("FUNCTIONS the-package", doRead());
		
		protocolWriter.writeFunctionRequest("p");
		assertEquals("FUNCTIONS p", doRead());
		
		protocolWriter.writeFunctionRequest("CL-USER");
		assertEquals("FUNCTIONS CL-USER", doRead());
	}
	
	@Test
	public void testWriteMacroRequest()
	throws Exception
	{
		protocolWriter.writeMacroRequest("the-package");
		assertEquals("MACROS the-package", doRead());
		
		protocolWriter.writeMacroRequest("p");
		assertEquals("MACROS p", doRead());
		
		protocolWriter.writeMacroRequest("CL-USER");
		assertEquals("MACROS CL-USER", doRead());
	}
	
	private String doRead()
	throws Exception
	{
		return reader.readLine();
	}
	
}
