package de.defmacro.dandelion.internal.core.connection.protocol;

import java.io.*;
import java.util.List;
import org.junit.*;

import de.defmacro.dandelion.internal.core.connection.*;
import de.defmacro.dandelion.internal.core.connection.IResult.TResult;
import de.defmacro.dandelion.internal.core.meta.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

@SuppressWarnings("SIC")
public class TestProtocolReader 
{
	@SuppressWarnings("UwF")
	private Writer writer;
	@SuppressWarnings("UwF")
	private IProtocolReader protocolReader;
	
	@Before
	public void setUp()
	throws Exception
	{
		final PipedInputStream pipeInput = new PipedInputStream();
		final PipedOutputStream pipeOutput = new PipedOutputStream(pipeInput);
		writer = new OutputStreamWriter(pipeOutput);
		
		//dummy Implementierung IConnection
		IEnvironment dummyServer = new Environment("localhost", 31337, "dummy", "xxx");
		IConnection dummyConnection = new LispConnection(dummyServer) {
			@Override
			public InputStream getInputStream() throws ConnectionException {
				return pipeInput;
			}
		};
		protocolReader = new ProtocolReader(dummyConnection);
	}
	
	@After
	public void tearDown()
	throws Exception
	{
		protocolReader = null;
	}
	
	@Test
	public void testReadSuccess() 
	throws Exception 
	{
		doWrite("OK");
		protocolReader.readSuccess(); //keiner Fehler wird ausgeloest -> OK
		
		doWrite("BLUBB");
		try {
			protocolReader.readSuccess();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {
			//nichts 
		}
		
		doWrite("");
		try {
			protocolReader.readSuccess();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {
			//nichts 
		}
	}
	
	@Test
	public void testReadEvalResult()
	throws Exception
	{
		IResult result;
		
		doWrite("nonsens");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/* nichts */}
		
		//Fehler Result-Form fehlt
		doWrite("OK :my-package");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("OK illegal()symbol");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("ERROR not valid");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("ERROR");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("ERROR beschreibungbase64");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("READ-ERROR "); 
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("READ-ERROR beschreibung "); //blank zuviel
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("READ-ERROR beschreibung nochwasnichterlaubt"); 
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		//nur eval-error
		doWrite("EVAL-ERROR");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("EVAL-ERROR ");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		//restart ohne beschreibung
		doWrite("EVAL-ERROR beschreibung restart1");
		try {
			protocolReader.readEvalResult();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("OK :new-package Zm9ybQ==");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.SUCCESS, result.getTyp());
		assertEquals(":new-package", result.getPackage());
		assertFalse(result.hasMultipleValues());
		assertEquals("form", result.getResult());
		assertEquals(0, result.getRestarts().size());
		assertEquals(null, result.getErrorDescription());
		
		doWrite("OK :new-package Zm9ybQ== bXVsdGlwbGUtdmFsdWUx");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.SUCCESS, result.getTyp());
		assertEquals(":new-package", result.getPackage());
		assertTrue(result.hasMultipleValues());
		assertEquals("form", result.getResult());
		assertEquals(1, result.getMultipleValueResult().size());
		assertEquals("multiple-value1", result.getMultipleValueResult().get(0));
		assertEquals(0, result.getRestarts().size());
		assertEquals(null, result.getErrorDescription());
		
		doWrite("OK :new-package Zm9ybQ== bXVsdGlwbGUtdmFsdWUx bXVsdGlwbGUtdmFsdWUy");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.SUCCESS, result.getTyp());
		assertEquals(":new-package", result.getPackage());
		assertTrue(result.hasMultipleValues());
		assertEquals("form", result.getResult());
		assertEquals(2, result.getMultipleValueResult().size());
		assertEquals("multiple-value1", result.getMultipleValueResult().get(0));
		assertEquals("multiple-value2", result.getMultipleValueResult().get(1));
		assertEquals(0, result.getRestarts().size());
		assertEquals(null, result.getErrorDescription());
		
		//error ohne Restarts
		doWrite("EVAL-ERROR U3ludGFjdGljIGVycm9yIGluIGZvcm0gKEZVTkNUSU9OIChMQU1CREEgImJsYSIgKERFQ0xBUkUgKExBTUJEQS1OQU1FIE1ZLUZVTkNUSU9OKSkgKEJMT0NLIE1ZLUZVTkNUSU9OKSkpOiBMYW1iZGEtbGlzdCAiYmxhIiBpcyBub3QgYSBsaXN0Lg==");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.EVAL_ERROR, result.getTyp());
		assertEquals(null, result.getPackage());
		assertEquals(null, result.getResult());
		assertEquals("Syntactic error in form (FUNCTION (LAMBDA \"bla\" (DECLARE (LAMBDA-NAME MY-FUNCTION)) (BLOCK MY-FUNCTION))): Lambda-list \"bla\" is not a list.", result.getErrorDescription());
		assertEquals(0, result.getRestarts().size());
		
		{ //error ein Restart
		doWrite("EVAL-ERROR RmVobGVybWVsZHVuZw== restart-sym1 ZGVzY3JpcHRpb24x");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.EVAL_ERROR, result.getTyp());
		assertEquals(null, result.getPackage());
		assertEquals(null, result.getResult());
		assertEquals("Fehlermeldung", result.getErrorDescription());
		assertEquals(1, result.getRestarts().size());
		IRestart restart = result.getRestarts().get(0);
		assertEquals(restart.getName(), "restart-sym1");
		assertEquals(restart.getDescription(), "description1");
		}
		{ //error drei Restarts
		doWrite("EVAL-ERROR RmVobGVybWVsZHVuZw== restart-sym1 ZGVzY3JpcHRpb24x restart-sym2 cmVzdGFydC1vcHRpb24y restartX U3ludGFjdGljIGVycm9yIGluIGZvcm0gKEZVTkNUSU9OIChMQU1CREEgImJsYSIgKERFQ0xBUkUgKExBTUJEQS1OQU1FIE1ZLUZVTkNUSU9OKSkgKEJMT0NLIE1ZLUZVTkNUSU9OKSkpOiBMYW1iZGEtbGlzdCAiYmxhIiBpcyBub3QgYSBsaXN0Lg==");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.EVAL_ERROR, result.getTyp());
		assertEquals(null, result.getPackage());
		assertEquals(null, result.getResult());
		assertEquals("Fehlermeldung", result.getErrorDescription());
		assertEquals(3, result.getRestarts().size());
		IRestart restart = result.getRestarts().get(0);
		assertEquals(restart.getName(), "restart-sym1");
		assertEquals(restart.getDescription(), "description1");
		restart = result.getRestarts().get(1);
		assertEquals(restart.getName(), "restart-sym2");
		assertEquals(restart.getDescription(), "restart-option2");
		restart = result.getRestarts().get(2);
		assertEquals("restartX", restart.getName());
		assertEquals("Syntactic error in form (FUNCTION (LAMBDA \"bla\" (DECLARE (LAMBDA-NAME MY-FUNCTION)) (BLOCK MY-FUNCTION))): Lambda-list \"bla\" is not a list.",
				restart.getDescription());
		}
		
		doWrite("READ-ERROR aWxsZWdhbCBmb3Jt");
		result = protocolReader.readEvalResult();
		assertEquals(TResult.READ_ERROR, result.getTyp());
		assertEquals(null, result.getPackage());
		assertEquals(null, result.getResult());
		assertEquals(0, result.getRestarts().size());
		assertEquals("illegal form", result.getErrorDescription());
		
	}
	
	@Test
	public void testReadPackageList()
	throws Exception
	{
		doWrite("PACKAGE-LIST");
		try {
			protocolReader.readPackageList();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("PACKAGE-LIST XX");
		try {
			protocolReader.readPackageList();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("PACKAGE-LIST 25");
		doWrite(" df ");
		try {
			protocolReader.readPackageList();
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		
		
		List<String> result;
		
		doWrite("PACKAGE-LIST 0");
		result = protocolReader.readPackageList();
		assertTrue(result.isEmpty());
		
		doWrite("PACKAGE-LIST 1");
		doWrite("DE.FH-TRIER.EVALSERVER.PROTOCOL");
		result = protocolReader.readPackageList();
		
		assertEquals("DE.FH-TRIER.EVALSERVER.PROTOCOL" , result.get(0));
		
		doWrite("PACKAGE-LIST 3");
		doWrite("DE.FH-TRIER.EVALSERVER.PROTOCOL");
		doWrite("COMMON-LISP-USER");
		doWrite("THE-PACKAGE");
		result = protocolReader.readPackageList();
		
		assertEquals("DE.FH-TRIER.EVALSERVER.PROTOCOL" , result.get(0));
		assertEquals("COMMON-LISP-USER", result.get(1));
		assertEquals("THE-PACKAGE", result.get(2));
	}
	
	@Test
	public void testReadFunctionList()
	throws Exception
	{
		doWrite("FUNCTION-LIST");
		try {
			protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("FUNCTION-LIST XX");
		try {
			protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("FUNCTION-LIST 25");
		doWrite("");
		try {
			protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("FUNCTION-LIST 478");
		doWrite("SYM-OHNE-DOC ");
		try {
			protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("ERROR");
		try {
			protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
			fail("ProtocolException expected");
		} catch (ProtocolException e) {/*nichts*/}
		
		doWrite("ERROR Tk8tU1VDSC1QQUNLQUdF");
		try {
			protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
			fail("ProtocolException expected");
		} catch (ProtocolException e) {
			assertEquals("ERROR NO-SUCH-PACKAGE", e.getMessage());
		}
		
		List<IMetaSymbol> result;
		
		doWrite("FUNCTION-LIST 0");
		result = protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
		assertTrue(result.isEmpty());
		
		doWrite("FUNCTION-LIST 1");
		doWrite("MAPCAR RG9rc3RyaW5n");
		result = protocolReader.readFunctionSymbols("x", TMetaType.MACRO);
		{
			IMetaSymbol meta = result.get(0);
			assertEquals("X", meta.getPackage());
			assertEquals("MAPCAR", meta.getSymbolName());
			assertEquals("Dokstring", meta.getDocumentation());
			assertTrue(meta.getArgumentList().isEmpty());
			assertEquals(TMetaType.MACRO, meta.getType());
		}
		
		doWrite("FUNCTION-LIST 1");
		doWrite("MAP TklM fn list");
		result = protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
		{
			IMetaSymbol meta = result.get(0);
			assertEquals("X", meta.getPackage());
			assertEquals("MAP", meta.getSymbolName());
			assertEquals(SymbolMetaSymbol.NO_DOCUMENTATION, meta.getDocumentation());
			assertArrayEquals(new String[] {"fn", "list"}, meta.getArgumentList().toArray());
			assertEquals(TMetaType.FUNCTION, meta.getType());
		}
		
		doWrite("FUNCTION-LIST 2");
		doWrite("MAP TklM fn list");
		doWrite("LAMBDA TGFtZGJhIEZ1bmN0aW9u x");
		result = protocolReader.readFunctionSymbols("x", TMetaType.FUNCTION);
		{
			IMetaSymbol meta = result.get(0);
			assertEquals("X", meta.getPackage());
			assertEquals("MAP", meta.getSymbolName());
			assertEquals(SymbolMetaSymbol.NO_DOCUMENTATION, meta.getDocumentation());
			assertArrayEquals(new String[] {"fn", "list"}, meta.getArgumentList().toArray());
			assertEquals(TMetaType.FUNCTION, meta.getType());
		}
		{
			IMetaSymbol meta = result.get(1);
			assertEquals("X", meta.getPackage());
			assertEquals("LAMBDA", meta.getSymbolName());
			assertEquals("Lamdba Function", meta.getDocumentation());
			assertArrayEquals(new String[] {"x"}, meta.getArgumentList().toArray());
			assertEquals(TMetaType.FUNCTION, meta.getType());
		}
	}
	
	private void doWrite(String string)
	throws Exception
	{
		writer.write(string+"\n");
		writer.flush();
	}
}
