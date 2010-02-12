package de.defmacro.dandelion.internal.core.meta;


import java.util.Arrays;
import org.junit.*;

import static org.junit.Assert.*;

public class TestMetaSymbol 
{
	@Test
	public void testCreation()
	{
		try {
			new FunctionMetaSymbol("package", null, "doku", null, TMetaType.FUNCTION);
			fail("NullPointerException expected");
		} catch(NullPointerException e) { /*nichts*/ }
		
		try {
			new FunctionMetaSymbol("package", "symbol-name", null, null, null);
			fail("NullPointerException expected");
		} catch(NullPointerException e) { /*nichts*/ }
		
		try {
			new FunctionMetaSymbol(null, null, null, null, null);
			fail("NullPointerException expected");
		} catch(NullPointerException e) { /*nichts*/ }
		
		{
			IMetaSymbol sym = new FunctionMetaSymbol("pack", "MAPCAR", "Doumentation xy", Arrays.asList(new String[] { "arg1", "arg2" }), TMetaType.MACRO);
			assertEquals("pack", sym.getPackage());
			assertEquals("MAPCAR", sym.getSymbolName());
			assertEquals("Doumentation xy", sym.getDocumentation());
			assertEquals("arg1", sym.getArgumentList().get(0));
			assertEquals("arg2", sym.getArgumentList().get(1));
			assertTrue(sym.getType() == TMetaType.MACRO);
		}
		{
			IMetaSymbol sym = new FunctionMetaSymbol("package", "name", "docu", Arrays.asList(new String[] { "arg1" }), TMetaType.FUNCTION);
			assertEquals("package", sym.getPackage());
			assertEquals("name", sym.getSymbolName());
			assertEquals("docu", sym.getDocumentation());
			assertEquals("arg1", sym.getArgumentList().get(0));
			assertTrue(sym.getType() == TMetaType.FUNCTION);
		}
		{
			IMetaSymbol sym = new FunctionMetaSymbol("pack", "sym-name", null, null, TMetaType.FUNCTION);
			assertEquals("pack", sym.getPackage());
			assertEquals("sym-name", sym.getSymbolName());
			assertEquals(SymbolMetaSymbol.NO_DOCUMENTATION, sym.getDocumentation());
			assertEquals(SymbolMetaSymbol.EMPTY_ARGUMENTS, sym.getArgumentList());
			assertTrue(sym.getType() == TMetaType.FUNCTION);
		}
		{
			IMetaSymbol sym = new FunctionMetaSymbol("p", "sym-name", TMetaType.FUNCTION);
			assertEquals("p", sym.getPackage());
			assertEquals("sym-name", sym.getSymbolName());
			assertEquals(SymbolMetaSymbol.NO_DOCUMENTATION, sym.getDocumentation());
			assertEquals(SymbolMetaSymbol.EMPTY_ARGUMENTS, sym.getArgumentList());
			assertTrue(sym.getType() == TMetaType.FUNCTION);
		}
	}
	
	@Test
	public void testCompareTo()
	{
		IMetaSymbol sym0 = new FunctionMetaSymbol("p", "zzz", "Dokumenation xy", Arrays.asList(new String[] { "arg1" }), TMetaType.FUNCTION);
		IMetaSymbol sym1 = new FunctionMetaSymbol("package", "aaa", null, null, TMetaType.FUNCTION);
		IMetaSymbol sym2 = new FunctionMetaSymbol("p2", "zzz", "Doku", null, TMetaType.MACRO);
		IMetaSymbol sym3 = new FunctionMetaSymbol("x", "a", null, null, TMetaType.MACRO);
		IMetaSymbol sym4 = new FunctionMetaSymbol("p", "zzz", null, null, TMetaType.MACRO);
		IMetaSymbol sym5 = new FunctionMetaSymbol("p", "zzz", null, null, TMetaType.FUNCTION);
		IMetaSymbol sym6 = new FunctionMetaSymbol("P", "ZZZ", null, null, TMetaType.FUNCTION);
		
		assertTrue(sym1.compareTo(sym2) < 0);
		assertTrue(sym2.compareTo(sym1) > 0);
		assertTrue(sym0.compareTo(sym2) < 0);
		assertTrue(sym2.compareTo(sym0) > 0);
		assertTrue(sym3.compareTo(sym1) < 0);
		assertTrue(sym1.compareTo(sym3) > 0);
		assertTrue(sym0.compareTo(sym4) < 0);
		assertTrue(sym4.compareTo(sym0) > 0);
		
		assertTrue(sym5.compareTo(sym0) == 0);
		assertTrue(sym0.compareTo(sym5) == 0);
		assertTrue(sym6.compareTo(sym0) < 0);
		assertTrue(sym0.compareTo(sym6) > 0);
	}
	
	@Test
	public void testEqualsAndHashcode()
	{
		IMetaSymbol sym0 = new FunctionMetaSymbol("package", "zzz", "Dokumenation xy", Arrays.asList(new String[] { "arg1" }), TMetaType.FUNCTION);
		IMetaSymbol sym2 = new FunctionMetaSymbol("package", "zzz", "Doku", null, TMetaType.MACRO);
		IMetaSymbol sym3 = new FunctionMetaSymbol("package", "zzz", "Dokumenation xy", Arrays.asList(new String[] { "arg1" }), TMetaType.FUNCTION);
		IMetaSymbol sym4 = new FunctionMetaSymbol("PACKAGE", "zzz", "Dokumenation xy", Arrays.asList(new String[] { "arg1" }), TMetaType.FUNCTION);
		
		assertFalse(sym0.equals(sym2));
		assertFalse(sym2.equals(sym0));
		assertTrue(sym0.equals(sym3));
		assertTrue(sym3.equals(sym0));
		assertFalse(sym0.equals(sym4));
		assertFalse(sym4.equals(sym0));
		
		assertFalse(sym0.hashCode() ==  sym2.hashCode());
		assertFalse(sym2.hashCode() ==  sym0.hashCode());
		assertTrue(sym0.hashCode() == sym3.hashCode());
		assertTrue(sym3.hashCode() == sym0.hashCode());
		assertFalse(sym0.hashCode() == sym4.hashCode());
	}
}
