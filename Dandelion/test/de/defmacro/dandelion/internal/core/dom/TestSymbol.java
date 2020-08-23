package de.defmacro.dandelion.internal.core.dom;

import static org.junit.Assert.*;
import static de.defmacro.dandelion.testutils.AssertUtil.*;

import org.eclipse.jface.text.Position;
import org.junit.Test;

public class TestSymbol {

	@Test
	public void testSymbolString() 
	{
		{
			Symbol sym = new Symbol("symbol");
			assertSymbolEquals(sym, "symbol", null, null, true, false);
			assertEquals("symbol", sym.getQualifiedName());
		}
		{
			Symbol sym2 = new Symbol("x");
			assertSymbolEquals(sym2, "x", null, null, true, false);
			assertEquals("x", sym2.getQualifiedName());
		}
	}

	@Test
	public void testSymbolStringPosition() {
		Position position = new Position(50, 51);
		
		{
			Symbol sym = new Symbol("symbol", position);
			assertSymbolEquals(sym, "symbol", position, null, true, false);
			assertEquals("symbol", sym.getQualifiedName());
		}
		
		{
			Symbol sym = new Symbol("symbol", null);
			assertSymbolEquals(sym, "symbol", null, null, true, false);
			assertEquals("symbol", sym.getQualifiedName());
		}
	}

	@Test
	public void testSymbolStringPositionStringBooleanBoolean() 
	{
		Position position = new Position(50, 78);
		
		{
			Symbol sym = new Symbol("symbol", position, null, true, false);
			assertSymbolEquals(sym, "symbol", position, null, true, false);
			assertEquals("symbol", sym.getQualifiedName());
		}
		
		{
			Symbol sym = new Symbol("symbol", position, "de.fh-trier", true, false);
			assertSymbolEquals(sym, "symbol", position, "de.fh-trier", true, false);
			assertEquals("de.fh-trier:symbol", sym.getQualifiedName());
		}
		
		{
			Symbol sym = new Symbol("symbol", position, "de.fh-trier", true, true);
			assertSymbolEquals(sym, "symbol", position, "de.fh-trier", true, true);
			assertEquals("de.fh-trier::symbol", sym.getQualifiedName());
		}
		
		{
			Symbol sym = new Symbol("symbol", position, null, false, false);
			assertSymbolEquals(sym, "symbol", position, null, false, false);
			assertEquals("#:symbol", sym.getQualifiedName());
		}
	}
	
	@Test
	public void testIllegalSymbol()
	{
		try {
			new Symbol(null);
			fail("NullPointerException expected");
		} catch (NullPointerException e) {
			//no-op
		}
		
		try {
			//package angabe aber nicht interned
			new Symbol("my-sym", null, "package", false, true);
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) {
			//no-op
		}
		
		try {
			//kein package aber interned
			new Symbol("my-sym", null, null, false, true);
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) {
			//no-op
		}
	}
}
