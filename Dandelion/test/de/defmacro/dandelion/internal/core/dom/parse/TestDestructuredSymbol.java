package de.defmacro.dandelion.internal.core.dom.parse;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestDestructuredSymbol {

	@Test
	public void testDestructuredSymbol() 
	{
		{
			DestructuredSymbol destructSymbol = new DestructuredSymbol("qualifier", "name", true);
			assertEquals("qualifier", destructSymbol.getQualifier());
			assertEquals("name", destructSymbol.getSymbolName());
			assertTrue(destructSymbol.isPrivate());
		}
		{
			DestructuredSymbol destructSymbol = new DestructuredSymbol(null, null, false);
			assertNull(destructSymbol.getQualifier());
			assertNull(destructSymbol.getSymbolName());
			assertFalse(destructSymbol.isPrivate());
		}
	}
}
