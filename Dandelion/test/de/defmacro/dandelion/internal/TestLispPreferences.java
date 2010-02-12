package de.defmacro.dandelion.internal;

import java.util.*;
import org.junit.Test;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.preferences.LispPreferences;

import static org.junit.Assert.*;

public class TestLispPreferences 
{
	@Test
	public void testEncodeDecodeOutlineVisibleTypes()
	{
		Set<TSExpression> emptySet = new TreeSet<TSExpression>();
		String nullEncoded = LispPreferences.encodeOutlineVisibleTypes(emptySet);
		assertEquals("", nullEncoded);
		Set<TSExpression> nullDecoded = LispPreferences.decodeOutlineVisibleTypes("");
		assertTrue(nullDecoded.isEmpty());
		
		Set<TSExpression> encodeOne = EnumSet.of(TSExpression.DEFUN);
		String oneEncoded = LispPreferences.encodeOutlineVisibleTypes(encodeOne);
		Set<TSExpression> decodedOne = LispPreferences.decodeOutlineVisibleTypes(oneEncoded);
		assertEquals(1, decodedOne.size());
		assertTrue(decodedOne.contains(TSExpression.DEFUN));
			
		Set<TSExpression> encodeThree = EnumSet.of(TSExpression.DEFMACRO, TSExpression.FORM, TSExpression.LAMBDA);
		String threeEncoded = LispPreferences.encodeOutlineVisibleTypes(encodeThree);
		Set<TSExpression> decodedThree = LispPreferences.decodeOutlineVisibleTypes(threeEncoded);
		assertEquals(3, decodedThree.size());
		assertTrue(decodedThree.contains(TSExpression.DEFMACRO));
		assertTrue(decodedThree.contains(TSExpression.FORM));
		assertTrue(decodedThree.contains(TSExpression.LAMBDA));
		
		Set<TSExpression> encodeAll = EnumSet.allOf(TSExpression.class);
		String encodedAll = LispPreferences.encodeOutlineVisibleTypes(encodeAll);
		Set<TSExpression> decodedAll = LispPreferences.decodeOutlineVisibleTypes(encodedAll);
		assertEquals(TSExpression.values().length, decodedAll.size());
		assertEquals(encodeAll, decodedAll);
	}
}
