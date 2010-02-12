package de.defmacro.dandelion.testutils;

import static org.junit.Assert.*;
import java.util.*;
import org.eclipse.jface.text.*;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.core.dom.parse.DestructuredSymbol;

public class AssertUtil 
{
	private AssertUtil()
	{
		//keine Instanz
	}
	
	private static void assertSymbolAttributesEquals(final Symbol sym, final String symbolName, final Position position,
			final String qualifier, final boolean interned, final boolean privateSym)
	{
		if(position == null) {
			assertNull(sym.getPosition());
		} else {
			assertTrue(sym.getPosition().equals(position));
		}
		
		assertEquals(interned, sym.isInterned());
		assertEquals(privateSym, sym.isPrivate());
		assertEquals(qualifier != null, sym.isQualified());
		assertEquals(symbolName, sym.getSymbolName());
		assertEquals(qualifier, sym.getQualifier());
	}
	
	public static void assertSymbolNameEquals(final String expected, final Symbol symbol) {
		assertTrue("expected " + expected + " but was " + symbol.getSymbolName(), expected.equalsIgnoreCase(symbol.getSymbolName()));
	}
	
	public static void assertDestructuringEquals(final String expected, final IDocument doc, final RequiredParameterDefinition def)
	throws BadLocationException
	{
		assertTrue(def.isDestructuring());
		assertStringInDocumentEquals(expected, def.getDestructuringParameter().getPosition(), doc);
	}
	
	/**
	 * Testet eine NICHT destructuring Parameter.
	 * Fuer destructuring test siehe {@link AssertUtil#assertDestructuringEquals(String, IDocument, RequiredParameterDefinition)}
	 * @param expected
	 * @param def
	 */
	public static void assertSymbolNameEquals(final String expected, final RequiredParameterDefinition def)
	{
		assertFalse(def.isDestructuring());
		assertSymbolNameEquals(expected, def.getParameterSymbol());
	}
	
	/**
	 * Symbolnamen vergleich mit check der Position gegen Dokument
	 * @param expected
	 * @param symbol
	 * @param doc
	 * @throws BadLocationException 
	 */
	public static void assertSNEquals(final String expected, final Symbol symbol, final IDocument doc) throws BadLocationException
	{
		assertSNEquals(expected, symbol, null, doc);
	}
	
	public static void assertSNEquals(final String expected, final Symbol symbol, final String expectedInDocument, final IDocument doc) 
	throws BadLocationException
	{
		assertSymbolNameEquals(expected, symbol);
		String test = null;
		if(expectedInDocument == null) {
			test = expected;
		} else {
			test = expectedInDocument;
		}
		assertStringInDocumentEquals(test, symbol.getPosition(), doc);
		//assertEquals(test, doc.get(symbol.getPosition().getOffset(), symbol.getPosition().getLength()));
	}
	
	public static void assertStringInDocumentEquals(final String expected, final Position pos, final IDocument doc)
	throws BadLocationException
	{
		assertEquals(expected, doc.get(pos.getOffset(), pos.getLength()));
	}
	
	public static void assertSNAttributesEquals(final Symbol sym, final String qualifier, final boolean interned, final boolean privateSym)
	{
		assertEquals(qualifier, sym.getQualifier());
		assertEquals(interned, sym.isInterned());
		assertEquals(privateSym, sym.isPrivate());
		if(qualifier != null) {
			assertTrue(sym.isQualified());
		}
	}
	
	
	public static void assertSymbolEquals(final Symbol sym, final String symbolName, final Position position,
			final String qualifier, final boolean interned, final boolean privateSym)
	{
		assertTrue(sym.getTyp() == TSExpression.SYMBOL);
		assertSymbolAttributesEquals(sym, symbolName, position, qualifier, interned, privateSym);
	}
	
	public static void assertStringSymbolEquals(final Symbol sym, final String symbolName, final Position position,
			final String qualifier, final boolean interned, final boolean privateSym)
	{
		assertTrue(sym instanceof StringSymbol);
		assertTrue(sym.getTyp() == TSExpression.STRING_SYMBOL);
		assertSymbolAttributesEquals(sym, symbolName, position, qualifier, interned, privateSym);
	}
	
	
	public static void assertKeywordSymbolEquals(final Symbol sym, final String symbolName, final Position position,
			final String qualifier, final boolean interned, final boolean privateSym)
	{
		assertTrue(sym instanceof KeywordSymbol);
		assertTrue(sym.getTyp() == TSExpression.KEYWORD_SYMBOL);
		assertSymbolAttributesEquals(sym, symbolName, position, qualifier, interned, privateSym);
	}
	
	public static void assertReaderSymbolEquals(final Symbol sym, final String symbolName, final Position position,
			final String qualifier, final boolean interned, final boolean privateSym, final char dispatchChar)
	{
		assertTrue(sym instanceof ReaderSymbol);
		assertTrue(sym.getTyp() == TSExpression.READER_SYMBOL);
		assertTrue(dispatchChar == ((ReaderSymbol)sym).getDispatchCharacter());
		assertSymbolAttributesEquals(sym, symbolName, position, qualifier, interned, privateSym);
	}
	
	public static void assertDestructuredSymbolEquals(final DestructuredSymbol sym, final String symbolName, 
			final String qualifier, final boolean isPrivate)
	{
		assertEquals(symbolName, sym.getSymbolName());
		assertEquals(qualifier, sym.getQualifier());
		assertEquals(isPrivate, sym.isPrivate());
	}
	
	public static void assertSexpModelHasNoMalformations(final ISexpModel model, final int mustSee)
	{
		DOMTestVisitorAssertNoMalformation visitor = new DOMTestVisitorAssertNoMalformation();
		model.accept(visitor);
		assertFalse(model.hasMalformation());
		assertEquals(mustSee, visitor.seen());
	}
	
	public static <T> void assertListEqualsSet(final List<T> list, final Set<T> setTwo) 
	{
		assertEquals(list.size(), setTwo.size());
		
		for(T t : list) { //jedes Element aus Menge eins ist auch in Menge zwei enthalten
			assertTrue(setTwo.contains(t));
		}
	}
	
	public static void assertMalformationsAreEqual(final ISexpModel model)
	{
		DOMTestVisitorMalformationCollector collector = new DOMTestVisitorMalformationCollector();
		model.accept(collector);
		assertListEqualsSet(model.getMalformations(), collector.getMalformations());
	}
	
	public static void assertOptionalEquals(final OrdinaryLambdaList lambdaList)
	{
		assertSymbolNameEquals("opt1", lambdaList.getOptionalParameters().get(0).getParameterSymbol());
		assertSymbolNameEquals("form", ((Form)lambdaList.getOptionalParameters().get(0).getInitValue()).getFunctionSymbol());
		assertFalse(lambdaList.getOptionalParameters().get(0).hasSuppliedTestSymbol());
		
		assertSymbolNameEquals("opt2", lambdaList.getOptionalParameters().get(1).getParameterSymbol());
		assertFalse(lambdaList.getOptionalParameters().get(1).hasInitValue());
		assertFalse(lambdaList.getOptionalParameters().get(1).hasSuppliedTestSymbol());
		
		assertSymbolNameEquals("opt3", lambdaList.getOptionalParameters().get(2).getParameterSymbol());
		assertSymbolNameEquals("quote", ((Form)lambdaList.getOptionalParameters().get(2).getInitValue()).getFunctionSymbol());
		assertSymbolNameEquals("test-p", lambdaList.getOptionalParameters().get(2).getSuppliedTestSymbol());
	}
	
	public static void assertKeyEquals(final OrdinaryLambdaList lambdaList, final boolean hasAllowOtherKeys)
	{
		assertSymbolNameEquals("key", lambdaList.getKeywordParameters().get(0).getParameterSymbol());
		assertSymbolNameEquals("quote", ((Form)lambdaList.getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
		assertFalse(lambdaList.getKeywordParameters().get(0).hasSuppliedTestSymbol());

		assertSymbolNameEquals("key2", lambdaList.getKeywordParameters().get(1).getParameterSymbol());
		assertFalse(lambdaList.getKeywordParameters().get(1).hasInitValue());
		assertFalse(lambdaList.getKeywordParameters().get(1).hasSuppliedTestSymbol());

		assertSymbolNameEquals("key3", lambdaList.getKeywordParameters().get(2).getParameterSymbol());
		assertSymbolNameEquals("quote", ((Form)lambdaList.getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
		assertSymbolNameEquals("test-p", lambdaList.getKeywordParameters().get(2).getSuppliedTestSymbol());
		
		assertEquals(hasAllowOtherKeys, lambdaList.hasAllowOtherKeys());
	}
	
	public static void assertAuxEquals(final OrdinaryLambdaList lambdaList)
	{
		assertSymbolNameEquals("aux1", lambdaList.getAuxParameters().get(0).getParameterSymbol());
		assertSymbolNameEquals("form", ((Form)lambdaList.getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
		assertFalse(lambdaList.getAuxParameters().get(0).hasSuppliedTestSymbol());
		
		assertSymbolNameEquals("aux2", lambdaList.getAuxParameters().get(1).getParameterSymbol());
		assertFalse(lambdaList.getAuxParameters().get(1).hasInitValue());
		assertFalse(lambdaList.getAuxParameters().get(1).hasSuppliedTestSymbol());
		
		assertSymbolNameEquals("aux3", lambdaList.getAuxParameters().get(2).getParameterSymbol());
		assertSymbolNameEquals("quote", ((Form)lambdaList.getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
		assertSymbolNameEquals("test-p", lambdaList.getAuxParameters().get(2).getSuppliedTestSymbol());
	}
}
