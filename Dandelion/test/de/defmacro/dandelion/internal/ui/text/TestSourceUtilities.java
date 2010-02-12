package de.defmacro.dandelion.internal.ui.text;

import org.eclipse.jface.text.*;
import org.junit.*;

import de.defmacro.dandelion.internal.ui.text.SourceUtilities;
import de.defmacro.dandelion.internal.ui.text.partition.LispPartitionConstants;
import de.defmacro.dandelion.testutils.TestProject;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;
import static de.defmacro.dandelion.internal.ui.text.SourceUtilities.*;

public class TestSourceUtilities 
{
	@SuppressWarnings("UwF")
	private TestProject project;
	
	@Before
	public void setUp()
	throws Exception
	{
		project = new TestProject();
	}
	
	@After
	public void tearDown()
	throws Exception
	{
		project.dispose();
	}
	
	@Test
	public void testNormalizeNewline()
	{
		StringBuilder string = new StringBuilder("test");
		string.append(System.getProperty("line.separator"));	
		assertEquals("test\n", normalizeNewline(string).toString());
		
		//schon normal
		string = new StringBuilder("naechster Test\n");
		assertEquals("naechster Test\n", normalizeNewline(string).toString());
		
		//kein Newline am Ende
		string = new StringBuilder("mehrzeiliger\nText");
		assertEquals("mehrzeiliger\nText\n", normalizeNewline(string).toString());
	}
	
	@Test
	public void testValidForm()
	throws Exception
	{
		IDocument document = project.getLispPartitionedTestDocument(project.getValidTestFile());
		assertTrue(SourceUtilities.validForm(document, 30, 585)); //komplette Form
		assertTrue(SourceUtilities.validForm(document, 589, 603)); //Symbol
		assertTrue(SourceUtilities.validForm(document, 589, 600)); //Teilbereich Symbol
		assertTrue(SourceUtilities.validForm(document, 608, 615)); //String Symbol
		assertTrue(SourceUtilities.validForm(document, 658, 660)); // 'x
		assertFalse(SourceUtilities.validForm(document, 391, 492)); //letzte schliessende Klammer fehlt
		assertFalse(SourceUtilities.validForm(document, 560, 580)); //Kommentarblock
		assertFalse(SourceUtilities.validForm(document, 658, 659));
	}
	
	@Test
	public void testValidFormWithBackquote()
	throws Exception
	{
		{
			IDocument document = project.getDocumentFor("`");
			assertFalse(SourceUtilities.validForm(document, 0, document.getLength()));
		}
		{
			IDocument document = project.getDocumentFor("`a");
			assertTrue(SourceUtilities.validForm(document, 0, document.getLength()));
		}
		{
			IDocument document = project.getDocumentFor("`,a");
			assertTrue(SourceUtilities.validForm(document, 0, document.getLength()));
		}
		{
			IDocument document = project.getDocumentFor(",a");
			assertTrue(SourceUtilities.validForm(document, 0, document.getLength()));
		}
		{
			IDocument document = project.getDocumentFor("`(1 2 ,3)");
			assertTrue(SourceUtilities.validForm(document, 0, document.getLength()));
		}
		{
			IDocument document = project.getDocumentFor("```b");
			assertTrue(SourceUtilities.validForm(document, 0, document.getLength()));
		}
	}
	
	@Test
	public void testFindMatchingParenthesis()
	throws Exception
	{
		IDocument document = project.getLispPartitionedTestDocument(project.getValidTestFile());
		IDocument invalidDoc = project.getLispPartitionedTestDocument(project.getInvalidTestFile());
		IRegion region;
		
		//suche nach rechts
		region = SourceUtilities.findMatchingParenthesis(document, 10, true);
		assertEquals(10, region.getOffset());
		assertEquals(25+1-10, region.getLength());
		
		region = SourceUtilities.findMatchingParenthesis(document, 391, true);
		assertEquals(391, region.getOffset());
		assertEquals(493+1-391, region.getLength());
		
		//passende Klammer in Kommentar suchen
		region = SourceUtilities.findMatchingParenthesis(document, 185, true);
		assertEquals(185, region.getOffset());
		assertEquals(190+1-185, region.getLength());
		
		region = SourceUtilities.findMatchingParenthesis(invalidDoc, 30, true);
		assertNull(region);
		
		//suche nach links
		region = SourceUtilities.findMatchingParenthesis(document, 25, false);
		assertEquals(10, region.getOffset());
		assertEquals((25-10)+1, region.getLength());
		
		region = SourceUtilities.findMatchingParenthesis(document, 493, false);
		assertEquals(391, region.getOffset());
		assertEquals((493-391)+1, region.getLength());
		
		region = SourceUtilities.findMatchingParenthesis(document, 190, false);
		assertEquals(185, region.getOffset());
		assertEquals((190-185)+1, region.getLength());
	}
	
	@Test
	public void testHasValidParenthesisNesting()
	throws Exception
	{
		IDocument document = project.getLispPartitionedTestDocument(project.getInvalidTestFile());
		assertTrue(SourceUtilities.hasValidParenthesisNesting(document, 10, 26, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		assertTrue(SourceUtilities.hasValidParenthesisNesting(document, 217, 225, LispPartitionConstants.LISP_PARTITION_COMMENT));
		assertTrue(SourceUtilities.hasValidParenthesisNesting(document, 534, 553, LispPartitionConstants.LISP_PARTITION_STRING));
		
		//test Bereich ohne Klammer
		assertTrue(SourceUtilities.hasValidParenthesisNesting(document, 33, 62, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		
		
		assertFalse(SourceUtilities.hasValidParenthesisNesting(document, 396, 494, LispPartitionConstants.LISP_PARTITION_DEFAULT)); //Klammer zu wenig
		assertFalse(SourceUtilities.hasValidParenthesisNesting(document, 217, 224, LispPartitionConstants.LISP_PARTITION_COMMENT));
		assertFalse(SourceUtilities.hasValidParenthesisNesting(document, 534, 552, LispPartitionConstants.LISP_PARTITION_STRING));
	}
	
	@Test
	public void testHasInvalidParenthesisNesting()
	throws Exception
	{
		IDocument documentMisc = project.getLispPartitionedTestDocument(project.getMiscFormTestFile());
		IDocument documentValid = project.getLispPartitionedTestDocument(project.getValidTestFile());
		
		assertFalse(SourceUtilities.hasInvalidParenthesisNesting(documentValid, 10, 17, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		assertFalse(SourceUtilities.hasInvalidParenthesisNesting(documentValid, 391, 490, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		assertFalse(SourceUtilities.hasInvalidParenthesisNesting(documentValid, 185, 188, LispPartitionConstants.LISP_PARTITION_COMMENT));
		assertFalse(SourceUtilities.hasInvalidParenthesisNesting(documentValid, 534, 541, LispPartitionConstants.LISP_PARTITION_STRING));
		
		//test Bereich ohne Klammer
		assertFalse(SourceUtilities.hasInvalidParenthesisNesting(documentValid, 33, 62, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		
		assertTrue(SourceUtilities.hasInvalidParenthesisNesting(documentMisc, 0, 17, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		assertTrue(SourceUtilities.hasInvalidParenthesisNesting(documentMisc, 35, 60, LispPartitionConstants.LISP_PARTITION_DEFAULT));
		assertTrue(SourceUtilities.hasInvalidParenthesisNesting(documentMisc, 65, 69, LispPartitionConstants.LISP_PARTITION_STRING));
		assertTrue(SourceUtilities.hasInvalidParenthesisNesting(documentMisc, 97, 132, LispPartitionConstants.LISP_PARTITION_COMMENT));
	}
}
