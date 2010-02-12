package de.defmacro.dandelion.internal.ui.text;

import static org.junit.Assert.*;

import java.util.List;
import org.eclipse.jface.text.*;
import org.junit.*;
import de.defmacro.dandelion.testutils.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

public class TestSourceUtilitiesExtractForms
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
		project = null;
	}
	
	@Test
	public void testExtractorWithFormTestFile()
	throws Exception
	{
		//VisitableDocument visitDoc = new VisitableDocument(project.getLispPartitionedTestDocument(project.getFormTestFile()));
		//visitDoc.accept(visitor);
		List<Position> tlList = SourceUtilities.extractForms(project.getLispPartitionedTestDocument(project.getFormTestFile()));
		assertEquals(7, tlList.size());
		
		assertEquals(new Position(0, 10), tlList.get(0));
		assertEquals(new Position(46, 1+71-46), tlList.get(1));
		assertEquals(new Position(80, 1+87-80), tlList.get(2));
		assertEquals(new Position(99, 1+112-99), tlList.get(3));
		assertEquals(new Position(115, 1+136-115), tlList.get(4));
		assertEquals(new Position(139, 1+154-139), tlList.get(5));
		assertEquals(new Position(324, 1+329-324), tlList.get(6));
		
		//assertTrue(visitor.isMalformed()); //wegen ' am Ende, Position werden aber trotzdem korrekt extrahiert
	}
	
	@Test
	public void testExtractor()
	throws Exception
	{
		{
			IDocument document = project.getDocumentFor(
					"`,a\n" +
					"(a b \n 1 2 \n) \n" +
					"'''''',k\n" +
					"``,,,p\n" +
					"abc\n" +
					"( (\n \t 'x `k ,,,,```,z (\n (\n ) ) \n) )\n");
			List<Position> tlList = SourceUtilities.extractForms(document);
			assertEquals(6, tlList.size());
		}
	}
	
	@Test(expected = StructureException.class)
	public void testExtractorWithStructureMalformation()
	throws Exception
	{
		IDocument doc = project.getDocumentFor(
				"(a b \n 1 2 \n) \n" +
				"abc def ghe\n" +
				"( \n"); //schliessende Klammer fehlt
		
		SourceUtilities.extractForms(doc);
	}
	
	@Test
	public void testExtractorNestedQuote()
	throws Exception
	{
		IDocument doc = project.getDocumentFor("'('x)");
		List<Position> tlList = SourceUtilities.extractForms(doc);
		assertEquals(1, tlList.size());
		assertEquals(new Position(0, 5), tlList.get(0));
	}
	
	@Test
	public void testExtractorBackquote()
	throws Exception
	{
		{
			IDocument doc = project.getDocumentFor("`,a");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(1, tlList.size());
		}
		{
			IDocument doc = project.getDocumentFor("`   a");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(1, tlList.size());
		}
		{
			IDocument doc = project.getDocumentFor("`   a `b");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(2, tlList.size());
		}
		{
			IDocument doc = project.getDocumentFor("a `b `b (1 2 3) '(1 2 `(a ,b))");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(5, tlList.size());
		}
		{
			IDocument doc = project.getDocumentFor("`(1 2 ,3 `(1 ,2 ,3) z)");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(1, tlList.size());
		}
		{
			IDocument doc = project.getDocumentFor("`(1 2 ,3 `(1 ,2 ,3) z)\n" +
					"`a");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(2, tlList.size());
		}
		{
			IDocument doc = project.getDocumentFor("````a");
			List<Position> tlList = SourceUtilities.extractForms(doc);
			assertEquals(1, tlList.size());
			assertEquals(0, tlList.get(0).getOffset());
			assertEquals(5, tlList.get(0).getLength());
		}
	}
	
	@Test
	public void testExtractorBackquoteWithError()
	throws Exception
	{
		{
			IDocument doc = project.getDocumentFor("`");
			assertEquals(0, SourceUtilities.extractForms(doc).size());
			//assertTrue(visitor.isMalformed());
		}
		{
			IDocument doc = project.getDocumentFor("`\n'\n,");
			assertEquals(0, SourceUtilities.extractForms(doc).size());
			//assertTrue(visitor.isMalformed());
		}
		{
			IDocument doc = project.getDocumentFor("`(a b c ,d)\n" +
					"a b c ''''");
			assertEquals(4, SourceUtilities.extractForms(doc).size());
			//assertTrue(visitor.isMalformed());
		}
	}
	
	@Test
	public void testExtractorWithReaderMacros()
	throws Exception
	{
		{
			IDocument doc = project.getDocumentFor(
					"#+clisp (1 2 3)\n" +
					"#+clisp symbol\n" +
					"'#+clisp (1 2 3)\n" +
					"`'',#+clisp (setf x 'y)\n" +
					"'``,, #+clisp '''```,,symbol\n" +
					"#.symbol\n" +
					"#. symbol\n" +
					"#.(form)\n" +
					"#. (form)\n" +
					"#-(or clisp lispworks) (error 'bla)\n" +
					"#+(and clisp lispworks) (error 'bla)\n" +
					"'``#+(and clisp lispworks) (error 'bla)\n" +
					"#. 'z\n" +
					"#C(3.0s1 2.0s-1)  ;A complex with small float parts. \n" +
					"#C(5 -3)          ;A ``Gaussian integer''   \n" +
					"#C(5/3 7.0)       ;Will be converted internally to #C(1.66666 7.0) \n" +
					"#C(0 1)\n \n" +
					"#0A((0 1 5) (foo 2 (hot dog)))\n" +
					"#S(structure-name slot-key slot-value)\n" +
					"#+(or clisp) symbol\n" +
					"#+(and lispworks) symbol\n" +
					"#:uninterned\n" +
					"#p\"/pfadangabe\"\n");
			assertEquals(23, SourceUtilities.extractForms(doc).size());
		}
	}
	
	@Test
	public void testExtractorWithSelectionInCommentWithSkipComments()
	throws Exception
	{
		{
			IDocument doc = project.getDocumentFor(
					"#|\n" +
					"(setf z' \"string((\") \n" +
					"#\\( \n" +
					"|#\n");
			//Selektion in Kommentar, aber Kommentare werden uebersprungen
			assertEquals(0, SourceUtilities.extractForms(doc, 2, doc.getLength()-3).size());
		}
	}
	
	@Test
	public void testExtractorWithSelectionInComentWithoutSkipComment()
	throws Exception
	{
		{
			IDocument doc = project.getDocumentFor(
					"#|\n" +
					"(setf z' \"string((\") \n" +
					"#\\( \n" +
					"|#\n");
			//Selektion in Kommentar, Kommentare nicht ueberspringen
			assertEquals(2, SourceUtilities.extractForms(doc, 2, doc.getLength()-3, false).size());
		}
	}
	
	@Test
	public void testExtractorWithSelectionInComentCheckContentType()
	throws Exception
	{
		{
			IDocument doc = project.getDocumentFor(
					"#|\n" +
					"(setf z' \"string((\") \n" +
					"#\\( \n" +
					"|#\n");
			//Komplett selektiert, Selektionsanfang ist nicht kommentar, hier wird Kommentar uebersprungen
			assertEquals(0, SourceUtilities.extractFormsCheckContentType(doc, 0, doc.getLength()).size());
		}
		{
			IDocument doc = project.getDocumentFor(
					"  #|\n" +
					"(setf z' \"string((\") \n" +
					"#\\( \n" +
					"|#\n");
			//nicht 0-Offset
			assertEquals(0, SourceUtilities.extractFormsCheckContentType(doc, 2, doc.getLength()).size());
		}
		{
			IDocument doc = project.getDocumentFor(
					"#|\n" +
					"(setf z' \"string((\") \n" +
					"#\\( \n" +
					"|#\n");
			//Selektion in Kommentar, Kommentar wird nicht uebersprungen und die Forms aus Kommentar werden extrahiert
			assertEquals(2, SourceUtilities.extractFormsCheckContentType(doc, 2, doc.getLength()-3).size());
		}
		{
			IDocument doc = project.getDocumentFor(
					";Kommentar\n" +
					"(setf z' \"string((\") \n" +
					";Kommentar2\n");
			//Selektion in Kommentar, Kommentar wird nicht uebersprungen und die Forms aus Kommentar werden extrahiert
			assertEquals(1, SourceUtilities.extractFormsCheckContentType(doc, 11, doc.getLength()-11).size());
			assertEquals(1, SourceUtilities.extractFormsCheckContentType(doc, 0, doc.getLength()).size());
			assertEquals(3, SourceUtilities.extractFormsCheckContentType(doc, 1, doc.getLength()).size());
		}
		{
			IDocument doc = project.getDocumentFor(
					";'test\n" +
					"#+clisp 'test \n");
			assertEquals(1, SourceUtilities.extractFormsCheckContentType(doc, 0, doc.getLength()).size());
			assertEquals(2, SourceUtilities.extractFormsCheckContentType(doc, 1, doc.getLength()-1).size());
		}
		{
			IDocument doc = project.getDocumentFor(
					"#\n");
			assertEquals(1, SourceUtilities.extractFormsCheckContentType(doc, 0, doc.getLength()).size());
		}
	}
}
	
	
