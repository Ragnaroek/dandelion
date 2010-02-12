package de.defmacro.dandelion.internal.ui.text;

import org.eclipse.jface.text.IDocument;
import org.junit.*;

import de.defmacro.dandelion.internal.ui.text.VisitableDocument;
import de.defmacro.dandelion.testutils.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

public class TestVisitableDocument 
{	
	@SuppressWarnings("UwF")
	private VisitableDocument visitDocValid;
	@SuppressWarnings("UwF")
	private SExpressionTestVisitor visitor;
	@SuppressWarnings("UwF")
	private TestProject project;
	
	@Before
	public void setUp()
	throws Exception
	{
		project = new TestProject();
		visitDocValid = new VisitableDocument(project.getLispPartitionedTestDocument(project.getValidTestFile()));
		visitor = new SExpressionTestVisitor();
	}
	
	@After
	public void tearDown()
	throws Exception
	{
		project.dispose();
		project = null;
		visitDocValid = null;
		visitor = null;
	}
	
	@Test
	public void testAcceptSexpressionVisitor()
	throws Exception
	{	
		visitDocValid.accept(visitor);
		assertEquals(5, visitor.openOffsetsTL.size());
		assertEquals(5, visitor.closeOffsetsTL.size());
		assertEquals(10, visitor.closeOffsets.size());
		assertEquals(10, visitor.openOffsets.size());
		assertEquals(4, visitor.startSymbolOffsetsTL.size());
		assertEquals(4, visitor.endSymbolOffsetTL.size());
		assertEquals(5, visitor.quotes.size());
		assertEquals(0, visitor.malformations.size());
		
		assertArrayEquals(new Integer[] {10, 30, 648, 670, 679}, visitor.openOffsetsTL.toArray(new Integer[3]));
		assertArrayEquals(new Integer[] {25, 584, 653, 676, 692}, visitor.closeOffsetsTL.toArray(new Integer[3]));
		
		assertArrayEquals(new Integer[] {64, 88, 391, 396, 397, 403, 466, 472, 680, 687}, visitor.openOffsets.toArray(new Integer[8]));
		assertArrayEquals(new Integer[] {104, 105, 421, 422, 490, 491, 492, 493, 684, 691}, visitor.closeOffsets.toArray(new Integer[8]));
		
		assertArrayEquals(new Integer[] {589, 608, 659, 665}, visitor.startSymbolOffsetsTL.toArray(new Integer[3]));
		assertArrayEquals(new Integer[] {603, 615, 659, 667}, visitor.endSymbolOffsetTL.toArray(new Integer[3]));
		
		assertArrayEquals(new Integer[] {658, 664, 669, 678, 686}, visitor.quotes.toArray());
		
		assertArrayEquals(new Integer[] {11, 19, 31, 37, 65, 69, 73, 78, 89, 101, 392, 398, 404, 418, 
								    467, 473, 487, 533, 649, 671, 673, 675, 681, 683, 688, 690}, visitor.startSymbolOffsets.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {17, 24, 35, 62, 67, 71, 76, 86, 99, 103, 394, 401, 416, 420, 
				                    470, 485, 489, 553, 652, 671, 673, 675, 681, 683, 688, 690}, visitor.endSymbolOffsets.toArray(new Integer[1]));
	}
	
	@Test
	public void testAcceptSexpressionVisitorWithBounds()
	throws Exception
	{
		visitDocValid.accept(visitor, 9, 26, true);
		
		assertEquals(0, visitor.closeOffsets.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.startSymbolOffsetsTL.size());
		assertEquals(0, visitor.endSymbolOffsetTL.size());
		assertEquals(0, visitor.quotes.size());
		assertEquals(0, visitor.malformations.size());
		
		assertArrayEquals(new Integer[] {10}, visitor.openOffsetsTL.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {25}, visitor.closeOffsetsTL.toArray(new Integer[1]));
		
		assertArrayEquals(new Integer[] {11, 19}, visitor.startSymbolOffsets.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {17, 24}, visitor.endSymbolOffsets.toArray(new Integer[1]));
	}
	
	@Test
	public void testAcceptSexpressionVisitorWithInvalidNesting()
	throws Exception
	{
		VisitableDocument visitDocInvalid = new VisitableDocument(project.getLispPartitionedTestDocument(project.getInvalidTestFile()));
		visitDocInvalid.accept(visitor);

		assertEquals(1, visitor.malformations.size());
		assertEquals(30, visitor.malformations.get(0).position.getOffset());
	}
	
	@Test
	public void testFormTypes()
	throws Exception
	{
		VisitableDocument visitFormTypes = new VisitableDocument(project.getLispPartitionedTestDocument(project.getFormTestFile()));
		visitFormTypes.accept(visitor);
		
		assertEquals(0, visitor.malformations.size());
		
		assertArrayEquals(new Integer[] {1, 46, 119, 141, 324}, visitor.openOffsetsTL.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {9, 71, 136, 154, 329}, visitor.closeOffsetsTL.toArray(new Integer[1]));
		
		assertArrayEquals(new Integer[] {65, 143, 145, 147}, visitor.openOffsets.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {66, 148, 150, 152}, visitor.closeOffsets.toArray(new Integer[1]));
		
		assertArrayEquals(new Integer[] {80, 103}, visitor.startSymbolOffsetsTL.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {87, 112}, visitor.endSymbolOffsetTL.toArray(new Integer[1]));
		
		assertArrayEquals(new Integer[] {0, 99, 115, 139, 334}, visitor.quotes.toArray());

		assertArrayEquals(new Integer[] {2, 4, 6, 8, 47, 53, 68, 120, 129, 325}, visitor.startSymbolOffsets.toArray(new Integer[1]));
		assertArrayEquals(new Integer[] {2, 4, 6, 8, 51, 63, 70, 127, 135, 328}, visitor.endSymbolOffsets.toArray(new Integer[1]));
	}
	
	@Test
	public void testVisitWithSkipCommentCompleteFileSelected()
	throws Exception
	{
		IDocument document = project.getDocumentFor(
				"#|Kommentar\nam\nAnfang\n|#" +
				";Zeilenkommentar (form wird nicht beachtet)\n" +
				"#|Komm)entar|#(#|Kom(mentar|#setf x 'y)");
		
		VisitableDocument visitDoc = new VisitableDocument(document);
		visitDoc.accept(visitor, 0, visitDoc.getDocument().getLength(), true);
		
		assertEquals(0, visitor.malformations.size());
		assertEquals(0, visitor.closeOffsets.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.startSymbolOffsetsTL.size());
		assertEquals(0, visitor.endSymbolOffsetTL.size());
		
		assertEquals(1, visitor.quotes.size());
		
		assertEquals("(#|Kom(mentar|#setf x 'y)", document.get(visitor.openOffsetsTL.get(0), 1+visitor.closeOffsetsTL.get(0) - visitor.openOffsetsTL.get(0)));
		assertEquals("setf", document.get(visitor.startSymbolOffsets.get(0), 1+visitor.endSymbolOffsets.get(0) - visitor.startSymbolOffsets.get(0)));
		assertEquals("x", document.get(visitor.startSymbolOffsets.get(1), 1+visitor.endSymbolOffsets.get(1) - visitor.startSymbolOffsets.get(1)));
		assertEquals("y", document.get(visitor.startSymbolOffsets.get(2), 1+visitor.endSymbolOffsets.get(2) - visitor.startSymbolOffsets.get(2)));
		assertEquals("'", document.get(visitor.quotes.get(0), 1));
	}
	
	@Test
	public void testVisitWithSkipCommentSubsequenceSelected()
	throws Exception
	{   //sollte gleiches Ergebnis wie completFileSelected liefern
		IDocument document = project.getDocumentFor(
				"#|Kommentar\nam\nAnfang\n|#" +
				";Zeilenkommentar (form wird nicht beachtet)\n" +
				"#|Komm)entar|#(#|Kom(mentar|#setf x 'y)");
		
		VisitableDocument visitDoc = new VisitableDocument(document);
		visitDoc.accept(visitor, 15, visitDoc.getDocument().getLength(), true);
		
		assertEquals(0, visitor.malformations.size());
		assertEquals(0, visitor.closeOffsets.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.startSymbolOffsetsTL.size());
		assertEquals(0, visitor.endSymbolOffsetTL.size());
		
		assertEquals("(#|Kom(mentar|#setf x 'y)", document.get(visitor.openOffsetsTL.get(0), 1+visitor.closeOffsetsTL.get(0) - visitor.openOffsetsTL.get(0)));
		assertEquals("setf", document.get(visitor.startSymbolOffsets.get(0), 1+visitor.endSymbolOffsets.get(0) - visitor.startSymbolOffsets.get(0)));
		assertEquals("x", document.get(visitor.startSymbolOffsets.get(1), 1+visitor.endSymbolOffsets.get(1) - visitor.startSymbolOffsets.get(1)));
		assertEquals("y", document.get(visitor.startSymbolOffsets.get(2), 1+visitor.endSymbolOffsets.get(2) - visitor.startSymbolOffsets.get(2)));
		assertEquals("'", document.get(visitor.quotes.get(0), 1));
	}
	
	@Test
	public void testVisitWithoutSkipComment()
	throws Exception
	{ 
		IDocument document = project.getDocumentFor(
				";Kommentar\n" +
				"symbol\n" +
				"\"string\"\n" +
				"#\\c\n" +
				"#|Kommentar|#\n" +
				"(form)" +
				"#| test |#" ); //3 Symbole
		
		VisitableDocument visitDoc = new VisitableDocument(document);
		visitDoc.accept(visitor, 0, visitDoc.getDocument().getLength(), false);
		
		assertEquals(0, visitor.malformations.size());
		assertEquals(0, visitor.quotes.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.closeOffsets.size());
		
		//Kommentarbegrenzer werden auch zum Symbol (oder Teil davon)
		assertEquals(";Kommentar", document.get(visitor.startSymbolOffsetsTL.get(0), 1+visitor.endSymbolOffsetTL.get(0) - visitor.startSymbolOffsetsTL.get(0)));
		assertEquals("symbol", document.get(visitor.startSymbolOffsetsTL.get(1), 1+visitor.endSymbolOffsetTL.get(1) - visitor.startSymbolOffsetsTL.get(1)));
		assertEquals("\"string\"", document.get(visitor.startSymbolOffsetsTL.get(2), 1+visitor.endSymbolOffsetTL.get(2) - visitor.startSymbolOffsetsTL.get(2)));
		assertEquals("#\\c", document.get(visitor.startSymbolOffsetsTL.get(3), 1+visitor.endSymbolOffsetTL.get(3) - visitor.startSymbolOffsetsTL.get(3)));
		assertEquals("#|Kommentar|#", document.get(visitor.startSymbolOffsetsTL.get(4), 1+visitor.endSymbolOffsetTL.get(4) - visitor.startSymbolOffsetsTL.get(4)));
		assertEquals("#|", document.get(visitor.startSymbolOffsetsTL.get(5), 1+visitor.endSymbolOffsetTL.get(5) - visitor.startSymbolOffsetsTL.get(5)));
		assertEquals("test", document.get(visitor.startSymbolOffsetsTL.get(6), 1+visitor.endSymbolOffsetTL.get(6) - visitor.startSymbolOffsetsTL.get(6)));
		assertEquals("|#", document.get(visitor.startSymbolOffsetsTL.get(7), 1+visitor.endSymbolOffsetTL.get(7) - visitor.startSymbolOffsetsTL.get(7)));
		
		assertEquals("(form)", document.get(visitor.openOffsetsTL.get(0), 1+visitor.closeOffsetsTL.get(0) - visitor.openOffsetsTL.get(0)));
		assertEquals("form", document.get(visitor.startSymbolOffsets.get(0), 1+visitor.endSymbolOffsets.get(0) - visitor.startSymbolOffsets.get(0)));
	}
	
	@Test
	public void testVisitWithoutSkipCommentWithStringsAndChars()
	throws Exception
	{ 
		IDocument document = project.getDocumentFor(
				"#|" +
				"(format nil ~a \"string(\")\n" +
				"#\\(\n" +
				"|#" ); //3 Symbole
		
		VisitableDocument visitDoc = new VisitableDocument(document);
		visitDoc.accept(visitor, 0, visitDoc.getDocument().getLength(), false);
		
		assertEquals(0, visitor.malformations.size());
		assertEquals(0, visitor.quotes.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.closeOffsets.size());
		
		//Kommentarbegrenzer werden auch zum Symbol (oder Teil davon)
		assertEquals("#|", document.get(visitor.startSymbolOffsetsTL.get(0), 1+visitor.endSymbolOffsetTL.get(0) - visitor.startSymbolOffsetsTL.get(0)));
		assertEquals("#\\(", document.get(visitor.startSymbolOffsetsTL.get(1), 1+visitor.endSymbolOffsetTL.get(1) - visitor.startSymbolOffsetsTL.get(1)));
		assertEquals("|#", document.get(visitor.startSymbolOffsetsTL.get(2), 1+visitor.endSymbolOffsetTL.get(2) - visitor.startSymbolOffsetsTL.get(2)));
		
		assertEquals("(format nil ~a \"string(\")", document.get(visitor.openOffsetsTL.get(0), 1+visitor.closeOffsetsTL.get(0) - visitor.openOffsetsTL.get(0)));
		
		assertEquals("format", document.get(visitor.startSymbolOffsets.get(0), 1+visitor.endSymbolOffsets.get(0) - visitor.startSymbolOffsets.get(0)));
		assertEquals("nil", document.get(visitor.startSymbolOffsets.get(1), 1+visitor.endSymbolOffsets.get(1) - visitor.startSymbolOffsets.get(1)));
		assertEquals("~a", document.get(visitor.startSymbolOffsets.get(2), 1+visitor.endSymbolOffsets.get(2) - visitor.startSymbolOffsets.get(2)));
		assertEquals("\"string(\"", document.get(visitor.startSymbolOffsets.get(3), 1+visitor.endSymbolOffsets.get(3) - visitor.startSymbolOffsets.get(3)));
	}
	
	@Test
	public void testVisitWithBackquoteAndComma()
	throws Exception
	{
		IDocument document = project.getDocumentFor(
				";Kommentar ,, `\n" +
				"`sym \n" + //1bq, 1sym
				"`(1 2 ,a)\n" + //1bq, 1comma, 3sym, 1tl
				"```b\n" +//3bq, 1sym
				"`,b\n" +//1bg, 1comma, 1 sym
				"`(`(1 ,a) ,b `,x)\n" + //3bq, 3comma, 4sym, 1tl, 1sl
				"\"backquote-instring `\"\n" + //0bq, 1sym (string)
				"#\\`");
		VisitableDocument visitDoc = new VisitableDocument(document);
		visitDoc.accept(visitor, 0, visitDoc.getDocument().getLength(), true);
	
		assertEquals(0, visitor.quotes.size());
		assertEquals(0, visitor.malformations.size());
		assertEquals(9, visitor.backquotes.size());
		assertEquals(5, visitor.commas.size());
		
		for(int i=0;i<9;i++) {
			assertEquals("`", document.get(visitor.backquotes.get(i), 1+visitor.backquotes.get(i) - visitor.backquotes.get(i)));
		}
		for(int i=0;i<5;i++) {
			assertEquals(",", document.get(visitor.commas.get(i), 1+visitor.commas.get(i) - visitor.commas.get(i)));
		}
		
		assertEquals("sym", document.get(visitor.startSymbolOffsetsTL.get(0), 1+visitor.endSymbolOffsetTL.get(0) - visitor.startSymbolOffsetsTL.get(0)));
		assertEquals("b", document.get(visitor.startSymbolOffsetsTL.get(1), 1+visitor.endSymbolOffsetTL.get(1) - visitor.startSymbolOffsetsTL.get(1)));
		assertEquals("b", document.get(visitor.startSymbolOffsetsTL.get(2), 1+visitor.endSymbolOffsetTL.get(2) - visitor.startSymbolOffsetsTL.get(2)));
		assertEquals("\"backquote-instring `\"", document.get(visitor.startSymbolOffsetsTL.get(3), 1+visitor.endSymbolOffsetTL.get(3) - visitor.startSymbolOffsetsTL.get(3)));
		assertEquals("#\\`", document.get(visitor.startSymbolOffsetsTL.get(4), 1+visitor.endSymbolOffsetTL.get(4) - visitor.startSymbolOffsetsTL.get(4)));
		
		assertEquals("1", document.get(visitor.startSymbolOffsets.get(0), 1+visitor.endSymbolOffsets.get(0) - visitor.startSymbolOffsets.get(0)));
		assertEquals("2", document.get(visitor.startSymbolOffsets.get(1), 1+visitor.endSymbolOffsets.get(1) - visitor.startSymbolOffsets.get(1)));
		assertEquals("a", document.get(visitor.startSymbolOffsets.get(2), 1+visitor.endSymbolOffsets.get(2) - visitor.startSymbolOffsets.get(2)));
		assertEquals("1", document.get(visitor.startSymbolOffsets.get(3), 1+visitor.endSymbolOffsets.get(3) - visitor.startSymbolOffsets.get(3)));
		assertEquals("a", document.get(visitor.startSymbolOffsets.get(4), 1+visitor.endSymbolOffsets.get(4) - visitor.startSymbolOffsets.get(4)));
		assertEquals("b", document.get(visitor.startSymbolOffsets.get(5), 1+visitor.endSymbolOffsets.get(5) - visitor.startSymbolOffsets.get(5)));
		assertEquals("x", document.get(visitor.startSymbolOffsets.get(6), 1+visitor.endSymbolOffsets.get(6) - visitor.startSymbolOffsets.get(6)));
		
		assertEquals("(1 2 ,a)", document.get(visitor.openOffsetsTL.get(0), 1+visitor.closeOffsetsTL.get(0) - visitor.openOffsetsTL.get(0)));
		assertEquals("(`(1 ,a) ,b `,x)", document.get(visitor.openOffsetsTL.get(1), 1+visitor.closeOffsetsTL.get(1) - visitor.openOffsetsTL.get(1)));
		
		assertEquals("(1 ,a)", document.get(visitor.openOffsets.get(0), 1+visitor.closeOffsets.get(0) - visitor.openOffsets.get(0)));
	}
	
	@Test
	public void testVisitMixedContentTypeSymbols()
	throws Exception
	{
		IDocument document = project.getDocumentFor(
				"#p\"/pfad/angabe\"\n" + // Symbol ist zusammengesetzt aus normalem Content-Type und String-Content-Type
				"ier#\\c\n" + //Normaler-Typ und char
				"\"string\"symbol\n" + //zwei symbole
				"sym\"string\"rest\n" + //ein symbol
				"#\\xyze \n" // ein symbol
				);
		VisitableDocument visitDoc = new VisitableDocument(document);
		visitDoc.accept(visitor, 0, visitDoc.getDocument().getLength(), true);
		
		assertEquals(0, visitor.openOffsetsTL.size());
		assertEquals(0, visitor.closeOffsetsTL.size());
		assertEquals(0, visitor.closeOffsets.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.endSymbolOffsets.size());
		assertEquals(0, visitor.startSymbolOffsets.size()); 
		assertEquals(0, visitor.quotes.size()); 
		assertEquals(0, visitor.backquotes.size());
		assertEquals(0, visitor.commas.size());
		assertEquals(0, visitor.malformations.size()); 
		
		assertEquals("#p\"/pfad/angabe\"", document.get(visitor.startSymbolOffsetsTL.get(0), 1+visitor.endSymbolOffsetTL.get(0) - visitor.startSymbolOffsetsTL.get(0)));
		assertEquals("ier#\\c", document.get(visitor.startSymbolOffsetsTL.get(1), 1+visitor.endSymbolOffsetTL.get(1) - visitor.startSymbolOffsetsTL.get(1)));
		assertEquals("\"string\"", document.get(visitor.startSymbolOffsetsTL.get(2), 1+visitor.endSymbolOffsetTL.get(2) - visitor.startSymbolOffsetsTL.get(2)));
		assertEquals("symbol", document.get(visitor.startSymbolOffsetsTL.get(3), 1+visitor.endSymbolOffsetTL.get(3) - visitor.startSymbolOffsetsTL.get(3)));
		assertEquals("sym\"string\"rest", document.get(visitor.startSymbolOffsetsTL.get(4), 1+visitor.endSymbolOffsetTL.get(4) - visitor.startSymbolOffsetsTL.get(4)));
		assertEquals("#\\xyze", document.get(visitor.startSymbolOffsetsTL.get(5), 1+visitor.endSymbolOffsetTL.get(5) - visitor.startSymbolOffsetsTL.get(5)));
	}
	
	/* Alle assertions
	 *  assertEquals(0, visitor.openOffsetsTL.size());
		assertEquals(0, visitor.closeOffsetsTL.size());
		assertEquals(0, visitor.closeOffsets.size());
		assertEquals(0, visitor.openOffsets.size());
		assertEquals(0, visitor.startSymbolOffsetsTL.size()); -
		assertEquals(0, visitor.endSymbolOffsetTL.size()); -
		assertEquals(0, visitor.endSymbolOffsets.size()); -
		assertEquals(0, visitor.startSymbolOffsets.size()); - 
		assertEquals(0, visitor.quotes.size());  -
		assertEquals(0, visitor.backquotes.size()); -
		assertEquals(0, visitor.commas.size()); -
		assertEquals(0, visitor.malformations.size()); -
	 */
}
