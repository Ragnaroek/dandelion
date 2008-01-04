package de.fh_trier.eclipse.lisp.internal.core.dom;

import java.util.List;

import org.eclipse.jface.text.*;
import org.junit.*;

import de.fh_trier.eclipse.lisp.testutils.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;
import static de.fh_trier.eclipse.lisp.testutils.AssertUtil.*;
import static de.fh_trier.eclipse.lisp.internal.core.dom.Symbol.*;

public class TestSexpModel 
{
	@SuppressWarnings("UwF")
	private TestProject project;
	private IDocument testDocument;
	@SuppressWarnings("UwF")
	private ISexpModel model;
	
	@Before
	public void setUp()
	throws Exception
	{
		project = new TestProject();
		testDocument = project.getLispPartitionedTestDocument(project.getValidTestFile());
		model = new SexpModel(testDocument);
	}
	
	@After
	public void tearDown()
	throws Exception
	{
		project.dispose();
		project = null;
	}

	@Test
	public void testEmptyModel()
	{
		assertNull(model.getRoot());
		assertEquals(0, model.getTopLevelForms().size());
	}
	
	@Test
	public void testValidFile()
	throws Exception
	{
		model.createDOM();
		
		assertNotNull(model.getRoot());
		assertNull(model.getRoot().getParent());
		assertEquals(9, model.getTopLevelForms().size());
		assertFalse(model.hasMalformation());
		
		List<SExpression> tlForm = model.getTopLevelForms();
		SExpression node;
		SExpression child;
		SExpression child2;
		SExpression child3;
		SExpression child4;
		Form form;
		SExpression sexp;
		
		sexp = tlForm.get(0);
		assertTrue(new Position(10, 1+25-10).equals(sexp.getPosition()));
		assertTrue(sexp.getParent() == model.getRoot());
		assertEquals(2, sexp.getChildren().size());
		assertTrue(sexp.isToplevel());
		
		{
			Symbol sym = (Symbol)sexp.getChild(0);
			assertTrue(sym.getParent() == sexp);
			assertEquals("require", sym.getSymbolName());
			assertTrue(new Position(11, 1+17-11).equals(sexp.getChild(0).getPosition()));
		}
		{
			Symbol sym = (StringSymbol)sexp.getChild(1);
			assertTrue(sym.getParent() == sexp);
			assertEquals("\"comm\"", sym.getSymbolName());
			assertTrue(new Position(19, 1+24-19).equals(sexp.getChild(1).getPosition()));
		}
		sexp = null;

		node = tlForm.get(1);
		DefunForm defun = (DefunForm)node;
		assertTrue(new Position(30, 584+1-30).equals(defun.getPosition()));
		assertTrue(defun.getParent() == model.getRoot());
		assertEquals(5, defun.getChildren().size());
		assertTrue(node.isToplevel());
		assertEquals("defun", defun.getFunctionSymbol().getSymbolName());
		
			//defun-symbol
			assertTrue(new Position(31, 35+1-31).equals(defun.getFunctionSymbol().getPosition()));
			assertTrue(defun.getFunctionSymbol() == defun.getChild(0));
			assertTrue(defun.getFunctionSymbol().getParent() == defun);
			assertFalse(defun.getFunctionSymbol().isToplevel());
		
			//funktionsname: webengine++lisp-webfetcher
			assertEquals("webengine++lisp-webfetcher", ((Symbol)defun.getChild(1)).getSymbolName());
			assertTrue(new Position(37, 62+1-37).equals(defun.getChild(1).getPosition()));
			assertTrue(defun.getChild(1).getParent() == defun);
			assertFalse(defun.getChild(1).isToplevel());
			
			//lambda-list
			child = defun.getChild(2);
			assertTrue(new Position(64, 105+1-64).equals(child.getPosition()));
			assertEquals(5, child.getChildren().size());
			assertTrue(child.getParent() == defun);
			assertFalse(child.isToplevel());
			
				assertEquals("num", ((Symbol)child.getChild(0)).getSymbolName());
				assertTrue(new Position(65, 67+1-65).equals(child.getChild(0).getPosition()));
				assertTrue(child.getChild(0).getParent() == child);
				assertFalse(child.getChild(0).isToplevel());
			
				assertEquals("url", ((Symbol)child.getChild(1)).getSymbolName());
				assertTrue(new Position(69, 71+1-69).equals(child.getChild(1).getPosition()));
				assertTrue(child.getChild(1).getParent() == child);
				assertFalse(child.getChild(1).isToplevel());
				
				assertEquals("&key", ((Symbol)child.getChild(2)).getSymbolName());
				assertTrue(new Position(73, 76+1-73).equals(child.getChild(2).getPosition()));
				assertTrue(child.getChild(2).getParent() == child);
				assertFalse(child.getChild(2).isToplevel());
				
				assertEquals("dest-path", ((Symbol)child.getChild(3)).getSymbolName());
				assertTrue(new Position(78, 86+1-78).equals(child.getChild(3).getPosition()));
				assertTrue(child.getChild(3).getParent() == child);
				assertFalse(child.getChild(3).isToplevel());
				
				//und nochmal liste
				child2 = child.getChild(4);
				assertTrue(new Position(88, 104+1-88).equals(child2.getPosition()));
				assertEquals(2, child2.getChildren().size());
				assertTrue(child2.getParent() == child);
				assertFalse(child2.isToplevel());
					
					assertEquals("want-string", ((Symbol)child2.getChild(0)).getSymbolName());
					assertTrue(new Position(89, 99+1-89).equals(child2.getChild(0).getPosition()));
					assertTrue(child2.getChild(0).getParent() == child2);
					assertFalse(child2.getChild(0).isToplevel());
					
					assertEquals("nil", ((Symbol)child2.getChild(1)).getSymbolName());
					assertTrue(new Position(101, 103+1-101).equals(child2.getChild(1).getPosition()));
					assertTrue(child2.getChild(1).getParent() == child2);
					assertFalse(child2.getChild(1).isToplevel());
				child2 = null;
			child = null;
			
			//let-block
			child = defun.getChildren().get(3);
			assertTrue(new Position(391, 493+1-391).equals(child.getPosition()));
			assertEquals(2, child.getChildren().size());
			assertTrue(child.getParent() == defun);
			assertFalse(child.isToplevel());
			
				assertEquals("let", ((Symbol)child.getChild(0)).getSymbolName());
				assertTrue(new Position(392, 394+1-392).equals(child.getChild(0).getPosition()));
				assertTrue(child.getChild(0).getParent() == child);
				assertFalse(child.getChild(0).isToplevel());
				
				child2 = child.getChild(1); //let variablen liste
				assertTrue(new Position(396, 492+1-396).equals(child2.getPosition()));
				assertEquals(2, child2.getChildren().size());
				assertTrue(child2.getParent() == child);
				assertFalse(child2.isToplevel());
					
					child3 = child2.getChildren().get(0); //liste (host...
					assertTrue(new Position(397, 422+1-397).equals(child3.getPosition()));
					assertTrue(child3.hasChildren());
					assertEquals(2, child3.getChildren().size());
					assertTrue(child3.getParent() == child2);
					assertFalse(child3.isToplevel());
				
						assertEquals("host", ((Symbol)child3.getChild(0)).getSymbolName());
						assertTrue(new Position(398, 401+1-398).equals(child3.getChild(0).getPosition()));
						assertTrue(child3.getChild(0).getParent() == child3);
						assertFalse(child3.getChild(0).isToplevel());
					
						child4 = child3.getChild(1); //aufruf (url++get-host...
						assertTrue(new Position(403, 421+1-403).equals(child4.getPosition()));
						assertEquals(2, child4.getChildren().size());
						assertTrue(child4.getParent() == child3);
						assertFalse(child4.isToplevel());
						
							assertEquals("url++get-host", ((Symbol)child4.getChild(0)).getSymbolName());
							assertTrue(new Position(404, 416+1-404).equals(child4.getChild(0).getPosition()));
							assertTrue(child4.getChild(0).getParent() == child4);
							assertFalse(child4.getChild(0).isToplevel());
					
							assertEquals("url", ((Symbol)child4.getChild(1)).getSymbolName());
							assertTrue(new Position(418, 420+1-418).equals(child4.getChild(1).getPosition()));
							assertTrue(child4.getChild(1).getParent() == child4);
							assertFalse(child4.getChild(1).isToplevel());
						child4 = null;
						
					child3 = child2.getChildren().get(1); //(page...
					assertTrue(new Position(466, 491+1-466).equals(child3.getPosition()));
					assertEquals(2, child3.getChildren().size());
					assertTrue(child3.getParent() == child2);
					assertFalse(child3.isToplevel());
					
						assertEquals("page", ((Symbol)child3.getChild(0)).getSymbolName());
						assertTrue(new Position(467, 470+1-467).equals(child3.getChild(0).getPosition()));
						assertTrue(child3.getChild(0).getParent() == child3);
						assertFalse(child3.getChild(0).isToplevel());
						
						child4 = child3.getChildren().get(1); //aufruf (url++get-page url)
						assertTrue(new Position(472, 490+1-472).equals(child4.getPosition()));
						assertEquals(2, child4.getChildren().size());
						assertTrue(child4.getParent() == child3);
						assertFalse(child4.isToplevel());
						
							assertEquals("url++get-page", ((Symbol)child4.getChild(0)).getSymbolName());
							assertTrue(new Position(473, 485+1-473).equals(child4.getChild(0).getPosition()));
							assertTrue(child4.getChild(0).getParent() == child4);
							assertFalse(child4.getChild(0).isToplevel());
						
							assertEquals("url", ((Symbol)child4.getChild(1)).getSymbolName());
							assertTrue(new Position(487, 489+1-487).equals(child4.getChild(1).getPosition()));
							assertTrue(child4.getChild(1).getParent() == child4);
							assertFalse(child4.getChild(1).isToplevel());
						child4 = null;
					child3 = null;
				child2 = null;
			child = null;
			
			assertEquals("\"(Klammer in String)\"", ((StringSymbol)defun.getChild(4)).getSymbolName());
			assertTrue(new Position(533, 553+1-533).equals(defun.getChild(4).getPosition()));
			assertTrue(defun.getChild(4).getParent() == defun);
			assertFalse(defun.getChild(4).isToplevel());
		defun = null;
						
		node = tlForm.get(2);
		Symbol sym = (Symbol)node;
		assertFalse(sym.hasChildren());
		assertTrue(new Position(589, 603+1-589).equals(sym.getPosition()));
		assertEquals("toplevel-symbol", sym.getSymbolName());
		assertTrue(node.getParent() == model.getRoot());
		assertTrue(node.isToplevel());
		sym = null;
		
		node = tlForm.get(3);
		sym = (Symbol)node;
		assertFalse(sym.hasChildren());
		assertTrue(new Position(608, 615+1-608).equals(sym.getPosition()));
		assertTrue(sym instanceof StringSymbol);
		assertEquals("\"String\"", sym.getSymbolName());
		assertTrue(node.getParent() == model.getRoot());
		assertTrue(node.isToplevel());
		sym = null;
		
		node = tlForm.get(4); //(form)
		assertEquals(1, node.getChildren().size());
		assertTrue(new Position(648, 653+1-648).equals(node.getPosition()));
		assertTrue(node.getParent() == model.getRoot());
		assertTrue(node.isToplevel());
		
			assertEquals("form", ((Symbol)node.getChild(0)).getSymbolName());
			assertTrue(new Position(649, 652+1-649).equals(node.getChild(0).getPosition()));
			assertTrue(node.getChild(0).getParent() == node);
			assertFalse(node.getChild(0).isToplevel());
		node = null;
		
		node = tlForm.get(5);
		form = (Form)node;
		assertTrue(new Position(658, 659+1-658).equals(form.getPosition()));
		assertEquals("quote", form.getFunctionSymbol().getSymbolName());
		assertEquals(2, form.getChildren().size());
		assertTrue(node.getParent() == model.getRoot());
		assertTrue(node.isToplevel());
		node = null;
			
		{
			Symbol sym2 = (Symbol)form.getChild(0);
			assertTrue(sym2 == form.getFunctionSymbol());
			assertTrue(sym2.getParent() == form);
		}
		
			child = form.getChild(1);
			assertFalse(child.hasChildren());
			assertTrue(new Position(659, 1).equals(child.getPosition()));
			assertEquals("x", ((Symbol)child).getSymbolName());
			assertTrue(child.getParent() == form);
			assertFalse(child.isToplevel());
			child = null;
		form = null;
		
		form = (Form)tlForm.get(6);
		assertTrue(new Position(664, 667+1-664).equals(form.getPosition()));
		assertEquals("quote", form.getFunctionSymbol().getSymbolName());
		assertEquals(2, form.getChildren().size());
		assertTrue(form.getParent() == model.getRoot());
		assertTrue(form.isToplevel());
		
		{
			Symbol sym2 = (Symbol)form.getChild(0);
			assertTrue(sym2 == form.getFunctionSymbol());
			assertTrue(sym2.getParent() == form);
		}
		
			child = form.getChild(1);
			assertFalse(child.hasChildren());
			assertTrue(new Position(665, 3).equals(child.getPosition()));
			assertEquals("sym", ((Symbol)child).getSymbolName());
			assertTrue(child.getParent() == form);
			assertFalse(child.isToplevel());
			child = null;
		form = null;
		
		form = (Form)tlForm.get(7);
		assertTrue(new Position(669, 676+1-669).equals(form.getPosition()));
		assertEquals("quote", form.getFunctionSymbol().getSymbolName());
		assertEquals(2, form.getChildren().size());
		assertTrue(form.getParent() == model.getRoot());
		assertTrue(form.isToplevel());
		
		{
			Symbol sym2 = (Symbol)form.getChild(0);
			assertTrue(sym2 == form.getFunctionSymbol());
			assertTrue(sym2.getParent() == form);
		}
		
			child = form.getChild(1);
			assertTrue(new Position(670, 676+1-670).equals(child.getPosition()));
			assertEquals(3, child.getChildren().size());
			assertTrue(child.getParent() == form);
			assertFalse(child.isToplevel());
			
				child2 = child.getChild(0);
				assertFalse(child2.hasChildren());
				assertTrue(new Position(671, 1).equals(child2.getPosition()));
				assertEquals("1", ((Symbol)child2).getSymbolName());
				assertTrue(child2.getParent() == child);
				assertFalse(child2.isToplevel());
				child2 = null;
				
				child2 = child.getChild(1);
				assertFalse(child2.hasChildren());
				assertTrue(new Position(673, 1).equals(child2.getPosition()));
				assertEquals("2", ((Symbol)child2).getSymbolName());
				assertTrue(child2.getParent() == child);
				assertFalse(child2.isToplevel());
				child2 = null;
				
				child2 = child.getChild(2);
				assertFalse(child2.hasChildren());
				assertTrue(new Position(675, 1).equals(child2.getPosition()));
				assertEquals("3", ((Symbol)child2).getSymbolName());
				assertTrue(child2.getParent() == child);
				assertFalse(child2.isToplevel());
				child2 = null;
			child = null;
		form = null;
		
		
		form = (Form)tlForm.get(8);
		assertTrue(new Position(678, 692+1-678).equals(form.getPosition()));
		assertEquals("quote", form.getFunctionSymbol().getSymbolName());
		assertEquals(2, form.getChildren().size());
		assertTrue(form.getParent() == model.getRoot());
		assertTrue(form.isToplevel());
		
		{
			Symbol sym2 = (Symbol)form.getChild(0);
			assertTrue(sym2 == form.getFunctionSymbol());
			assertTrue(sym2.getParent() == form);
		}
		
			SExpression quotedForm = form.getChild(1);
			assertTrue(new Position(679, 692+1-679).equals(quotedForm.getPosition()));
			assertEquals(2, quotedForm.getChildren().size());
			assertTrue(quotedForm.getParent() == form);
			assertFalse(quotedForm.isToplevel());
			
				child = quotedForm.getChild(0);
				assertTrue(new Position(680, 684+1-680).equals(child.getPosition()));
				assertEquals(2, child.getChildren().size());
				assertTrue(child.getParent() == quotedForm);
				assertFalse(child.isToplevel());
				
					child2 = child.getChild(0);
					assertTrue(new Position(681, 1).equals(child2.getPosition()));
					assertEquals("1", ((Symbol)child2).getSymbolName());
					assertTrue(child2.getParent() == child);
					assertFalse(child2.isToplevel());
					
					child2 = child.getChild(1);
					assertTrue(new Position(683, 1).equals(child2.getPosition()));
					assertEquals("2", ((Symbol)child2).getSymbolName());
					assertTrue(child2.getParent() == child);
					assertFalse(child2.isToplevel());
					child2 = null;
				child = null;
				
				child = quotedForm.getChild(1);
				assertTrue(new Position(686, 691+1-686).equals(child.getPosition()));
				assertEquals("quote", ((Form)child).getFunctionSymbol().getSymbolName());
				assertEquals(2, child.getChildren().size());
				assertTrue(child.getParent() == quotedForm);
				assertFalse(child.isToplevel());
				
				{
					Symbol sym2 = (Symbol)child.getChild(0);
					assertTrue(sym2 == ((Form)child).getFunctionSymbol());
					assertTrue(sym2.getParent() == child);
				}
				
					child2 = child.getChild(1);
					assertTrue(new Position(687, 691+1-687).equals(child2.getPosition()));
					assertEquals(2, child2.getChildren().size());
					assertTrue(child2.getParent() == child);
					assertFalse(child2.isToplevel());
					
						child3 = child2.getChild(0);
						assertTrue(new Position(688, 1).equals(child3.getPosition()));
						assertEquals("a", ((Symbol)child3).getSymbolName());
						assertTrue(child3.getParent() == child2);
						assertFalse(child2.isToplevel());
						
						child3 = child2.getChild(1);
						assertTrue(new Position(690, 1).equals(child3.getPosition()));
						assertEquals("b", ((Symbol)child3).getSymbolName());
						assertTrue(child3.getParent() == child2);
						assertFalse(child3.isToplevel());
						child3 = null;
				child = null;
		form = null;
	}
	
	@Test
	public void testFormtypFile()
	throws Exception
	{
		IDocument formDoc = project.getLispPartitionedTestDocument(project.getFormTestFile());
		model = new SexpModel(formDoc);
		model.createDOM();
		
		assertNotNull(model.getRoot());
		assertNull(model.getRoot().getRoot());
		assertEquals(7, model.getTopLevelForms().size());
		assertFalse(model.hasMalformation());
		
		{
			Form quote = (Form)model.getTopLevelForms().get(0);
			assertEquals(2, quote.getChildren().size());
			assertEquals("quote", quote.getFunctionSymbol().getSymbolName());
			assertTrue(new Position(0, 10).equals(quote.getPosition()));
			assertTrue(quote.getParent() == model.getRoot());
			assertTrue(quote.isToplevel());
			
				SExpression quoteChild = quote.getChild(1);
				assertEquals(4, quoteChild.getChildren().size());
				assertTrue(new Position(1, 9).equals(quoteChild.getPosition()));
				assertTrue(quoteChild.getParent() == quote);
				assertFalse(quoteChild.isToplevel());
					
					Symbol sym = (Symbol)quoteChild.getChild(0);
					assertEquals("1", sym.getSymbolName());
					assertTrue(new Position(2, 1).equals(sym.getPosition()));
					assertTrue(sym.getParent() == quoteChild);
					assertFalse(sym.isToplevel());
					
					sym = (Symbol)quoteChild.getChild(1);
					assertEquals("2", sym.getSymbolName());
					assertTrue(new Position(4, 1).equals(sym.getPosition()));
					assertTrue(sym.getParent() == quoteChild);
					assertFalse(sym.isToplevel());
					
					sym = (Symbol)quoteChild.getChild(2);
					assertEquals("3", sym.getSymbolName());
					assertTrue(new Position(6, 1).equals(sym.getPosition()));
					assertTrue(sym.getParent() == quoteChild);
					assertFalse(sym.isToplevel());
					
					sym = (Symbol)quoteChild.getChild(3);
					assertEquals("4", sym.getSymbolName());
					assertTrue(new Position(8, 1).equals(sym.getPosition()));
					assertTrue(sym.getParent() == quoteChild);
					assertFalse(sym.isToplevel());
		}
		{
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(1);
			assertEquals(4, defun.getChildren().size());
			assertEquals("defun", defun.getFunctionSymbol().getSymbolName());
			assertTrue(new Position(46, 1+71-46).equals(defun.getPosition()));
			assertTrue(defun.getFunctionSymbol() == defun.getChild(0));
			assertTrue(defun.getParent() == model.getRoot());
			assertTrue(defun.isToplevel());
			
				Symbol sym = (Symbol)defun.getChild(1);
				assertEquals("my-function", sym.getSymbolName());
				assertTrue(new Position(53, 63+1-53).equals(sym.getPosition()));
				assertTrue(sym.getParent() == defun);
				assertFalse(sym.isToplevel());
				
				SExpression defunChild = defun.getChild(2);
				assertFalse(defunChild.hasChildren());
				assertTrue(new Position(65, 2).equals(defunChild.getPosition()));
				assertTrue(defunChild.getParent() == defun);
				assertFalse(defunChild.isToplevel());
				
				sym = (Symbol)defun.getChild(3);
				assertEquals("nil", sym.getSymbolName());
				assertTrue(new Position(68, 3).equals(sym.getPosition()));
				assertTrue(sym.getParent() == defun);
				assertFalse(sym.isToplevel());
		}
		{
			Symbol sym = (Symbol)model.getTopLevelForms().get(2);
			assertFalse(sym.hasChildren());
			assertTrue(sym instanceof StringSymbol);
			assertEquals("\"string\"", sym.getSymbolName());
			assertTrue(new Position(80, 1+87-80).equals(sym.getPosition()));
			assertTrue(sym.getParent() == model.getRoot());
			assertTrue(sym.isToplevel());
		}
		{
			Form quote = (Form)model.getTopLevelForms().get(3);
			assertEquals(2, quote.getChildren().size());
			assertEquals("quote", quote.getFunctionSymbol().getSymbolName());
			assertTrue(new Position(99, 1+112-99).equals(quote.getPosition()));
			assertTrue(quote.getParent() == model.getRoot());
			assertTrue(quote.isToplevel());
			
				Symbol quotedSymbol = (Symbol)quote.getChild(1);
				assertFalse(quotedSymbol.hasChildren());
				assertEquals("quoted-sym", quotedSymbol.getSymbolName());
				assertTrue(new Position(103, 112+1-103).equals(quotedSymbol.getPosition()));
				assertTrue(quotedSymbol.getParent() == quote);
				assertFalse(quotedSymbol.isToplevel());
			
		}
		{
			Form quote = (Form)model.getTopLevelForms().get(4);
			assertEquals(2, quote.getChildren().size());
			assertEquals("quote", quote.getFunctionSymbol().getSymbolName());
			assertTrue(new Position(115, 1+136-115).equals(quote.getPosition()));
			assertTrue(quote.getParent() == model.getRoot());
			assertTrue(quote.isToplevel());
				
				SExpression quoteChild = quote.getChild(1);
				assertEquals(2, quoteChild.getChildren().size());
				assertTrue(new Position(119, 1+136-119).equals(quoteChild.getPosition()));
				assertTrue(quoteChild.getParent() == quote);
				assertFalse(quoteChild.isToplevel());
				
					Symbol sym = (Symbol)quoteChild.getChild(0);
					assertTrue(sym instanceof StringSymbol);
					assertEquals("\"quoted\"", sym.getSymbolName());
					assertTrue(new Position(120, 127+1-120).equals(sym.getPosition()));
					assertTrue(sym.getParent() == quoteChild);
					assertFalse(sym.isToplevel());
					
					sym = (Symbol)quoteChild.getChild(1);
					assertTrue(sym instanceof StringSymbol);
					assertEquals("\"list)\"", sym.getSymbolName());
					assertTrue(new Position(129, 135+1-129).equals(sym.getPosition()));
					assertTrue(sym.getParent() == quoteChild);
					assertFalse(sym.isToplevel());
		}
		{
			Form quote = (Form)model.getTopLevelForms().get(5);
			assertEquals(2, quote.getChildren().size());
			assertEquals("quote", quote.getFunctionSymbol().getSymbolName());
			assertTrue(new Position(139, 1+154-139).equals(quote.getPosition()));
			assertTrue(quote.getParent() == model.getRoot());
			assertTrue(quote.isToplevel());
				
				SExpression quoteChild = quote.getChild(1);
				assertEquals(1, quoteChild.getChildren().size());
				assertTrue(new Position(141, 1+154-141).equals(quoteChild.getPosition()));
				assertTrue(quoteChild.getParent() == quote);
				assertFalse(quoteChild.isToplevel());
					
					SExpression quoteChild2 = quoteChild.getChild(0);
					assertEquals(1, quoteChild2.getChildren().size());
					assertTrue(new Position(143, 1+152-143).equals(quoteChild2.getPosition()));
					assertTrue(quoteChild2.getParent() == quoteChild);
					assertFalse(quoteChild2.isToplevel());
						
						SExpression quoteChild3 = quoteChild2.getChild(0);
						assertEquals(1, quoteChild3.getChildren().size());
						assertTrue(new Position(145, 1+150-145).equals(quoteChild3.getPosition()));
						assertTrue(quoteChild3.getParent() == quoteChild2);
						assertFalse(quoteChild3.isToplevel());
						
							SExpression quoteChild4 = quoteChild3.getChild(0);
							assertFalse(quoteChild4.hasChildren());
							assertTrue(new Position(147, 2).equals(quoteChild4.getPosition()));
							assertTrue(quoteChild4.getParent() == quoteChild3);
							assertFalse(quoteChild4.isToplevel());
		}
		{
			SExpression sexp = model.getTopLevelForms().get(6);
			assertEquals(1, sexp.getChildren().size());
			assertTrue(new Position(324, 1+329-324).equals(sexp.getPosition()));
			assertTrue(sexp.getParent() == model.getRoot());
			assertTrue(sexp.isToplevel());
			
				Symbol child = (Symbol)sexp.getChild(0);
				assertEquals("form", child.getSymbolName());
				assertTrue(new Position(325, 328+1-325).equals(child.getPosition()));
				assertTrue(child.getParent() == sexp);
				assertFalse(child.isToplevel());
				
		}
		/*
		//TODO' alleine Fehler?? wird nicht erkannt
		{
			Form quote = (Form)model.getTopLevelForms().get(7);
			assertFalse(quote.hasChildren());
			assertEquals("quote", quote.getFunctionSymbol());
			assertTrue(new Position(334, 1).equals(quote.getPosition()));
		} */
	}
	
	@Test
	public void testValidBackquoteSimple()
	throws Exception
	{
		
		ISexpModel model = project.getSexpModelFor(
				"`a\n" +
				"`\"abcdefdf \"\n" +
				"`#\\c\n" +
				"`(1 2 3 4)\n"
				);
		IDocument d = model.getDocument();
		List<SExpression> tl = model.getTopLevelForms();
		assertEquals(4, tl.size());
		assertSNEquals("`", ((Form)tl.get(0)).getFunctionSymbol(), d);
		assertSNEquals("a", (Symbol)tl.get(0).getChild(1), d);
		
		assertSNEquals("`", ((Form)tl.get(1)).getFunctionSymbol(), d);
		assertSNEquals("\"abcdefdf \"", (Symbol)tl.get(1).getChild(1), d);
		
		assertSNEquals("`", ((Form)tl.get(2)).getFunctionSymbol(), d);
		assertSNEquals("c", (ReaderSymbol)tl.get(2).getChild(1), "#\\c", d);
		assertEquals('\\', ((ReaderSymbol)tl.get(2).getChild(1)).getDispatchCharacter());
		
		assertSNEquals("`", ((Form)tl.get(3)).getFunctionSymbol(), d);
		{
			Form form = (Form)tl.get(3).getChild(1);
			assertSNEquals("1", (Symbol)form.getChild(0), d);
			assertSNEquals("2", (Symbol)form.getChild(1), d);
			assertSNEquals("3", (Symbol)form.getChild(2), d);
			assertSNEquals("4", (Symbol)form.getChild(3), d);
		}
	}
	
	@Test
	public void testValidBackquoteWithComma()
	throws Exception
	{
		
		ISexpModel model = project.getSexpModelFor(
				"`,a\n" +
				"`,(1 2)\n" +
				"`,(1 ,2)\n" + //ungueltig, wird aber nicht erkannt
				"`(,1 ,2 3 4)\n" +
				"`(1 `,(a ,b) ,c)"); //nested
		IDocument d = model.getDocument();
		List<SExpression> tl = model.getTopLevelForms();
		assertEquals(5, tl.size());
		
		{
			Form bq = (Form)tl.get(0);
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			{
				Form comma = (Form)bq.getChild(1);
				assertSNEquals(",", comma.getFunctionSymbol(), d);
				assertTrue(comma.getFunctionSymbol().getParent() == comma);
				assertSNEquals("a", (Symbol)comma.getChild(1), d);
				assertTrue(comma.getChild(1).getParent() == comma);
			}
		}
		
		//`,(1 2)
		{
			Form form = (Form)tl.get(1);
			assertSNEquals("`", form.getFunctionSymbol(), d);
			assertTrue(form == form.getFunctionSymbol().getParent());
			{
				Form comma = (Form)form.getChild(1);
				assertSNEquals(",", comma.getFunctionSymbol(), d);
				assertTrue(comma.getFunctionSymbol().getParent() == comma);
				{
					Form form2 = (Form)comma.getChild(1);
					assertSNEquals("1", (Symbol)form2.getChild(0), d);
					assertTrue(form2 == form2.getChild(0).getParent());
					assertSNEquals("2", (Symbol)form2.getChild(1), d);
					assertTrue(form2 == form2.getChild(1).getParent());
				}
			}
		}
		
		//`,(1 ,2)
		{
			Form form = (Form)tl.get(2);
			assertSNEquals("`", form.getFunctionSymbol(), d);
			assertTrue(form == form.getFunctionSymbol().getParent());
			
			{
				Form comma = (Form)form.getChild(1);
				assertSNEquals(",", comma.getFunctionSymbol(), d);
				assertTrue(comma.getFunctionSymbol().getParent() == comma);
				{
					Form form2 = (Form)comma.getChild(1);
					assertSNEquals("1", (Symbol)form2.getChild(0), d);
					assertTrue(form2 == form2.getChild(0).getParent());
					
					{
						Form comma2 = (Form)form2.getChild(1);
						assertSNEquals(",", comma2.getFunctionSymbol(), d);
						assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
						
						assertSNEquals("2", (Symbol)comma2.getChild(1), d);
						assertTrue(comma2 == comma2.getChild(1).getParent());
					}
				}
			}
		}
		
		//`(,1 ,2 3 4)
		{
			Form form0 = (Form)tl.get(3);
			assertSNEquals("`", form0.getFunctionSymbol(), d);
			assertTrue(form0 == form0.getFunctionSymbol().getParent());
			{
				SExpression sexp = form0.getChild(1);
				{
					Form comma = (Form)sexp.getChild(0);
					assertSNEquals(",", comma.getFunctionSymbol(), d);
					assertTrue(comma.getFunctionSymbol().getParent() == comma);
					
					assertSNEquals("1", (Symbol)comma.getChild(1), d);
					assertTrue(comma.getChild(1).getParent() == comma);
				}				
				{
					Form comma = (Form)sexp.getChild(1);
					assertSNEquals(",", comma.getFunctionSymbol(), d);
					assertTrue(comma.getFunctionSymbol().getParent() == comma);
					
					assertSNEquals("2", (Symbol)comma.getChild(1), d);
					assertTrue(comma.getChild(1).getParent() == comma);
				}
				
				assertSNEquals("3", (Symbol)sexp.getChild(2), d);
				assertTrue(sexp == sexp.getChild(2).getParent());
				assertSNEquals("4", (Symbol)sexp.getChild(3), d);
				assertTrue(sexp == sexp.getChild(3).getParent());
			}
		}
		
		//`(1 `,(a ,b) ,c)
		{
			Form bq = (Form)tl.get(4);
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			{
				Form form = (Form)bq.getChild(1);
				assertSNEquals("1", (Symbol)form.getChild(0), d);
				assertTrue(form == form.getChild(0).getParent());
				
				{
					Form nestedBq = (Form)form.getChild(1);
					assertSNEquals("`", nestedBq.getFunctionSymbol(), d);
					assertTrue(nestedBq.getFunctionSymbol().getParent() == nestedBq);
					assertSNEquals("`", (Symbol)nestedBq.getChild(0), d);
					assertTrue(nestedBq.getChild(0).getParent() == nestedBq);
					
					{
						Form comma = (Form)nestedBq.getChild(1);
						assertSNEquals(",", comma.getFunctionSymbol(), d);
						assertTrue(comma.getFunctionSymbol().getParent() == comma);
						{
							Form subForm = (Form)comma.getChild(1);
							assertSNEquals("a", subForm.getFunctionSymbol(), d);
							assertTrue(subForm.getFunctionSymbol().getParent() == subForm);
							
							{
								Form comma2 = (Form)subForm.getChild(1);
								assertSNEquals(",", comma2.getFunctionSymbol(), d);
								assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
								
								assertSNEquals("b", (Symbol)comma2.getChild(1), d);
								assertTrue(comma2.getChild(1).getParent() == comma2);
							}
						}
					}
				}
				{
					Form comma = (Form)form.getChild(2);
					assertSNEquals(",", comma.getFunctionSymbol(), d);
					assertTrue(comma.getFunctionSymbol().getParent() == comma);
					
					assertSNEquals("c", (Symbol)comma.getChild(1), d);
					assertTrue(comma.getChild(1).getParent() == comma);
				}	
			}
		}
	}
	
	@Test
	public void testValidBackquotedBackquote()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"```\n" + //gehoert zum a in der naechsten Zeile
				"`    `   ` `a\n" +
				"`,\n`a\n" +
				"`\t`  ``(1 ,2)\n" + //ungueltig, bisher aber nocht nicht erkannt als Fehler
				"`,``,(,1 ,,2 `,3 4)\n" + //ungueltig
				"`,,`,,a\n" +
				",,a");  //ungueltig
		IDocument d = model.getDocument();
		List<SExpression> tl = model.getTopLevelForms();
		assertEquals(6, tl.size());
		
		{
			Form bq = (Form)tl.get(0);
			for(int i=0;i<6;i++) {
				assertSNEquals("`", bq.getFunctionSymbol(), d);
				assertTrue(bq.getFunctionSymbol().getParent() == bq);
				bq = (Form)bq.getChild(1);
			}
			
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			assertSNEquals("a", (Symbol)bq.getChild(1), d);
			assertTrue(bq.getChild(1).getParent() == bq);
		}
		
		//`,\n`a\n
		{
			Form bq = (Form)tl.get(1);
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			{
				Form comma = (Form)bq.getChild(1);
				assertSNEquals(",", comma.getFunctionSymbol(), d);
				assertTrue(comma.getFunctionSymbol().getParent() == comma);
				{
					Form bq2 = (Form)comma.getChild(1);
					assertSNEquals("`", bq2.getFunctionSymbol(), d);
					assertTrue(bq2.getFunctionSymbol().getParent() == bq2);
					
					assertSNEquals("a", (Symbol)bq2.getChild(1), d);
					assertTrue(bq2.getChild(1).getParent() == bq2);
				}
			}
		}
		
		//`\t`  ``(1 ,2)\n
		{
			Form bq = (Form)tl.get(2);
			for(int i=0;i<3;i++) {
				assertSNEquals("`", bq.getFunctionSymbol(), d);
				assertTrue(bq.getFunctionSymbol().getParent() == bq);
				bq = (Form)bq.getChild(1);
			}
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			{
				Form form = (Form)bq.getChild(1);
				assertSNEquals("1", form.getFunctionSymbol(), d);
				assertTrue(form.getFunctionSymbol().getParent() == form);
				{
					Form comma = (Form)form.getChild(1);
					assertSNEquals(",", comma.getFunctionSymbol(), d);
					assertTrue(comma.getFunctionSymbol().getParent() == comma);
					
					assertSNEquals("2", (Symbol)comma.getChild(1), d);
					assertTrue(comma.getChild(1).getParent() == comma);	
				}
			}
		}
		
		//`,``,(,1 ,,2 `,3 4)
		{
			Form bq = (Form)tl.get(3);
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			Form comma0 = (Form)bq.getChild(1);
			assertSNEquals(",", comma0.getFunctionSymbol(), d);
			assertTrue(comma0.getFunctionSymbol().getParent() == comma0);
			
			Form bq0 = (Form)comma0.getChild(1);
			assertSNEquals("`", bq0.getFunctionSymbol(), d);
			assertTrue(bq0.getFunctionSymbol().getParent() == bq0);
			
			Form bq_1 = (Form)bq0.getChild(1);
			assertSNEquals("`", bq_1.getFunctionSymbol(), d);
			assertTrue(bq_1.getFunctionSymbol().getParent() == bq_1);
			
			bq = bq_1;
			{
				Form comma = (Form)bq.getChild(1);
				assertSNEquals(",", comma.getFunctionSymbol(), d);
				assertTrue(comma.getFunctionSymbol().getParent() == comma);
				
				{
					SExpression sexp = comma.getChild(1);
					{
						Form comma2 = (Form)sexp.getChild(0);
						assertSNEquals(",", comma2.getFunctionSymbol(), d);
						assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
						
						assertSNEquals("1", (Symbol)comma2.getChild(1), d);
						assertTrue(comma2.getChild(1).getParent() == comma2);	
					}
					{
						Form comma2 = (Form)sexp.getChild(1);
						assertSNEquals(",", comma2.getFunctionSymbol(), d);
						assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
						{
							Form comma3 = (Form)comma2.getChild(1);
							assertSNEquals(",", comma3.getFunctionSymbol(), d);
							assertTrue(comma3.getFunctionSymbol().getParent() == comma3);
							
							assertSNEquals("2", (Symbol)comma3.getChild(1), d);
							assertTrue(comma3.getChild(1).getParent() == comma3);	
						}
					}
					{
						Form bq2 = (Form)sexp.getChild(2);
						assertSNEquals("`", bq2.getFunctionSymbol(), d);
						assertTrue(bq2.getFunctionSymbol().getParent() == bq2);
						{
							Form comma2 = (Form)bq2.getChild(1);
							assertSNEquals(",", comma2.getFunctionSymbol(), d);
							assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
							
							assertSNEquals("3", (Symbol)comma2.getChild(1), d);
							assertTrue(comma2.getChild(1).getParent() == comma2);	
						}
					}
					
					assertSNEquals("4", (Symbol)sexp.getChild(3), d);
					assertTrue(sexp.getChild(3).getParent() == sexp);	
				}
			}
		}
		//`,,`,,a
		{
			Form bq = (Form)tl.get(4);
			assertSNEquals("`", bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			Form comma0 = (Form)bq.getChild(1);
			assertSNEquals(",", comma0.getFunctionSymbol(), d);
			assertTrue(comma0.getFunctionSymbol().getParent() == comma0);
			
			Form comma1 = (Form)comma0.getChild(1);
			assertSNEquals(",", comma1.getFunctionSymbol(), d);
			assertTrue(comma1.getFunctionSymbol().getParent() == comma1);
			
			Form bq1 = (Form)comma1.getChild(1);
			assertSNEquals("`", bq1.getFunctionSymbol(), d);
			assertTrue(bq1.getFunctionSymbol().getParent() == bq1);
			
			Form comma2 = (Form)bq1.getChild(1);
			assertSNEquals(",", comma2.getFunctionSymbol(), d);
			assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
			
			Form comma3 = (Form)comma2.getChild(1);
			assertSNEquals(",", comma3.getFunctionSymbol(), d);
			assertTrue(comma3.getFunctionSymbol().getParent() == comma3);
			
			assertSNEquals("a", (Symbol)comma3.getChild(1), d);
			assertTrue(comma3.getChild(1).getParent() == comma3);
		}
		//,,a
		{
			Form comma2 = (Form)tl.get(5);
			assertSNEquals(",", comma2.getFunctionSymbol(), d);
			assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
			
			Form comma3 = (Form)comma2.getChild(1);
			assertSNEquals(",", comma3.getFunctionSymbol(), d);
			assertTrue(comma3.getFunctionSymbol().getParent() == comma3);
			
			assertSNEquals("a", (Symbol)comma3.getChild(1), d);
			assertTrue(comma3.getChild(1).getParent() == comma3);
		}
	}
	
	@Test
	public void testValidQuotedQuote()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"''\n" + //gehoert zum a in der naechsten Zeile
				"'aa\n" +
				"''''''''''''''''(a b)\n" +
				"''''''''''''''''symbol\n" + //ungueltig, bisher aber nocht nicht erkannt als Fehler
				"''''(a '''''''b)\n" + //ungueltig
				"'(a ''''''''b)\n");  //ungueltig
		IDocument d = model.getDocument();
		List<SExpression> tl = model.getTopLevelForms();
		assertEquals(5, tl.size());
		
		{
			Form quote = (Form)tl.get(0);
			for(int i=0;i<2;i++) {
				assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
				assertTrue(quote.getFunctionSymbol().getParent() == quote);
				quote = (Form)quote.getChild(1);
			}
			
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			assertSNEquals("aa", (Symbol)quote.getChild(1), d);
			assertTrue(quote.getChild(1).getParent() == quote);
		}
		
		{ //''''''''''''''''(a b)
			Form quote = (Form)tl.get(1);
			for(int i=0;i<15;i++) {
				assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
				assertTrue(quote.getFunctionSymbol().getParent() == quote);
				quote = (Form)quote.getChild(1);
			}
			
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			{
				Form form = (Form)quote.getChild(1);
				assertSNEquals("a", form.getFunctionSymbol(), d);
				assertTrue(form.getFunctionSymbol().getParent() == form);
				
				assertSNEquals("b", (Symbol)form.getChild(1), d);
				assertTrue(form.getChild(1).getParent() == form);
			}
		}
		
		{ //''''''''''''''''symbol
			Form quote = (Form)tl.get(2);
			for(int i=0;i<15;i++) {
				assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
				assertTrue(quote.getFunctionSymbol().getParent() == quote);
				quote = (Form)quote.getChild(1);
			}
			
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			assertSNEquals("symbol", (Symbol)quote.getChild(1), d);
			assertTrue(quote.getChild(1).getParent() == quote);
		}
		
		{ //''''(a '''''''b)
			Form quote = (Form)tl.get(3);
			for(int i=0;i<3;i++) {
				assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
				assertTrue(quote.getFunctionSymbol().getParent() == quote);
				quote = (Form)quote.getChild(1);
			}
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			{
				Form form = (Form)quote.getChild(1);
				assertSNEquals("a", form.getFunctionSymbol(), d);
				assertTrue(form.getFunctionSymbol().getParent() == form);
				
				Form quote2 = (Form)form.getChild(1);
				for(int i=0;i<6;i++) {
					assertSNEquals(SYM_QUOTE, quote2.getFunctionSymbol(), "'", d);
					assertTrue(quote2.getFunctionSymbol().getParent() == quote2);
					quote2 = (Form)quote2.getChild(1);
				}
				
				assertSNEquals(SYM_QUOTE, quote2.getFunctionSymbol(), "'", d);
				assertTrue(quote2.getFunctionSymbol().getParent() == quote2);
				
				assertSNEquals("b", (Symbol)quote2.getChild(1), d);
				assertTrue(quote2.getChild(1).getParent() == quote2);
			}
		}
		
		{//'(a ''''''''b)
			Form quote = (Form)tl.get(4);
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			{
				Form form = (Form)quote.getChild(1);
				assertSNEquals("a", form.getFunctionSymbol(), d);
				assertTrue(form.getFunctionSymbol().getParent() == form);
				
				Form quote2 = (Form)form.getChild(1);
				for(int i=0;i<7;i++) {
					assertSNEquals(SYM_QUOTE, quote2.getFunctionSymbol(), "'", d);
					assertTrue(quote2.getFunctionSymbol().getParent() == quote2);
					quote2 = (Form)quote2.getChild(1);
				}
				assertSNEquals(SYM_QUOTE, quote2.getFunctionSymbol(), "'", d);
				assertTrue(quote2.getFunctionSymbol().getParent() == quote2);
				
				assertSNEquals("b", (Symbol)quote2.getChild(1), d);
				assertTrue(quote2.getChild(1).getParent() == quote2);
			}
		}
	}
	
	@Test
	public void testValidMixedQuoteCommaBackquote()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"`,'a\n" +
				"```''``,`a\n" +
				"`,'(a ,b)\n" +
				",'`(a b)\n" +
				"`(,`,a ,'b)\n" +
				"`,,'sym\n");
		IDocument d = model.getDocument();
		List<SExpression> tl = model.getTopLevelForms();
		assertEquals(6, tl.size());
		
		{
			Form bq = (Form)tl.get(0);
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			Form comma = (Form)bq.getChild(1);
			assertSNEquals(SYM_COMMA, comma.getFunctionSymbol(), d);
			assertTrue(comma.getFunctionSymbol().getParent() == comma);
			
			Form quote = (Form)comma.getChild(1);
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			assertSNEquals("a", (Symbol)quote.getChild(1), d);
			assertTrue(quote.getChild(1).getParent() == quote);
		}
		{//```''``,`a
			Form bq = (Form)tl.get(1);
			for(int i=0;i<2;i++) {
				assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
				assertTrue(bq.getFunctionSymbol().getParent() == bq);
				bq = (Form)bq.getChild(1);
			}
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			{
				Form quote = (Form)bq.getChild(1);
				assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(),"'", d);
				assertTrue(quote.getFunctionSymbol().getParent() == quote);
				{
					Form quote2 = (Form)quote.getChild(1);
					assertSNEquals(SYM_QUOTE, quote2.getFunctionSymbol(),"'", d);
					assertTrue(quote2.getFunctionSymbol().getParent() == quote2);
					{
						Form bq2 = (Form)quote2.getChild(1);
						assertSNEquals(SYM_BACKQUOTE, bq2.getFunctionSymbol(), d);
						assertTrue(bq2.getFunctionSymbol().getParent() == bq2);
						{
							Form bq3 = (Form)bq2.getChild(1);
							assertSNEquals(SYM_BACKQUOTE, bq3.getFunctionSymbol(), d);
							assertTrue(bq3.getFunctionSymbol().getParent() == bq3);
							{
								Form quote3 = (Form)bq3.getChild(1);
								assertSNEquals(SYM_COMMA, quote3.getFunctionSymbol(), d);
								assertTrue(quote3.getFunctionSymbol().getParent() == quote3);
								{
									Form bq4 = (Form)quote3.getChild(1);
									assertSNEquals(SYM_BACKQUOTE, bq4.getFunctionSymbol(), d);
									assertTrue(bq4.getFunctionSymbol().getParent() == bq4);
									
									assertSNEquals("a", (Symbol)bq4.getChild(1), d);
									assertTrue(bq4.getChild(1).getParent() == bq4);
								}
							}
						}
					}
				}
			}
		}
		{//`,'(a ,b)
			Form bq = (Form)tl.get(2);
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			Form comma = (Form)bq.getChild(1);
			assertSNEquals(SYM_COMMA, comma.getFunctionSymbol(), d);
			assertTrue(comma.getFunctionSymbol().getParent() == comma);
			
			Form quote = (Form)comma.getChild(1);
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			{	
				Form form = (Form)quote.getChild(1);
				assertSNEquals("a", form.getFunctionSymbol(), d);
				assertTrue(form.getFunctionSymbol().getParent() == form);
				{
					Form comma2 = (Form)form.getChild(1);
					assertSNEquals(SYM_COMMA, comma2.getFunctionSymbol(), d);
					assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
					
					assertSNEquals("b", (Symbol)comma2.getChild(1), d);
					assertTrue(comma2.getChild(1).getParent() == comma2);
				}
			}
		}
		{//,'`(a b)
			Form comma = (Form)tl.get(3);
			assertSNEquals(SYM_COMMA, comma.getFunctionSymbol(), d);
			assertTrue(comma.getFunctionSymbol().getParent() == comma);
			
			Form quote = (Form)comma.getChild(1);
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			Form bq = (Form)quote.getChild(1);
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			{	
				Form form = (Form)bq.getChild(1);
				assertSNEquals("a", form.getFunctionSymbol(), d);
				assertTrue(form.getFunctionSymbol().getParent() == form);
				
				assertSNEquals("b", (Symbol)form.getChild(1), d);
				assertTrue(form.getChild(1).getParent() == form);
			}
		}
		{//`(,`,a ,'b)
			Form bq = (Form)tl.get(4);
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			{
				SExpression sexp = bq.getChild(1);
				
				Form comma = (Form)sexp.getChild(0);
				assertSNEquals(SYM_COMMA, comma.getFunctionSymbol(), d);
				assertTrue(comma.getFunctionSymbol().getParent() == comma);
				
				Form bq2 = (Form)comma.getChild(1);
				assertSNEquals(SYM_BACKQUOTE, bq2.getFunctionSymbol(), d);
				assertTrue(bq2.getFunctionSymbol().getParent() == bq2);
				
				Form comma2 = (Form)bq2.getChild(1);
				assertSNEquals(SYM_COMMA, comma2.getFunctionSymbol(), d);
				assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
				assertSNEquals("a", (Symbol)comma2.getChild(1), d);
				assertTrue(comma2.getChild(1).getParent() == comma2);
				
				Form comma3 = (Form)sexp.getChild(1);
				assertSNEquals(SYM_COMMA, comma3.getFunctionSymbol(), d);
				assertTrue(comma3.getFunctionSymbol().getParent() == comma3);
				
				Form quote = (Form)comma3.getChild(1);
				assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
				assertTrue(quote.getFunctionSymbol().getParent() == quote);
				assertSNEquals("b", (Symbol)quote.getChild(1), d);
				assertTrue(quote.getChild(1).getParent() == quote);
			}
		}
		{//`,,'sym
			Form bq = (Form)tl.get(5);
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), d);
			assertTrue(bq.getFunctionSymbol().getParent() == bq);
			
			Form comma = (Form)bq.getChild(1);
			assertSNEquals(SYM_COMMA, comma.getFunctionSymbol(), d);
			assertTrue(comma.getFunctionSymbol().getParent() == comma);
			
			Form comma2 = (Form)comma.getChild(1);
			assertSNEquals(SYM_COMMA, comma2.getFunctionSymbol(), d);
			assertTrue(comma2.getFunctionSymbol().getParent() == comma2);
			
			Form quote = (Form)comma2.getChild(1);
			assertSNEquals(SYM_QUOTE, quote.getFunctionSymbol(), "'", d);
			assertTrue(quote.getFunctionSymbol().getParent() == quote);
			
			assertSNEquals("sym", (Symbol)quote.getChild(1), d);
			assertTrue(quote.getChild(1).getParent() == quote);
		}
	}
	
	@Test 
	public void testSymbolTopLevelPlainSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"einfaches-symbol\n" +
				"#:uninterned-sym\n" +
				"de.fh-trier::private-sym \n" +
				"de.fh-trier:public-sym\n" +
				"package:*global-special*");
		assertEquals(5, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			Symbol sym = ((Symbol)tl.get(0));
			assertSNEquals("einfaches-symbol", sym, doc);
			assertSNAttributesEquals(sym, null, true, false);
		}
		{
			Symbol sym = ((Symbol)tl.get(1));
			assertSNEquals("uninterned-sym", sym, "#:uninterned-sym", doc);
			assertSNAttributesEquals(sym, null, false, false);
		}
		{
			Symbol sym = ((Symbol)tl.get(2));
			assertSNEquals("private-sym", sym, "de.fh-trier::private-sym", doc);
			assertSNAttributesEquals(sym, "de.fh-trier", true, true);
		}
		{
			Symbol sym = ((Symbol)tl.get(3));
			assertSNEquals("public-sym", sym, "de.fh-trier:public-sym", doc);
			assertSNAttributesEquals(sym, "de.fh-trier", true, false);
		}
		{
			Symbol sym = ((Symbol)tl.get(4));
			assertSNEquals("*global-special*", sym, "package:*global-special*", doc);
			assertSNAttributesEquals(sym, "package", true, false);
		}
	}
	
	@Test 
	public void testSymbolTopLevelKeywordSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				":keyword\n" +
				":key::mit-doppelpunkt\n" +
				":*kjdkfm/§\n");
		assertEquals(3, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
	
		{
			Symbol sym = ((Symbol)tl.get(0));
			assertSNEquals(":keyword", sym, doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertTrue(sym instanceof KeywordSymbol);
		}
		{
			Symbol sym = ((Symbol)tl.get(1));
			assertSNEquals(":key::mit-doppelpunkt", sym, doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertTrue(sym instanceof KeywordSymbol);
		}
		{
			Symbol sym = ((Symbol)tl.get(2));
			assertSNEquals(":*kjdkfm/§", sym, doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertTrue(sym instanceof KeywordSymbol);
		}
	}
	
	@Test
	public void testSymbolToplevelString()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"\"string symbol \"\n" + //normaler string
				"#:\"my-string\"\n" +
				"de.fh-trier::\"the-string\" \n" +
				"de.fh-trier:\" stri))ng((\"");
		assertEquals(4, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			Symbol sym = ((Symbol)tl.get(0));
			assertSNEquals("\"string symbol \"", sym, doc);
			assertSNAttributesEquals(sym, null, true, false);
		}
		{
			Symbol sym = ((Symbol)tl.get(1));
			assertSNEquals("\"my-string\"", sym, "#:\"my-string\"", doc);
			assertSNAttributesEquals(sym, null, false, false);
		}
		{
			Symbol sym = ((Symbol)tl.get(2));
			assertSNEquals("\"the-string\"", sym, "de.fh-trier::\"the-string\"", doc);
			assertSNAttributesEquals(sym, "de.fh-trier", true, true);
		}
		{
			Symbol sym = ((Symbol)tl.get(3));
			assertSNEquals("\" stri))ng((\"", sym, "de.fh-trier:\" stri))ng((\"", doc);
			assertSNAttributesEquals(sym, "de.fh-trier", true, false);
		}
	}
	
	@Test
	public void testSymbolOnTopLevelSimpleReaderSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"#\\c\n" + //char symbol
				"#'function\n" +
				"#*1010011010\n" + //bit vector
				"#5*1010011010\n" +
				"#B1101\n" + //rational
				"#b101/11\n" +
				"#o37/15\n" + //octal
				"#O105\n" +
				"#P\"/pathname\"\n" +
				"#2r11010101\n" +
				"#3r-21010\n" +
				"#xF00\n");
		assertEquals(12, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(0));
			assertSNEquals("c", sym, "#\\c", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('\\', sym.getDispatchCharacter());
			assertTrue(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(1));
			assertSNEquals("function", sym, "#'function", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('\'', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(2));
			assertSNEquals("1010011010", sym, "#*1010011010", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('*', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(3));
			assertSNEquals("1010011010", sym, "#5*1010011010", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('*', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(4));
			assertSNEquals("1101", sym, "#B1101", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('b', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(5));
			assertSNEquals("101/11", sym, "#b101/11", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('b', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(6));
			assertSNEquals("37/15", sym, "#o37/15", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('o', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(7));
			assertSNEquals("105", sym, "#O105", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('o', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(8));
			assertSNEquals("\"/pathname\"", sym, "#P\"/pathname\"", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('p', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(9));
			assertSNEquals("11010101", sym, "#2r11010101", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('r', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(10));
			assertSNEquals("-21010", sym, "#3r-21010", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('r', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(11));
			assertSNEquals("F00", sym, "#xF00", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('x', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
	}
	
	@Test
	public void testSymbolOnTopLevelReaderConditionSymbols()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"#+clisp symbol\n" +
				"#+clisp (form)\n" +
				"#+clisp \"string\"" +
				"#+(and clisp lispworks mcl) symbol\n" +
				"#+(and clisp lispworks mcl) (progn (form1) (form2))\n" +
				"#-clisp symbol\n" +
				"#-clisp (form)\n" +
				"#-(or clisp lispworks mcl) symbol\n" +
				"#-(or clisp lispworks mcl) (progn (form1) (form2))\n" +
				"#+(and clisp) \"string\"\n");
		assertEquals(10, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(0));
			assertSNEquals("clisp", sym, "#+clisp symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(1));
			assertSNEquals("clisp", sym, "#+clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject()).getFunctionSymbol(), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(2));
			assertSNEquals("clisp", sym, "#+clisp \"string\"", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("\"string\"", (StringSymbol)sym.getObject(), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(3));
			assertSNEquals("", sym, "#+(and clisp lispworks mcl) symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("symbol", (Symbol)sym.getObject(1), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(4));
			assertSNEquals("", sym, "#+(and clisp lispworks mcl) (progn (form1) (form2))", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("progn", ((Form)sym.getObject(1)).getFunctionSymbol(), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(5));
			assertSNEquals("clisp", sym, "#-clisp symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(6));
			assertSNEquals("clisp", sym, "#-clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject()).getFunctionSymbol(), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(7));
			assertSNEquals("", sym, "#-(or clisp lispworks mcl) symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("or", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("symbol", (Symbol)sym.getObject(1), doc);
		}
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(8));
			assertSNEquals("", sym, "#-(or clisp lispworks mcl) (progn (form1) (form2))", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("or", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("progn", ((Form)sym.getObject(1)).getFunctionSymbol(), doc);
		} 
		{
			ReaderSymbol sym = ((ReaderSymbol)tl.get(9));
			assertSNEquals("", sym, "#+(and clisp) \"string\"", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("\"string\"", (StringSymbol)sym.getObject(1), doc);
		}
	}
	
	@Test
	public void testSymbolSubLevelReaderConditionSymbols()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(defun test () #+clisp symbol)\n" +
				"(defun test () #+clisp (form))\n" +
				"(defun test () #+clisp \"string\")\n" +
				"(defun test () #+(and clisp lispworks mcl) symbol)\n" +
				"(defun test () #+(and clisp lispworks mcl) (progn (form1) (form2)))\n" +
				"(defun test () #-clisp symbol)\n" +
				"(defun test () #-clisp (form))\n" +
				"(defun test () #-(or clisp lispworks mcl) symbol)\n" +
				"(defun test () #-(or clisp lispworks mcl) (progn (form1) (form2)))\n" +
				"(defun test () #+(and clisp) \"string\")\n");
		assertEquals(10, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			DefunForm defun = ((DefunForm)tl.get(0));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("clisp", sym, "#+clisp symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(1));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("clisp", sym, "#+clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject()).getFunctionSymbol(), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(2));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("clisp", sym, "#+clisp \"string\"", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("\"string\"", (StringSymbol)sym.getObject(), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(3));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("", sym, "#+(and clisp lispworks mcl) symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("symbol", (Symbol)sym.getObject(1), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(4));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("", sym, "#+(and clisp lispworks mcl) (progn (form1) (form2))", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("progn", ((Form)sym.getObject(1)).getFunctionSymbol(), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(5));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("clisp", sym, "#-clisp symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(6));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("clisp", sym, "#-clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject()).getFunctionSymbol(), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(7));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("", sym, "#-(or clisp lispworks mcl) symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("or", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("symbol", (Symbol)sym.getObject(1), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(8));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("", sym, "#-(or clisp lispworks mcl) (progn (form1) (form2))", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('-', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("or", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("progn", ((Form)sym.getObject(1)).getFunctionSymbol(), doc);
		} 
		{
			DefunForm defun = ((DefunForm)tl.get(9));
			ReaderSymbol sym = (ReaderSymbol)defun.getBody().get(0);
			assertSNEquals("", sym, "#+(and clisp) \"string\"", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("\"string\"", (StringSymbol)sym.getObject(1), doc);
		}
	}
	
	@Test
	public void testSymbolQuotedReaderConditionSymbols()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(defun test () '#+clisp 'symbol)\n" +
				"(defun test () `#+clisp (form))\n" +
				"`(defun test () ,#+clisp \"string\")\n" +
				"'(defun test () ,```''#+(and clisp lispworks mcl) 'symbol)\n" +
				"'#+clisp (form)\n" +
				"'#+clisp symbol\n" +
				"'#+(and clisp lispworks) (form)\n" +
				"'#+(and clisp lispworks) sym\n" +
				"`',#+clisp symbol\n" +
				"`',#+clisp (form)\n");
		assertEquals(10, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			DefunForm defun = ((DefunForm)tl.get(0));
			
			Form form = (Form)defun.getBody().get(0);
			assertSNEquals("quote", form.getFunctionSymbol(), "'", doc);
			assertEquals("'#+clisp 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp 'symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("quote", ((Form)sym.getObject()).getFunctionSymbol(), "'", doc);
			assertSNEquals("symbol", (Symbol)sym.getObject().getChild(1), doc);
		}
		{
			DefunForm defun = ((DefunForm)tl.get(1));
			
			Form form = (Form)defun.getBody().get(0);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`#+clisp (form)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject()).getFunctionSymbol(), doc);
		}
		{
			Form bq = (Form)tl.get(2);
			assertSNEquals(SYM_BACKQUOTE, bq.getFunctionSymbol(), doc);
			assertEquals("`(defun test () ,#+clisp \"string\")", doc.get(bq.getPosition().getOffset(), bq.getPosition().getLength()));
			
			Form defun = ((Form)bq.getChild(1));
			Form form = (Form)defun.getChild(3);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",#+clisp \"string\"", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp \"string\"", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("\"string\"", (StringSymbol)sym.getObject(), doc);
		}
		{//"'(defun test () ,```''#+(and clisp lispworks mcl) 'symbol)\n"
			Form quote = (Form)tl.get(3);
			assertSNEquals("quote", quote.getFunctionSymbol(),"'", doc);
			assertEquals("'(defun test () ,```''#+(and clisp lispworks mcl) 'symbol)", doc.get(quote.getPosition().getOffset(), quote.getPosition().getLength()));
			
			Form defun = (Form)quote.getChild(1);
			Form form = (Form)defun.getChild(3);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",```''#+(and clisp lispworks mcl) 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("```''#+(and clisp lispworks mcl) 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("``''#+(and clisp lispworks mcl) 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`''#+(and clisp lispworks mcl) 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_QUOTE, form.getFunctionSymbol(), "'", doc);
			assertEquals("''#+(and clisp lispworks mcl) 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_QUOTE, form.getFunctionSymbol(), "'", doc);
			assertEquals("'#+(and clisp lispworks mcl) 'symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("", sym, "#+(and clisp lispworks mcl) 'symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			assertSNEquals("quote", ((Form)sym.getObject(1)).getFunctionSymbol(), "'", doc);
		}
		{//'#+clisp (form)
			Form quote = (Form)tl.get(4);
			assertSNEquals("quote", quote.getFunctionSymbol(),"'", doc);
			assertEquals("'#+clisp (form)", doc.get(quote.getPosition().getOffset(), quote.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)quote.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
			assertNull(sym.getObject(1));
		}
		{//'#+clisp symbol
			Form quote = (Form)tl.get(5);
			assertSNEquals("quote", quote.getFunctionSymbol(),"'", doc);
			assertEquals("'#+clisp symbol", doc.get(quote.getPosition().getOffset(), quote.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)quote.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(0), doc);
			assertNull(sym.getObject(1));
		}
		{//'#+(and clisp lispworks) (form)
			Form quote = (Form)tl.get(6);
			assertSNEquals("quote", quote.getFunctionSymbol(),"'", doc);
			assertEquals("'#+(and clisp lispworks) (form)", doc.get(quote.getPosition().getOffset(), quote.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)quote.getChild(1);
			assertSNEquals("", sym, "#+(and clisp lispworks) (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
			assertSNEquals("form",((Form)sym.getObject(1)).getFunctionSymbol(), doc);
		}
		{//'#+(and clisp lispworks) sym
			Form quote = (Form)tl.get(7);
			assertSNEquals("quote", quote.getFunctionSymbol(),"'", doc);
			assertEquals("'#+(and clisp lispworks) sym", doc.get(quote.getPosition().getOffset(), quote.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)quote.getChild(1);
			assertSNEquals("", sym, "#+(and clisp lispworks) sym", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
			assertSNEquals("sym",(Symbol)sym.getObject(1), doc);
		}
		{//`',#+clisp symbol
			Form form = (Form)tl.get(8);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`',#+clisp symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals("quote", form.getFunctionSymbol(), "'", doc);
			assertEquals("',#+clisp symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",#+clisp symbol", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(0), doc);
		}
		{//`',#+clisp (form)
			Form form = (Form)tl.get(9);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`',#+clisp (form)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals("quote", form.getFunctionSymbol(), "'", doc);
			assertEquals("',#+clisp (form)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",#+clisp (form)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("clisp", sym, "#+clisp (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
	}
	
	@Test
	public void testNestedReaderConditionSymbols()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"#+clisp #-clisp sym\n" +
				"#+clisp #+(or clisp lispworks) (form)\n" +
				"#+(and clisp lispworks) #+clisp sym\n" +
				"#+(and clips lispworks) #+(or clisp lispworks) (form)\n");
		assertEquals(4, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(0);
			assertSNEquals("clisp", sym, "#+clisp #-clisp sym", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			
			ReaderSymbol nestedSym = (ReaderSymbol)sym.getObject();
			assertSNEquals("clisp", nestedSym, "#-clisp sym", doc);
			assertSNAttributesEquals(nestedSym, null, true, false);
			assertEquals('-', nestedSym.getDispatchCharacter());
			assertFalse(nestedSym.isCharSymbol());
		}
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(1);
			assertSNEquals("clisp", sym, "#+clisp #+(or clisp lispworks) (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			
			ReaderSymbol nestedSym = (ReaderSymbol)sym.getObject();
			assertSNEquals("", nestedSym, "#+(or clisp lispworks) (form)", doc);
			assertSNAttributesEquals(nestedSym, null, true, false);
			assertEquals('+', nestedSym.getDispatchCharacter());
			assertFalse(nestedSym.isCharSymbol());
			assertSNEquals("or", ((Form)nestedSym.getObject()).getFunctionSymbol(), doc);
			
			Form form = (Form)nestedSym.getObject(1);
			assertSNEquals("form", form.getFunctionSymbol(), doc);
		}
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(2);
			assertSNEquals("", sym, "#+(and clisp lispworks) #+clisp sym", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			
			ReaderSymbol nestedSym = (ReaderSymbol)sym.getObject(1);
			assertSNEquals("clisp", nestedSym, "#+clisp sym", doc);
			assertSNAttributesEquals(nestedSym, null, true, false);
			assertEquals('+', nestedSym.getDispatchCharacter());
			assertFalse(nestedSym.isCharSymbol());
			
			assertSNEquals("sym", (Symbol)nestedSym.getObject(), doc);
		}
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(3);
			assertSNEquals("", sym, "#+(and clips lispworks) #+(or clisp lispworks) (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('+', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("and", ((Form)sym.getObject()).getFunctionSymbol(), doc);
			
			ReaderSymbol nestedSym = (ReaderSymbol)sym.getObject(1);
			assertSNEquals("", nestedSym, "#+(or clisp lispworks) (form)", doc);
			assertSNAttributesEquals(nestedSym, null, true, false);
			assertEquals('+', nestedSym.getDispatchCharacter());
			assertFalse(nestedSym.isCharSymbol());
			assertSNEquals("or", ((Form)nestedSym.getObject()).getFunctionSymbol(), doc);
			
			assertSNEquals("form", ((Form)nestedSym.getObject(1)).getFunctionSymbol(), doc);
		}
	}
	
	@Test
	public void testFormExpectingReaderSymbols()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"#.(form)\n" +
				"#.symbol\n" +
				"#. symbol\n" +
				"#. (form)\n" +
				"`,#. (form)\n" +
				"#(1 2 3 4)\n" +
				"#             (1 2 3 4)\n" +
				"#6(1 2 3 4 5 6)\n" +
				"`,#(1 2 3 4)" +
				"#2A((0 1 5) (foo 2 (hot dog)))\n" +
				"`,#2A((0 1 5) (foo 2 (hot dog)))");
		
		assertEquals(11, model.getTopLevelForms().size());
		List<SExpression> tl = model.getTopLevelForms();
		IDocument doc = model.getDocument();
		
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(0);
			assertSNEquals("", sym, "#.(form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('.', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(1);
			assertSNEquals("symbol", sym, "#.symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('.', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertNull(sym.getObject());
		}
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(2);
			assertSNEquals("", sym, "#. symbol", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('.', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("symbol", (Symbol)sym.getObject(0), doc);
		}
		{
			ReaderSymbol sym = (ReaderSymbol)tl.get(3);
			assertSNEquals("", sym, "#. (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('.', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{//`,#. (form)
			Form form = (Form)tl.get(4);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`,#. (form)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",#. (form)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("", sym, "#. (form)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('.', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("form", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{//#(1 2 3 4)
			ReaderSymbol sym = (ReaderSymbol)tl.get(5);
			assertSNEquals(ReaderSymbol.VECTOR, sym, "#(1 2 3 4)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('(', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("1", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{//#             (1 2 3 4)
			ReaderSymbol sym = (ReaderSymbol)tl.get(6);
			assertSNEquals(ReaderSymbol.VECTOR, sym, "#             (1 2 3 4)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('(', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("1", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{//#6(1 2 3 4 5 6)
			ReaderSymbol sym = (ReaderSymbol)tl.get(7);
			assertSNEquals(ReaderSymbol.VECTOR, sym, "#6(1 2 3 4 5 6)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('(', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("1", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{//`,#(1 2 3 4)
			Form form = (Form)tl.get(8);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`,#(1 2 3 4)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",#(1 2 3 4)", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals(ReaderSymbol.VECTOR, sym, "#(1 2 3 4)", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('(', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertSNEquals("1", ((Form)sym.getObject(0)).getFunctionSymbol(), doc);
		}
		{//#2A((0 1 5) (foo 2 (hot dog)))
			ReaderSymbol sym = (ReaderSymbol)tl.get(9);
			assertSNEquals("", sym, "#2A((0 1 5) (foo 2 (hot dog)))", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('a', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertTrue(sym.getObject().getTyp() == TSExpression.SEXPRESSION);
			assertSNEquals("0", ((Form)sym.getObject(0).getChild(0)).getFunctionSymbol(), doc);
		}
		{//`,#2A((0 1 5) (foo 2 (hot dog)))
			Form form = (Form)tl.get(10);
			assertSNEquals(SYM_BACKQUOTE, form.getFunctionSymbol(), doc);
			assertEquals("`,#2A((0 1 5) (foo 2 (hot dog)))", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			form = (Form)form.getChild(1);
			assertSNEquals(SYM_COMMA, form.getFunctionSymbol(), doc);
			assertEquals(",#2A((0 1 5) (foo 2 (hot dog)))", doc.get(form.getPosition().getOffset(), form.getPosition().getLength()));
			
			ReaderSymbol sym = (ReaderSymbol)form.getChild(1);
			assertSNEquals("", sym, "#2A((0 1 5) (foo 2 (hot dog)))", doc);
			assertSNAttributesEquals(sym, null, true, false);
			assertEquals('a', sym.getDispatchCharacter());
			assertFalse(sym.isCharSymbol());
			assertTrue(sym.getObject().getTyp() == TSExpression.SEXPRESSION);
			assertSNEquals("0", ((Form)sym.getObject(0).getChild(0)).getFunctionSymbol(), doc);
		}
	}
}
