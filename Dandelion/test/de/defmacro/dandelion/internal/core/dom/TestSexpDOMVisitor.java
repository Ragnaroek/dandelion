package de.defmacro.dandelion.internal.core.dom;

import org.eclipse.jface.text.*;
import org.junit.*;
import de.defmacro.dandelion.testutils.*;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

public class TestSexpDOMVisitor 
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
		model.createDOM();
		
	}
	
	@After
	public void tearDown()
	throws Exception
	{
		project.dispose();
	}
	
	@Test
	public void testVisitValidTestFile()
	{
		DOMTestVisitorSexpCollector visitor = new DOMTestVisitorSexpCollector();
		model.accept(visitor);
		
		assertEquals(0, visitor.defmacros.size());
		assertEquals(0, visitor.defpackages.size());
		assertEquals(1, visitor.defuns.size());
		assertEquals(15, visitor.forms.size());
		assertEquals(0, visitor.inpackages.size());
		assertEquals(0, visitor.lambdas.size());
		assertEquals(2, visitor.sexps.size());
		assertEquals(12, visitor.symbols.size());
		assertFalse(model.hasMalformation());
		assertTrue(model == visitor.suppliedModel);
	}
	
	@Test
	public void testVisitAllTypes()
	throws Exception
	{
		ISexpModel m = project.getSexpModelFor("" +
				"(in-package :xy)\n" +
				"(lambda () nil)\n" +
				"(defmacro my-macro (x y))\n" +
				"(defun my-function (a b &optional (x 'y)))\n" +
				"symbol\n" +
				"#+clisp\n" +
				"#\\(\n" +
				"(mapcar #'func nil)\n" +
				"(\"string\" x y)\n");
		
		DOMTestVisitorSexpCollector visitor = new DOMTestVisitorSexpCollector();
		m.accept(visitor);
		
		assertFalse(m.hasMalformation());
		assertEquals(1, visitor.defmacros.size());
		assertEquals(0, visitor.defpackages.size());
		assertEquals(1, visitor.defuns.size());
		assertEquals(1, visitor.forms.size());
		assertEquals(1, visitor.inpackages.size());
		assertEquals(1, visitor.lambdas.size());
		assertEquals(1, visitor.sexps.size());
		assertEquals(10, visitor.symbols.size());
		assertTrue(m == visitor.suppliedModel);
	}
	
	@Test
	public void testVisitAllTypesSublevel()
	throws Exception
	{
		ISexpModel m = project.getSexpModelFor("" +
				"(   (in-package :xy)  )\n" +
				"(   (lambda () nil)   )\n" +
				"(   (defmacro my-macro (x y))   )\n" +
				"(   (defun my-function (a b &optional (x 'y)))   )\n" +
				"( symbol )\n" + //form
				"( #+clisp )\n" +
				"( #\\(  )\n" +
				"(  (mapcar #'func nil)  )\n" +
				"(  (\"string\" x y)    )\n");
		
		DOMTestVisitorSexpCollector visitor = new DOMTestVisitorSexpCollector();
		m.accept(visitor);
		
		assertEquals(2, m.getMalformations().size()); //2 warnings, in-package + defun nicht auf toplevel
		assertEquals(1, visitor.defmacros.size());
		assertEquals(0, visitor.defpackages.size());
		assertEquals(1, visitor.defuns.size());
		assertEquals(2, visitor.forms.size());
		assertEquals(1, visitor.inpackages.size());
		assertEquals(1, visitor.lambdas.size());
		assertEquals(9, visitor.sexps.size());
		assertEquals(9, visitor.symbols.size());
		assertTrue(m == visitor.suppliedModel);
	}
}
