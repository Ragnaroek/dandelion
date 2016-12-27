package de.defmacro.dandelion.internal.core.dom.parse;

import static org.junit.Assert.*;
import static de.defmacro.dandelion.testutils.AssertUtil.*;

import org.junit.*;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.testutils.*;

public class TestInpackageParser 
{
	private static final int INPACKAGE_CHILD_COUNT = 2; //inpackage selbst + package name
	
	private static TestProject project;
	
	@BeforeClass
	public static void setUpClass()
	throws Exception
	{
		project = new TestProject();
	}
	
	@AfterClass
	public static void tearDownClass()
	throws Exception
	{
		project.dispose();
		project = null;
	}
	
	//gueltige inpackage-deklarationen
	
	@Test
	public void testKeywordAsStringDesignator()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(in-package :my-package)");
		assertSexpModelHasNoMalformations(model, INPACKAGE_CHILD_COUNT);
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("in-package", in.getFunctionSymbol());
		assertSymbolNameEquals(":my-package", in.getPackage());
	}
	
	@Test
	public void testStringAsStringDesignator()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(   in-package  \"PACKAGE\" )");
		assertSexpModelHasNoMalformations(model, INPACKAGE_CHILD_COUNT);
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("in-package", in.getFunctionSymbol());
		assertSymbolNameEquals("\"PACKAGE\"", in.getPackage());
	}
	
	@Test
	public void testEmptyStringAsStringDesignator()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(in-package  \"\" )");
		assertSexpModelHasNoMalformations(model, INPACKAGE_CHILD_COUNT);
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("in-package", in.getFunctionSymbol());
		assertSymbolNameEquals("\"\"", in.getPackage());
	}
	
	@Test
	public void testCharAsStringDesignator()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(in-package #\\c   )");
		assertSexpModelHasNoMalformations(model, INPACKAGE_CHILD_COUNT);
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("in-package", in.getFunctionSymbol());
		assertSymbolNameEquals("c", in.getPackage());
	}
	
	@Test
	public void testNotInternedSymbolAsStringDesignator()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(in-package #:not-interned)");
		assertSexpModelHasNoMalformations(model, INPACKAGE_CHILD_COUNT);
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("in-package", in.getFunctionSymbol());
		assertSymbolNameEquals("not-interned", in.getPackage());
	}
	
	
	@Test
	public void testSymbolAsStringDesignator()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(in-package       a-symbol  )");
		assertSexpModelHasNoMalformations(model, INPACKAGE_CHILD_COUNT);
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("in-package", in.getFunctionSymbol());
		assertSymbolNameEquals("a-symbol", in.getPackage());
	}
	
	@Test
	public void testBackquotedIllegalInpackages()
	throws Exception
	{
		//Fehlerhaft aber backquoted
		{
			ISexpModel model = project.getSexpModelFor("`(in-package)");
			assertSexpModelHasNoMalformations(model, 2);
		}
		{
			ISexpModel model = project.getSexpModelFor("`(in-package \"lower-case-string\")");
			assertSexpModelHasNoMalformations(model, 3);
		}
		{
			ISexpModel model = project.getSexpModelFor("`(defun bla-blubb nil (progn (in-package :key :illegal)))");
			assertSexpModelHasNoMalformations(model, 8);
		}
	}
	
	@Test
	public void testBackquotedIllegalInpackagesWithReaderMacro()
	throws Exception
	{
		//Fehlerhaft aber backquoted
		{
			ISexpModel model = project.getSexpModelFor("#+clisp `(in-package)");
			assertSexpModelHasNoMalformations(model, 2);
		}
		{
			ISexpModel model = project.getSexpModelFor("#-clisp `(in-package \"lower-case-string\")");
			assertSexpModelHasNoMalformations(model, 3);
		}
		{
			ISexpModel model = project.getSexpModelFor("`#+(or clisp lispworks) (defun bla-blubb nil (progn (in-package :key :illegal)))");
			assertSexpModelHasNoMalformations(model, 12);
		}
	}
	
	//ungueltige inpackage-deklarationen

	@Test
	public void testIllegalSymbolAsStringDesignator()
	throws Exception
	{
		{
		ISexpModel model = project.getSexpModelFor("(in-package #-clisp)");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertEquals(1, in.getMalformations().size());
		assertTrue(in.hasMalformation());
		assertMalformationsAreEqual(model);
		}
		{
		ISexpModel model = project.getSexpModelFor("(in-package #<object>)");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertEquals(1, in.getMalformations().size());
		assertTrue(in.hasMalformation());
		assertMalformationsAreEqual(model);
		}
	}
	
	@Test
	public void testNoPackageSymbol()
	throws Exception
	{
		{
		ISexpModel model = project.getSexpModelFor("(in-package          )");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertEquals(1, in.getMalformations().size());
		assertTrue(in.hasMalformation());
		assertMalformationsAreEqual(model);
		}
		{
		ISexpModel model = project.getSexpModelFor("(in-package)");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertEquals(1, in.getMalformations().size());
		assertTrue(in.hasMalformation());
		assertMalformationsAreEqual(model);
		}
	}
	
	@Test
	public void testTooMuchPackageSymbols()
	throws Exception
	{
		{
		ISexpModel model = project.getSexpModelFor("(in-package \"package\" im-too-much)");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertEquals(1, in.getMalformations().size());
		assertTrue(in.hasMalformation());
		assertMalformationsAreEqual(model);
		}
		{
		ISexpModel model = project.getSexpModelFor("(in-package  symbol and much more symbols)");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertEquals(1, in.getMalformations().size());
		assertTrue(in.hasMalformation());
		assertMalformationsAreEqual(model);
		}
	}
	
	@Test
	public void testSemanticNotAllUpperCase()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(in-package \"lower-in-STRING\")");
		InpackageForm in = (InpackageForm)model.getTopLevelForms().get(0);
		assertTrue(in.hasMalformation(true));
		assertMalformationsAreEqual(model);
		assertEquals(1, in.getChild(1).getMalformations().size());
		assertEquals(TSeverity.WARNING, in.getChild(1).getMalformations().get(0).getSeverity());
	}
	
}
