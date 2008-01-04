package de.fh_trier.eclipse.lisp.internal.core.dom.parse;

import static de.fh_trier.eclipse.lisp.testutils.AssertUtil.*;
import static org.junit.Assert.*;

import org.junit.*;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import de.fh_trier.eclipse.lisp.testutils.TestProject;

public class TestDefmacroLambdaListParser 
{
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
	
	//	+++++++++++++++++Korrekte Defmacros
	@Test
	public void testMinimalCorrectDefmacro()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro my-macro nil)");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("my-macro", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertTrue(defmacro.getBody().isEmpty());
	}

	@Test
	public void testDefmacroWithNilLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro macro ())");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("macro", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertTrue(defmacro.getBody().isEmpty());
	}
	
	@Test
	public void testDefmacroWithNilSymbolLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro my-m nIL)");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("my-m", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertTrue(defmacro.getBody().isEmpty());
	}
	
	@Test
	public void testDefmacroWithNilBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro my-macro () nil)");
		assertSexpModelHasNoMalformations(model, 2);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("my-macro", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertSymbolNameEquals("nil", (Symbol)defmacro.getBody().get(0));
	}
	
	@Test
	public void testDefmacroWithLetBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(   defmacro  the-macro-test () \n\t(let ((my-var 'x))))");
		assertSexpModelHasNoMalformations(model, 6);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("the-macro-test", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertEquals(1, defmacro.getBody().size());
	}
	
	@Test
	public void testDefmacroWithPrognBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro progn-macro () \n\t(progn (pprint \"bla\") \n\t(pprint #\\c)))");
		assertSexpModelHasNoMalformations(model, 6);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("progn-macro", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertEquals(1, defmacro.getBody().size());
	}
	
	@Test
	public void testDefmacroWithOneCharName()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro + () \n\t(progn (pprint \"bla\") \n\t(pprint #\\c)))");
		assertSexpModelHasNoMalformations(model, 6);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defmacro", defmacro.getFunctionSymbol());
		assertSymbolNameEquals("+", defmacro.getDefinedName());
		assertTrue(defmacro.getLambdaList().isNil());
		assertEquals(1, defmacro.getBody().size());
	}
	
	// Tests aus Ordinary Lambda List
	
	@Test
	public void testDefmacroLambdaRequiredOptionalRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertTrue(defmacro.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
		
		{ //alles angegeben
			ISexpModel model = project.getSexpModelFor("(defmacro f (required (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz) " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertTrue(defmacro.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaOptional()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&optional opt))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("opt", defmacro.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertFalse(defmacro.getLambdaList().getOptionalParameters().get(0).hasInitValue());
			assertFalse(defmacro.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
		}
		{ //opt mit default init
			ISexpModel model = project.getSexpModelFor("(defmacro do (&optional (opt 1)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("opt", defmacro.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("1", (Symbol)defmacro.getLambdaList().getOptionalParameters().get(0).getInitValue());
			assertFalse(defmacro.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
		}
		{ //opt mit default init
			ISexpModel model = project.getSexpModelFor("(defmacro do (&optional (opt (form) supplied-test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("opt", defmacro.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defmacro.getLambdaList().getOptionalParameters().get(0).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("supplied-test-p", defmacro.getLambdaList().getOptionalParameters().get(0).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefmacroLambdaRestAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (&rest rest-sym &aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());

			assertSymbolNameEquals("rest-sym", defmacro.getLambdaList().getRestParameter());
			
			assertSymbolNameEquals("aux1", defmacro.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defmacro.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defmacro.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defmacro.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defmacro.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defmacro.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defmacro.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defmacro.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredOptionalAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (a (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());
	
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++2 (a b c &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++2", defmacro.getDefinedName());
	
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defmacro.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (a (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required-1 required-2 required-param-3 \n" +
					"(var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required-1 required-2 required-param-3 \n" +
					"(var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p) ))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	//Body Tests
	
	
	@Test
	public void testDefmacroLambdaBody()
	throws Exception
	{
		{ //mehrere otional parameter gemischt
			ISexpModel model = project.getSexpModelFor("(defmacro body-macro (&body the-body))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("body-macro", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
		}
		{ //mehrere otional parameter gemischt
			ISexpModel model = project.getSexpModelFor("(defmacro def (&body body))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("def", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredBody()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (a &body rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());
			
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getBodyParameter());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (a b c (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)  &body rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());
			
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getBodyParameter());
		}
	}
	
	
	@Test
	public void testDefmacroLambdaOptionalBody()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (&optional (opt1 (form)) opt2 (opt3 'z test-p) &body body))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());
	
			assertSymbolNameEquals("opt1", defmacro.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defmacro.getLambdaList().getOptionalParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defmacro.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt2", defmacro.getLambdaList().getOptionalParameters().get(1).getParameterSymbol());
			assertFalse(defmacro.getLambdaList().getOptionalParameters().get(1).hasInitValue());
			assertFalse(defmacro.getLambdaList().getOptionalParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt3", defmacro.getLambdaList().getOptionalParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getOptionalParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defmacro.getLambdaList().getOptionalParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaBodyKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (&body body-sym &key (key 'x) key2 (key3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());

			assertSymbolNameEquals("body-sym", defmacro.getLambdaList().getBodyParameter());
			
			assertSymbolNameEquals("key", defmacro.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defmacro.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key2", defmacro.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defmacro.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defmacro.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key3", defmacro.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defmacro.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertFalse(defmacro.getLambdaList().hasAllowOtherKeys());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (&body body &key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());

			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertSymbolNameEquals("key", defmacro.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defmacro.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key2", defmacro.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defmacro.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defmacro.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key3", defmacro.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defmacro.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertTrue(defmacro.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	@Test
	public void testDefmacroLambdaBodyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (&body body &aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());

			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertSymbolNameEquals("aux1", defmacro.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defmacro.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defmacro.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defmacro.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defmacro.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defmacro.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defmacro.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defmacro.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defmacro.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredOptionalBody()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++ (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defmacro.getDefinedName());
	
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro ++2 (a b c (var1 exp1 (var2 exp2 (var3 exp3))) \t(varx vary varz)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++2", defmacro.getDefinedName());
	
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredBodyKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (a \n" +
					"&body the-body " +
					"&key (key 'x) key2 (key3 'z test-p)))\n");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required-1 required-2 required-param-3 (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&body body " +
					"&key (key 'x) key2 (key3 'z test-p)))\n");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required-1 required-2 required-param-3 (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&body body " +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))\n");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredBodyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (a \n" +
					"&body body " +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertAuxEquals(defmacro.getLambdaList());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required-1 required-2 required-param-3 (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz)\n" +
					"&body body " +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(4));
			
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}

	@Test
	public void testDefmacroLambdaRequiredOptionalBodyKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro f (required " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body \n" +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertTrue(defmacro.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
		
		{ //alles angegeben
			ISexpModel model = project.getSexpModelFor("(defmacro f (required (var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz) " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body-variable \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertTrue(defmacro.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("f", defmacro.getDefinedName());
			
			assertSymbolNameEquals("required", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("body-variable", defmacro.getLambdaList().getBodyParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequired()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2))");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defmacro.getDefinedName());
		assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
		assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
		assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
	}
	
	@Test
	public void testDefmacroLambdaWholeOptional()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defmacro x (&whole wholevar &optional (opt1 (form)) opt2 (opt3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("x", defmacro.getDefinedName());
	
			assertOptionalEquals(defmacro.getLambdaList());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRest()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &rest the-rest))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeBody()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole-variable &body the-body))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole-variable", defmacro.getLambdaList().getWholeParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("\t(defmacro function (&whole whole \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("\t(defmacro function (&whole whole \n" +
					"&key (key 'x) key2 (key3 'z test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("\t(defmacro function (&whole whole-var \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("whole-var", defmacro.getLambdaList().getWholeParameter());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2(var1 exp1 (var2 exp2 (var3 exp3))) (varx vary varz) &optional  (opt1 (form)) opt2 (opt3 'z test-p)))");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defmacro.getDefinedName());
		assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
		
		assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
		assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
		assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
		assertDestructuringEquals("(varx vary varz)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
		
		assertOptionalEquals(defmacro.getLambdaList());
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2 (var1 exp1 (var2 exp2 (var3 exp3)))(varx)&rest rest))");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defmacro.getDefinedName());
		assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
		
		assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
		assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
		assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
		assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
		
		assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx) &body body))");
		assertSexpModelHasNoMalformations(model, 1);
		DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defmacro.getDefinedName());
		assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
		
		assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
		assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
		assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
		assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
		
		assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
			"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2 \n" +
			"&key (key 'x) key2 (key3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar a req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
			"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertSymbolNameEquals("a", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalRest()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalBody()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRestKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &rest the-rest\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &rest the-rest\n" +
					"&key (key 'x) key2 (key3 'z test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRestAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &rest the-rest\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeBodyKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &body body\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &body body\n" +
					"&key (key 'x) key2 (key3 'z test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeBodyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("\t(defmacro \tfunction \t(&whole \twhole &body the-body\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalRest()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalBody()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalRestKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest\n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalRestAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalBodyKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body\n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalBodyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRestKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &rest the-rest\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &rest the-rest\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeBodyKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &body the-body\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &body the-body\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalRestKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest &key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest &key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalBodyKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body b &key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("b", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body b &key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("b", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalRestAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest r &aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("r", defmacro.getLambdaList().getRestParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalBodyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body bo &aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("bo", defmacro.getLambdaList().getBodyParameter());
			
			assertOptionalEquals(defmacro.getLambdaList());
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredRestKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&rest rest\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&rest rest\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredBodyKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&body body\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro do (&whole wholevar req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&body b\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("do", defmacro.getDefinedName());
			assertSymbolNameEquals("wholevar", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("b", defmacro.getLambdaList().getBodyParameter());
			
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalBodyKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body the-body\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body the-body\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("the-body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeOptionalRestKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole \n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalRestKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole \n" +
					"req-1 req-2\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&rest rest\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	//&env test
	@Test
	public void testDefmacroLambdaWholeEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &environment env))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaEnvReq()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&environment env \n" +
					"req-1 req-2\n))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaReqEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"req-1 req-2\n" +
					"&environment env))");
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
		}
	}
	
	@Test
	public void testDefmacroLambdaEnvOptional()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&environment environment \n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("environment", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaOptionalEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&environment environment))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("environment", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaKeyAuxEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)\n" +
					"&environment environment))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("environment", defmacro.getLambdaList().getEnvironmentParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)\n" +
					"&environment environment))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("environment", defmacro.getLambdaList().getEnvironmentParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaKeyEnvAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&environment environment\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)\n" +
					"))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("environment", defmacro.getLambdaList().getEnvironmentParameter());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&environment environment\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)\n" +
					"))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("environment", defmacro.getLambdaList().getEnvironmentParameter());
			assertKeyEquals(defmacro.getLambdaList(), false);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredOptionalBodyEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body b &environment e\n" +
					"))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("e", defmacro.getLambdaList().getEnvironmentParameter());
			assertSymbolNameEquals("b", defmacro.getLambdaList().getBodyParameter());
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredOptionalEnvBody()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&environment e &body b \n" +
					"))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("e", defmacro.getLambdaList().getEnvironmentParameter());
			assertSymbolNameEquals("b", defmacro.getLambdaList().getBodyParameter());
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaRequiredEnvOptionalBody()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx) &environment e\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					" &body b \n" +
					"))"); 
			assertSexpModelHasNoMalformations(model, 1);
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			assertSymbolNameEquals("e", defmacro.getLambdaList().getEnvironmentParameter());
			assertSymbolNameEquals("b", defmacro.getLambdaList().getBodyParameter());
			assertOptionalEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeEnvRequiredOptionalBodyKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole &environment env \n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredEnvOptionalBodyKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx) &environment env\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&body body\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalEnvBodyKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) &environment env\n" +
					"&body body\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalBodyEnvKeyAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body &environment env\n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalBodyKeyEnvAux()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body \n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys &environment env\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p)))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("env", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalBodyKeyAuxEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&body body \n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p) &environment envi))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("body", defmacro.getLambdaList().getBodyParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("envi", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroLambdaWholeRequiredOptionalRestKeyAuxEnv()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro function (&whole whole\n" +
					"req-1 req-2 (var1 exp1 (var2 exp2 (var3 exp3))) (varx)\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" + 
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z\t test-p) &environment e))"); 
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			
			DefmacroForm defmacro = (DefmacroForm)model.getTopLevelForms().get(0);
			assertEquals(1, defmacro.getMalformations().size());
			
			assertSymbolNameEquals("req-1", defmacro.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("req-2", defmacro.getLambdaList().getRequiredParameters().get(1));
			assertDestructuringEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(2));
			assertDestructuringEquals("(varx)", model.getDocument(), defmacro.getLambdaList().getRequiredParameters().get(3));
			
			assertEquals(1, defmacro.getMalformations().size());
			assertSymbolNameEquals("function", defmacro.getDefinedName());
			assertSymbolNameEquals("rest", defmacro.getLambdaList().getRestParameter());
			assertSymbolNameEquals("whole", defmacro.getLambdaList().getWholeParameter());
			assertSymbolNameEquals("e", defmacro.getLambdaList().getEnvironmentParameter());
			assertOptionalEquals(defmacro.getLambdaList());
			assertKeyEquals(defmacro.getLambdaList(), true);
			assertAuxEquals(defmacro.getLambdaList());
		}
	}
	
	@Test
	public void testDefmacroBackquotedDefmacro()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor(
					"(defmacro propmacro (propname)\n" +
					"\"Macro to define a property.\"\n" +
					"\t`(defmacro ,propname (obj &optional \"string\")\n" +
					"\t`(get ,obj ',',propname)))");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"(mapcar #'(lambda (x)\n" +
					"\t`(defmacro \"string\" (&aux aux &optional opt &rest rest) (progn \n" + //komplett fehlerhaftes defmacro
					"\t`(get ,obj ',',propname)))) liste)");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //aber kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"`(defmacro \"string\" (&aux aux &optional opt &rest rest) nil)");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //aber kein Fehler
		}
	}
	
	@Test
	public void testDefmacroBackquotedDefmacroWithReaderMacro()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor(
					"(defmacro propmacro (propname)\n" +
					"\"Macro to define a property.\"\n" +
					"\t`#'(defmacro ,propname (obj &optional \"string\")\n" +
					"\t`(get ,obj ',',propname)))");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"(mapcar #'(lambda (x)\n" +
					"\t`#+(or clisp lispworks) (defmacro \"string\" (&aux aux &optional opt &rest rest) (progn \n" + //komplett fehlerhaftes defmacro
					"\t`(get ,obj ',',propname)))) liste)");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //aber kein Fehler
		}
	}
	
	@Test
	public void testDefmacroDestructuringParameterSimple()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor(
					"(defmacro destruct-macro ((a b c))\n" +
					"body (progn (setf x 'y))\n" +
					")");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"(defmacro destruct-macro ((a b c) (x y z))\n" +
					"body (progn (setf x 'y))" +
					")");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
	}
	
	@Test
	public void testDefmacroDestructuringParameterTree()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor(
					"(defmacro destruct-macro (((x y z) (a b c) c))\n" +
					"body (progn (setf x 'y))\n" +
					")");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"(defmacro destruct-macro ((a (x (m n o) z) c) ((k l m) destruct z) (more (destruct tree) (tree2 (tree3 tree4))))\n" +
					"body (progn (setf x 'y))\n" +
					")");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
	}
	
	//Fehlerhafte Defmacros
	
	@Test
	public void testDefmacroErrorNoNameNoLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
	}
	
	@Test
	public void testDefmacroErrorNoLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro name)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
	}
	
	@Test
	public void testDefmacroErrorLambdaListNotNilSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro x dsf)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
	}
	
	@Test
	public void testDefmacroErrorLambdaListIsString()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro mn \"string\")");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorLambdaListIsKeyword()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro mn :keyword)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorLambdaListIsReaderMacro()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro mn #\\c)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsString()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro \"string-as-macro-name\" (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string-as-macro-name\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsKeyword()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro :keyword (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsReaderMacro()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro #\\c (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsForm()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro (setf x 'y) (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(setf x 'y)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsQuoted()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro 'fn (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("'fn", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsUnquoted()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro ,fn (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(",fn", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFunctionNameIsBackquoted()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro `(a b ,c) (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("`(a b ,c)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorTwoRestVars()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro err-f (&rest rest rest2) nil)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("rest2", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	//falsche Parameter symbole
	
	@Test
	public void testDefmacroErrorStringInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (\"string 0\" &optional \"string (1\" &rest \"string 2\" &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 0\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorStringInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required &optional \"string (1\" &rest \"string 2\" &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string (1\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		
	}
	
	@Test
	public void testDefmacroErrorStringAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required &optional opt &rest \"string 2\" &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 2\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		
	}
	
	@Test
	public void testDefmacroErrorStringInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required &optional opt &rest rest &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 3\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorStringInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required &optional opt &rest rest &key key &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 4\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorKeywordInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (:keyword1 &optional :key1 &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorKeywordInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (r &optional :key1 &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorKeywordAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (r &optional o &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key2", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorKeywordInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (r &optional o &rest r &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key3", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorKeywordInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (r &optional o &rest r &key k &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key4", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorCharInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (#\\r &optional #\\o &rest #\\r &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\r", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorCharInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (req &optional #\\o &rest #\\r &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\o", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorCharAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (req &optional opt &rest #\\r &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\r", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorCharInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (req &optional opt &rest res &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\k", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorCharInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (req &optional opt &rest res &key key &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\a", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFormInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required-sym &optional (form2) &rest (form3) &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form2)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFormAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required-sym &optional optional-sym &rest (form3) &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form3)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFormInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required-sym &optional optional-sym &rest rest-sym &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form4)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorFormInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (required-sym &optional optional-sym &rest rest-sym &key key-sym &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form5)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorQuoteAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (r &optional optional &rest 'quoted1 &key '( bla blubb) &aux '  quote-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("'quoted1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	//quote in &optional &key nicht falsch
	
	@Test
	public void testDefmacroErrorBackQuoteAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (r1 r2 &optional opt &rest `rest &key `(key ,bla ,blubb) &aux `  bq-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("`rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	//aux angegeben -> fehlerhafte Ordnung Markierung in kompletter Lambda-Liste
	@Test
	public void testDefmacroErrorWrongCombinationAuxKey()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&aux aux-var aux-var2 &key key-var))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		//fehler-markierung an &key
		assertStringInDocumentEquals("&key", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationAuxRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&aux aux-var aux-var2 &rest rest))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationAuxOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&aux aux-var aux-var2 &optional opt))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&optional", model.getMalformations().get(0).getPosition(), model.getDocument());
	}

	@Test
	public void testDefmacroErrorWrongCombinationAuxKeyRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&aux aux-var aux-var2 &key key &rest rest))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&key", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationAuxRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&aux aux-var aux-var2 &rest rest &optional opt-1 opt2))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationAuxKeyRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&aux aux-var aux-var2 &key key-1 key-2 &rest rest &optional opt-1 opt2))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&key", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationKeyRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&key key-1 key-2 &rest rest))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationKeyOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&key key-1 key-2 &optional opt-1 opt-2 opt-3))s");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&optional", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationKeyRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&key key-1 key-2 &rest rest &optional opt-1 opt-2 opt-3))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefmacroErrorWrongCombinationRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defmacro fn (&rest rest &optional opt-1 opt-2 opt-3))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&optional", model.getMalformations().get(0).getPosition(), model.getDocument());
	}

	@Test
	public void testDefmacroErrorInitFormIllegalLength()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg1 (+ 1 2 3) sym error)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(arg1 (+ 1 2 3) sym error)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg1 (+ 1 2 3) sym error error2 error3 error4 error5)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(arg1 (+ 1 2 3) sym error error2 error3 error4 error5)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormSexpAsInitForm()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg1 (\"string\" \"string2\"))))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"string\" \"string2\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg1 (\"string\" \"string2\") sym)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"string\" \"string2\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormStringAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (\"sym\" \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"sym\" \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("\"string\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormKeywordAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (:key \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(:key \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" :key)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals(":key", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormReaderSymbolAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (#\\x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(#\\x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" #\\c)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormFormAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional ((+ 1 2 3) \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("((+ 1 2 3) \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" (+ 1 2 3))))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(+ 1 2 3)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormQuoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional ('x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("('x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" 'x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("'x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormUnquoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (,x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(,x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" ,x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals(",x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorInitFormBackquoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (`x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(`x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional (arg \"string\" `x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("`x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorWrongOrderWhole()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &optional &whole ))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( required &whole ))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( required &optional opt &whole whole))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( required &optional opt &rest rest &whole whole))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( required &optional opt &rest rest &key key &whole whole))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( required &optional opt &rest rest &key key &aux aux &whole whole))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &aux aux &whole whole))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefmacroErrorEnvironmentMoreThenOnce()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &optional opt &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &optional opt &body body &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &optional opt &body body &key key &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &optional opt &body body &key key &aux aux &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &optional opt &body body &key key &aux aux &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defmacro fn ( &environment env a b c &environment env ab  &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
}
