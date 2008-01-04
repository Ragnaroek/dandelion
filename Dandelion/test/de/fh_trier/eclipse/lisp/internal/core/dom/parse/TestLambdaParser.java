package de.fh_trier.eclipse.lisp.internal.core.dom.parse;


import static de.fh_trier.eclipse.lisp.testutils.AssertUtil.*;
import static org.junit.Assert.*;

import org.junit.*;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import de.fh_trier.eclipse.lisp.testutils.TestProject;

public class TestLambdaParser 
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
	
	// Test mit Stichproben aus LambdaList Test
	
	@Test
	public void testMinimalCorrectLambda()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda ())");
		assertSexpModelHasNoMalformations(model, 1);
		LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("lambda", lambda.getFunctionSymbol());
		assertSymbolNameEquals("nil", lambda.getDefinedName());
		assertTrue(lambda.getLambdaList().isNil());
		assertTrue(lambda.getBody().isEmpty());
	}
	
	@Test
	public void testLambdaWithNilSymbolLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda nIL)");
		assertSexpModelHasNoMalformations(model, 1);
		LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("lambda", lambda.getFunctionSymbol());
		assertSymbolNameEquals("nil", lambda.getDefinedName());
		assertTrue(lambda.getLambdaList().isNil());
		assertTrue(lambda.getBody().isEmpty());
	}
	
	@Test
	public void testLambdaWithNilBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda () nil)");
		assertSexpModelHasNoMalformations(model, 2);
		LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("lambda", lambda.getFunctionSymbol());
		assertSymbolNameEquals("nil", lambda.getDefinedName());
		assertTrue(lambda.getLambdaList().isNil());
		assertSymbolNameEquals("nil", (Symbol)lambda.getBody().get(0));
	}
	
	@Test
	public void testLambdaWithLetBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda () \n\t(let ((my-var 'x))))");
		assertSexpModelHasNoMalformations(model, 6);
		LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("lambda", lambda.getFunctionSymbol());
		assertSymbolNameEquals("nil", lambda.getDefinedName());
		assertTrue(lambda.getLambdaList().isNil());
		assertEquals(1, lambda.getBody().size());
	}
	
	@Test
	public void testLambdaWithPrognBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda () \n\t(progn (pprint \"bla\") \n\t(pprint #\\c)))");
		assertSexpModelHasNoMalformations(model, 6);
		LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("lambda", lambda.getFunctionSymbol());
		assertSymbolNameEquals("nil", lambda.getDefinedName());
		assertTrue(lambda.getLambdaList().isNil());
		assertEquals(1, lambda.getBody().size());
	}

	@Test
	public void testLambdaBackquoted()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("`(lambda (,bla) (progn (pprint ,test) (pprint ,x)))");
			assertEquals(0, model.getMalformations().size());
		}
		
		{
			ISexpModel model = project.getSexpModelFor("(defun test-func () " +
					"`(lambda (,bla) (progn (pprint ,test) (pprint ,x))))");
			assertEquals(0, model.getMalformations().size());
		}
	}
	
	@Test
	public void testLambdaBackquotedWithReaderMacro()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("`#'(lambda (,bla) (progn (pprint ,test) (pprint ,x)))");
			assertEquals(0, model.getMalformations().size());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun test-func () " +
					"`#'(lambda (,bla) (progn (pprint ,test) (pprint ,x))))");
			assertEquals(0, model.getMalformations().size());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun test-func () " +
					"`#+clisp (lambda (,bla) (progn (pprint ,test) (pprint ,x))))");
			assertEquals(0, model.getMalformations().size());
		}
	}
	
	//Stichproben aus ordinary lambda list
	
	/**
	 * Optional + Key => Warnung
	 */
	@Test
	public void testLambdaRequiredOptionalRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(lambda (required " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
			assertTrue(lambda.hasMalformation(TSeverity.WARNING));
			assertEquals(1, lambda.getMalformations().size());
			assertSymbolNameEquals("NIL", lambda.getDefinedName());
			
			assertSymbolNameEquals("required", lambda.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(lambda.getLambdaList());
			assertSymbolNameEquals("rest", lambda.getLambdaList().getRestParameter());
			assertKeyEquals(lambda.getLambdaList(), false);
			assertAuxEquals(lambda.getLambdaList());
		}
		
		{ //alles angegeben
			ISexpModel model = project.getSexpModelFor("(lambda (required " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
			assertTrue(lambda.hasMalformation(TSeverity.WARNING));
			assertEquals(1, lambda.getMalformations().size());
			assertSymbolNameEquals("NIL", lambda.getDefinedName());
			
			assertSymbolNameEquals("required", lambda.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(lambda.getLambdaList());
			assertSymbolNameEquals("rest", lambda.getLambdaList().getRestParameter());
			assertKeyEquals(lambda.getLambdaList(), true);
			assertAuxEquals(lambda.getLambdaList());
		}
	}
	
	@Test
	public void testLambdaRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(lambda (&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("NIL", lambda.getDefinedName());
	
			assertSymbolNameEquals("rest", lambda.getLambdaList().getRestParameter());
			assertKeyEquals(lambda.getLambdaList(), false);
			assertAuxEquals(lambda.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(lambda (&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("NIL", lambda.getDefinedName());
	
			assertSymbolNameEquals("rest", lambda.getLambdaList().getRestParameter());
			assertKeyEquals(lambda.getLambdaList(), true);
			assertAuxEquals(lambda.getLambdaList());
		}
	}
	
	@Test
	public void testLambdaRequiredOptionalRest()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(lambda (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("NIL", lambda.getDefinedName());
	
			assertSymbolNameEquals("a", lambda.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(lambda.getLambdaList());
			assertSymbolNameEquals("rest", lambda.getLambdaList().getRestParameter());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(lambda (a b c &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			LambdaForm lambda = (LambdaForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("NIL", lambda.getDefinedName());
	
			assertSymbolNameEquals("a", lambda.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", lambda.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", lambda.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(lambda.getLambdaList());
			assertSymbolNameEquals("rest", lambda.getLambdaList().getRestParameter());
		}
	}
	
	//ENDE Stichproben
	
	//Fehlerhafte
	
	@Test
	public void testLambdaErrorNoLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(lambda)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorLambdaListNotNilSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda dsf)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("dsf", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorLambdaListIsString()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda \"string\")");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorLambdaListIsKeyword()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda :keyword)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorLambdaListIsReaderMacro()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda #\\c)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
	}

	//Stichproben aus defun
	
	//TODO anpassen
	
	@Test
	public void testLambdaErrorTwoRestVars()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda (&rest rest rest2) nil)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("rest2", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorKeywordInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda (:keyword1 &optional :key1 &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorBackQuoteAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda (r1 r2 &optional opt &rest `rest &key `(key ,bla ,blubb) &aux `  bq-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("`rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorWrongCombinationAuxKey()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(lambda (&aux aux-var aux-var2 &key key-var))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		//fehler-markierung an &key
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &key key-var)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testLambdaErrorEnvironmentInOrdinaryLambdaList()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(lambda (&environment env &optional opt-1 opt-2 opt-3 &rest rest ))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(lambda (&optional opt-1 opt-2 opt-3 &rest rest  &environment env &key key &aux aux))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(lambda (&optional opt-1 opt-2 opt-3 &rest rest &key key &aux aux &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(&optional opt-1 opt-2 opt-3 &rest rest &key key &aux aux &environment env)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testLambdaErrorDestructureInOrdinaryLambdaList()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(lambda ( (var1 exp1 (var2 exp2 (var3 exp3))) (varx) ))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testLambdaErrorInitFormStringAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(lambda ( &optional (\"sym\" \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"sym\" \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(lambda ( &optional (arg \"string\" \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("\"string\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testLambdaErrorInitFormBackquoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(lambda ( &optional (`x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(`x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(lambda ( &optional (arg \"string\" `x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("`x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
}
