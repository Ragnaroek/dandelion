package de.defmacro.dandelion.internal.core.dom.parse;

import org.junit.*;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.testutils.*;
import static de.defmacro.dandelion.testutils.AssertUtil.*;
import static org.junit.Assert.*;

/**
 * Defun-Parser Test + OrdinaryLambdaListParser Test
 * @author Michael Bohn
 *
 */
public class TestDefunLambdaListParser 
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
	
	//+++++++++++++++++Korrekte Defuns
	@Test
	public void testMinimalCorrectDefun()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function nil)");
		assertSexpModelHasNoMalformations(model, 1);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("my-function", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertTrue(defun.getBody().isEmpty());
	}

	@Test
	public void testDefunWithNilLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function ())");
		assertSexpModelHasNoMalformations(model, 1);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("my-function", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertTrue(defun.getBody().isEmpty());
	}
	
	@Test
	public void testDefunWithNilSymbolLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function nIL)");
		assertSexpModelHasNoMalformations(model, 1);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("my-function", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertTrue(defun.getBody().isEmpty());
	}
	
	@Test
	public void testDefunWithNilBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function () nil)");
		assertSexpModelHasNoMalformations(model, 2);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("my-function", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertSymbolNameEquals("nil", (Symbol)defun.getBody().get(0));
	}
	
	@Test
	public void testDefunWithLetBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function () \n\t(let ((my-var 'x))))");
		assertSexpModelHasNoMalformations(model, 6);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("my-function", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertEquals(1, defun.getBody().size());
	}
	
	@Test
	public void testDefunWithPrognBody()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function () \n\t(progn (pprint \"bla\") \n\t(pprint #\\c)))");
		assertSexpModelHasNoMalformations(model, 6);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("my-function", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertEquals(1, defun.getBody().size());
	}
	
	@Test
	public void testDefunWithOneCharName()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun + () \n\t(progn (pprint \"bla\") \n\t(pprint #\\c)))");
		assertSexpModelHasNoMalformations(model, 6);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("defun", defun.getFunctionSymbol());
		assertSymbolNameEquals("+", defun.getDefinedName());
		assertTrue(defun.getLambdaList().isNil());
		assertEquals(1, defun.getBody().size());
	}
	
	//++++++Lambda-List Tests korrekt
	
	// Test nur required
	
	@Test
	public void testDefunLambdaOneRequiredParam()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun do (a))");
		assertSexpModelHasNoMalformations(model, 1);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defun.getDefinedName());
		assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
	}
	
	@Test
	public void testDefunLambdaThreeRequiredParam()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun do (a req-2))");
		assertSexpModelHasNoMalformations(model, 1);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defun.getDefinedName());
		assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
		assertSymbolNameEquals("req-2", defun.getLambdaList().getRequiredParameters().get(1));
	}
	
	@Test
	public void testDefunLambdaTenRequiredParam()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun do (a req-2 three four five 6 7 eight nine tenth-parameter))");
		assertSexpModelHasNoMalformations(model, 1);
		DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
		assertSymbolNameEquals("do", defun.getDefinedName());
		assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
		assertSymbolNameEquals("req-2", defun.getLambdaList().getRequiredParameters().get(1));
		assertSymbolNameEquals("three", defun.getLambdaList().getRequiredParameters().get(2));
		assertSymbolNameEquals("four", defun.getLambdaList().getRequiredParameters().get(3));
		assertSymbolNameEquals("five", defun.getLambdaList().getRequiredParameters().get(4));
		assertSymbolNameEquals("6", defun.getLambdaList().getRequiredParameters().get(5));
		assertSymbolNameEquals("7", defun.getLambdaList().getRequiredParameters().get(6));
		assertSymbolNameEquals("eight", defun.getLambdaList().getRequiredParameters().get(7));
		assertSymbolNameEquals("nine", defun.getLambdaList().getRequiredParameters().get(8));
		assertSymbolNameEquals("tenth-parameter", defun.getLambdaList().getRequiredParameters().get(9));
	}
	
	// Test nur &optional
	
	@Test
	public void testDefunLambdaOnlyOptionalKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun func (&optional))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("func", defun.getDefinedName());
			assertFalse(defun.getLambdaList().hasOptionalParameters());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyOptionalOne()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun do (&optional opt))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defun.getDefinedName());
			assertSymbolNameEquals("opt", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
		}
		{ //opt mit default init
			ISexpModel model = project.getSexpModelFor("(defun do (&optional (opt 1)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defun.getDefinedName());
			assertSymbolNameEquals("opt", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("1", (Symbol)defun.getLambdaList().getOptionalParameters().get(0).getInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
		}
		{ //opt mit default init
			ISexpModel model = project.getSexpModelFor("(defun do (&optional (opt (form) supplied-test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defun.getDefinedName());
			assertSymbolNameEquals("opt", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getOptionalParameters().get(0).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("supplied-test-p", defun.getLambdaList().getOptionalParameters().get(0).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefunLambdaOptionalMixed()
	throws Exception
	{
		{ //mehrere otional parameter gemischt
			ISexpModel model = project.getSexpModelFor("(defun do (&optional opt ( opt-init 1 ) (   optional-var " +
					"( \ncomplex-form (apply (format \"~a\" (eval 'x)))) test-p) " +
					"opt-arg 78))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("do", defun.getDefinedName());
			
			assertSymbolNameEquals("opt", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt-init", defun.getLambdaList().getOptionalParameters().get(1).getParameterSymbol());
			assertSymbolNameEquals("1", (Symbol)defun.getLambdaList().getOptionalParameters().get(1).getInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("optional-var", defun.getLambdaList().getOptionalParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("complex-form", ((Form)defun.getLambdaList().getOptionalParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getOptionalParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt-arg", defun.getLambdaList().getOptionalParameters().get(3).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(3).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(3).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("78", defun.getLambdaList().getOptionalParameters().get(4).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(4).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(4).hasSuppliedTestSymbol());
		}
	}	
	
	// Test nur &rest
	
	@Test
	public void testDefunLambdaRest()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun rest-function (&rest the-rest))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("rest-function", defun.getDefinedName());
			assertSymbolNameEquals("the-rest", defun.getLambdaList().getRestParameter());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defun def (&rest rest))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("def", defun.getDefinedName());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
		}
	}
	
	// Test nur &key
	
	@Test
	public void testDefunLambdaOnlyKeyKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("     (defun the-function-with-only-key (&key))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("the-function-with-only-key", defun.getDefinedName());
			assertFalse(defun.getLambdaList().hasKeywordParameters());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyKeyOne()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun\n key-f\n (&key the-key-to-the-door))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("key-f", defun.getDefinedName());
			assertSymbolNameEquals("the-key-to-the-door", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
		{ //key mit default form-init
			ISexpModel model = project.getSexpModelFor("(defun key (&key (key-var 'z)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("key", defun.getDefinedName());
			assertSymbolNameEquals("key-var", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
		{ //opt mit default init
			ISexpModel model = project.getSexpModelFor("(defun init++test (&key (key-var-full-init '(1 2 3 4 '(4 5)) test-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("init++test", defun.getDefinedName());
			assertSymbolNameEquals("key-var-full-init", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(0).getSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyKeyMixed()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun\n key-f\n (&key the-key-to-the-door\n" +
					"\t(key-var 'z)\n" +
					"(key-var-full-init '(1 2 3 4 '(4 5)) test-p)\t single-sym))" +
					"" +
					"");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("key-f", defun.getDefinedName());
			assertSymbolNameEquals("the-key-to-the-door", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key-var", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(1).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());
	
			assertSymbolNameEquals("key-var-full-init", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("single-sym", defun.getLambdaList().getKeywordParameters().get(3).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(3).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(3).hasSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyKeyKeyAndAllowOtherKeys()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("     (defun the-function-with-only-key (&key &allow-other-keys))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("the-function-with-only-key", defun.getDefinedName());
			assertFalse(defun.getLambdaList().hasKeywordParameters());
			assertTrue(defun.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyKeyMixedAndAllowOtherKeys()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun\n key-f\n (&key the-key-to-the-door\n" +
					"\t(key-var 'z)\n" +
					"(key-var-full-init '(1 2 3 4 '(4 5)) test-p)\t single-sym &allow-other-keys))" +
					"" +
					"");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("key-f", defun.getDefinedName());
			assertSymbolNameEquals("the-key-to-the-door", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key-var", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(1).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());
	
			assertSymbolNameEquals("key-var-full-init", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("single-sym", defun.getLambdaList().getKeywordParameters().get(3).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(3).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(3).hasSuppliedTestSymbol());
			
			assertTrue(defun.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	// Test &aux
	
	@Test
	public void testDefunLambdaOnlyAuxKey()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("     (defun the-function-with-only-aux (&aux))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("the-function-with-only-aux", defun.getDefinedName());
			assertFalse(defun.getLambdaList().hasAuxParameters());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyAuxOne()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun\n aux-f\n (&aux aux-var))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("aux-f", defun.getDefinedName());
			assertSymbolNameEquals("aux-var", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defun aux (&aux (aux-var '(z y))))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("aux", defun.getDefinedName());
			assertSymbolNameEquals("aux-var", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());	
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defun init++aux (&aux (aux-var-full-init 'quote test-aux-p)))"); 
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("init++aux", defun.getDefinedName());
			assertSymbolNameEquals("aux-var-full-init", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-aux-p", defun.getLambdaList().getAuxParameters().get(0).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefunLambdaOnlyAuxMixed()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun\n aux-f\n (&aux aux-var (aux-var-2 '(z y)) (aux-var-full-init 'quote test-aux-p)" +
					" single-aux) (progn (setf t 'x) (let (x 'y))))");
			assertSexpModelHasNoMalformations(model, 10);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("aux-f", defun.getDefinedName());
			assertSymbolNameEquals("aux-var", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
		
			assertSymbolNameEquals("aux-var-2", defun.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(1).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());	

			assertSymbolNameEquals("aux-var-full-init", defun.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-aux-p", defun.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("single-aux", defun.getLambdaList().getAuxParameters().get(3).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(3).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(3).hasSuppliedTestSymbol());
		}
	}
	
	//ab hier gemischte Tests
	
	/**
	 * Required Parameter + Optional Parameter angegeben
	 */
	@Test
	public void testDefunLambdaRequiredOptional()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(  defun ++ (a &optional b))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(  defun +2 (a string x &optional b (a 'i) (z (format nil \"~a\" 'x) test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("+2", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("string", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("x", defun.getLambdaList().getRequiredParameters().get(2));
			
			assertSymbolNameEquals("b", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getOptionalParameters().get(1).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getOptionalParameters().get(1).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("z", defun.getLambdaList().getOptionalParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("format", ((Form)defun.getLambdaList().getOptionalParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getOptionalParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredRest()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a b c &rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &key key))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &key key &allow-other-keys))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());
			
			assertTrue(defun.getLambdaList().hasAllowOtherKeys());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a b c &key (key 'x) key2 (key3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("key2", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("key3", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	
	@Test
	public void testDefunLambdaRequiredAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &aux aux))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			
			assertSymbolNameEquals("aux", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a b c &aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			
			assertSymbolNameEquals("aux1", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defun.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defun.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefunLambdaOptionalRest()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&optional (opt1 (form)) opt2 (opt3 'z test-p) &rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("opt1", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getOptionalParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt2", defun.getLambdaList().getOptionalParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt3", defun.getLambdaList().getOptionalParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getOptionalParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getOptionalParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
		}
	}
	
	@Test
	public void testDefunLambdaOptionalAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p))\n)");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("opt1", defun.getLambdaList().getOptionalParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getOptionalParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt2", defun.getLambdaList().getOptionalParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getOptionalParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("opt3", defun.getLambdaList().getOptionalParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getOptionalParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getOptionalParameters().get(2).getSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux1", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defun.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defun.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefunLambdaRestKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&rest rest-sym &key (key 'x) key2 (key3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());

			assertSymbolNameEquals("rest-sym", defun.getLambdaList().getRestParameter());
			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key2", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key3", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&rest rest-sym &key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());

			assertSymbolNameEquals("rest-sym", defun.getLambdaList().getRestParameter());
			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key2", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key3", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertTrue(defun.getLambdaList().hasAllowOtherKeys());
		}
	}
	
	//Dateien zwischendurch loeschen
	@Test
	public void clearProject()
	throws Exception
	{
		tearDownClass();
		setUpClass();
	}
	
	@Test
	public void testDefunLambdaRestAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&rest rest-sym &aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());

			assertSymbolNameEquals("rest-sym", defun.getLambdaList().getRestParameter());
			
			assertSymbolNameEquals("aux1", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defun.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defun.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	/**
	 * Warning, aber sonst korrekt geparst.
	 * @throws Exception
	 */
	@Test
	public void testDefunLambdaOptionalKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun warning (&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("warning", defun.getDefinedName());

			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), true);
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun warning (&optional (opt1 (form)) opt2 (opt3 'z test-p)\n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("warning", defun.getDefinedName());

			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), false);
		}
	}
	
	@Test
	public void testDefunLambdaKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&key (key 'x) key2 (key3 'z test-p) &aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());

			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key2", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key3", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertFalse(defun.getLambdaList().hasAllowOtherKeys());
			
			assertSymbolNameEquals("aux1", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defun.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defun.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (&key (key 'x) key2 (key3 'z test-p) &allow-other-keys &aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());

			
			assertSymbolNameEquals("key", defun.getLambdaList().getKeywordParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(0).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key2", defun.getLambdaList().getKeywordParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getKeywordParameters().get(1).hasSuppliedTestSymbol());

			assertSymbolNameEquals("key3", defun.getLambdaList().getKeywordParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getKeywordParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getKeywordParameters().get(2).getSuppliedTestSymbol());
			
			assertTrue(defun.getLambdaList().hasAllowOtherKeys());
			
			assertSymbolNameEquals("aux1", defun.getLambdaList().getAuxParameters().get(0).getParameterSymbol());
			assertSymbolNameEquals("form", ((Form)defun.getLambdaList().getAuxParameters().get(0).getInitValue()).getFunctionSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(0).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux2", defun.getLambdaList().getAuxParameters().get(1).getParameterSymbol());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasInitValue());
			assertFalse(defun.getLambdaList().getAuxParameters().get(1).hasSuppliedTestSymbol());
			
			assertSymbolNameEquals("aux3", defun.getLambdaList().getAuxParameters().get(2).getParameterSymbol());
			assertSymbolNameEquals("quote", ((Form)defun.getLambdaList().getAuxParameters().get(2).getInitValue()).getFunctionSymbol());
			assertSymbolNameEquals("test-p", defun.getLambdaList().getAuxParameters().get(2).getSuppliedTestSymbol());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredOptionalRest()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++2 (a b c &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++2", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredOptionalKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), false);
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++2 (a b c &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("++2", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), false);
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++2 (a b c &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("++2", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), true);
		}
	}
	
	@Test
	public void testDefunLambdaRequiredOptionalAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertAuxEquals(defun.getLambdaList());
		}
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++2 (a b c &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++2", defun.getDefinedName());
	
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("b", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("c", defun.getLambdaList().getRequiredParameters().get(2));
			
			assertOptionalEquals(defun.getLambdaList());
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaOptionalRestKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun x (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest\n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("x", defun.getDefinedName());
	
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun x (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest\n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("x", defun.getDefinedName());
	
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), true);
		}
	}
	
	@Test
	public void testDefunLambdaOptionalRestAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun ++ (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest the-rest\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("++", defun.getDefinedName());
	
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("the-rest", defun.getLambdaList().getRestParameter());
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaOptionalKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun x (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("x", defun.getDefinedName());
	
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun x (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("x", defun.getDefinedName());
	
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), true);
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun x (&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("x", defun.getDefinedName());
	
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun x (&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("x", defun.getDefinedName());
	
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), true);
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredOptionalRestKey()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun defun (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("defun", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun defun (required-1 required-2 required-param-3 " +
					"\n &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("defun", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun defun (required-1 required-2 required-param-3 " +
					"\n &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p)  \n  &allow-other-keys))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("defun", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), true);
		}
	}
	
	@Test
	public void testDefunLambdaRequiredOptionalRestAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (required-1 required-2 required-param-3\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredOptionalKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (a &optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (required-1 required-2 required-param-3\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (required-1 required-2 required-param-3\n" +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&key (key 'x) key2 (key3 'z test-p)&allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertOptionalEquals(defun.getLambdaList());
			assertKeyEquals(defun.getLambdaList(), true);
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaRequiredRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (a \n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("a", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (required-1 required-2 required-param-3 \n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p)\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (required-1 required-2 required-param-3 \n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys\n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p) ))");
			assertSexpModelHasNoMalformations(model, 1);
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required-1", defun.getLambdaList().getRequiredParameters().get(0));
			assertSymbolNameEquals("required-2", defun.getLambdaList().getRequiredParameters().get(1));
			assertSymbolNameEquals("required-param-3", defun.getLambdaList().getRequiredParameters().get(2));
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), true);
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunLambdaOptionalRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest " +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), true);
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	//komplette Lamba-Liste
	@Test
	public void testDefunLambdaRequiredOptionalRestKeyAux()
	throws Exception
	{
		{ 
			ISexpModel model = project.getSexpModelFor("(defun f (required " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), false);
			assertAuxEquals(defun.getLambdaList());
		}
		
		{ //alles angegeben
			ISexpModel model = project.getSexpModelFor("(defun f (required " +
					"&optional (opt1 (form)) opt2 (opt3 'z test-p) \n" +
					"&rest rest \n" +
					"&key (key 'x) key2 (key3 'z test-p) &allow-other-keys \n" +
					"&aux (aux1 (form)) aux2 (aux3 'z test-p)))");
			assertTrue(model.hasMalformation(TSeverity.WARNING));
			assertEquals(1, model.getMalformations().size());
			DefunForm defun = (DefunForm)model.getTopLevelForms().get(0);
			assertTrue(defun.hasMalformation(TSeverity.WARNING));
			assertEquals(1, defun.getMalformations().size());
			assertSymbolNameEquals("f", defun.getDefinedName());
			
			assertSymbolNameEquals("required", defun.getLambdaList().getRequiredParameters().get(0));
			assertOptionalEquals(defun.getLambdaList());
			assertSymbolNameEquals("rest", defun.getLambdaList().getRestParameter());
			assertKeyEquals(defun.getLambdaList(), true);
			assertAuxEquals(defun.getLambdaList());
		}
	}
	
	@Test
	public void testDefunOptionalKeyWithoutVar()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun my-function-name (&optional &key))");
		assertMalformationsAreEqual(model);
		assertEquals(0, model.getMalformations().size());
		//keine Warnung, hier kann nichts schief gehen da keine Parameter verlangt werden -> Warnung erst wenn auch
		//wirklich parameter angegeben werden
	}
	
	//+++++++++++++++++++++++ Backquoted kein Fehler
	
	@Test
	public void testDefunQuoteInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &optional '(form)))");
		
		assertMalformationsAreEqual(model);
		assertEquals(0, model.getMalformations().size());
	}
	
	@Test
	public void testDefunQuoteInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &key '(key)))");
		
		assertMalformationsAreEqual(model);
		assertEquals(0, model.getMalformations().size());
	}
	
	@Test
	public void testDefunQuoteInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &aux '(aux)))");
		
		assertMalformationsAreEqual(model);
		assertEquals(0, model.getMalformations().size());
	}
	
	@Test
	public void testDefunBackQuoteInOptionalKeyAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r1 r2 &optional `(form ,bla) &rest rest &key `(key ,bla ,blubb) &aux `  bq-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size()); //warning wegen &optional + &key
		assertEquals(TSeverity.WARNING, model.getMalformations().get(0).getSeverity());
	}
	
	@Test
	public void testDefunBackquotedDefun()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor(
					"(defun propmacro (propname)\n" +
					"\"Macro to define a property.\"\n" +
					"\t`(defun ,propname (obj)\n" + //,propname ist nicht erlaubt (bei ungebackquotetem defun)
					"\t`(get ,obj ',',propname)))");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"(mapcar #'(lambda (x)\n" +
					"\t`(defun \"string\" (&aux aux &optional opt &rest rest) (progn \n" + //komplett fehlerhaftes defun
					"\t`(get ,obj ',',propname)))) liste)");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //aber kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"` #+(or clisp lispworks) (defun \"string\" (&aux aux &optional opt &rest rest) nil)");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //aber kein Fehler
		}
	}
	
	@Test
	public void testDefunBackquotedDefunWithReaderMacro()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor(
					"`#+clisp (defun propmacro (propname)\n" +
					"\"Macro to define a property.\"\n" +
					"\t`(defun ,propname (obj)\n" + //,propname ist nicht erlaubt (bei ungebackquotetem defun)
					"\t`(get ,obj ',',propname)))");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //kein Fehler
		}
		{
			ISexpModel model = project.getSexpModelFor(
					"(mapcar #'(lambda (x)\n" +
					"\t`#'(defun \"string\" (&aux aux &optional opt &rest rest) (progn \n" + //komplett fehlerhaftes defun
					"\t`(get ,obj ',',propname)))) liste)");
			assertMalformationsAreEqual(model);
			assertEquals(0, model.getMalformations().size()); //aber kein Fehler
		}
	}
	
	//+++++++++++++++++++++++ Fehlerhafte Defuns
	
	@Test
	public void testDefunErrorNoNameNoLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(defun)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorNoLambdaList()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun name)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(defun name)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorLambdaListIsString()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn \"string\")");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorLambdaListIsKeyword()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn :keyword)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorLambdaListIsReaderMacro()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn #\\c)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorLambdaListNotNilSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun x dsf)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("dsf", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsString()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun \"string-name \" (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string-name \"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsKeyword()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun :keyword (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsReaderMacro()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun #\\c (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsForm()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun (setf x 'y) (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(setf x 'y)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsQuoted()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun 'fn (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("'fn", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsUnquoted()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun ,fn (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(",fn", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFunctionNameIsBackquoted()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun `(a b ,c) (&optional opt) (progn 'body))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("`(a b ,c)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorTwoRestVars()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun err-f (&rest rest rest2) nil)");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("rest2", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	//falsche Parameter symbole
	
	@Test
	public void testDefunErrorStringInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (\"string 0\" &optional \"string (1\" &rest \"string 2\" &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 0\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorStringInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required &optional \"string (1\" &rest \"string 2\" &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string (1\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		
	}
	
	@Test
	public void testDefunErrorStringAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required &optional opt &rest \"string 2\" &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 2\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		
	}
	
	@Test
	public void testDefunErrorStringInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required &optional opt &rest rest &key \"string 3\" &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 3\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorStringInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required &optional opt &rest rest &key key &aux \"string 4\"))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("\"string 4\"", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorKeywordInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (:keyword1 &optional :key1 &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":keyword1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorKeywordInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &optional :key1 &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorKeywordAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &optional o &rest :key2 &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key2", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorKeywordInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &optional o &rest r &key :key3 &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key3", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorKeywordInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &optional o &rest r &key k &aux :key4))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals(":key4", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorCharInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (#\\r &optional #\\o &rest #\\r &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\r", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorCharInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (req &optional #\\o &rest #\\r &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\o", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorCharAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (req &optional opt &rest #\\r &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\r", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorCharInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (req &optional opt &rest res &key #\\k &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\k", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorCharInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (req &optional opt &rest res &key key &aux #\\a))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("#\\a", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFormInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn ((form1) &optional (form2) &rest (form3) &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form1)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFormInOptionalParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required-sym &optional (form2) &rest (form3) &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form2)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFormAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required-sym &optional optional-sym &rest (form3) &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form3)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFormInKeyParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required-sym &optional optional-sym &rest rest-sym &key (form4) &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form4)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorFormInAuxParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (required-sym &optional optional-sym &rest rest-sym &key key-sym &aux (form5)))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(form5)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorQuoteInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn ('quoted &optional '(form) &rest 'quoted1 &key '( bla blubb) &aux '  quote-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("'quoted", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorQuoteAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r &optional optional &rest 'quoted1 &key '( bla blubb) &aux '  quote-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("'quoted1", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	//quote in &optional &key nicht falsch
	
	@Test
	public void testDefunErrorBackQuoteInRequiredParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (`backquoted &optional `(form ,bla) &rest `rest &key `(key ,bla ,blubb) &aux `  bq-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("`backquoted", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorBackQuoteAsRestParameterSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (r1 r2 &optional opt &rest `rest &key `(key ,bla ,blubb) &aux `  bq-aux))");
		
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("`rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	//aux angegeben -> fehlerhafte Ordnung Markierung in kompletter Lambda-Liste
	@Test
	public void testDefunErrorWrongCombinationAuxKey()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&aux aux-var aux-var2 &key key-var))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		//fehler-markierung an &key
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &key key-var)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationAuxRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&aux aux-var aux-var2 &rest rest))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &rest rest)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationAuxOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&aux aux-var aux-var2 &optional opt))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &optional opt)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}

	@Test
	public void testDefunErrorWrongCombinationAuxKeyRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&aux aux-var aux-var2 &key key &rest rest))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &key key &rest rest)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationAuxRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&aux aux-var aux-var2 &rest rest &optional opt-1 opt2))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &rest rest &optional opt-1 opt2)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationAuxKeyRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&aux aux-var aux-var2 &key key-1 key-2 &rest rest &optional opt-1 opt2))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("(&aux aux-var aux-var2 &key key-1 key-2 &rest rest &optional opt-1 opt2)", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationKeyRest()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&key key-1 key-2 &rest rest))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationKeyOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&key key-1 key-2 &optional opt-1 opt-2 opt-3))s");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&optional", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationKeyRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&key key-1 key-2 &rest rest &optional opt-1 opt-2 opt-3))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&rest", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWrongCombinationRestOptional()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(defun fn (&rest rest &optional opt-1 opt-2 opt-3))");
		assertMalformationsAreEqual(model);
		assertEquals(1, model.getMalformations().size());
		assertStringInDocumentEquals("&optional", model.getMalformations().get(0).getPosition(), model.getDocument());
	}
	
	@Test
	public void testDefunErrorWholeInDefun()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn (&whole whole &rest rest &optional opt-1 opt-2 opt-3))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn (&optional opt-1 opt-2 opt-3 &rest rest &whole whole))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&whole", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorEnvironmentInDefun()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn (&environment env &optional opt-1 opt-2 opt-3 &rest rest ))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn (&optional opt-1 opt-2 opt-3 &rest rest  &environment env &key key &aux aux))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("&environment", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn (&optional opt-1 opt-2 opt-3 &rest rest &key key &aux aux &environment env))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(&optional opt-1 opt-2 opt-3 &rest rest &key key &aux aux &environment env)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorDestructureInDefun()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( (var1 exp1 (var2 exp2 (var3 exp3))) (varx) ))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(var1 exp1 (var2 exp2 (var3 exp3)))", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormIllegalLength()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg1 (+ 1 2 3) sym error)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(arg1 (+ 1 2 3) sym error)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg1 (+ 1 2 3) sym error error2 error3 error4 error5)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(arg1 (+ 1 2 3) sym error error2 error3 error4 error5)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormSexpAsInitForm()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg1 (\"string\" \"string2\"))))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"string\" \"string2\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg1 (\"string\" \"string2\") sym)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"string\" \"string2\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormStringAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (\"sym\" \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(\"sym\" \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("\"string\"", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormKeywordAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (:key \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(:key \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" :key)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals(":key", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormReaderSymbolAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (#\\x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(#\\x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" #\\c)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("#\\c", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormFormAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional ((+ 1 2 3) \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("((+ 1 2 3) \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" (+ 1 2 3))))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(+ 1 2 3)", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormQuoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional ('x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("('x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" 'x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("'x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormUnquoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (,x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(,x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" ,x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals(",x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
	
	@Test
	public void testDefunErrorInitFormBackquoteAsSymbol()
	throws Exception
	{
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (`x \"string\")))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("(`x \"string\")", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
		{
			ISexpModel model = project.getSexpModelFor("(defun fn ( &optional (arg \"string\" `x)))");
			assertMalformationsAreEqual(model);
			assertEquals(1, model.getMalformations().size());
			assertStringInDocumentEquals("`x", model.getMalformations().get(0).getPosition(), model.getDocument());
		}
	}
}
