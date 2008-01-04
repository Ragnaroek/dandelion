package de.fh_trier.eclipse.lisp.internal.core.dom;

import static org.junit.Assert.*;
import static de.fh_trier.eclipse.lisp.testutils.AssertUtil.*;
import static de.fh_trier.eclipse.lisp.internal.core.dom.TypeFactory.*;

import java.util.*;

import org.eclipse.jface.text.Position;
import org.junit.*;

import de.fh_trier.eclipse.lisp.internal.core.dom.Symbol;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

public class TestTypeFactory
{
	@SuppressWarnings("UwF")
	private Position position = null;
	
	@After
	public void tearDown()
	{
		position = null;
	}
	
	@Before
	public void setUp()
	{
		position = new Position(13, 12);
	}
	
	private Position getPosition()
	{
		return position == null ? new Position(0, 0) : position;
	}
	
	//normale Symbole testen
	
	private void testInternedUnqualifiedPublicSymbol(final String symbol)
	{
		assertSymbolEquals(createSymbol(symbol, getPosition()), symbol, getPosition(), null, true, false);
	}
	
	private Symbol testInternedQualifiedPublicSymbol(final String expectedSymbolName, final String qualifier, final String symbol)
	{
		Symbol sym = createSymbol(symbol, getPosition());
		assertSymbolEquals(sym, expectedSymbolName, getPosition(), qualifier, true, false);
		return sym;
	}
	
	private void testInternedQualifiedPrivateSymbol(final String expectedSymbolName, final String qualifier, final String symbol)
	{
		assertSymbolEquals(createSymbol(symbol, getPosition()), expectedSymbolName, getPosition(), qualifier, true, true);
	}
	
	private Symbol testUninternedSymbol(final String expectedSymbolName, final String symbol)
	{
		Symbol sym = createSymbol(symbol, getPosition());
		assertSymbolEquals(sym, expectedSymbolName, getPosition(), null, false, false);
		return sym;
	}
	
	@Test
	public void testCreateNil()
	{
		assertSymbolEquals(createSymbol(null, getPosition()), "NIL", getPosition(), null, true, false);
	}
	
	private Symbol testInternedUnqualifiedPublicStringSymbol(final String symbol)
	{
		Symbol sym = createSymbol(symbol, getPosition());
		assertStringSymbolEquals(sym, symbol, getPosition(), null, true, false);
		return sym;
	}
	
	private Symbol testInternedUnqualifiedPublicKeywordSymbol(final String symbol)
	{
		Symbol sym = createSymbol(symbol, getPosition());
		assertKeywordSymbolEquals(sym, symbol, getPosition(), null, true, false);
		return sym;
	}
	
	private Symbol testInternedUnqualifiedPublicReaderSymbol(final String symbol, final String symbolNameExpected, final char dispatch)
	{
		Symbol sym = createSymbol(symbol, getPosition());
		assertReaderSymbolEquals(sym, symbolNameExpected, getPosition(), null, true, false, dispatch);
		return sym;
	}
	
	@Test
	public void testCreateIllegalSymbols()
	{
		//nicht interned aber qualifier angegeben
		try {
			new Symbol("sym", new Position(50, 75), "de.fh-trier", false, false);
			fail("IllegalArgumentException expeceted");
		} catch (IllegalArgumentException e) {
			//no-op
		}
		
		//private aber kein qualifier angeben
		try {
			new Symbol("sym", new Position(50, 75), null, false, true);
			fail("IllegalArgumentException expeceted");
		} catch (IllegalArgumentException e) {
			//no-op
		}
	}
	
	@Test
	public void testCreateInternedPublicUnqualifiedSymbol() 
	{
		testInternedUnqualifiedPublicSymbol("");
		testInternedUnqualifiedPublicSymbol("symbol");
		testInternedUnqualifiedPublicSymbol("x");
		testInternedUnqualifiedPublicSymbol("nil");
		testInternedUnqualifiedPublicSymbol("&key");
		testInternedUnqualifiedPublicSymbol("<my-var!>->");
		testInternedUnqualifiedPublicSymbol("my-sym#\"");
		testInternedUnqualifiedPublicSymbol("kljsdf12/§%$==&$&");
	}
	
	@Test
	public void testCreateInternedPublicQualifiedSymbol()
	{
		testInternedQualifiedPublicSymbol("", "package", "package:");
		testInternedQualifiedPublicSymbol("symbolName", "de.fh_trier.eclipse", "de.fh_trier.eclipse:symbolName");
		testInternedQualifiedPublicSymbol("*globalSpecial*", "cl-user", "cl-user:*globalSpecial*");
		testInternedQualifiedPublicSymbol("+constant+", "cl-user", "cl-user:+constant+");
		testInternedQualifiedPublicSymbol("&%§$%&\"+", "cl-user", "cl-user:&%§$%&\"+");
		testInternedQualifiedPublicSymbol("my-symbolwith:more", "my-package", "my-package:my-symbolwith:more");
		
		testInternedQualifiedPublicSymbol("\"\"", "cl-user", "cl-user:\"\"");
		testInternedQualifiedPublicSymbol("\"my-string\"", "cl-user", "cl-user:\"my-string\"");
		testInternedQualifiedPublicSymbol("\"my-string with blank and\" \\\" escape\"", "package", "package:\"my-string with blank and\" \\\" escape\"");
		testInternedQualifiedPublicSymbol("\" \"", "de.fh-trier", "de.fh-trier:\" \""); //nur blank
		testInternedQualifiedPublicSymbol("\"?\"", "x", "x:\"?\"");
		testInternedQualifiedPublicSymbol("\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"", "my.-package-", "my.-package-:\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"");
		testInternedQualifiedPublicSymbol("\"schliessendes string ende \"fehlt", "package-erkannt", "package-erkannt:\"schliessendes string ende \"fehlt");
		
		testInternedQualifiedPublicSymbol("#<object>", "cl-user", "cl-user:#<object>"); //nicht reader symbol
		testInternedQualifiedPublicSymbol("#<","cl-user","cl-user:#<");
		testInternedQualifiedPublicSymbol("#<object>", "de.fh-trier", "de.fh-trier:#<object>");
		testInternedQualifiedPublicSymbol("#'function", "pack", "pack:#'function");
		testInternedQualifiedPublicSymbol("#\\c", "cl-user", "cl-user:#\\c");
		testInternedQualifiedPublicSymbol("#\\space", "package", "package:#\\space");
		testInternedQualifiedPublicSymbol("#\\backquote", "x", "x:#\\backquote");
	}
	
	@Test
	public void testCreateUninternedSymbols()
	{
		testUninternedSymbol("", "#:");
		testUninternedSymbol("symbol", "#:symbol");
		testUninternedSymbol("NIL", "#:NIL");
		testUninternedSymbol("&key", "#:&key");
		testUninternedSymbol("xxxx::bla", "#:xxxx::bla");
		testUninternedSymbol("var:bla", "#:var:bla");
		testUninternedSymbol("blubb#:", "#:blubb#:");
		
		testUninternedSymbol("\"my-uninterned-symbol\"", "#:\"my-uninterned-symbol\"");
		testUninternedSymbol("\"\"", "#:\"\"");
		testUninternedSymbol("\"my-string\"", "#:\"my-string\"");
		testUninternedSymbol("\"my-string with blank and\" \\\" escape\"", "#:\"my-string with blank and\" \\\" escape\"");
		testUninternedSymbol("\" \"", "#:\" \""); //nur blank
		testUninternedSymbol("\"?\"", "#:\"?\"");
		testUninternedSymbol("\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"", "#:\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"");

		testUninternedSymbol(":test", "#::test"); //nicht keyword
		testUninternedSymbol(":::test,", "#::::test,");
	
		testUninternedSymbol("#<object>","#:#<object>"); //nicht reader symbol, symbole werden wie gelesen ausgegeben nur mit nicht interned markiert
		testUninternedSymbol("#<", "#:#<");
		testUninternedSymbol("#<object>", "#:#<object>");
		testUninternedSymbol("#'function", "#:#'function");
		testUninternedSymbol("#\\c", "#:#\\c");
		testUninternedSymbol("#\\space", "#:#\\space");
		testUninternedSymbol("#\\backquote", "#:#\\backquote");
	}
	
	@Test
	public void testCreateInternedPrivateQualifiedSymbol()
	{
		testInternedQualifiedPrivateSymbol("", "pack", "pack::");
		testInternedQualifiedPrivateSymbol("symbolName", "de.fh_trier.eclipse", "de.fh_trier.eclipse::symbolName");
		testInternedQualifiedPrivateSymbol("*globalSpecial*", "cl-user", "cl-user::*globalSpecial*");
		testInternedQualifiedPrivateSymbol("+constant+", "cl-user", "cl-user::+constant+");
		testInternedQualifiedPrivateSymbol("&%§$%&\"+", "cl-user", "cl-user::&%§$%&\"+");
		testInternedQualifiedPrivateSymbol("my-symbolwith::more", "my-package", "my-package::my-symbolwith::more");
		
		testInternedQualifiedPrivateSymbol("\"\"", "cl-user", "cl-user::\"\"");
		testInternedQualifiedPrivateSymbol("\"my-string\"", "cl-user", "cl-user::\"my-string\"");
		testInternedQualifiedPrivateSymbol("\"my-string with blank and\" \\\" escape\"", "package", "package::\"my-string with blank and\" \\\" escape\"");
		testInternedQualifiedPrivateSymbol("\" \"", "de.fh-trier", "de.fh-trier::\" \""); //nur blank
		testInternedQualifiedPrivateSymbol("\"?\"", "x", "x::\"?\"");
		testInternedQualifiedPrivateSymbol("\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"", "my.-package-", "my.-package-::\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"");
		testInternedQualifiedPrivateSymbol("\"schliessendes string ende \"fehlt", "package-erkannt", "package-erkannt::\"schliessendes string ende \"fehlt");
		
		testInternedQualifiedPrivateSymbol(":symbolName", "de.fh-trier", "de.fh-trier:::symbolName"); //nicht key-symbol
		testInternedQualifiedPrivateSymbol(":symbol::Name", "de.fh-trier", "de.fh-trier:::symbol::Name");
		
		testInternedQualifiedPrivateSymbol("#<object>", "cl-user", "cl-user::#<object>"); //nicht reader symbol
		testInternedQualifiedPrivateSymbol("#<","cl-user","cl-user::#<");
		testInternedQualifiedPrivateSymbol("#<object>", "de.fh-trier", "de.fh-trier::#<object>");
		testInternedQualifiedPrivateSymbol("#'function", "pack", "pack::#'function");
		testInternedQualifiedPrivateSymbol("#\\c", "cl-user", "cl-user::#\\c");
		testInternedQualifiedPrivateSymbol("#\\space", "package", "package::#\\space");
		testInternedQualifiedPrivateSymbol("#\\backquote", "x", "x::#\\backquote");
		testInternedQualifiedPrivateSymbol("#(1", "my-package", "my-package::#(1");
		testInternedQualifiedPrivateSymbol("#4(1", "the-package", "the-package::#4(1");
		testInternedQualifiedPrivateSymbol("#0A((0", "pac", "pac::#0A((0");
	}
	
	//String symbole
	
	

	@Test
	public void testCreateCreateInternedPublicUnqualifiedStringSymbol()
	{
		testInternedUnqualifiedPublicStringSymbol("\"\"");
		testInternedUnqualifiedPublicStringSymbol("\"my-string\"");
		testInternedUnqualifiedPublicStringSymbol("\"my-string with blank and\" \\\" escape\"");
		testInternedUnqualifiedPublicStringSymbol("\" \""); //nur blank
		testInternedUnqualifiedPublicStringSymbol("\"?\"");
		testInternedUnqualifiedPublicStringSymbol("\" Ein String mit \"Zeilenumbruch \n nächste Zeile\"");
		
		Symbol sym = testInternedUnqualifiedPublicStringSymbol("\"schliessendes string ende \"fehlt");
		assertTrue(sym.hasMalformation());
		assertEquals(1, sym.getMalformations().size());
	}
	
	//Keyword Symbol
	
	
	
	@Test
	public void testCreateCreateInternedPublicUnqualifiedKeywordSymbol()
	{
		testInternedUnqualifiedPublicKeywordSymbol(":key");
		testInternedUnqualifiedPublicKeywordSymbol("::key");
		testInternedUnqualifiedPublicKeywordSymbol(":keyword-symbol:my-symbol");
		testInternedUnqualifiedPublicKeywordSymbol(":key::more-of-key");
		testInternedUnqualifiedPublicKeywordSymbol(":#keyw-i\"%&\"%§$\"");
	}
	
	//Reader Symbol
	
	
	
	@Test
	public void testCreateCreateInternedPublicUnqualifiedReaderSymbol()
	{
		testInternedUnqualifiedPublicReaderSymbol("#<","",'<');
		testInternedUnqualifiedPublicReaderSymbol("#<object>", "object>", '<');
		testInternedUnqualifiedPublicReaderSymbol("#'function", "function", '\'');
		testInternedUnqualifiedPublicReaderSymbol("##sharp", "sharp", '#');
		testInternedUnqualifiedPublicReaderSymbol("#r4578", "4578", 'r');
		testInternedUnqualifiedPublicReaderSymbol("#+allegro", "allegro", '+');
		testInternedUnqualifiedPublicReaderSymbol("#{curly}", "curly}", '{');
		testInternedUnqualifiedPublicReaderSymbol("#\\c", "c", '\\');
		testInternedUnqualifiedPublicReaderSymbol("#\\space", "space", '\\');
		testInternedUnqualifiedPublicReaderSymbol("#\\backquote", "backquote", '\\');
		testInternedUnqualifiedPublicReaderSymbol("#(1 2 3)", "1 2 3)", '(');
		testInternedUnqualifiedPublicReaderSymbol("#4(1 2 3)", "1 2 3)", '(');
		testInternedUnqualifiedPublicReaderSymbol("#0A((0 1 5) (foo 2 (hot dog)))","((0 1 5) (foo 2 (hot dog)))", 'a');
		testInternedUnqualifiedPublicReaderSymbol("#0A", "", 'a');
		testInternedUnqualifiedPublicReaderSymbol("#0", "vector", '('); //wird eventuell vector
		testInternedUnqualifiedPublicReaderSymbol("#", "vector", '('); //hier auch
	}
	
	@Test
	public void testCreateMalformedSymbol()
	{
		StringSymbol sym = (StringSymbol)testInternedUnqualifiedPublicStringSymbol("\"my-string"); // " fehlt am Ende
		assertTrue(sym.hasMalformation(TSeverity.ERROR));
		
		sym = (StringSymbol)testInternedUnqualifiedPublicStringSymbol("\""); //nur "
		assertTrue(sym.hasMalformation(TSeverity.ERROR));
		
		//wird nicht als String-Symbol erkannt, daher auch kein Fehler
		Symbol symbol = testUninternedSymbol("\"my-string", "#:\"my-string");
		assertFalse(symbol.hasMalformation());
		
		symbol = testInternedQualifiedPublicSymbol("\"my-string", "cl-user", "cl-user:\"my-string");
		assertFalse(symbol.hasMalformation());
	}
	
	@Test
	public void testCreateMalformedSymbolWithNullPosition()
	{
		position = null; //createSymbol wird mit null als Position aufgerufen
		
		StringSymbol sym = (StringSymbol)testInternedUnqualifiedPublicStringSymbol("\"my-string"); // " fehlt am Ende
		assertTrue(sym.hasMalformation(TSeverity.ERROR));
		
		sym = (StringSymbol)testInternedUnqualifiedPublicStringSymbol("\""); //nur "
		assertTrue(sym.hasMalformation(TSeverity.ERROR));
		
		Symbol symbol = testUninternedSymbol("\"my-string", "#:\"my-string");
		assertFalse(symbol.hasMalformation());
		
		symbol = testInternedQualifiedPublicSymbol("\"my-string", "cl-user", "cl-user:\"my-string");
		assertFalse(symbol.hasMalformation());
	}
	
	@Test
	public void testDestructureSymbol()
	{
		assertDestructuredSymbolEquals(destructureSymbol("symbol"), "symbol", null, false);
		assertDestructuredSymbolEquals(destructureSymbol("de.fh-trier:bla"), "bla", "de.fh-trier", false);
		assertDestructuredSymbolEquals(destructureSymbol("package-only:"), "", "package-only", false);
		assertDestructuredSymbolEquals(destructureSymbol("my-package:sym:bla"),"sym:bla" , "my-package", false);
		assertDestructuredSymbolEquals(destructureSymbol("de.fh-trier.my.package23::private"), "private", "de.fh-trier.my.package23", true);
		assertDestructuredSymbolEquals(destructureSymbol("package-only::"), "", "package-only", true);
		assertDestructuredSymbolEquals(destructureSymbol("my-package::sym::bla"),"sym::bla" , "my-package", true);
		assertDestructuredSymbolEquals(destructureSymbol(""), "", null, false);
		
		try {
			destructureSymbol(null);
			fail("NullPointerException expected");
		} catch (NullPointerException e) {
			//no-op
		}
	}
	
	// Test createSExpression
	
	
	@Test
	public void testCreateForm()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("fn"));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			List<SExpression> formList = new ArrayList<SExpression>();
			formList.add(new Symbol("mapcar"));
			formList.add(new ReaderSymbol("identity", new Position(0, 30), '\''));
			list.add(new Symbol("bla")); //erstes element liste = symbol
			list.add(new Form(formList, new Position(0,60)));
			SExpression created = TypeFactory.createSExpression(list, new Position(0, 70), true);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
			assertTrue(created.getPosition().equals(new Position(0, 70)));
		}
	}
	
	@Test
	public void testCreateSexp()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new StringSymbol("\"string\""));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.SEXPRESSION);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			List<SExpression> formList = new ArrayList<SExpression>();
			formList.add(new Symbol("mapcar"));
			formList.add(new ReaderSymbol("identity", new Position(0, 30), '\''));
			list.add(new Form(formList, new Position(0,60)));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0, 70), true);
			
			assertTrue(created.getTyp() == TSExpression.SEXPRESSION);
			assertTrue(created.getPosition().equals(new Position(0, 70)));
		}
	}
	
	@Test
	public void testCreateDefun()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defun"));
			list.add(new Symbol("fn"));
			list.add(new Symbol(Symbol.SYM_NIL));
			list.add(new Symbol(Symbol.SYM_NIL));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.DEFUN);
			assertTrue(created.getPosition().equals(new Position(0,10)));
			assertFalse(((DefunForm)created).getLambdaList().hasRequiredParameters());
			assertFalse(((DefunForm)created).getLambdaList().hasOptionalParameters());
			assertFalse(((DefunForm)created).getLambdaList().hasKeywordParameters());
			assertFalse(((DefunForm)created).getLambdaList().hasRestParameter());
			assertFalse(((DefunForm)created).getLambdaList().hasAuxParameters());
		}
		
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defun"));
			list.add(new Symbol("fn2"));
			
			List<SExpression> lambdaList = new ArrayList<SExpression>();
			lambdaList.add(new Symbol("required"));
			lambdaList.add(new Symbol("&optional"));
			list.add(new SExpression(lambdaList));
			list.add(new Symbol(Symbol.SYM_NIL)); //body
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.DEFUN);
			assertTrue(created.getPosition().equals(new Position(0,10)));
			assertTrue(((DefunForm)created).getLambdaList().getRequiredParameters().size() == 1);
			assertTrue(((DefunForm)created).getLambdaList().getRequiredParameters().size() == 1);
			assertFalse(((DefunForm)created).getLambdaList().hasKeywordParameters());
			assertFalse(((DefunForm)created).getLambdaList().hasRestParameter());
			assertFalse(((DefunForm)created).getLambdaList().hasAuxParameters());
		}
	}
	
	@Test
	public void testCreateDefmacro()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defmacro"));
			list.add(new Symbol("macro-name"));
			list.add(new Symbol(Symbol.SYM_NIL));
			list.add(new Symbol(Symbol.SYM_NIL));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.DEFMACRO);
			assertTrue(created.getPosition().equals(new Position(0,10)));
			assertTrue(created instanceof DefmacroForm);
			assertFalse(((DefmacroForm)created).getLambdaList().hasWholeParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasRequiredParameters());
			assertFalse(((DefmacroForm)created).getLambdaList().hasOptionalParameters());
			assertFalse(((DefmacroForm)created).getLambdaList().hasKeywordParameters());
			assertFalse(((DefmacroForm)created).getLambdaList().hasEnvironmentParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasBodyParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasRestParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasAuxParameters());
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defmacro"));
			list.add(new Symbol("macro-name2"));
			
			List<SExpression> lambdaList = new ArrayList<SExpression>();
			lambdaList.add(new Symbol("&whole"));
			lambdaList.add(new Symbol("whole-var"));
			lambdaList.add(new Symbol("required"));
			lambdaList.add(new Symbol("&optional"));
			lambdaList.add(new Symbol("opt-var"));
			list.add(new SExpression(lambdaList));
			list.add(new Symbol(Symbol.SYM_NIL)); //body
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.DEFMACRO);
			assertTrue(created.getPosition().equals(new Position(0,10)));
			assertTrue(created instanceof DefmacroForm);
			assertTrue(((DefmacroForm)created).getLambdaList().hasWholeParameter());
			assertTrue(((DefmacroForm)created).getLambdaList().getRequiredParameters().size() == 1);
			assertTrue(((DefmacroForm)created).getLambdaList().getOptionalParameters().size() == 1);
			assertFalse(((DefmacroForm)created).getLambdaList().hasKeywordParameters());
			assertFalse(((DefmacroForm)created).getLambdaList().hasEnvironmentParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasBodyParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasRestParameter());
			assertFalse(((DefmacroForm)created).getLambdaList().hasAuxParameters());
		}
	}
	
	@Test
	public void testCreateInpackage()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("in-package"));
			list.add(new Symbol("package"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.INPACKAGE);
			assertTrue(created instanceof InpackageForm);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("in-package"));
			list.add(new KeywordSymbol(":package"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.INPACKAGE);
			assertTrue(created instanceof InpackageForm);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
	}
	
	@Test
	public void testCreateDefpackage()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defpackage"));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.DEFPACKAGE);
			assertTrue(created instanceof DefpackageForm);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
	}
	
	@Test
	public void testCreateLambda()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("lambda"));
			list.add(new Symbol(Symbol.SYM_NIL));
			list.add(new Symbol(Symbol.SYM_NIL));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), true);
			
			assertTrue(created.getTyp() == TSExpression.LAMBDA);
			assertTrue(created instanceof LambdaForm);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
	}
	
	@Test
	public void testCreateNoCheck()
	{
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("lambda"));
			list.add(new Symbol(Symbol.SYM_NIL));
			list.add(new Symbol(Symbol.SYM_NIL));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), false);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
		}
		
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defpackage"));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), false);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("in-package"));
			list.add(new Symbol("package"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), false);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defmacro"));
			list.add(new Symbol("macro-name"));
			list.add(new Symbol(Symbol.SYM_NIL));
			list.add(new Symbol(Symbol.SYM_NIL));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), false);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new Symbol("defun"));
			list.add(new Symbol("fn"));
			list.add(new Symbol(Symbol.SYM_NIL));
			list.add(new Symbol(Symbol.SYM_NIL));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), false);
			
			assertTrue(created.getTyp() == TSExpression.FORM);
			assertTrue(created instanceof Form);
		}
		//sexp
		{
			List<SExpression> list = new ArrayList<SExpression>();
			list.add(new StringSymbol("\"string\""));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0,10), false);
			
			assertTrue(created.getTyp() == TSExpression.SEXPRESSION);
			assertTrue(created.getPosition().equals(new Position(0,10)));
		}
		{
			List<SExpression> list = new ArrayList<SExpression>();
			List<SExpression> formList = new ArrayList<SExpression>();
			formList.add(new Symbol("mapcar"));
			formList.add(new ReaderSymbol("identity", new Position(0, 30), '\''));
			list.add(new Form(formList, new Position(0,60)));
			list.add(new Symbol("bla"));
			SExpression created = TypeFactory.createSExpression(list, new Position(0, 70), false);
			
			assertTrue(created.getTyp() == TSExpression.SEXPRESSION);
			assertTrue(created.getPosition().equals(new Position(0, 70)));
		}
	}
}
