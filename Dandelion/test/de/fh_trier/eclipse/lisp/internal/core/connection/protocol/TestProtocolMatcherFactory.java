package de.fh_trier.eclipse.lisp.internal.core.connection.protocol;

import java.util.regex.*;

import org.junit.*;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

public class TestProtocolMatcherFactory
{
	@SuppressWarnings("UwF")
	private IProtocolPatternFactory fFactory;
	
	@Before
	public void setUp()
	{
		fFactory = new ProtocolPatternFactory();
	}	
	
	@After
	public void tearDown()
	{
		fFactory = null;
	}
	
	
	@Test
	public void testSuccessPattern() 
	throws Exception 
	{
		Pattern p;
		p = fFactory.getSuccessPattern();
		assertTrue(p.matcher("OK").matches());
		
		p = fFactory.getSuccessPattern();
		assertTrue(p.matcher("ok").matches());
		
		p = fFactory.getSuccessPattern();
		assertFalse(p.matcher("OK ").matches());
		
		p = fFactory.getSuccessPattern();
		assertFalse(p.matcher("ERROR").matches());
		
		p = fFactory.getSuccessPattern();
		assertFalse(p.matcher("ok package-sym cmVzdWx0LXN5bQ==").matches());
		
		p = fFactory.getSuccessPattern();
		assertFalse(p.matcher("OKMiTmehr").matches());
		
	}
	
	@Test
	public void testEvalResultPattern() 
	throws Exception 
	{
		Pattern p;
		
		//OK Result
		//KHJlc3VsdC1mb3JtKQ== == (result-form)
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("OK :new-package KHJlc3VsdC1mb3JtKQ==").matches());
		
		//ein Ergebnis
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("ok package-sym cmVzdWx0LXN5bQ==").matches());
		
		//drei Eregbnisse
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("OK :my-package KHNldGYgeCAneCk= KHNldGYgeCAnsdf= skdrCAneCk=").matches());
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("OK illegal(").matches());
		
		//keine Form als Ergebnis
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("OK new-package").matches());
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("ERROR beschreibung").matches()); //nur error nicht erlaubt
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("ERROR").matches()); //nur error nicht erlaubt
		
		//*****************Eval-Error Result
		
		//kein Restart
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("EVAL-ERROR RmVobGVybWVsZHVuZw==").matches());
		
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("EVAL-ERROR b").matches());
		
		//ein Restart
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("EVAL-ERROR RmVobGVybWVsZHVuZw== sym1 ADdfla=").matches());
		
		//drei Restarts
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("eval-error RmVobGVybWVsZHVuZw== restartX sdj2356+/= restartY Adjkr0145skdjk+== restartY Adjkr0145skdjk+==").matches());
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("Illegal response").matches());
		
		//ungueltiges base64
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("EVAL-ERROR sdff&$%§&§sdff").matches());
		
		//illegals base64
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("eval-error restartX sjkldf=_").matches());
		
		//ein restart ohne beschreibung
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("EVAL-error RmVobGVybWVsZHVuZw== restartX").matches());
		
		//zweiter restart ohne beschreibung
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("EVAL-error RmVobGVybWVsZHVuZw== restartX sdkfjf restartY").matches());
		
		//illegales blank am ende
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("EVAL-ERROR RmVobGVybWVsZHVuZw== sym1 ADdfla= ").matches());
		
		
		//*****************read-Error Result
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("READ-ERROR RmVobGVybWVsZHVuZw==").matches());
		p = fFactory.getEvalResultPattern();
		assertTrue(p.matcher("READ-ERROR a").matches());
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("READ-ERROR ").matches());
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("READ-ERROR").matches());

		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("READ-ERROR RmVobGVybWVsZHVuZw== ").matches()); //leerzeichen zuviel
		
		p = fFactory.getEvalResultPattern();
		assertFalse(p.matcher("READ-ERROR RmVobGVybWVsZHVuZw== skjdfjj").matches()); //zweimal reason
	}
	
	@Test
	public void testPackageListInitPattern()
	{
		Pattern p;
		p = fFactory.getPackageListInitPattern();
		
		assertTrue(p.matcher("PACKAGE-LIST 0").matches());
		assertTrue(p.matcher("PACKAGE-LIST 5417").matches());
		
		assertFalse(p.matcher("").matches());
		assertFalse(p.matcher("PACKAGE").matches());
		assertFalse(p.matcher("PACKAGE-LIST -1").matches());
		assertFalse(p.matcher("PACKAGE-LIST NOT_A_NUMBER").matches());
		assertFalse(p.matcher("PACKAGE-LIST 5 ").matches());
		assertFalse(p.matcher("PACKAGE-LIST").matches());
		assertFalse(p.matcher("PACKAGE-LIST 0 4").matches());
	}
	
	@Test
	public void testPackageListElementPattern()
	{
		Pattern p;
		p = fFactory.getPackageListElementPattern();
		
		assertTrue(p.matcher("abcdefghijklmno+pqr/stuveABCEDEFDK==").matches());
		assertTrue(p.matcher("a").matches());
		assertTrue(p.matcher("sdfsfdfkj&$%").matches());
		
		assertFalse(p.matcher("").matches());
		assertFalse(p.matcher("ajkdfj kdfj").matches());
	}
	
	@Test
	public void testFunctionListInitPattern()
	{
		Pattern p;
		p = fFactory.getFunctionListInitPattern();
		
		assertTrue(p.matcher("FUNCTION-LIST 0").matches());
		assertTrue(p.matcher("FUNCTION-LIST 5471").matches());
		assertTrue(p.matcher("ERROR base64").matches());
		assertTrue(p.matcher("ERROR a").matches());
		
		assertFalse(p.matcher("").matches());
		assertFalse(p.matcher("FUNCTION-LIST").matches());
		assertFalse(p.matcher("FUNCTION-LIST -1").matches());
		assertFalse(p.matcher("FUNCTION-LIST NOT_A_NUMBER").matches());
		assertFalse(p.matcher("FUNCTION-LIST 5 ").matches());
		assertFalse(p.matcher("FUNCTION-LIST").matches());
		assertFalse(p.matcher("FUNCTION-LIST 0 4").matches());
		assertFalse(p.matcher("ERROR").matches());
		assertFalse(p.matcher("ERROR ").matches());
	}
	
	@Test
	public void testFunctionListElementPattern()
	{
		Pattern p;
		p = fFactory.getFunctionListElementPattern();
		
		assertTrue(p.matcher("MAPCAR buerzadfh").matches());
		assertTrue(p.matcher("a u37== NIL").matches());
		assertTrue(p.matcher("LAMBDA TklM").matches());
		assertTrue(p.matcher("LAMBDA jdf== a b c").matches());
		assertTrue(p.matcher("DEFUN TklM &optional z u test &rest rest").matches());
		
		assertFalse(p.matcher("").matches());
		assertFalse(p.matcher("nur-symbol").matches());
		assertFalse(p.matcher("kein-base64 $&§ a").matches());
		assertFalse(p.matcher("leerzeichen zuviel doc ").matches());
		assertFalse(p.matcher("leerzeichen doc a b c d efgh ").matches());
		
	}
	
	@Test
	public void testWordSplitPattern() 
	throws Exception 
	{
		Pattern p;
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher(" ").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher("  ").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher(" \t").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher(" \n").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher("\t").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher("\t\t").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher("\n").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher("\n\t").matches());
		
		p = fFactory.getWordSplitPattern();
		assertTrue(p.matcher("\t\n").matches());
		
		//kein match
		
		p = fFactory.getWordSplitPattern();
		assertFalse(p.matcher("").matches());
		
		p = fFactory.getWordSplitPattern();
		assertFalse(p.matcher("keinLeerzeichen").matches());
		
		p = fFactory.getWordSplitPattern();
		assertFalse(p.matcher("sdf\t").matches());
		
		p = fFactory.getWordSplitPattern();
		assertFalse(p.matcher(" sdf").matches());
		
		p = fFactory.getWordSplitPattern();
		assertFalse(p.matcher("\nsdf").matches());
	}
	
	@Test
	public void testWordSplit() 
	throws Exception 
	{
		Pattern p = fFactory.getWordSplitPattern();
		
		{
			String[] split = p.split("abc");
			assertArrayEquals(new String[] {"abc"}, split);
		}
		
		{
			String[] split = p.split("abc   ");
			assertArrayEquals(new String[] {"abc"}, split);
		}
		
		{
			//trailing space wird nicht gesplitet
			String[] split = p.split("abc def \n");
			assertArrayEquals(new String[] {"abc", "def"}, split);
		}
		
		{
			String[] split = p.split("a b c");
			assertArrayEquals(new String[] {"a", "b", "c"}, split);
		}
		
		{
			String[] split = p.split(" a b c");
			assertArrayEquals(new String[] {"", "a", "b", "c"}, split);
		}
		
		{
			String[] split = p.split("a\tb\nc");
			assertArrayEquals(new String[] {"a", "b", "c"}, split);
		}
		
		{
			String[] split = p.split("a  \t    b\n\n\t\n     c");
			assertArrayEquals(new String[] {"a", "b", "c"}, split);
		}
		
		{
			String[] split = p.split("ERROR :paket asdf");
			assertArrayEquals(new String[] {"ERROR", ":paket", "asdf"}, split);
		}
		
		{
			String[] split = p.split("1 2\n3\t\t4          5\n\n\n\t\t\t  \t\t\n   6");
			assertArrayEquals(new String[] {"1", "2", "3", "4", "5", "6"}, split);
		}
		
	}
}
