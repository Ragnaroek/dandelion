package de.defmacro.dandelion.internal.core.dom;

import org.junit.*;
import de.defmacro.dandelion.testutils.TestProject;
import static org.junit.Assert.*;

public class TestSexpModelEnclosingTopLevel {

	private static TestProject project;
	
	@BeforeClass
	public static void setUpClass() throws Exception {
		project = new TestProject();
	}

	@AfterClass
	public static void tearDownClass() throws Exception {
		project.dispose();
		project = null;
	}
	
	@Test
	public void testEmptyFile()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("     " +
				"            \n" +
				"     \n" +
				"     \n");
		
		assertNull(model.getEnclosingTopLevelForm(0));
		assertNull(model.getEnclosingTopLevelForm(10));
		assertNull(model.getEnclosingTopLevelForm(model.getDocument().getLength()));
	}
	
	@Test
	public void testIncompleteSExpression()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("\n\n\t(defun function \n" +
				"nil\n" +
				";hier fehlt die schliessende Klammer \n");
		
		assertNull(model.getEnclosingTopLevelForm(0));
		assertNull(model.getEnclosingTopLevelForm(10));
		assertNull(model.getEnclosingTopLevelForm(model.getDocument().getLength()));
	}
	
	@Test
	public void testGluedSExpression()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(Defvar *knotentypliste*)(defvar *kantenrelationen*)");
		
		{
			Form form = (Form)model.getEnclosingTopLevelForm(0);
			assertEquals("*knotentypliste*",  ((Symbol)form.getChild(1)).getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(25);
			assertEquals("*knotentypliste*",  ((Symbol)form.getChild(1)).getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(26);
			assertEquals("*kantenrelationen*",  ((Symbol)form.getChild(1)).getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(model.getDocument().getLength());
			assertEquals("*kantenrelationen*",  ((Symbol)form.getChild(1)).getSymbolName());
		}
	}
	
	@Test
	public void testQuotedSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("'x");
		{
			Form form = (Form)model.getEnclosingTopLevelForm(0);
			assertEquals("quote", form.getFunctionSymbol().getSymbolName());
			assertEquals("x", ((Symbol)form.getChild(1)).getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(1);
			assertEquals("quote", form.getFunctionSymbol().getSymbolName());
			assertEquals("x", ((Symbol)form.getChild(1)).getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(2);
			assertEquals("quote", form.getFunctionSymbol().getSymbolName());
			assertEquals("x", ((Symbol)form.getChild(1)).getSymbolName());
		}
	}
	
	@Test
	public void testSymbol()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("top-level-symbol");
		{
			Symbol sym = (Symbol)model.getEnclosingTopLevelForm(0);
			assertEquals("top-level-symbol", sym.getSymbolName());
		}
		{
			Symbol sym = (Symbol)model.getEnclosingTopLevelForm(3);
			assertEquals("top-level-symbol", sym.getSymbolName());
		}
		{
			Symbol sym = (Symbol)model.getEnclosingTopLevelForm(model.getDocument().getLength());
			assertEquals("top-level-symbol", sym.getSymbolName());
		}
	}
	
	@Test
	public void testNested()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("(let (let1 (let2 (let3 (let4) " +
				")\n" +
				")\n" +
				")\n" +
				")");
		{
			Form form = (Form)model.getEnclosingTopLevelForm(0);
			assertEquals("let", form.getFunctionSymbol().getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(5);
			assertEquals("let", form.getFunctionSymbol().getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(6);
			assertEquals("let", form.getFunctionSymbol().getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(24);
			assertEquals("let", form.getFunctionSymbol().getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(32);
			assertEquals("let", form.getFunctionSymbol().getSymbolName());
		}
		{
			Form form = (Form)model.getEnclosingTopLevelForm(model.getDocument().getLength());
			assertEquals("let", form.getFunctionSymbol().getSymbolName());
		}
	}
}
