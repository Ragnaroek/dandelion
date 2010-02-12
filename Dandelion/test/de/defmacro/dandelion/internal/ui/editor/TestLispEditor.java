package de.defmacro.dandelion.internal.ui.editor;

import java.util.List;

import org.eclipse.jface.text.IDocument;
import org.junit.*;

import de.defmacro.dandelion.internal.ui.text.StructureException;
import de.defmacro.dandelion.testutils.TestProject;

import static org.junit.Assert.*;

public class TestLispEditor 
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

	@Test
	public void testGetSourceSelectionWithoutInpackageWithoutErrorsOneFormSelected() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(defun my-function nil x)\n" +
				"(form2 mit args)\n \n");
		editor.selectAndReveal(0, 25);
	
		ILispSourceSelection selection = editor.getSourceSelection();
		assertFalse(selection.isEmpty());
		assertFalse(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(1, forms.size());
		
		assertEquals("CL-USER", forms.get(0).getPackage());
		assertEquals("(defun my-function nil x)", forms.get(0).getForm());
	}

	@Test
	public void testGetSourceSelectionWithoutInpackageWithoutErrorsMultipleFormsSelected() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(defun my-function nil x)\n" +
				"(form2 mit args)\n \n");
		//komplettes Dokument selektieren
		editor.selectAndReveal(0, editor.getDocumentProvider().getDocument(editor.getEditorInput()).getLength());
	
		ILispSourceSelection selection = editor.getSourceSelection();
		assertFalse(selection.isEmpty());
		assertFalse(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(2, forms.size());
		
		assertEquals("CL-USER", forms.get(0).getPackage());
		assertEquals("(defun my-function nil x)", forms.get(0).getForm());
		
		assertEquals("CL-USER", forms.get(1).getPackage());
		assertEquals("(form2 mit args)", forms.get(1).getForm());
	}
	
	@Test
	public void testGetSourceSelectionWithInpackageOneFormSelected() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(in-package :package-name)(defun my-function nil x)\n" +
		"(form2 mit args)\n \n");
		
		editor.selectAndReveal(26, 25);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals(":package-name", forms.get(0).getPackage());
			assertEquals("(defun my-function nil x)", forms.get(0).getForm());
		}
		
		editor.selectAndReveal(52, 17);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals(":package-name", forms.get(0).getPackage());
			assertEquals("(form2 mit args)", forms.get(0).getForm());
		}
	}
	
	@Test
	public void testGetSourceSelectionSymbolSelected() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("symbol\n" +
		"(form2 mit args)\n \n");
		
		editor.selectAndReveal(0, 6);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("symbol", forms.get(0).getForm());
		}
		
		editor.selectAndReveal(1, 4);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("ymbo", forms.get(0).getForm());
		}
	}
	
	//#####  Selektion Kommentar testen
	
	@Test
	public void testGetSourceSelectionInCommentSelected() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("symbol\n" +
			"#| (defun commented-function () \n()) |#");
		
		editor.selectAndReveal(10, 33);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("(defun commented-function () \n())", forms.get(0).getForm());
		}
	}
	
	@Test
	public void testGetSourceSelectionInCommentSelectionContainsStringAndChar() 
	throws Exception
	{
		//Klammern in String und char
		ILispEditor editor = project.getEditorFor("symbol\n" +
			"#| (defun commented-function () \n" +
			"\"string(\"\n" +
			"#\\(\n)" +
			" |#");
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		editor.selectAndReveal(10, doc.getLength()-2-10-1);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("(defun commented-function () \n" +
					"\"string(\"\n" +
					"#\\(\n)", forms.get(0).getForm());
		}
	}
	
	@Test
	public void testGetSourceSelectionInCommentCodeStartOffset() 
	throws Exception
	{
		//Der selektierte offset liegt im Code-Content-Type deshalb wird Kommentar uebersprungen
		ILispEditor editor = project.getEditorFor("symbol\n" +
			"#| (defun commented-function () \n" +
			"\"string(\"\n" +
			"\\#)\n)" +
			" |#");
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		
		editor.selectAndReveal(7, doc.getLength()-7);
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertTrue(selection.isEmpty());
			assertFalse(selection.hasErrors());
		}
	}
	
	@Test
	public void testGetSourceSelectionInCommentCodeStartCommentAlsoSelected() 
	throws Exception
	{
		//Der Blockkommentar wird uebersprungen
		ILispEditor editor = project.getEditorFor("symbol\n" +
			"#| (defun commented-function () \n" +
			"\"string(\"\n" +
			"\\#)\n)" +
			" |#");
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
	
		editor.selectAndReveal(0, doc.getLength());
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("symbol", forms.get(0).getForm());
		}
	}
	
	@Test
	public void testGetSourceSelectionInCommentSelectedWithPackage() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(in-package :pack)\n" +
			"#| (defun commented-function () \n()) |#");
		
		editor.selectAndReveal(22, 34); //+ 1 blank selektiert
		{
			ILispSourceSelection selection = editor.getSourceSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals(":pack", forms.get(0).getPackage());
			assertEquals("(defun commented-function () \n())", forms.get(0).getForm());
		}
	}
	
	@Test(expected = StructureException.class)
	public void testGetSourceSelectionInCommentSelectedWithErrors() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(in-package :pack)\n" +
			"#| (defun commented-function () \n()) |#");
		
		editor.selectAndReveal(22, 20); //defun nicht komplett selektiert
		editor.getSourceSelection();
	}
	
	//	##### ENDE Selektion Kommentar testen
	
	@Test
	public void testGetSourceSelectionWithInpackageMultipleFormsSelected() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(in-package :package-name)(defun my-function nil x)\n" +
		"(form2 mit args)\n \n");
		
		editor.selectAndReveal(26, 43);
		
		ILispSourceSelection selection = editor.getSourceSelection();
		assertFalse(selection.isEmpty());
		assertFalse(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(2, forms.size());

		assertEquals(":package-name", forms.get(0).getPackage());
		assertEquals("(defun my-function nil x)", forms.get(0).getForm());

		assertEquals(":package-name", forms.get(1).getPackage());
		assertEquals("(form2 mit args)", forms.get(1).getForm());
	}
	
	@Test
	public void testGetSourceSelectionWithSyntaxErrors() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(in-package :package-name)(defun my-function)");
		
		editor.selectAndReveal(26, 19);
		
		ILispSourceSelection selection = editor.getSourceSelection();
		assertFalse(selection.isEmpty());
		assertTrue(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(1, forms.size());

		assertEquals(":package-name", forms.get(0).getPackage());
		assertEquals("(defun my-function)", forms.get(0).getForm());
	}
	
	@Test(expected = StructureException.class)
	public void testGetSourceSelectionWithStructureErrors() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(in-package :package-name)(defun my-function)");
		editor.selectAndReveal(26, 18); //Klammer defun nicht mit selektiert
		editor.getSourceSelection();
	}
	
	@Test
	public void testGetSourceSelectionEmptySelection() 
	throws Exception
	{
		{
			ILispEditor editor = project.getEditorFor("                       ");
			editor.selectAndReveal(2, 5); //whitespace selektiert
			assertTrue(editor.getSourceSelection().isEmpty());
		}
		{ //kein Bereich selektiert
			ILispEditor editor = project.getEditorFor("  (defun my-function) ");
			editor.selectAndReveal(1, 0); //whitespace selektiert
			assertTrue(editor.getSourceSelection().isEmpty());
		}
	}
	
	@Test
	public void testGetFileSelectionWithoutInpackageWithoutErrors()
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(defun my-function nil x)\n" +
											"(form2 mit args)\n \n" +
											"symbol\n\n" +
											"(form3 mit noch mehr args) \n\n" +
											"#\\c my-symbol :my-key");
		ILispSourceSelection selection = editor.getFileSelection();
		assertFalse(selection.isEmpty());
		assertFalse(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(7, forms.size());
		
		assertEquals("CL-USER", forms.get(0).getPackage());
		assertEquals("(defun my-function nil x)", forms.get(0).getForm());
		
		assertEquals("CL-USER", forms.get(1).getPackage());
		assertEquals("(form2 mit args)", forms.get(1).getForm());
		
		assertEquals("CL-USER", forms.get(2).getPackage());
		assertEquals("symbol", forms.get(2).getForm());
		
		assertEquals("CL-USER", forms.get(3).getPackage());
		assertEquals("(form3 mit noch mehr args)", forms.get(3).getForm());
		
		assertEquals("CL-USER", forms.get(4).getPackage());
		assertEquals("#\\c", forms.get(4).getForm());
		
		assertEquals("CL-USER", forms.get(5).getPackage());
		assertEquals("my-symbol", forms.get(5).getForm());
		
		assertEquals("CL-USER", forms.get(6).getPackage());
		assertEquals(":my-key", forms.get(6).getForm());
	}
	
	@Test
	public void testGetFileSelectionWithInpackage()
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(defun fl nil nil)  \n" +
				"\t(in-package :de.fh-trier)\n \n" +
				"symbol\n\n" +
				"(in-package :ie.dkit) \n" +
				"(form3 mit noch mehr args) \n\n" +
				"     \n");
		ILispSourceSelection selection = editor.getFileSelection();
		assertFalse(selection.isEmpty());
		assertFalse(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(5, forms.size());
		
		assertEquals("CL-USER", forms.get(0).getPackage());
		assertEquals("(defun fl nil nil)", forms.get(0).getForm());
		
		assertEquals("CL-USER", forms.get(1).getPackage());
		assertEquals("(in-package :de.fh-trier)", forms.get(1).getForm());
		
		assertEquals(":de.fh-trier", forms.get(2).getPackage());
		assertEquals("symbol", forms.get(2).getForm());
		
		assertEquals(":de.fh-trier", forms.get(3).getPackage());
		assertEquals("(in-package :ie.dkit)", forms.get(3).getForm());
		
		assertEquals(":ie.dkit", forms.get(4).getPackage());
		assertEquals("(form3 mit noch mehr args)", forms.get(4).getForm());
	}
	
	@Test
	public void testGetFileSelectionWithSyntaxErrors()
	throws Exception
	{
		{ //Error
			ILispEditor editor = project.getEditorFor("(defun fl) ;fehlerhaftes defun  \n");
			ILispSourceSelection selection = editor.getFileSelection();
			assertFalse(selection.isEmpty());
			assertTrue(selection.hasErrors());
		}
		{ //Warnung, kein Fehler!
			ILispEditor editor = project.getEditorFor("(in-package \"bla\")");
			ILispSourceSelection selection = editor.getFileSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
		}
	}
	
	@Test(expected = StructureException.class)
	public void testGetFileSelectionWithStructureErrors()
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("\n\n\n(defun fl) \n" +
				"(in-package :bla ;schliessende Klammer fehlt  \t\n ;fehlerhaftes defun  \n");
		editor.getFileSelection();
	}
	
	@Test
	public void testGetFileSelectionEmptySelection()
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("#| Hier\n steht\n nur\n Kommentar\n |#" +
				";rest auskommentiert (in-package :bla");
		ILispSourceSelection selection = editor.getFileSelection();
		assertTrue(selection.isEmpty());
	}

	@Test
	public void testGetTopLevelFormSelectionWithoutInpackageWithoutErrors() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("\n\n\n(defun fl nil ()) \n" +
		"(mapcar #'append '(1 2 3)) \n" +
		"symbol");
		
		editor.selectAndReveal(6, 0);
		{
			ILispSourceSelection selection = editor.getTopLevelFormSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("(defun fl nil ())", forms.get(0).getForm());
		}
		
		editor.selectAndReveal(26, 5);
		{
		    ILispSourceSelection selection = editor.getTopLevelFormSelection();
		    assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());
			
			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("(mapcar #'append '(1 2 3))", forms.get(0).getForm());
		}
		
		editor.selectAndReveal(54, 0);
		{
		    ILispSourceSelection selection = editor.getTopLevelFormSelection();
		    assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());
			
			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("symbol", forms.get(0).getForm());
		}
	}

	@Test
	public void testGetTopLevelFormSelectionWithInpackage() 
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("\n\n\n(defun fl nil ()) \n" +
		"(mapcar #'append '(1 2 3)) \n" +
		"(in-package :my-package)\n" +
		"symbol");
		
		editor.selectAndReveal(6, 0);
		{
			ILispSourceSelection selection = editor.getTopLevelFormSelection();
			assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());

			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("(defun fl nil ())", forms.get(0).getForm());
		}
		
		editor.selectAndReveal(26, 5);
		{
		    ILispSourceSelection selection = editor.getTopLevelFormSelection();
		    assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());
			
			assertEquals("CL-USER", forms.get(0).getPackage());
			assertEquals("(mapcar #'append '(1 2 3))", forms.get(0).getForm());
		}
		
		editor.selectAndReveal(79, 0);
		{
		    ILispSourceSelection selection = editor.getTopLevelFormSelection();
		    assertFalse(selection.isEmpty());
			assertFalse(selection.hasErrors());
			List<PackageBoundForm> forms = selection.getForms();
			assertEquals(1, forms.size());
			
			assertEquals(":my-package", forms.get(0).getPackage());
			assertEquals("symbol", forms.get(0).getForm());
		}
	}
	
	@Test
	public void testGetTopLevelFormSelectionWithSyntaxErrors()
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(defun fl) \n");
		editor.selectAndReveal(6, 0);
		
		ILispSourceSelection selection = editor.getTopLevelFormSelection();
		assertFalse(selection.isEmpty());
		assertTrue(selection.hasErrors());
		List<PackageBoundForm> forms = selection.getForms();
		assertEquals(1, forms.size());

		assertEquals("CL-USER", forms.get(0).getPackage());
		assertEquals("(defun fl)", forms.get(0).getForm());
		
	}
	
	@Test
	public void testGetTopLevelFormSelectionWithStructureErrors()
	throws Exception
	{
		ILispEditor editor = project.getEditorFor("(defun fl");
		editor.selectAndReveal(6, 0);
		
		ILispSourceSelection selection = editor.getTopLevelFormSelection();
		assertTrue(selection.isEmpty());
	}
}
