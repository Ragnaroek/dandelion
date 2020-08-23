package de.defmacro.dandelion.internal.core.dom;


import org.eclipse.jface.text.Position;
import org.junit.*;
import static org.junit.Assert.*;

import de.defmacro.dandelion.testutils.TestProject;

public class TestSexpModelContainsError 
{
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
	public void testNoError()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("sym\n" +
				"(defun x nil nil)\n" +
				"bla\n" +
				"(setf z 'y)\n"
				);
		assertFalse(model.containsError(new Position(0, model.getDocument().getLength())));
		assertFalse(model.containsError(new Position(5, model.getDocument().getLength() - 10)));
		assertFalse(model.containsError(new Position(10, model.getDocument().getLength() - 25)));
		assertFalse(model.containsError(new Position(15, 1)));
	}
	
	@Test
	public void testWithError()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("sym\n" + //0-4
				"(defun \"string\" nil nil)\n");
		//       |5   |10
		
		assertTrue(model.containsError(new Position(0, model.getDocument().getLength())));
		assertFalse(model.containsError(new Position(0, 4)));
		assertTrue(model.containsError(new Position(5, model.getDocument().getLength()-5)));
		assertTrue(model.containsError(new Position(10, model.getDocument().getLength() - 15)));
		assertFalse(model.containsError(new Position(model.getDocument().getLength() - 25, 0)));
	}
	
}
