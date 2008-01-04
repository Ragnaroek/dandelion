package de.fh_trier.eclipse.lisp.internal.core.dom;

import org.junit.*;

import de.fh_trier.eclipse.lisp.testutils.TestProject;
import static org.junit.Assert.*;
import static de.fh_trier.eclipse.lisp.testutils.AssertUtil.*;

public class TestSexpModelFindInpackage 
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
	public void testNoInpackage()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor("sym\n" +
				"(defun x nil nil)\n" +
				"bla\n" +
				"(setf z 'y)\n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertEquals("CL-USER", in.getPackage().getSymbolName());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertEquals("CL-USER", in.getPackage().getSymbolName());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertEquals("CL-USER", in.getPackage().getSymbolName());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertEquals("CL-USER", in.getPackage().getSymbolName());
		}
	}
	
	@Test
	public void testTopLevelOneInpackageFirst()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(in-package :my-package)\n\n\n\n\n\n\n  " +
				"(defun x nil nil)\n" +
				"bla\n" +
				"(setf z 'y)\n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals(":my-package", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals(":my-package", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals(":my-package", in.getPackage());
		}
	}
	
	@Test
	public void testTopLevelOneInpackageMiddel()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(defun d nil\n" +
				"   (setf a 't)\n)\n\n\n\n\n\n  " +
				"(defun x nil nil)\n" +
				"(in-package \"DE.FH-TRIER.EVALSERVER\") \n" +
				"bla\n" +
				"(setf z 'y)\n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{ //in-package selbst
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals("\"de.fh-trier.evalserver\"", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(4).getPosition());
			assertSymbolNameEquals("\"de.fh-trier.evalserver\"", in.getPackage());
		}
	}
	
	@Test
	public void testTopLevelOneInpackageEnd()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(defun d nil\n" +
				"   (setf a 't)\n)\n\n\n\n\n\n  " +
				"(defun x nil nil)\n" +
				" \n" +
				"bla\n" +
				"(setf z 'y)\n" +
				"(in-package \"DE.FH-TRIER.EVALSERVER\")\n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{ 
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(4).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
	}
	
	@Test
	public void testTopLevelInpackeNoNewline()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(in-package :pack1)(form)");
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
	}
	
	@Test
	public void testTopLevelTwoInpackagesStart()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(in-package :pack1)\n" +
				"(in-package pack2)\n" +
				"(defun d nil\n" +
				"   (setf a 't)\n)\n\n\n\n\n\n  " +
				"(defun x nil nil)\n" +
				" \n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
		{ 
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals("pack2", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals("pack2", in.getPackage());
		}
	}
	
	@Test
	public void testTopLevelTwoInpackagesMiddel()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(in-package :pack1)\n" +
				"(defun d nil\n" +
				"   (setf a 't)\n)\n\n\n\n\n\n  " +
				"(in-package pack2)\n" +
				"(defun x nil nil)\n" +
				" \n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
		{ 
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals("pack2", in.getPackage());
		}
	}
	
	@Test
	public void testTopLevelTwoInpackagesEnd()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(in-package :pack1)\n" +
				"(defun d nil\n" +
				"   (setf a 't)\n)\n\n\n\n\n\n  " +
				"(defun x nil nil)\n" +
				"(in-package pack2)\n" +
				" \n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
		{ 
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
		}
	}
	
	@Test
	public void testNestedInpackagesEnd()
	throws Exception
	{
		ISexpModel model = project.getSexpModelFor(
				"(in-package :pack1)\n" +
				"(defun d nil\n" +
				"   (setf a 't)\n\n\n\n\n\n\n" +
				"	(in-package :bad) \n" +
				"	(setf sym 'bad) )    \n" +
				"(defun x nil nil)\n" +
				"(in-package pack2)\n" +
				" \n"
				);
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(0).getPosition());
			assertSymbolNameEquals("CL-USER", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(1).getPosition());
			assertSymbolNameEquals(":pack1", in.getPackage());
			
			{
				DefunForm defun = (DefunForm)model.getTopLevelForms().get(1);
				InpackageForm inChild = model.getInpackage(defun.getBody().get(0).getPosition());
				assertSymbolNameEquals(":pack1", inChild.getPackage());
			}
			{ //in-package selbst
				DefunForm defun = (DefunForm)model.getTopLevelForms().get(1);
				InpackageForm inChild = model.getInpackage(defun.getBody().get(1).getPosition());
				assertSymbolNameEquals(":pack1", inChild.getPackage());
			}
			{ //in-package selbst
				DefunForm defun = (DefunForm)model.getTopLevelForms().get(1);
				InpackageForm inChild = model.getInpackage(defun.getBody().get(2).getPosition());
				assertSymbolNameEquals(":bad", inChild.getPackage());
			}
		}
		{ 
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(2).getPosition());
			assertSymbolNameEquals(":bad", in.getPackage());
		}
		{
			InpackageForm in = model.getInpackage(model.getTopLevelForms().get(3).getPosition());
			assertSymbolNameEquals(":bad", in.getPackage());
		}
	}
}
