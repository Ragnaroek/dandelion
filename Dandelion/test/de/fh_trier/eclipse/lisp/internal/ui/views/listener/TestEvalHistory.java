package de.fh_trier.eclipse.lisp.internal.ui.views.listener;

import org.junit.*;
import de.fh_trier.eclipse.lisp.internal.ui.views.listener.EvalHistory.HistoryEntry;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

public class TestEvalHistory
{
	@SuppressWarnings("UwF")
	private IEvalHistory history;
	
	@Before
	public void setUp() 
	throws Exception 
	{
		history = new EvalHistory(10);
	}

	@After
	public void tearDown() 
	throws Exception 
	{
		history = null;
	}

	@Test
	public void testCycledAdd()
	{
		for(int i=0; i < 11; i++) { //11 Element einfuegen, damit soll der erste eintrag geloescht werden
			history.addEntry(""+i);
		}
		
		assertEquals(10, history.getSize());
		
		for(int i=0; i < 10; i++) {
			assertEquals(""+(i+1), history.getForm(i));
		}
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalSize()
	{
		new EvalHistory(0);
	}
	
	@Test
	public void testIterator()
	{
		for(int i=0;i<5;i++) {
			history.addEntry(""+i,i);
		}
		
		int cnt = 0;
		for( HistoryEntry entry : history ) {
			assertEquals(""+cnt, entry.getForm());
			assertEquals((long)cnt, entry.getTimestamp());
			cnt++;
		}
	}
	
	@Test
	public void testToArray()
	{
		for(int i=0;i<5;i++) {
			history.addEntry(""+i,i);
		}
		
		HistoryEntry[] entry = history.toArray();
		for(int i=0;i<5;i++) {
			assertEquals(""+i, entry[i].getForm());
			assertEquals(""+i, entry[i].getShortedForm(10, true));
		}
	}
	
	@Test
	public void testShortedForm()
	{
		String form = "(defun my-function () \n (let (dkd kdkd kdkd kdkdk kd)))\n";
		String form2 = "(do-this)";
		String form3 = "nil";
		
		history.addEntry(form);
		history.addEntry(form2);
		history.addEntry(form3);
		assertEquals(form.subSequence(0, 17)+"...", history.getShortedForm(0, 20, false));
		assertEquals("...", history.getShortedForm(0, 3, false));
		assertEquals(form2, history.getShortedForm(1, 20, false));
		assertEquals(form3, history.getShortedForm(2, 3, false));
	}
	
	public void testShortedFormWithNewlineReplaced()
	{
		String form = "(defun my-function ()\n(let (dkd kdkd kdkd kdkdk kd)))\n";
		String expected = "(defun my-function ()(let (dkd kdkd kdkd kdkdk kd)))";
		
		history.addEntry(form);
		assertEquals(expected.subSequence(0, 17)+"...", history.getShortedForm(0, 20, true));
		assertEquals(expected, history.getShortedForm(0, expected.length(), true));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalMaxChars()
	{
		history.addEntry("");
		history.getShortedForm(0, 2, true);
	}
}
