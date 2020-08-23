package de.defmacro.dandelion.internal.core.dom;

import java.util.*;

import org.eclipse.jface.text.Position;
import org.junit.*;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

public class TestSExpressionPositionComparator 
{
	@SuppressWarnings("UwF")
	private List<SExpression> fTestList;
	
	@Before
	public void setUp()
	{
		fTestList = new ArrayList<SExpression>();
	}
	
	@After
	public void tearDown()
	{
		fTestList = null;
	}
	
	@Test
	public void testAllNullPositions()
	{
		SExpression s1 = new SExpression();
		SExpression s2 = new SExpression();
		SExpression s3 = new SExpression();
		SExpression s4 = new SExpression();
		
		fTestList.add(s1);
		fTestList.add(s2);
		fTestList.add(s3);
		fTestList.add(s4);
		Collections.sort(fTestList, SExpression.POSITION_COMPARATOR);
		
		assertTrue(fTestList.get(0) == s1);
		assertTrue(fTestList.get(1) == s2);
		assertTrue(fTestList.get(2) == s3);
		assertTrue(fTestList.get(3) == s4);	
	}
	
	@Test
	public void test1Null3NonNull()
	{
		SExpression s1 = new SExpression();
		SExpression s2 = new SExpression(null, new Position(1,1));
		SExpression s3 = new SExpression(null, new Position(512, 1));
		SExpression s4 = new SExpression(null, new Position(20, 347));

		fTestList.add(s1);
		fTestList.add(s2);
		fTestList.add(s3);
		fTestList.add(s4);
		Collections.sort(fTestList, SExpression.POSITION_COMPARATOR);

		assertTrue(fTestList.get(0) == s2);
		assertTrue(fTestList.get(1) == s4);
		assertTrue(fTestList.get(2) == s3);
		assertTrue(fTestList.get(3) == s1); //null am Schluss	
	}
	
	@Test
	public void test2Null2NonNull()
	{
		SExpression s1 = new SExpression();
		SExpression s2 = new SExpression(null, new Position(1045, 1475));
		SExpression s3 = new SExpression();
		SExpression s4 = new SExpression(null, new Position(20, 347));

		fTestList.add(s1);
		fTestList.add(s2);
		fTestList.add(s3);
		fTestList.add(s4);
		Collections.sort(fTestList, SExpression.POSITION_COMPARATOR);

		assertTrue(fTestList.get(0) == s4);
		assertTrue(fTestList.get(1) == s2);
		assertTrue(fTestList.get(2) == s1); //null Sortierung bleibt gleich
		assertTrue(fTestList.get(3) == s3);
	}
	
	@Test
	public void test3Null1NonNull()
	{
		SExpression s1 = new SExpression();
		SExpression s2 = new SExpression(null, new Position(1045, 1475));
		SExpression s3 = new SExpression();
		SExpression s4 = new SExpression();

		fTestList.add(s1);
		fTestList.add(s2);
		fTestList.add(s3);
		fTestList.add(s4);
		Collections.sort(fTestList, SExpression.POSITION_COMPARATOR);

		assertTrue(fTestList.get(0) == s2);
		assertTrue(fTestList.get(1) == s1);
		assertTrue(fTestList.get(2) == s3);
		assertTrue(fTestList.get(3) == s4); //null am Schluss	
	}
	
	@Test
	public void testAllNonNull()
	{
		SExpression s1 = new SExpression(null, new Position(1, 215));
		SExpression s2 = new SExpression(null, new Position(1045, 1475));
		SExpression s3 = new SExpression(null, new Position(5, 10)); //ueberlappend
		SExpression s4 = new SExpression(null, new Position(1045, 12)); //gleicher offset

		fTestList.add(s1);
		fTestList.add(s2);
		fTestList.add(s3);
		fTestList.add(s4);
		Collections.sort(fTestList, SExpression.POSITION_COMPARATOR);

		assertTrue(fTestList.get(0) == s1);
		assertTrue(fTestList.get(1) == s3);
		assertTrue(fTestList.get(2) == s2); //Sortierung gleicher offset bleibt gleich
		assertTrue(fTestList.get(3) == s4); 
	}
}
