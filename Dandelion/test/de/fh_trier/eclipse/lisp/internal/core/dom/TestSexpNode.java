package de.fh_trier.eclipse.lisp.internal.core.dom;

import java.util.*;

import org.junit.*;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

@SuppressWarnings("SIC")
public class TestSexpNode 
{
	@SuppressWarnings("UwF")
	private SExpression testNode;

	@Before
	public void setUp()
	{
		testNode = new SExpression()
		{
			@Override
			public void accept(ISexpDOMVisitor visitor) {
				//no-op
			}
		};
	}
	
	@After
	public void tearDown()
	{
		testNode = null;
	}
	
	@Test
	public void testAppendChild()
	{
		assertFalse(testNode.hasChildren());
		assertNull(testNode.getChildren());
		testNode.appendChild(new SExpression(null));
		assertTrue(testNode.hasChildren());
		assertNotNull(testNode.getChildren());
	}
	
	@Test
	public void testAppendChildOrder()
	{
		SExpression[] childs = new SExpression[5];
		for(int i=0;i<5;i++) {
			childs[i] = new SExpression(null);
			testNode.appendChild(childs[i]);
		}
		
		for(int i=0;i<5;i++) {
			assertTrue(testNode.getChild(i) == childs[i]);
		}
	}
	
	@Test
	public void testAppendOrderWith1000()
	{
		SExpression[] childs = new SExpression[1000];
		for(int i=0;i<1000;i++) {
			childs[i] = new SExpression(null);
			testNode.appendChild(childs[i]);
		}
		
		assertTrue(testNode.hasChildren());
		assertEquals(1000, testNode.getChildren().size());
		
		for(int i=0;i<1000;i++) {
			assertTrue(testNode.getChild(i) == childs[i]);
		}
	}
	
	@Test
	public void testSetsParent()
	{
		SExpression[] childs = new SExpression[5];
		for(int i=0;i<5;i++) {
			childs[i] = new SExpression(null);
			testNode.appendChild(childs[i]);
		}
		
		for(int i=0;i<5;i++) {
			assertTrue(testNode.getChild(i).getParent() == testNode);
		}
	}
	
	@Test
	public void testAppendChilds()
	{
		List<SExpression> childs = new ArrayList<SExpression>();
		
		for(int i=0;i<1000;i++) {
			childs.add(new SExpression(null));
		}
		testNode.appendChilds(childs);
		assertTrue(testNode.hasChildren());
		assertEquals(1000, testNode.getChildren().size());
		
		for(int i=0;i<1000;i++) {
			assertTrue(testNode.getChild(i) == childs.get(i));
		}
	}
}
