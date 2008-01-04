package de.fh_trier.eclipse.lisp.internal.core.dom.parse;

import static org.junit.Assert.*;

import java.util.*;

import org.eclipse.jface.text.Position;
import org.junit.*;

import de.fh_trier.eclipse.lisp.internal.core.dom.SExpression;
import edu.umd.cs.findbugs.annotations.SuppressWarnings;

public class TestParserInput 
{
	private SExpression inputSExpression;
	@SuppressWarnings("UwF")
	private IParserInput parserInput;
	private int expectedCount = 10;
	@SuppressWarnings("UwF")
	private List<SExpression> childs;
	
	@Before
	public void setUp() throws Exception 
	{
		childs = new ArrayList<SExpression>(10);
		for(int i=0; i<expectedCount; i++) {
			SExpression child = new SExpression();
			child.appendChild(new SExpression());
			childs.add(new SExpression());
		}
		inputSExpression = new SExpression(childs);
		parserInput = new ParserInput(inputSExpression);
	}

	@After
	public void tearDown() throws Exception {
		parserInput = null;
	}

	@Test
	public void testParserInputPositionListOfSExpression() {
		
		List<SExpression> childs = new ArrayList<SExpression>();
		for(int i=0; i<5;i++) {
			childs.add(new SExpression());
		}
		Position position = new Position(0, 50);
		IParserInput input = new ParserInput(position, childs);
		assertEquals(position, input.getInputSExpression().getPosition());
		assertEquals(5, input.getInputSExpression().getChildren().size());
		
		for(int i=0; i<5;i++) {
			assertTrue(childs.get(i) == input.getInputSExpression().getChild(i));
		}
	}

	@Test
	public void testParserInputPositionListOfSExpressionWithNullInput() {
		IParserInput input = new ParserInput(null, null);
		assertFalse(input.getInputSExpression().hasChildren());
		assertNull(input.getInputSExpression().getPosition());
	}
	
	@Test
	public void testParserInputSExpression() {
		List<SExpression> childs = new ArrayList<SExpression>();
		for(int i=0; i<12;i++) {
			childs.add(new SExpression());
		}
		Position position = new Position(42, 784);
		SExpression root = new SExpression(childs, position);
		IParserInput input = new ParserInput(root);
		
		assertEquals(position, input.getInputSExpression().getPosition());
		assertEquals(12, input.getInputSExpression().getChildren().size());
		
		for(int i=0; i<12;i++) {
			assertTrue(childs.get(i) == input.getInputSExpression().getChild(i));
		}
	}
	
	@Test(expected = NullPointerException.class)
	@SuppressWarnings("NP") //Test auf NPE
	public void testParserInputSExpressionWithNullInput() {
		new ParserInput(null);
	}

	@Test
	public void testGetInputSExpression() {
		assertTrue(parserInput.getInputSExpression() == inputSExpression);
	}

	@Test
	public void testHasNext() {
		for(int i=0;i<expectedCount;i++) {
			assertTrue(parserInput.hasNext());
			parserInput.next();
		}
		assertFalse(parserInput.hasNext());
	}
	
	@Test
	public void testHasNextEmpty() {
		IParserInput input = new ParserInput(null, null);
		assertFalse(input.hasNext());
		
		try {
			input.next();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
	}

	@Test
	public void testCurrent() {
		try {
			parserInput.current();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
		
		for(int i=0;i<expectedCount;i++) {
			parserInput.next();
			assertTrue(childs.get(i) == parserInput.current());
		}
	}
	
	@Test
	public void testCurrentEmpty() {
		IParserInput input = new ParserInput(null, null);
		try {
			input.current();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
	}

	@Test
	public void testNextEmpty() {
		IParserInput input = new ParserInput(null, null);
		
		try {
			input.next();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
	}

	@Test
	public void testPushBack() {
		try {
			parserInput.pushBack();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
		
		while(parserInput.hasNext()) {
			parserInput.next();
		}
		
		for(int i=0;i<expectedCount;i++) {
			parserInput.pushBack();
		}
		
		try {
			parserInput.pushBack();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
		
		try {
			parserInput.current();
			fail("IllegalStateException expected");
		} catch (IllegalStateException e) {
			//nichts
		}
		
		assertTrue(childs.get(0) == parserInput.next());
	}

	@Test(expected = IllegalStateException.class)
	public void testPushBackEmty() {
		new ParserInput(null, null).pushBack();
	}
	
	@Test(expected = UnsupportedOperationException.class)
	public void testRemove() {
		parserInput.remove();
	}

	@Test
	public void testSize() {
		assertEquals(expectedCount, parserInput.size());
	}
	
	@Test
	public void testSizeEmpty() {
		IParserInput input = new ParserInput(null, null);
		assertEquals(0, input.size());
	}
}
