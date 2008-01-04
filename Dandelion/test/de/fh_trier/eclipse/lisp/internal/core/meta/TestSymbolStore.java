package de.fh_trier.eclipse.lisp.internal.core.meta;


import java.util.*;
import org.junit.*;

import edu.umd.cs.findbugs.annotations.SuppressWarnings;
import static org.junit.Assert.*;

public class TestSymbolStore 
{
	@SuppressWarnings("UwF")
	private ISymbolStore fStore;
	
	@Before
	public void setUp() 
	throws Exception 
	{
		fStore = new SymbolStore();
		fStore.addPackage("CL-USER");
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL-USER", "SYM2", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL-USER", "D", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL-USER", "DE", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION));
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO));
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO));
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL-USER", "D", TMetaType.MACRO));
		
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL", "SYM1", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL", "SYM2", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL", "D", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL", "DE", TMetaType.FUNCTION));
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION));
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO));
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO));
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL", "D", TMetaType.MACRO));
	}

	@After
	public void tearDown() 
	throws Exception 
	{
		fStore = null;
	}

	private SortedSet<IMetaSymbol> getWholeStore()
	{
		SortedSet<IMetaSymbol> result = new TreeSet<IMetaSymbol>();
		
		result.addAll(fStore.getFunctionSymbols("CL-USER"));
		result.addAll(fStore.getFunctionSymbols("CL"));
		result.addAll(fStore.getMacroSymbols("CL-USER"));
		result.addAll(fStore.getMacroSymbols("CL"));
		
		result.add(new SymbolMetaSymbol("CL-USER", "CL-USER", TMetaType.PACKAGE));
		result.add(new SymbolMetaSymbol("CL", "CL", TMetaType.PACKAGE));
		
		return result;
	}
	
	@Test
	public void testInternSymbolTwice()
	{
		assertEquals(3, fStore.getMacroSymbols("CL").size());
		fStore.internMacroSymbol(new FunctionMetaSymbol("CL", "D", TMetaType.MACRO));
		assertEquals(3, fStore.getMacroSymbols("CL").size());
		
		assertEquals(5, fStore.getFunctionSymbols("CL-USER").size());
		fStore.internFunctionSymbol(new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION));
		assertEquals(5, fStore.getFunctionSymbols("CL-USER").size());
	}
	
	@Test
	public void testInternWrongType()
	{
		try {
			fStore.internFunctionSymbol(new FunctionMetaSymbol("my-package", "name", TMetaType.MACRO));
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) { /* nichts */ }
		
		try {
			fStore.internMacroSymbol(new FunctionMetaSymbol("my-package", "name", TMetaType.FUNCTION));
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) { /* nichts */ }
		
		List<IMetaSymbol> mixedList = new ArrayList<IMetaSymbol>(2);
		mixedList.add(new FunctionMetaSymbol("pack", "function", TMetaType.FUNCTION));
		mixedList.add(new FunctionMetaSymbol("pack", "macro", TMetaType.MACRO));
		
		try {
			fStore.internFunctionSymbol("my-package", mixedList);
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) { /* nichts */ }
		
		try {
			fStore.internMacroSymbol("my-package", mixedList);
			fail("IllegalArgumentException expected");
		} catch (IllegalArgumentException e) { /* nichts */ }
	}
	
	
	@Test
	public void testEmpty()
	{
		assertTrue(new SymbolStore().isEmpty());
		assertFalse(fStore.isEmpty());
	}
	
	@Test
	public void testInitialized()
	{
		assertFalse(new SymbolStore().isInitialized());
		assertFalse(fStore.isInitialized());
		fStore.setInitialized(true);
		assertTrue(fStore.isInitialized());
		fStore.setInitialized(false);
		assertFalse(fStore.isInitialized());
	}
	
	@Test
	public void testhasPackage()
	{
		assertTrue(fStore.hasPackage("CL"));
		assertTrue(fStore.hasPackage("CL-USER"));
		assertTrue(fStore.hasPackage("cl"));
		assertTrue(fStore.hasPackage("Cl-uSer"));
		assertFalse(fStore.hasPackage("my-package"));
	}
	
	@Test
	public void testGetSymbols()
	{
		assertEquals(5, fStore.getFunctionSymbols("CL-USER").size());
		assertEquals(3, fStore.getMacroSymbols("CL-USER").size());
		assertEquals(5, fStore.getFunctionSymbols("CL").size());
		assertEquals(3, fStore.getMacroSymbols("CL").size());
		
		assertTrue(fStore.getFunctionSymbols("NON_EXISTING_PACK").isEmpty());
		assertTrue(new SymbolStore().getFunctionSymbols("CL-USER").isEmpty());
		
	}
	
	@Test
	public void testPrefixQueryInWholeStorePrefixD()
	{
		IMetaSymbol[] result = fStore.prefixQuery("D").toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
	}
	
	@Test
	public void testPrefixQueryInWholeStorePrefixDEF()
	{
		IMetaSymbol[] result = fStore.prefixQuery("DEF").toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
	}
	
	@Test
	public void testPrefixQueryInWholeStoreUnknownPrefix()
	{
		assertTrue(fStore.prefixQuery("A").isEmpty());
	}
	
	@Test
	public void testPrefixQueryInPackagePrefixD()
	{
		IMetaSymbol[] result = fStore.prefixQuery("CL", "D").toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO)});
		
		result = fStore.prefixQuery("CL-USER", "D").toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
	}
	
	@Test
	public void testPrefixQueryInPackagePrefixDEF()
	{
		IMetaSymbol[] result = fStore.prefixQuery("CL", "DEF").toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO)});
		
		result = fStore.prefixQuery("CL-USER", "DEF").toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
	}
	
	@Test
	public void testPrefixQueryInPackageUnknownPrefix()
	{
		assertTrue(fStore.prefixQuery("CL", "A").isEmpty());
		assertTrue(fStore.prefixQuery("CL-USER", "A").isEmpty());
	}
	
	@Test
	public void testFullTextQueryNonExistingPackage()
	{
		assertTrue(fStore.fullTextQuery("UNKNOWN", Arrays.asList(new String[] {"D"}), TSearchMode.MODE_AND).isEmpty());
		assertTrue(fStore.fullTextQuery("UNKNOWN", Arrays.asList(new String[] {"D"}), TSearchMode.MODE_OR).isEmpty());
		assertTrue(fStore.fullTextQuery("UNKNOWN", Arrays.asList(new String[] {"D"}), TSearchMode.MODE_NOT).isEmpty());
	}
	
	@Test
	public void testFullTextQueryEmptyString()
	{
		assertEquals(getWholeStore(), fStore.fullTextQuery(Arrays.asList(new String[] {""}), TSearchMode.MODE_AND));
		assertEquals(getWholeStore(), fStore.fullTextQuery(Arrays.asList(new String[] {""}), TSearchMode.MODE_OR));
		assertTrue(fStore.fullTextQuery("UNKNOWN", Arrays.asList(new String[] {""}), TSearchMode.MODE_NOT).isEmpty());
	}
	
	@Test
	public void testFullTextQueryEmptyWordlist()
	{
		assertTrue(fStore.fullTextQuery(Arrays.asList(new String[] {}), TSearchMode.MODE_AND).isEmpty());
		assertTrue(fStore.fullTextQuery(Arrays.asList(new String[] {}), TSearchMode.MODE_OR).isEmpty());
		assertTrue(fStore.fullTextQuery(Arrays.asList(new String[] {}), TSearchMode.MODE_NOT).isEmpty());
	}
	
	@Test
	public void testFullTextQueryEmptyWordlistSinglePackage()
	{
		assertTrue(fStore.hasPackage("CL-USER"));
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {}), TSearchMode.MODE_AND).isEmpty());
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {}), TSearchMode.MODE_OR).isEmpty());
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {}), TSearchMode.MODE_NOT).isEmpty());
	}
	
	@Test
	public void testFullTextQueryAndOnePackageOneWord()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"MAC"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
		                     new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"EF"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"sym1"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION)});
		
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"UNKNOWN_SYMBOL"}), TSearchMode.MODE_AND).isEmpty());
	}
	
	@Test
	public void testFullTextQueryAndOnePackageMoreWords()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"mac", "MACRO", "M"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
		                     new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"EF", "de", "DEF"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"sym1", "SYM", "1"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION)});
		
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"UNKNOWN_SYMBOL", "DEFMACRO", "D"}), TSearchMode.MODE_AND).isEmpty());
	}
	
	@Test
	public void testFullTextQueryAndAllPackages()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"mac", "MACRO", "M"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
							 new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),
		                     new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),
		                     });
		
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"EF", "de", "DEF"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),
				});
		
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"sym1", "sym", "1"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "SYM1", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION)
				});
		
		//package werden auch gefunden
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"CL", "C"}), TSearchMode.MODE_AND).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new SymbolMetaSymbol("CL", "CL", TMetaType.PACKAGE),
				new SymbolMetaSymbol("CL-USER", "CL-USER", TMetaType.PACKAGE)});
		
		assertTrue(fStore.fullTextQuery(Arrays.asList(new String[] {"unknown"}), TSearchMode.MODE_AND).isEmpty());
	}
	
	@Test
	public void testFullTextQueryOrOnePackageOneWord()
	{
		//entspricht AND
		
		IMetaSymbol[] result;
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"MAC"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
		                     new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO)});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"EF"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"sym1"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION)});
		
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"UNKNOWN_SYMBOL"}), TSearchMode.MODE_OR).isEmpty());
	}
	
	@Test
	public void testFullTextQueryOrOnePackageMoreWords()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"mac", "MACRO", "SYM"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "SYM2", TMetaType.FUNCTION),});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"DEF", "M"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION),
			    new FunctionMetaSymbol("CL-USER", "SYM2", TMetaType.FUNCTION),
		});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"sym1", "SYM", "1"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "SYM2", TMetaType.FUNCTION)});
		
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"UNKNOWN_SYMBOL", "ALSO_UNKNOWN"}), TSearchMode.MODE_OR).isEmpty());
	}
	
	@Test
	public void testFullTextQueryOrAllPackages()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"MACRO", "M"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
					new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),
					new FunctionMetaSymbol("CL-USER", "DEFMACRO", TMetaType.MACRO),
					new FunctionMetaSymbol("CL", "SYM1", TMetaType.FUNCTION),
					new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION),
					new FunctionMetaSymbol("CL", "SYM2", TMetaType.FUNCTION),
					new FunctionMetaSymbol("CL-USER", "SYM2", TMetaType.FUNCTION),});
		
		assertEquals(getWholeStore(), fStore.fullTextQuery(Arrays.asList(new String[] {"MACRO", "D", "SYM", "CL"}), TSearchMode.MODE_OR));
		
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"CL"}), TSearchMode.MODE_OR).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new SymbolMetaSymbol("CL", "CL", TMetaType.PACKAGE),
				new SymbolMetaSymbol("CL-USER", "CL-USER", TMetaType.PACKAGE)});
		
		assertTrue(fStore.fullTextQuery(Arrays.asList(new String[] {"unknown"}), TSearchMode.MODE_OR).isEmpty());
	}
	
	@Test
	public void testFullTextQueryNotOnePackageOneWord()
	{	
		IMetaSymbol[] result;
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"D"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "SYM1", TMetaType.FUNCTION),
			    new FunctionMetaSymbol("CL-USER", "SYM2", TMetaType.FUNCTION),});
		
		result = fStore.fullTextQuery("CL", Arrays.asList(new String[] {"sYm"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),});
		
		assertEquals(getWholeStore(), fStore.fullTextQuery(Arrays.asList(new String[] {"UNKNOWN_SYM"}), TSearchMode.MODE_NOT));
	}
	
	@Test
	public void testFullTextQueryNotOnePackageMoreWords()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery("CL", Arrays.asList(new String[] {"SYM1", "D"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "SYM2", TMetaType.FUNCTION)});
		
		result = fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"DEF", "M"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DE", TMetaType.FUNCTION),});
		
		result = fStore.fullTextQuery("CL", Arrays.asList(new String[] {"sym1", "SYM", "1"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new FunctionMetaSymbol("CL", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DEFMACRO", TMetaType.MACRO),});
		
		assertTrue(fStore.fullTextQuery("CL-USER", Arrays.asList(new String[] {"D", "SYM", "MACro", "m"}), TSearchMode.MODE_NOT).isEmpty());
	}
	
	@Test
	public void testFullTextQueryNotAllPackages()
	{
		IMetaSymbol[] result;
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"MACRO", "M"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new SymbolMetaSymbol("CL", "CL", TMetaType.PACKAGE),
				new SymbolMetaSymbol("CL-USER", "CL-USER", TMetaType.PACKAGE),
				new FunctionMetaSymbol("CL", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "D", TMetaType.MACRO),
				new FunctionMetaSymbol("CL", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DE", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL", "DEF", TMetaType.MACRO),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.FUNCTION),
				new FunctionMetaSymbol("CL-USER", "DEF", TMetaType.MACRO),});
		
		assertTrue(fStore.fullTextQuery(Arrays.asList(new String[] {"MACRO", "D", "SYM", "CL"}), TSearchMode.MODE_NOT).isEmpty());
		
		assertEquals(getWholeStore(), fStore.fullTextQuery(Arrays.asList(new String[] {"unknown"}), TSearchMode.MODE_NOT));
		
		result = fStore.fullTextQuery(Arrays.asList(new String[] {"CL-USER", "M", "DE", "D"}), TSearchMode.MODE_NOT).toArray(new IMetaSymbol[1]);
		assertArrayEquals(result, new IMetaSymbol[] {
				new SymbolMetaSymbol("CL", "CL", TMetaType.PACKAGE),});
	}
}
