package de.defmacro.dandelion.testutils;

import java.util.*;
import org.eclipse.jface.text.*;

import de.defmacro.dandelion.internal.ui.text.ISexpressionVisitor;

public class SExpressionTestVisitor
implements ISexpressionVisitor
{	
	public List<Integer> closeOffsets = new ArrayList<Integer>();
	public List<Integer> openOffsets = new ArrayList<Integer>();
	public List<Integer> closeOffsetsTL = new ArrayList<Integer>();
	public List<Integer> openOffsetsTL = new ArrayList<Integer>();
	public List<Integer> startSymbolOffsetsTL = new ArrayList<Integer>();
	public List<Integer> endSymbolOffsetTL = new ArrayList<Integer>();
	public List<Integer> quotes = new ArrayList<Integer>();
	public List<Integer> backquotes = new ArrayList<Integer>();
	public List<Integer> commas = new ArrayList<Integer>();
	public List<Integer> startSymbolOffsets = new ArrayList<Integer>();
	public List<Integer> endSymbolOffsets = new ArrayList<Integer>();
	public List<MalformationEntry> malformations = new ArrayList<MalformationEntry>();
	
	public static class MalformationEntry
	{
		public Position position;
		public int balance;
	}
	
	public void preVisit(IDocument document) {
		//nichts
	}
	
	public boolean visitParenthesisClose(int offset) {
		closeOffsets.add(offset);
		return false;
	}

	public boolean visitParenthesisOpen(int offset) {
		openOffsets.add(offset);
		return false;
	}

	public boolean visitTopLevelClose(int offset) {
		closeOffsetsTL.add(offset);	
		return false;
	}

	public boolean visitTopLevelOpen(int offset)
	{
		openOffsetsTL.add(offset);
		return false;
	}

	public boolean visitTopLevelSymbolStart(int offset) {
		startSymbolOffsetsTL.add(offset);
		return false;
	}

	public boolean visitTopLevelSymbolEnd(int offset) {
		endSymbolOffsetTL.add(offset);
		return false;
	}

	public boolean visitQuote(int offset) {
		quotes.add(offset);
		return false;
	}

	public boolean visitBackquote(int offset) {
		backquotes.add(offset);
		return false;
	}

	public boolean visitComma(int offset) {
		commas.add(offset);
		return false;
	}

	public boolean visitSymbolEnd(int offset) {
		endSymbolOffsets.add(offset);
		return false;
	}

	public boolean visitSymbolStart(int offset) {
		startSymbolOffsets.add(offset);
		return false;
	}

	public boolean parenthesisMalformation(int atOffset, int balance) {
		MalformationEntry entry = new MalformationEntry();
		entry.position = new Position(atOffset, 0);
		entry.balance = balance;
		malformations.add(entry);
		return false;
	}
}
