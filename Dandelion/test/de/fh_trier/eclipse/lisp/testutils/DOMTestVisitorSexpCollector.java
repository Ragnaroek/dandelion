package de.fh_trier.eclipse.lisp.testutils;

import java.util.*;

import org.eclipse.jface.text.Position;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;

public class DOMTestVisitorSexpCollector 
implements ISexpDOMVisitor
{	
	public ISexpModel suppliedModel;
	public List<DefmacroForm> defmacros = new ArrayList<DefmacroForm>();
	public List<DefpackageForm> defpackages = new ArrayList<DefpackageForm>();
	public List<DefunForm> defuns = new ArrayList<DefunForm>();
	public List<InpackageForm> inpackages = new ArrayList<InpackageForm>();
	public List<LambdaForm> lambdas = new ArrayList<LambdaForm>();
	public List<Symbol> symbols = new ArrayList<Symbol>();
	public List<SExpression> sexps = new ArrayList<SExpression>();
	public List<Form> forms = new ArrayList<Form>();
	
	public void preVisit(ISexpModel model) 
	{
		this.suppliedModel = model;
	}

	public boolean visit(DefmacroForm form) 
	{
		defmacros.add(form);
		return true;
	}

	public boolean visit(DefpackageForm form) {
		defpackages.add(form);
		return true;
	}

	public boolean visit(DefunForm form) {
		defuns.add(form);
		return true;
	}

	public boolean visit(InpackageForm form) {
		inpackages.add(form);
		return true;
	}

	public boolean visit(LambdaForm form) {
		lambdas.add(form);
		return true;
	}

	public boolean visit(Form form) {
		forms.add(form);
		return true;
	}

	public boolean visit(SExpression sexp) {
		sexps.add(sexp);
		return true;
	}

	public boolean visit(Symbol form) {
		symbols.add(form);
		return true;
	}
	
	public Integer[] getStartOffsets(List<? extends SExpression> list)
	{
		Integer[] result = new Integer[list.size()];
		
		for(int i=0; i<list.size(); i++) {
			result[i] = list.get(i).getPosition().getOffset();
		}
		
		return result;
	}
	
	public Integer[] getEndOffsets(List<? extends SExpression> list) 
	{
		Integer[] result = new Integer[list.size()];
		
		for(int i=0; i<list.size(); i++) {
			Position position = list.get(i).getPosition();
			result[i] = position.getOffset()+position.getLength()-1;
		}
		
		return result;
	}
}
