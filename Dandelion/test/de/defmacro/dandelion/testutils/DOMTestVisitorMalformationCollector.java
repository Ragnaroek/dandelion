package de.defmacro.dandelion.testutils;

import java.util.*;

import de.defmacro.dandelion.internal.core.dom.*;

public class DOMTestVisitorMalformationCollector 
implements ISexpDOMVisitor 
{
	private Set<ISyntacticalMalformation> malformationSet =
		new TreeSet<ISyntacticalMalformation>();
	
	public void preVisit(ISexpModel model) {
		//no-op
	}

	public boolean visit(InpackageForm form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(DefpackageForm form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(LambdaForm form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(DefmacroForm form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(DefunForm form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(Symbol form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(Form form) {
		addMalformations(form);
		return true;
	}

	public boolean visit(SExpression sexp) {
		addMalformations(sexp);
		return true;
	}
	
	private void addMalformations(final SExpression sexp)
	{
		if(sexp.hasMalformation()) {
			malformationSet.addAll(sexp.getMalformations());
		}
	}
	
	public Set<ISyntacticalMalformation> getMalformations()
	{
		return malformationSet;
	}
}
