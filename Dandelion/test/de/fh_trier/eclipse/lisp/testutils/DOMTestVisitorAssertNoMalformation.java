package de.fh_trier.eclipse.lisp.testutils;

import de.fh_trier.eclipse.lisp.internal.core.dom.*;
import static org.junit.Assert.*;

public class DOMTestVisitorAssertNoMalformation 
implements ISexpDOMVisitor 
{
	private int fSeen;
	
	public void preVisit(ISexpModel model) {
		//no-op
	}

	public boolean visit(InpackageForm form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(DefpackageForm form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(LambdaForm form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(DefmacroForm form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(DefunForm form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(Symbol form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(Form form) {
		assertNoMalformations(form);
		return true;
	}

	public boolean visit(SExpression sexp) {
		assertNoMalformations(sexp);
		return true;
	}
	
	private void assertNoMalformations(final SExpression sexp)
	{
		fSeen++;
		assertFalse(sexp.hasMalformation());
	}
	
	public int seen()
	{
		return fSeen;
	}
}
