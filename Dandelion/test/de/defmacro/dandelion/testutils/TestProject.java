package de.defmacro.dandelion.testutils;

import java.io.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.*;
import org.eclipse.ui.part.FileEditorInput;

import de.defmacro.dandelion.internal.core.dom.*;
import de.defmacro.dandelion.internal.ui.LispPerspective;
import de.defmacro.dandelion.internal.ui.editor.*;
import de.defmacro.dandelion.internal.ui.views.listener.*;

public final class TestProject
{
	public IProject project;

	public TestProject() throws CoreException {
		IWorkspaceRoot root= ResourcesPlugin.getWorkspace().getRoot();
		project= root.getProject("Project-1");
		project.create(null);
		project.open(null);
		populateProject();
	}
	
	private void populateProject()
	throws CoreException
	{
		IFile file = getValidTestFile();
		file.create(contentValidFile(), true, null);
		
		file = getInvalidTestFile();
		file.create(contentInvalidFile(), true, null);
		
		file = getFormTestFile();
		file.create(contentFormTypesFile(), true, null);
		
		file = getMiscFormTestFile();
		file.create(contentMiscFormsFile(), true, null);
	}
	
	public IProject getProject() {
		return project;
	}

	public IFile getValidTestFile()
	{
		return project.getFile("testfile-Valid.lisp");
	}
	
	public IFile getInvalidTestFile()
	{
		return project.getFile("testfile-Invalid.lisp");
	}
	
	public IFile getFormTestFile()
	{
		return project.getFile("testfile-formtypes.lisp");
	}
	
	public IFile getMiscFormTestFile()
	{
		return project.getFile("testfile-miscForms.lisp");
	}
	
	private IFile getRandomContentTestFile()
	{
		return project.getFile(Long.toString(System.currentTimeMillis()) + ((long)(Math.random()*1000)) + ".lisp");
	}
	
	public IDocument getLispPartitionedTestDocument(final IFile file)
	throws Exception
	{
		IDocument doc;
		IEditorPart editor = openLispPerspective().openEditor(new FileEditorInput(file), ILispEditor.ID);
		
		LispEditor lispEditor = (LispEditor)editor;
		doc = lispEditor.getDocumentProvider().getDocument(lispEditor.getEditorInput());
		
		return doc;
	}
	
	public ILispEditor getEditorFor(final String content)
	throws Exception
	{
		IFile file = getRandomContentTestFile();
		while(file.exists()) {
			file = getRandomContentTestFile();
			//file.delete(true, null);
		}
		
		Assert.isTrue(!file.exists());
		file.create(new ByteArrayInputStream(content.getBytes()), true, null);
		IEditorPart editor = openLispPerspective().openEditor(new FileEditorInput(file), ILispEditor.ID);
		ILispEditor lispEditor = (ILispEditor)editor;
		lispEditor.setSExpressionModel(getSexpModelFor(lispEditor.getDocumentProvider().getDocument(editor.getEditorInput())));
		
		return lispEditor;
	}
	
	public IDocument getDocumentFor(final String content)
	throws Exception
	{
		ILispEditor editor = getEditorFor(content);
		return editor.getDocumentProvider().getDocument(editor.getEditorInput());
	}
	
	public ISexpModel getSexpModelFor(final String content)
	throws Exception
	{
		return getSexpModelFor(content, true);
	}
	
	public ISexpModel getSexpModelFor(final String content, final boolean semanticCheck)
	throws Exception
	{
		ISexpModel model = new SexpModel(getDocumentFor(content));
		model.setSemanticValidation(semanticCheck);
		model.createDOM();
		return model;
	}
	
	public ISexpModel getSexpModelFor(final IDocument document)
	{
		ISexpModel model = new SexpModel(document);
		model.setSemanticValidation(true);
		model.createDOM();
		return model;
	}
	
	public ListenerView getListenerView()
	throws Exception
	{
		return (ListenerView)openLispPerspective().showView(IListenerView.ID);
	}
	
	private IWorkbenchPage openLispPerspective()
	throws Exception
	{
		return PlatformUI.getWorkbench().showPerspective(LispPerspective.PERSPECTIVE_ID, PlatformUI.getWorkbench().getActiveWorkbenchWindow());
	}
	
	public void dispose() throws CoreException {
		PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeAllEditors(true);
		project.delete(true, true, null);
	}
	
	private InputStream contentValidFile()
	{
		return getClass().getResourceAsStream("/test/testdata/testfile-valid");
	}
	
	private InputStream contentInvalidFile()
	{
		return getClass().getResourceAsStream("/test/testdata/testfile-nestingErrors");
	}
	
	private InputStream contentFormTypesFile()
	{
		return getClass().getResourceAsStream("/test/testdata/testfile-formtypes");
	}
	
	private InputStream contentMiscFormsFile()
	{
		return getClass().getResourceAsStream("/test/testdata/testfile-miscForms");
	}
}
