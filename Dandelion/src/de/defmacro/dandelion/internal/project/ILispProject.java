package de.defmacro.dandelion.internal.project;

import org.eclipse.core.resources.IProject;

import de.defmacro.dandelion.internal.core.connection.IEnvironment;

public interface ILispProject 
{
	public IEnvironment getEnvironment();
	public void setEnvironment(IEnvironment environment);
	public IProject getProject();
}
