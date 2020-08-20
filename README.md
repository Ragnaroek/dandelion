[![License](https://img.shields.io/badge/license-GPLv2-blue.svg)](https://github.com/Ragnaroek/rust-trellis/blob/master/LICENSE)

# Dandelion
Dandelion is a plugin for Eclipse that supports Lisp programming in the Eclipse platform. It comes with two ready to use Lisp environments: SBCL and CLISP. It is possible to connect other environments.

# Installation

There are two ways to install the plugin in Eclipse: Marketplace install and Update-Site installation.

## Eclipse Marketplace

Search for 'Dandelion' in the Eclipse Marketplace dialog (Help > Eclipse Marketplace...).
You have to choose an appropriate environment for your current platform.

## Update-Site Installation

Enter the URL to the Dandelion Update-Site in the Help > Install New Software... dialog:
https://ragnaroek.github.io/dandelion/
You have to choose an appropriate environment for your current platform.

## Setting the executable permissions on \*nix systems

The Common Lisp environments that come with the plugin are executables that cannot
be executed after installation. You probably will get an Exception if you try to
evaluate Lisp-Code in the editor. To fix this problem you have to give the environment
executables the execute permission with

`chmod +x <path/to/environment>`

You can extract the correct path to the environment from the resulting error message.

# Features

## Editor and Code Outline

Dandelion provides an editor with error highlighting and an
outline view to quickly navigate through big files:
![Editor and Code Outline](https://ragnaroek.github.io/dandelion/img/outline.png)

## Completion Proposal

The editor provides completions and documentation for functions/macros:
![Code Proposal](https://ragnaroek.github.io/dandelion/img/proposal.png)

## Apropos View

The apropos view allows you to search for functions/macros/packages and their documentation:
![Apropos](https://ragnaroek.github.io/dandelion/img/apropos.png)

## Out-of-the-Box Experience

Dandelion comes with ready to use Lisp environments. You do not have to install/configure anything. The plugin also provides a REPL for interactively trying out code:
![REPL](https://ragnaroek.github.io/dandelion/img/repl.png)

