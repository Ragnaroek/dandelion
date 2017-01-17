[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?fid=g3qj00&url=https%3A%2F%2Fgithub.com%2FRagnaroek%2Fdandelion%2Fblob%2Fmaster%2FREADME.md)

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

# Screenshots

![Completion Proposal](https://a.fsdn.com/con/app/proj/dandelion-ecl/screenshots/133391.jpg/182/137/2)

![Editor and Code Outline](https://a.fsdn.com/con/app/proj/dandelion-ecl/screenshots/133381.jpg/182/137/2)

![Apropos View](https://a.fsdn.com/con/app/proj/dandelion-ecl/screenshots/133393.jpg/182/137/2)
