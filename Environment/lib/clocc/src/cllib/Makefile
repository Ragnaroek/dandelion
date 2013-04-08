# CLLIB Makefile
# LISPTYPE should be set before using this file
#
# $Id: Makefile,v 2.31 2006/11/28 05:03:08 sds Exp $
# $Source: /cvsroot/clocc/clocc/src/cllib/Makefile,v $

TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := animals base card check clhs closio date datedl doall elisp fileio \
	fin geo gnuplot gq h2lisp html iter laser list log math miscprint \
	octave rpm server simple sorted stat string symb tilsla url withtype \
	inspect autoload htmlgen xml prompt matrix getopt tests cvs rng csv \
	base64 lift data ocaml
IMAGE_EXTRA = $(TOP)/clocc
BASE = $(IMAGE_EXTRA) base auto
NET = $(IMAGE_EXTRA) base url gq rpm
SCRIPT = $(IMAGE_EXTRA) base getopt
SYSTEM := cllib
ZIPEXTRA = ../defsystem-3.x/defsystem.lisp ../tools/metering/metering.lisp \
	$(TOP)/etc/GNU-GPL $(TOP)/bin/run-lisp
ZIPEXTRANOLINK = cllib/port/Makefile cllib/port/Changelog \
	cllib/port/port.system cllib/port/port.html cllib/port/*.lisp
ZIPEXTRALINK = ../port
DOCFILES = $(SYSTEM).html entities.xml
include $(TOP)/clocc.mk

ifneq ($(DUMPEXT),)
image:		image$(DUMPEXT)
image-net:	image-net$(DUMPEXT)
image-script:	image-script$(DUMPEXT)
endif

image$(DUMPEXT):	$(addsuffix .$(FASLEXT),$(BASE))
	$(RUNLISP) $(patsubst %,-i %,$^) -d image

image-net$(DUMPEXT):	$(addsuffix .$(FASLEXT),$(NET))
	$(RUNLISP) $(patsubst %,-i %,$^) -d image-net

cllib-net.exe:	$(addsuffix .$(FASLEXT),$(NET))
	$(CLISP) -norc $(patsubst %,-i %,$^) \
		-x '(saveinitmem "$@" :executable t :norc t)'

cllib.exe:	$(addsuffix .$(FASLEXT),$(IMAGE_EXTRA) $(SOURCES))
	$(CLISP) -norc $(patsubst %,-i %,$^) \
		-x '(saveinitmem "$@" :executable t :norc t)'

image-script$(DUMPEXT):	$(addsuffix .$(FASLEXT),$(SCRIPT))
	$(RUNLISP) $(patsubst %,-i %,$^) -d image-script

auto.$(LISPEXT):	$(addsuffix .$(LISPEXT),$(SOURCES))
	touch $@
	$(RUNLISP) $(CLOCC_TOP) -x '(mk:load-system "cllib")' \
	     -x "(funcall (intern \"AUTOLOAD-GENERATE\" \"CLLIB\") \
			  '($(patsubst %,\"%\",$^)) \"$@\")"

auto.$(FASLEXT):	auto.$(LISPEXT)
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$(BASE)) -c $^
