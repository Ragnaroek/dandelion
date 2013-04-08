# -*- Makefile -*-
# Common Makefile rules.
# This file requires GNU Make
#
# The following variables must already have been set:
# CLOCC_DUMP - space-separated list of lisps for which we dump images
# LISPEXT    - the lisp file extension (usually "lisp")
# LISPTYPE   - your implementation type (acl5, acl43, clisp, cmucl, gcl)
# SOURCES    - the list of source files to be compiled
# SYSTEM     - the system name for defsystem (usually the dir name)
# TOP        - the path to the top-level CLOCC directory
#
# $Id: clocc.mk,v 1.21 2005/06/28 14:08:09 sds Exp $
# $Source: /cvsroot/clocc/clocc/clocc.mk,v $

ifndef CLOCC_MK

CLOCC_MK = true

RUNLISP := $(TOP)/bin/run-lisp
LISPFILE := $(TOP)/bin/lisp-file
FASLEXT := $(shell $(RUNLISP) -faslext)
DUMPEXT := $(shell $(RUNLISP) -dumpext)
DO_DUMP := $(filter $(LISPTYPE),$(CLOCC_DUMP))
FASLFILES = *.fas *.lib *.axpf *.x86f *.hpf *.sgif *.sparcf *.fasl \
	*.o *.data *.ufsl *.abcl
JUNK = core *.core *.mem *.dxl *.list *~ *.log
LISPFILES = $(addsuffix .$(LISPEXT),$(SOURCES))
DOCFILES += ChangeLog $(SYSTEM).list
MAKEFILES = Makefile $(SYSTEM).system
ZIPEXTRA += $(TOP)/clocc.mk $(TOP)/clocc.lisp
RM  = /bin/rm -f
LN  = /bin/ln
ZIP = zip -9uD

ifneq ($(DO_DUMP),)
CLOCC_TOP =  -I $(TOP)/clocc-top
else
CLOCC_TOP =  -i $(TOP)/clocc-top
endif

default: force
	@echo " * you must specify a target, such as..."
	@echo " + system - run mk:compile-file on SYSTEM ($(SYSTEM))"
	@echo " + all - compile all files in SOURCES ($(SOURCES)) one by one (this will work only if the files are independent)"
	@echo " + ChangeLog - create the ChangeLog file using rcs2log"
	@echo " + $(SYSTEM).list - the list of all functons and variables defined by this system"
	@echo " + $(SYSTEM)-image$(DUMPEXT) - the memory image with everything ($(SOURCES))"
	@echo " + TAGS - Emacs tags"
	@echo " + $(SYSTEM).zip - the archive of SOURCES, DOCFILES ($(DOCFILES)), MAKEFILES ($(MAKEFILES)) and ZIPEXTRA ($(ZIPEXTRA))"

system: $(SYSTEM).system
	$(RUNLISP) $(CLOCC_TOP) -i $^ \
		-x '(funcall (intern "COMPILE-SYSTEM" :mk) "$(SYSTEM)")'

all: $(addsuffix .$(FASLEXT),$(SOURCES))

ifneq ($(DUMPEXT),)
$(SYSTEM)-image: $(SYSTEM)-image$(DUMPEXT)
endif

$(SYSTEM)-image$(DUMPEXT): $(LISPFILES)
	$(RUNLISP) $(CLOCC_TOP) \
		-x '(funcall (intern "COMPILE-SYSTEM" :mk) "$(SYSTEM)")' \
		-d $(SYSTEM)-image

%.$(FASLEXT): %.$(LISPEXT)
	$(RUNLISP) $(patsubst %,-i %,$(filter-out $<,$^)) -c $<

ChangeLog: $(LISPFILES)
	rcs2log $^ > $@

TAGS:	 $(LISPFILES)
	etags $^

$(SYSTEM).list: TAGS
	sed -e 's?^/.*/??' -e 's/ *.*/ ...)/' -e 's/,[0-9]*$$//' TAGS > $@

$(SYSTEM).zip: $(DOCFILES) $(LISPFILES) $(MAKEFILES)
	@$(RM) $(SYSTEM);
	@$(LN) -s . $(SYSTEM);
	@$(LN) -s . extra;
	@$(LN) -s $(ZIPEXTRA) .;
	@test -z "$(ZIPEXTRALINK)" || $(LN) -s $(ZIPEXTRALINK) .;
	@echo ...updating zip file $@...;
	@$(ZIP) $@ $(patsubst %,$(SYSTEM)/%,$^) $(ZIPEXTRANOLINK) \
		$(patsubst %,$(SYSTEM)/extra/%,$(notdir $(ZIPEXTRA)));
	@$(RM) $(SYSTEM) extra $(notdir $(ZIPEXTRA)) $(notdir $(ZIPEXTRALINK));

clean-all: force
	$(RM) $(FASLFILES) TAGS $(JUNK)

clean:
	$(RM) *.$(FASLEXT) core
ifneq ($(DUMPEXT),)
	$(RM) *$(DUMPEXT)
endif

force:

endif				# CLOCC_MK
