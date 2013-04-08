CLISP=clisp -ansi -norc
CMUCL_SAFE=/usr/lib/cmucl/lisp-safe.core
CMUCL_NORMAL=/usr/lib/cmucl/lisp-normal.core
CMUCL_SMALL=/usr/lib/cmucl/lisp-small.core
SBCL=sbcl --sysinit /dev/null --userinit /dev/null --noprogrammer
CMUCL=lisp -nositeinit -noinit -batch
GCL=gcl
ALL=clisp cmucl-normal cmucl-safe cmucl-small gcl sbcl
RM=rm -fr
MV=mv -f
MKDIR=mkdir -p

all: $(ALL)
	echo ok

.PHONY: $(ALL) clean

clean:
	$(RM) -rf $(ALL)

PRE_TEST=$(MKDIR) $@ ; \
	$(RM) *.erg $@/*;

POST_TEST=$(MV) *.erg $@ ; \
	ls -l $@ ; \
	cat $@/state-info.erg > $@-ERROR-IDS ; \
	grep -h '^Bugid: ' $@/*.erg  | cut -d ' ' -f 2 >> $@-ERROR-IDS ; \
	true;

clisp:
	$(PRE_TEST)
	time $(CLISP) -C -i tests.lisp < /dev/null && true
	$(RM) *.fas *.lib ;
	$(POST_TEST)

cmucl-normal:
	$(PRE_TEST)
	time $(CMUCL) -core $(CMUCL_NORMAL) -load tests.lisp
	$(RM) *.x86f ;
	$(POST_TEST)

cmucl-safe:
	$(PRE_TEST)
	time $(CMUCL) -core $(CMUCL_SAFE) -load tests.lisp
	$(RM) *.x86f ;
	$(POST_TEST)

cmucl-small:
	$(PRE_TEST)
	time $(CMUCL) -core $(CMUCL_SMALL) -load tests.lisp
	$(RM) *.x86f ;
	$(POST_TEST)

gcl:
	$(PRE_TEST)
	time $(GCL) -load tests.lisp
	$(RM) *.o ;
	$(POST_TEST)

sbcl:
	$(PRE_TEST)
	time $(SBCL) --eval '(load "tests")'
	$(RM) *.x86f *.fasl ;
	$(POST_TEST)

