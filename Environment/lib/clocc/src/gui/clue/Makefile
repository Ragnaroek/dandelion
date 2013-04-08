CLISP=clisp

install:
	cp -r pictures clue clio  \
	  $(DESTDIR)/usr/share/common-lisp/source/
	find $(DESTDIR)/usr/share/common-lisp/source/ -type d -name CVS -print0 | \
	   xargs -0 rm -rf 
	cp pictures.asd $(DESTDIR)/usr/share/common-lisp/source/pictures
	cp clio.asd $(DESTDIR)/usr/share/common-lisp/source/clio
	cp clue.asd $(DESTDIR)/usr/share/common-lisp/source/clue
	chown root.root $(DESTDIR)/usr/share/common-lisp/source/
	find $(DESTDIR)/usr/share/common-lisp/source/ -type f -print0 | \
	  xargs -0 chmod 0644 
	find $(DESTDIR)/usr/share/common-lisp/source/ -type d -print0 | \
	  xargs -0 chmod 0755 

clue-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-asd "clue")'

clio-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-asd "clio")'

clio-ex-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-asd "clio-examples")'

pictures-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-asd "pictures")'

clean-clisp:
	rm -fv `find . -name \*.fas -o -name \*.lib`
