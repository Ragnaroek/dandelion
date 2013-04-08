all:
	mkdir clxman/docIndex/ || true
	lisp -noinit -load run.lisp -eval '(progn (run) (quit))'

clean:
	rm -f clxman/*.html *.x86f mrindex  clxman/clxman.css 2> /dev/null || true
