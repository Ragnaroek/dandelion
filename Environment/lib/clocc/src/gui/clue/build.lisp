(load "library:subsystems/clx-library.x86f")
(load "library:subsystems/defsystem-library.x86f")

(load "sysdef")

(mk:oos 'clue :compile)
(mk:oos 'clio :compile)
(mk:oos 'pictures :compile)

(load "target:tools/setup")
(load "make-subsystems")
(quit)
