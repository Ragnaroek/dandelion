uk.org.bew.comm-ext : An extension to the Network interface for LispWorks
Licenced under the LLGPL. See LICENSE.txt for details.

uk.org.bew.comm-ext:open-unix-stream

The interface to UNIX domain socket streams provided by this mirrors
exactly that provided by the LispWorks 'comm:open-tcp-stream'
function. Currently, 'hostname', 'buffered' and 'timeout' are ignored.
'service' is the path to the filesystem entry (eg, "/tmp/.X11-unix/X0"
to open a domain socket stream to the X server).

Installation :

Unfortunatly, LW for Linux seems to ignore the LD_LIBRARY_PATH
variable - I cannot work out quite how it searches for shared objects.
Because of this, the path to the shared object which provides the
foreign function interface bit is 'hard-coded' in the file
comm-ext.lisp as the logical pathname "COMM-EXT:liblwcomm-ext.so". You
may also be surprised that I use FLI:REGISTER-MODULE to load the ffi
bit - see the knowledge base article ID 17007 on www.xanalys.com for
details. So, Installation is as follows.

1/ Define a logical pathname translation for where you intend to place
the shared object
eg:
(setf (logical-pathname-translations "COMM-EXT")
      '(("**;*.*.*"      "/home/barry/cl-lib/**/*.*")))

2/ Make the ffi :
make clean; make; make install INSTALL=/home/barry/cl-lib

3/ Load defsys.lisp and run 'Compile System COMM-EXT'

A nice test to see if this has all worked is to now compile and load
my CLX port for lispworks. Then, load the file qix.lisp and try

(xlib::qix :host "/local")
or
(xlib::qix :host "")

Have fun!


