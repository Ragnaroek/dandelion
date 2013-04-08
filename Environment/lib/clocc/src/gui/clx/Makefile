FILES=attributes.lisp clx.lisp debug display.lisp gcontext.lisp keysyms.lisp manager.lisp test \
 buffer.lisp clx.system demo doc.lisp graphics.lisp macros.lisp package.lisp text.lisp \
 bufmac.lisp CVS depdefs.lisp docs image.lisp requests.lisp translate.lisp \
 cl-clx.system dependent.lisp fonts.lisp input.lisp resource.lisp

install:
	install --owner=root --group=root --mode=644 cl-clx.system $(DESTDIR)/usr/share/common-lisp/systems/
	cp -r $(FILES) $(DESTDIR)/usr/share/common-lisp/repositories/cl-clx/

