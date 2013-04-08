MINPACK = chkder.f \
	dogleg.f \
	dpmpar.f \
	enorm.f \
	fdjac1.f \
	fdjac2.f \
	hybrd.f \
	hybrd1.f \
	hybrj.f \
	hybrj1.f \
	lmder.f \
	lmder1.f \
	lmdif.f \
	lmdif1.f \
	lmpar.f \
	lmstr.f \
	lmstr1.f \
	qform.f \
	qrfac.f \
	qrsolv.f \
	r1mpyq.f \
	r1updt.f \
	rwupdt.f

tst-hybrd : tst-hybrd.o $(MINPACK:.f=.o)
	$(F77) -o $@ $^
tst-lmder : tst-lmder.o $(MINPACK:.f=.o)
	$(F77) -o $@ $^
tst-lmdif : tst-lmdif.o  $(MINPACK:.f=.o)
	$(F77) -o $@ $^

run-lmdif : tst-lmdif
	tst-lmdif < lmdif-input.dat

run-lmder : tst-lmder
	tst-lmder < lmdif-input.dat

run-hybrd : tst-hybrd
	tst-hybrd < lmdif-input.dat

clean :
	rm -f *.o tst-hybrd tst-lmder tst-lmdif
