ODEPACK = \
	adjlr.f \
	cdrv.f \
	cntnzu.f \
	daigbt.f \
	dainvg.f \
	dainvgs.f \
	datp.f \
	datv.f \
	daxpy.f \
	dbnorm.f \
	dcfode.f \
	dcopy.f \
	ddecbt.f \
	ddot.f \
	dewset.f \
	dfnorm.f \
	dgbfa.f \
	dgbsl.f \
	dgefa.f \
	dgesl.f \
	dhefa.f \
	dhels.f \
	dheqr.f \
	dhesl.f \
	dintdy.f \
	diprep.f \
	diprepi.f \
	dlhin.f \
	dlsoda.f \
	dlsodar.f \
	dlsode.f \
	dlsodes.f \
	dlsodi.f \
	dlsodis.f \
	dlsodkr.f \
	dlsodpk.f \
	dlsoibt.f \
	dmnorm.f \
	dnrm2.f \
	dorthog.f \
	dpcg.f \
	dpcgs.f \
	dpjibt.f \
	dpkset.f \
	dprep.f \
	dprepi.f \
	dprepj.f \
	dprepji.f \
	dprja.f \
	dprjis.f \
	dprjs.f \
	drchek.f \
	droots.f \
	dscal.f \
	dsetpk.f \
	dslsbt.f \
	dsolbt.f \
	dsolpk.f \
	dsolss.f \
	dsolsy.f \
	dspigmr.f \
	dspiom.f \
	dsrcar.f \
	dsrckr.f \
	dsrcma.f \
	dsrcms.f \
	dsrcom.f \
	dsrcpk.f \
	dstoda.f \
	dstode.f \
	dstodi.f \
	dstodpk.f \
	dstoka.f \
	dumach.f \
	dumsum.f \
	dusol.f \
	dvnorm.f \
	idamax.f \
	iumach.f \
	ixsav.f \
	jgroup.f \
	md.f \
	mdi.f \
	mdm.f \
	mdp.f \
	mdu.f \
	nnfc.f \
	nnsc.f \
	nntc.f \
	nroc.f \
	nsfc.f \
	odrv.f \
	sro.f \
	xerrwd.f \
	xsetf.f \
	xsetun.f

FFLAGS = -g
F77FLAGS = -g

libodepack.a : $(ODEPACK:.f=.o)
	ar -r libodepack.a $^
	ranlib libodepack.a

demo-lsode : opkdemo1.o libodepack.a
	$(F77) -o $@ $^

demo-lsoda : opkdemo3.o libodepack.a
	$(F77) -o $@ $^

demo-lsodar : opkdemo4.o libodepack.a
	$(F77) -o $@ $^

demo-lsodpk : opkdemo5.o libodepack.a
	$(F77) -o $@ $^

demo-lsodkr : opkdemo6.o libodepack.a
	$(F77) -o $@ $^

demo-lsodi : opkdemo7.o libodepack.a
	$(F77) -o $@ $^

demo-lsoibt : opkdemo8.o libodepack.a
	$(F77) -o $@ $^

demo-lsodis : opkdemo9.o libodepack.a
	$(F77) -o $@ $^
