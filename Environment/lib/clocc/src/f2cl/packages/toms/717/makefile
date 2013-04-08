.SUFFIXES: .f .o
FFLAGS = -g
F77 = g77
L =

.f.o:
	$(F77) -c $(FFLAGS) $*.f

example.out: example.c dglfgb.o dgletc.o dmdc.o d1mach.o
	gcc -g -c example.c
	$(F77) example.o dglfgb.o dgletc.o dmdc.o $L
#	a.out >$@

both: out sout

out: madsen.out madsenb.out pmain.out mnpex1.out mnpex1b.out \
	mnpex2.out mnpex2b.out rent1.out rent1b.out rent2.out rent2b.out

sout: smadsen.out smadsenb.out spmain.out smnpex1.out smnpex1b.out \
	smnpex2.out mnpex2b.out srent1.out srent1b.out srent2.out srent2b.out

dmdc.f: dmdc.f0
	true  # Obtain dmcd.f from dmdc.f0 by activating the statements
	false # appropriate to your machine

smdc.f: smdc.f0
	true  # Obtain dmcd.f from smdc.f0 by activating the statements
	false # appropriate to your machine

madsen.out: madsen.f dglfg.o dgletc.o dmdc.o d1mach.o rhols.o madrj.o
	$(F77) madsen.f dglfg.o dgletc.o dmdc.o d1mach.o rhols.o madrj.o $L
	a.out >$@

madsenb.out: madsenb.f dglfgb.o dgletc.o dmdc.o d1mach.o rhols.o madrj.o
	$(F77) madsenb.f dglfgb.o dgletc.o dmdc.o d1mach.o rhols.o madrj.o $L
	a.out >$@

pmain: dpmain.o mecdf.o dglfg.o dglfgb.o dgletc.o dmdc.o d1mach.o
	$(F77) dpmain.o mecdf.o dglfg.o dglfgb.o dgletc.o dmdc.o $L -o $@

pmain.out: pmain pmain.in
	pmain <pmain.in >$@

mlmnp: mlmnp.o mecdf.o mnpsubs.o dglfg.o dgletc.o dmdc.o d1mach.o
	$(F77) mlmnp.o mecdf.o mnpsubs.o dglfg.o dgletc.o dmdc.o $L -o $@

mlmnpb: mlmnpb.o mecdf.o mnpsubs.o dglfgb.o dgletc.o dmdc.o d1mach.o
	$(F77) mlmnpb.o mecdf.o mnpsubs.o dglfgb.o dgletc.o dmdc.o $L -o $@

mnpex1.out mnpex1b.out: mlmnp mlmnpb mnpex1.fu1 daganzo.fu2
	rm -f fort.?
	ln mnpex1.fu1 fort.1
	ln daganzo.fu2 fort.2
	mlmnp >mnpex1.out
	mlmnpb >mnpex1b.out

mnpex2.out mnpex2b.out: mlmnp mlmnpb mnpex2.fu1 daganzo.fu2
	rm -f fort.?
	ln mnpex2.fu1 fort.1
	ln daganzo.fu2 fort.2
	mlmnp >mnpex2.out
	mlmnpb >mnpex2b.out

rent1.out rent1b.out: mlmnp mlmnpb rent1.fu1 rent.fu2
	rm -f fort.?
	ln rent1.fu1 fort.1
	ln rent.fu2 fort.2
	mlmnp >rent1.out
	mlmnpb >rent1b.out

rent2.out rent2b.out: mlmnp mlmnpb rent2.fu1 rent.fu2
	rm -f fort.?
	ln rent2.fu1 fort.1
	ln rent.fu2 fort.2
	mlmnp >rent2.out
	mlmnpb >rent2b.out

# single-precision runs...

smadsen.out: smadsen.f sglfg.o sgletc.o smdc.o
	$(F77) smadsen.f sglfg.o sgletc.o smdc.o $L
	a.out >$@

smadsenb.out: smadsenb.f sglfgb.o sgletc.o smdc.o
	$(F77) smadsenb.f sglfgb.o sgletc.o smdc.o $L
	a.out >$@

spmain: spmain.o smecdf.o sglfg.o sglfgb.o sgletc.o smdc.o
	$(F77) spmain.o smecdf.o sglfg.o sglfgb.o sgletc.o smdc.o $L -o $@

spmain.out: spmain pmain.in
	spmain <pmain.in >$@

smlmnp: smlmnp.o smecdf.o smnpsubs.o sglfg.o sgletc.o smdc.o
	$(F77) smlmnp.o smecdf.o smnpsubs.o sglfg.o sgletc.o smdc.o $L -o $@

smlmnpb: smlmnpb.o smecdf.o smnpsubs.o sglfgb.o sgletc.o smdc.o
	$(F77) smlmnpb.o smecdf.o smnpsubs.o sglfgb.o sgletc.o smdc.o $L -o $@

smnpex1.out smnpex1b.out: smlmnp smlmnpb mnpex1.fu1 daganzo.fu2
	rm -f fort.?
	ln mnpex1.fu1 fort.1
	ln daganzo.fu2 fort.2
	smlmnp >smnpex1.out
	smlmnpb >smnpex1b.out

smnpex2.out smnpex2b.out: smlmnp smlmnpb mnpex2.fu1 daganzo.fu2
	rm -f fort.?
	ln mnpex2.fu1 fort.1
	ln daganzo.fu2 fort.2
	smlmnp >smnpex2.out
	smlmnpb >smnpex2b.out

srent1.out srent1b.out: smlmnp smlmnpb rent1.fu1 rent.fu2
	rm -f fort.?
	ln rent1.fu1 fort.1
	ln rent.fu2 fort.2
	smlmnp >srent1.out
	smlmnpb >srent1b.out

srent2.out srent2b.out: smlmnp smlmnpb rent2.fu1 rent.fu2
	rm -f fort.?
	ln rent2.fu1 fort.1
	ln rent.fu2 fort.2
	smlmnp >srent2.out
	smlmnpb >srent2b.out

clean:
	rm -f *.out *.o pmain mlmnp mlmnpb spmain smlmnp smlmnpb
