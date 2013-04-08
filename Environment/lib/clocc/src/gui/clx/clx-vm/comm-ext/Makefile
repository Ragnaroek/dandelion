CFLAGS = -c -Wall -fPIC
INSTALL=
OBJS = comm_ext.o

liblwcomm-ext.so : $(OBJS)
	${CC} -o $@ $(OBJS) -shared -Xlinker -soname -Xlinker $@

clean :
	rm -f ${OBJS} liblwcomm-ext.so

install : liblwcomm-ext.so
	cp liblwcomm-ext.so $(INSTALL)/liblwcomm-ext.so

