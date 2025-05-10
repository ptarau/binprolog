#replace this with SRC=<absolute path> if it does not work on your machine

VERS=12xx
SRC = `pwd`
LIBDIR = ../lib
AR = ar vr
LIBS = -lm

GCC = gcc
OPTI= $(DEFS) -O

GCCS = $(GCC) -fomit-frame-pointer $(OPTI)3
CC= $(GCCS)

LDO = $(CC)
LDR = ld -r
LDS = ld

BINDIR = ../bin
OLDBP = ./ru

######################## END EDITABLE AREA ########################### 

P1=ru
P2=sym
P3=load
P4=engine

P5=builtins
P6=dict
P7=io
P7A=socket
P8=float
P9=debug
P10=gc
P11=main
P12=term
P13=termStore

CPROGS=$(P1).c $(P2).c $(P3).c $(P4).c $(P5).c $(P6).c $(P7).c $(P7A).c $(P8).c $(P9).c $(P10).c $(P11).c $(P12) $(P13) c.c stub.c
BP_PROGS=$(P1).c $(P2).c $(P3).c $(P4).c $(P5).c $(P6).c $(P7).c $(P7A).c $(P8).c $(P9).c $(P10).c $(P11).c $(P12) $(P13) wam.c binpro.c c.c

MAINOBS = $(P1).o $(P2).o $(P3).o $(P4).o
SELFOBS = $(MAINOBS)
OTHEROBS =  $(P5).o $(P6).o $(P7).o $(P7A).o $(P8).o $(P9).o $(P10).o $(P11).o $(P12).o $(P13).o

OBS =  $(MAINOBS) $(OTHEROBS) c.o stub.o

PPROGS = headers.pl top.pl bin.pl co.pl init.pl oper.pl lib.pl read.pl dcg.pl write.pl extra.pl net.pl other.pl c_comp.pl c_instr.pl run.pro wam.pro binpro.pro

CP = cp

nobp:
	$(CP) wam.ok wam.bp
	$(CP) defs.ok defs.h
	$(CP) prof.ok prof.h
	make ru
	make derived
	$(CP) wam.bp otherwam.bp
	touch wam.bp
	make step1
	make binpro OLDBP=./ru
	$(CP) binpro ../bin/bp

derived:
	echo "['oper.pl']. ['headers.pl']. go. " | $(OLDBP)

commit_to_new:
	$(CP) wam.bp wam.ok
	$(CP) defs.h defs.ok
  
it:	basic

extra:	all bp

basic:	minimal binpro lib

minimal:	ru wam.bp

most:	minimal step1 step2


mac_mt: rclean
	make CC="gcc -DW64 -DTHREADS=1 -m64 -arch x86_64 -g3 -O3 -Wall -fomit-frame-pointer -fmessage-length=0 -lpthread"

linux32_mt: rclean
	make CC="gcc -O3 -DTHREADS=1 -g -o bp -fomit-frame-pointer -lpthread -lm"

linux_mt: rclean
	make CC="gcc -O3 -DW64 -m64 -g -DTHREADS=1 -o bp -fomit-frame-pointer -lpthread -lm"


# bootsrapping

wam.bp:	otherwam.bp $(PPROGS) # $$$
#wam.bp:	 $(PPROGS)
	@echo bootsrapping BinProlog...
	echo "boot. " | ./ru -q3 otherwam.bp
	$(CP) wam.bp otherwam.bp
	touch wam.bp

ru: ru.a stub.o c.o
	@echo making ru with $(LIBS)
	$(LDO) -o ru c.o stub.o ru.a  $(LIBS)

#builtins.pl defs.h prof.h:	headers.pl init.pl

	
binpro:	binpro.o c.o wam.o ru.a
	$(LDO) -o binpro binpro.o c.o wam.o ru.a  $(LIBS)

binpro.o:	global.h c_defs.h binpro.h binpro.c
	$(CC) -o binpro.o -c binpro.c

step1 binpro.h binpro.c:	ru wam.bp binpro.pro
	./ru wam.bp "cmake(binpro,prolog),halt"

# makes a BinProlog library usable to generate a standalone executable

bp:	libbp.so binpro.o c.o
	#$(LDO) -o bp binpro.o c.o $(SRC)/libbp.so  $(LIBS)
	@echo assumed dynamic libraries are in $(SRC)
	@echo WORKS: $(LDO) -o bp binpro.o c.o wam.o ru.a  $(LIBS)
	gcc --verbose -o bp $(SRC)/libbp.so binpro.o c.o  $(LIBS)

libso: libbp.so

libbp.so:	global.h c_defs.h wam.o ru.a $(PPROGS)
	$(LDS) -o libbp.so wam.o ru.a  $(LIBS)

# C-ified wam-code library
wamlib:	wam.o

wam.o:	global.h c_defs.h wam.h wam.c # $$$
#wam.o:	wam.h wam.c
	$(CC) -o wam.o -c wam.c

step2 wam.h wam.c:	wam.pro $(PPROGS) # $$$
#wam.h wam.c:	wam.pro
#	echo "set_c_threshold(12,60). cmake(wam,prolog). " | ./ru wam.bp
	./ru wam.bp "cmake(wam,prolog),halt"


ru.a:	$(OTHEROBS) $(SELFOBS)
	$(AR) ru.a $(OTHEROBS) $(SELFOBS)

# full BinProlog library
lib:
	#binpro
	chmod a+r+x+w *.a
	$(CP) ru.a bp_lib.a
	$(AR) bp_lib.a wam.o binpro.o c.o


oclean:
	rm -f *.o *.so *.a *.wam ru binpro bp bpr *.obj *.ilk *.exe *.pdb
#	rm -f *.o *.so *.wam ru binpro bp wam.c wam.h binpro.c binpro.h *.obj

ruclean ru_clean:
	rm -f ru.*.*

clean:	oclean
	rm -f *.lib *.exp *.dll *.ccc *.hhh *.ooo *.so *.exe core *.asm wam.c wam.h binpro.h binpro.c binpro bp bpr
	rm -f *.s w2a
	rm -f *.tar.*.uue *.gz
	rm -f progs/*.wam progs/*.ql progs/*.bp

wamclean wam_clean:	wclean
	rm -f *.bp *.out wam.h wam.c binpro.h binpro.c binpro

wclean:
	rm -f *.wam
	rm -f progs/*.wam
	rm -f myprogs/*.wam

rclean distclean realclean real_clean: clean wam_clean ru_clean
	rm -f defs.h prof.h builtins.pl
	$(CP) full.pro wam.pro

#.IGNORE:
	
# ----------------- dependencies ------------------
sym.o: sym.c global.h defs.h
dict.o: dict.c global.h defs.h
io.o: io.c global.h defs.h
builtins.o: builtins.c defs.h global.h
load.o: load.c defs.h global.h 
ru.o: ru.c defs.h global.h
term.o: term.c term.h defs.h global.h
termStore.o: termStore.c termStore.h term.h defs.h global.h
engine.o: engine.c   defs.h global.h
float.o: float.c defs.h global.h
debug.o: debug.c defs.h prof.h global.h
gc.o: gc.c defs.h global.h gc.h
socket.o: socket.c global.h defs.h
c.o: global.h c.c defs.h
stub.o: global.h defs.h stub.c
main.o: global.h defs.h main.c
