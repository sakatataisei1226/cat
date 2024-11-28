PREFIX=../requester/lib/cotocoa
CFLAGS=-I${PREFIX}/include -ipo
LDFLAGS=-L${PREFIX}/lib
LIBS=-lctca
F_LIBS=-lctca_f -lctca_mod
FFLAGS          = -ipo -O3 -no-prec-div -fp-model fast=2 -xHost
#FFLAGS		= -fpp -Dwall=1 -CB -check all,noarg_temp_created -traceback -fpe0 -warn -C -no-ip
MPICC=mpiicc
MPIFC=mpiifort
CC=icc
FC=ifort

OBJS = worker.f90

.SUFFIXES: .f90 .o

all: worker

worker: $(OBJS)
	$(MPIFC) $(CFLAGS) $(LDFLAGS) $(FFLAGS) worker.f90 -o worker $(F_LIBS) $(LIBS)

tcp-server-mod.o:
	$(FC) $(FFLAGS) -c tcp-server-mod.f90

tcp-server-lib.o:
	$(CC) $(CFLAGS) -c tcp-server-lib.c

clean:
	/bin/rm -f  *.mod *.o worker
