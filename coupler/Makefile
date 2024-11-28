PREFIX=../../cotocoa
CFLAGS=-I${PREFIX}/include
LDFLAGS=-L${PREFIX}/lib
LIBS=-lctca
F_LIBS=-lctca_f -lctca_mod
FFLAGS          = -ipo -O3 -no-prec-div -fp-model fast=2 -xHost
FC              = mpiifort

OBJS = coupler.f90

.SUFFIXES: .f90 .o

all: coupler

coupler: $(OBJS)
	$(FC) $(CFLAGS) $(LDFLAGS) $(FFLAGS) coupler.f90 -o coupler $(F_LIBS) $(LIBS)

clean:
	/bin/rm -f  *.mod *.o coupler
