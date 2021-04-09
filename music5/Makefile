# Makefile for MUSIC V
# Victor Lazzarini, Jun 08

PROGS = pass1 pass2 pass3

all: $(PROGS)

pass1: pass1.f
	gfortran -o pass1 pass1.f

pass2: pass2.f
	gfortran -o pass2 pass2.f

pass3: pass3.f
	gfortran -o pass3 pass3.f

clean: $(PROGS)
	rm -f $(PROGS)