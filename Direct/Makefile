# I rewrote this to make it easier to maintain. Here are the main changes:
#
# * everything is built in a subdirectory so the root doesn't get cluttered
#
# * we can have independent build directories for different sets of compilation
#   flags (this makes optimization and profiling easier)
#
# * rules are generalized, so to add a file, we just append it to $(SRCS)

# source files
SRCS =	gem_com.f90 gem_equil.f90 gem_main.f90 gem_outd.f90 gem_fcnt.f90\
	gem_fft_wrapper.f90 gem_gkps_adi.f90 regtest.f90

# this next line is a bit weird; it generates object file names automatically
# from sources. The file *.f90 becomes build_dir/*.o
OBJS = $(addprefix $(OUTDIR)/,$(patsubst %.f90,%.o,$(SRCS)))

PLIB = $(OUTDIR)/gem_pputil.o

# compiler might be changed to PGI
F90 = ftn

# build directory
# equal to name of primary target or 'build' if none specified
ifdef MAKECMDGOALS
	OUTDIR = $(MAKECMDGOALS)
else
	OUTDIR = build
endif

# default options:
LIBS = $(DFFTPACK)
#OPT = -FR -r8 -heap-arrays -O2 -g -traceback -check bounds
OPT = -FR -r8 -O3 -qopenmp
MODOPT = -module $(OUTDIR)/modules

# here are all the targets, this shows how easy it is to add a new set of flags:

# build for haswell nodes
.PHONY: build
build: $(OUTDIR)/gem_main

# build for KNL nodes
.PHONY: build-knl
build-knl: $(OUTDIR)/gem_main
build-knl: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopenmp

# debug build (no optimization)
.PHONY: debug
debug: $(OUTDIR)/gem_main
debug: OPT = -FR -r8 -O0 -g

# vtune profiling on KNL nodes
.PHONY: vtune
vtune: $(OUTDIR)/gem_main
vtune: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopenmp -g -dynamic

# vtune profiling on haswell nodes
.PHONY: vtune-hsw
vtune-hsw: $(OUTDIR)/gem_main
vtune-hsw: OPT = -FR -r8 -O2 -g -dynamic

# ipm profiling
.PHONY: ipm
ipm: $(OUTDIR)/gem_main
ipm: LIBS += $$IPM

# compilation logs to see what gets vectorized
.PHONY: vectest
vectest: $(OUTDIR)/gem_main
vectest: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopenmp -qopt-report=5
vectest: LIBS += $$IPM

# build in a different directory and link with IPM for scaling tests (KNL)
.PHONY: scaling
scaling: $(OUTDIR)/gem_main
scaling: OPT = -FR -r8 -O2 -xMIC-AVX512
scaling: LIBS += $$IPM

# build in a different directory and link with IPM for scaling tests (KNL)
.PHONY: scaling-hsw
scaling-hsw: $(OUTDIR)/gem_main
scaling-hsw: OPT = -FR -r8 -O2
scaling-hsw: LIBS += $$IPM

# allinea map profiling (knl)
.PHONY: map
map: $(OUTDIR)/gem_main
map: OPT = -FR -r8 -O2 -g -xMIC-AVX512
map: LIBS += -Wl,@./allinea-profiler.ld

# allinea map profiling (haswell)
.PHONY: map-hsw
map-hsw: $(OUTDIR)/gem_main
map-hsw: OPT = -FR -r8 -O2 -g
map-hsw: LIBS += -Wl,@./allinea-profiler.ld

.PHONY: all
all: $(OUTDIR)/gem_main

# actual compilation rules:

# compile plib first
$(OBJS): $(PLIB)

# plib gets built first, so we can create our directories here
$(PLIB): gem_pputil.f90
	mkdir -p $(OUTDIR)/modules
	$(F90) -o $@ -c $(OPT) $(MODOPT) gem_pputil.f90

# actual compilation rules here:
$(OUTDIR)/gem_main: $(OBJS) $(PLIB)
	$(F90) -o $@ $(OPT) $(MODOPT) $(OBJS) $(PLIB) $(LIBS) 

$(OUTDIR)/gem_com.o: gem_com.f90 $(PLIB)
	$(F90) -o $@ -c $(OPT) gem_com.f90

$(OUTDIR)/gem_equil.o: gem_equil.f90 $(PLIB)
	$(F90) -o $@ -c $(OPT) $(MODOPT) gem_equil.f90

$(OUTDIR)/gem_gkps_adi.o: gem_gkps_adi.f90 gem_com.f90 gem_equil.f90 $(PLIB)
	$(F90) -o $@ -c $(OPT) $(MODOPT) gem_gkps_adi.f90

$(OUTDIR)/gem_main.o: $(addprefix $(OUTDIR)/,gem_fft_wrapper.o gem_com.o gem_equil.o)
$(OUTDIR)/gem_main.o: $(addprefix $(OUTDIR)/,gem_gkps_adi.o regtest.o)
$(OUTDIR)/gem_main.o: gem_main.f90 $(PLIB)
$(OUTDIR)/gem_main.o:
	$(F90) -o $@ -c -diag-disable=warn $(OPT) $(MODOPT) gem_main.f90

$(OUTDIR)/gem_outd.o: $(addprefix $(OUTDIR)/,gem_fft_wrapper.o gem_com.o gem_equil.o)
$(OUTDIR)/gem_outd.o: gem_outd.f90 $(PLIB)
$(OUTDIR)/gem_outd.o:
	$(F90) -o $@ -c $(OPT) $(MODOPT) gem_outd.f90

$(OUTDIR)/gem_fcnt.o: gem_fcnt.f90
	$(F90) -o $@ -c $(OPT) $(MODOPT) gem_fcnt.f90

$(OUTDIR)/gem_fft_wrapper.o: gem_fft_wrapper.f90
	$(F90) -o $@ -c $(OPT) $(MODOPT) gem_fft_wrapper.f90

$(OUTDIR)/regtest.o: regtest.f90
	$(F90) -o $@ -c $(OPT) $(MODOPT) regtest.f90

.PHONY: clean
clean:
	rm -f *.o *.lst *.mod gem_main
