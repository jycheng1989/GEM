# I rewrote this to make it easier to maintain. Here are the main changes:
#
# * everything is built in a subdirectory so the root doesn't get cluttered
#
# * we can have independent build directories for different sets of compilation
#   flags (this makes optimization and profiling easier)
#
# * rules are generalized, so to add a file, we just append it to $(SRCS)

# source files
SRCS = erf.f gem_com.f90 equil.f90 gem.f90 outd.f90 fcnt.f fft_wrapper.f regtest.f90

# this next line is a bit weird; it generates object file names automatically
# from sources (we have both .f and .f90 files, so we need to mess around a bit)
# it first substitutes out all the .f suffixes for .o, then all the .f90, and
# then prepends the build directory to the file name
OBJS = $(addprefix $(OUTDIR)/,$(patsubst %.f90,%.o,$(patsubst %.f,%.o,$(SRCS))))

# modules, their files should be compiled before some others
MODNAMES = fft_wrapper.mod gem_com.mod equil.mod regtest.mod
MODS = $(addprefix $(OUTDIR)/modules/,$(MODNAMES))

# pputil module must be compiled first
PLIB = $(OUTDIR)/pputil.o

# compiler (should we change this to ifort if we use intel-specific stuff?)
F90 = ftn

# build directory
# equal to name of primary target or 'build' if none specified
ifdef MAKECMDGOALS
	OUTDIR = $(MAKECMDGOALS)
else
	OUTDIR = build
endif

# default options
LIBS = $(DFFTPACK) -mkl
#OPT = -FR -r8 -heap-arrays -O2 -g -traceback -check bounds
OPT = -FR -r8 -O2 -qopenmp
MODOPT = -module $(OUTDIR)/modules


# here are all the targets, this shows how easy it is to add a new set of flags

# build for haswell nodes
.PHONY: build
build: $(OUTDIR)/gem

# build for KNL nodes
.PHONY: build-knl
build-knl: $(OUTDIR)/gem
build-knl: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopenmp

# debug build (no optimization)
.PHONY: debug
debug: $(OUTDIR)/gem
debug: OPT = -FR -r8 -O0 -g

# vtune profiling on KNL nodes
.PHONY: vtune
vtune: $(OUTDIR)/gem
vtune: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopenmp -g -dynamic

# vtune profiling on haswell nodes
.PHONY: vtune-hsw
vtune-hsw: $(OUTDIR)/gem
vtune-hsw: OPT = -FR -r8 -O2 -g -dynamic

# ipm profiling
.PHONY: ipm
ipm: $(OUTDIR)/gem
ipm: LIBS += $$IPM

# compilation logs to see what gets vectorized
.PHONY: vectest
vectest: $(OUTDIR)/gem
vectest: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopenmp -qopt-report=5
vectest: LIBS += $$IPM

# build in a different directory and link with IPM for scaling tests (KNL)
.PHONY: scaling
scaling: $(OUTDIR)/gem
scaling: OPT = -FR -r8 -O2 -xMIC-AVX512
scaling: LIBS += $$IPM

# build in a different directory and link with IPM for scaling tests (KNL)
.PHONY: scaling-hsw
scaling-hsw: $(OUTDIR)/gem
scaling-hsw: OPT = -FR -r8 -O2
scaling-hsw: LIBS += $$IPM

# allinea map profiling (knl)
.PHONY: map
map: $(OUTDIR)/gem
map: OPT = -FR -r8 -O2 -g -xMIC-AVX512
map: LIBS += -Wl,@./allinea-profiler.ld

# allinea map profiling (haswell)
.PHONY: map-hsw
map-hsw: $(OUTDIR)/gem
map-hsw: OPT = -FR -r8 -O2 -g
map-hsw: LIBS += -Wl,@./allinea-profiler.ld

.PHONY: all
all: $(OUTDIR)/gem

.PHONY: clean
clean:
	rm -rf $(OBJS) $(PLIB) $(OUTDIR)/modules/*.mod $(OUTDIR)/*.optrpt

# generalized compilation rules
$(OUTDIR)/gem: $(OBJS)
	mkdir -p $(OUTDIR)/out $(OUTDIR)/dump $(OUTDIR)/matrix
	cp *.txt $(OUTDIR)
	$(F90) -o $(OUTDIR)/gem $(OPT) $(MODOPT) $(OBJS) $(PLIB) $(LIBS)

# plib gets built first, so we can create our directories here
$(PLIB): pputil.f
	mkdir -p $(OUTDIR)/modules
	$(F90) -c $(OPT) $(MODOPT) -o $@ $<

$(OUTDIR)/%.o: %.f90
	$(F90) -c $(OPT) $(MODOPT) -o $@ $<

$(OUTDIR)/%.o: %.f
	$(F90) -c $(OPT) $(MODOPT) -o $@ $<

$(OUTDIR)/modules/%.mod: $(OUTDIR)/%.o;


# file-specific compilation options/dependencies can go down here:

# compile pputil first
$(OBJS): $(PLIB)

# required module files
$(OUTDIR)/gem.o: $(MODS)
$(OUTDIR)/outd.o: $(MODS)

# erf.f is in fixed format
$(OUTDIR)/erf.o: erf.f
	$(F90) -c $(OPT) $(MODOPT) -FI -o $@ $<
