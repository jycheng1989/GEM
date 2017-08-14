# I rewrote this to make it easier to maintain. Here are the main changes:
#
# * everything is built in a subdirectory so the root doesn't get cluttered
#
# * we can have independent build directories for different sets of compilation
#   flags (this makes optimization easier)
#
# * rules are generalized, so to add a file, we just append it to $(SRCS)

# source files
SRCS = erf.f gem_com.f90 equil.f90 gem.f90 outd.f90 fcnt.f fft_wrapper.f

# this next line is a bit weird; it generates object file names automatically
# from sources (we have both .f and .f90 files, so we need to mess around a bit)
# it first substitutes out all the .f suffixes for .o, then all the .f90, and
# then prepends the build directory to the file name
OBJS = $(addprefix $(OUTDIR)/,$(patsubst %.f90,%.o,$(patsubst %.f,%.o,$(SRCS))))

# modules, their files should be compiled before some others
MODNAMES = fft_wrapper.mod gem_com.mod equil.mod
MODS = $(addprefix $(OUTDIR)/,$(MODNAMES))

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
OPT = -FR -r8 -O2
MODOPT = -module $(OUTDIR)/modules


# here are all the targets, this shows how easy it is to add a new set of flags
.PHONY: build
build: $(OUTDIR)/gem

.PHONY: debug
debug: $(OUTDIR)/gem
debug: OPT = -FR -r8 -O0 -g

.PHONY: vtune
vtune: $(OUTDIR)/gem
vtune: OPT = -FR -r8 -O2 -xMIC-AVX512 -g -dynamic

.PHONY: ipm
ipm: $(OUTDIR)/gem
ipm: LIBS += $$IPM

.PHONY: vectest
vectest: $(OUTDIR)/gem
vectest: OPT = -FR -r8 -O2 -xMIC-AVX512 -qopt-report
vectest: LIBS += $$IPM

.PHONY: scaling
scaling: $(OUTDIR)/gem
scaling: OPT = -FR -r8 -O2 -xMIC-AVX512
scaling: LIBS += $$IPM

.PHONY: all
all: $(OUTDIR)/gem

.PHONY: clean
clean:
	rm -f $(OBJS) $(PLIB) *.mod *.optrpt

# generalized compilation rules
$(OUTDIR)/gem: $(OBJS)
	mkdir -p $(OUTDIR)/out $(OUTDIR)/dump $(OUTDIR)/matrix
	$(F90) -o $(OUTDIR)/gem $(OPT) $(MODOPT) $(OBJS) $(PLIB) $(LIBS)

# plib gets built first, so we can create our directories here
$(PLIB): pputil.f
	mkdir -p $(OUTDIR)/modules
	$(F90) -c $(OPT) $(MODOPT) -o $@ $<

$(OUTDIR)/%.o: %.f90
	$(F90) -c $(OPT) $(MODOPT) -o $@ $<

$(OUTDIR)/%.o: %.f
	$(F90) -c $(OPT) $(MODOPT) -o $@ $<

$(OUTDIR)/%.mod: $(OUTDIR)/%.o;


# file-specific compilation options/dependencies can go down here:

# compile pputil first
$(OBJS): $(PLIB)

# required module files
$(OUTDIR)/gem.o: $(MODS)
$(OUTDIR)/outd.o: $(MODS)

# erf.f is in fixed format
$(OUTDIR)/erf.o: erf.f
	$(F90) -c $(OPT) $(MODOPT) -FI -o $@ $<
