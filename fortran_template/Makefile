MODEL = GC2CMAQ.exe

#ckdesc3.o \
# List of Object files needed for linking
OBJECTS = \
 UTILIO_DEFN.o \
 CGRID_SPCS.o \
 CMAQ_DATA.o \
 BPCH2_DATA.o \
 geos2cmaq_semiflex.o \
 chem_map.o \
 cmaq_prof_driver.o \
 gc_spc_map.o \
 ngc_spc_map.o \
 lst_spc_map.o \
 findex.o \
 lat_lon.o \
 vinterp.o \
 vinterp_prof.o \
 repair_bcs.o \
 lr_interp.o

.SUFFIXES: .F .f

help:
	@echo "--------------------------------------------------"
	@echo "GC2CMAQ requires netcdf and ioapi (with m3utilio)."
	@echo
	@echo "These libraries and associated include files must be available via default search paths, or they can be provided to the compiler via the LDFLAGS and FFLAGS environmental variables"
	@echo
	@echo "e.g., with gfortran the call is as follows:"
	@echo "FC=gfortran FFLAGS=\"-I/usr/local/include/ -I. -ffixed-line-length-none -fconvert=big-endian\" LD_FLAGS=\"-lioapi -lnetcdf -lnetcdff\" make all"
	@echo
	@echo "e.g., with intel the call is as follows:"
	@echo "FC=ifort FFLAGS=\"-I/usr/local/include/ -I. -fixed -extend_source -132 -convert big_endian -O2\" LD_FLAGS=\"-lioapi -lnetcdf -lnetcdff\" make all"
	@echo "--------------------------------------------------"
	@echo
	@echo
    
all: $(MODEL)

$(MODEL): $(OBJECTS)
	$(FC) $(OBJECTS) $(LD_FLAGS) -o $@

chem_map.o: MAPPING.MAP BPCH2_DATA.o CMAQ_DATA.o
prof_map.o: MAPPING.PROFILE CMAQ_DATA.o prof_map.o
BPCH2_DATA.o: MAPPING.CSPEC MAPPING.TRACER
cmaq_data.o: MAPPING.MECH

.f.o.F.o:
	$(FC) -c $(F_FLAGS) $<

clean:
	rm -f $(OBJECTS) $(MODEL) *.mod


