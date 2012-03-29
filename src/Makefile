MODEL = GC2CMAQ_CB05.exe

# Compiler Definitions
 FC   = gfortran
 IOAPI_HOME=/home/wdx/lib/x86_64i/gfort/ioapi_3.1/
 NETCDF_HOME=/usr
# Include file subsitution definitions
 IOAPI_INC = $(IOAPI_HOME)/ioapi/fixed_src
 INCLUDES = -DSUBST_GRID_ID= \
 -DSUBST_IOPARMS=\"$(IOAPI_INC)/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"$(IOAPI_INC)/FDESC3.EXT\" \
 -DSUBST_IODECL=\"$(IOAPI_INC)/IODECL3.EXT\"

# Compiler Flags
 F_FLAGS = -I. -I/ -fconvert=big-endian -ffixed-form -ffixed-line-length-none -O2 -x f95-cpp-input $(INCLUDES)
 LINK_FLAGS = -i_static
 CPP_FLAGS =  

#Libraries
 IOAPI = -L$(IOAPI_HOME)/Linux2_x86_64gfort -lioapi
 NETCDF = -L$(NETCDF_HOME)/lib -lnetcdf
 LIBRARIES = $(IOAPI) $(NETCDF)

# List of Object files needed for linking
OBJECTS =\
 CMAQ_DATA.o \
 BPCH2_DATA.o \
 geos2cmaq.o \
 chem_map.o \
 prof_map.o \
 defaults.o \
 lat_lon.o \
 vinterp.o \
 vinterp_prof.o \
 repair_bcs.o \
 lr_interp.o

.SUFFIXES: .F .f

chem_map.o: MAPPING.MAP BPCH2_DATA.o CMAQ_DATA.o
prof_map.o: MAPPING.PROFILE CMAQ_DATA.o prof_map.o
BPCH2_DATA.o: MAPPING.CSPEC MAPPING.TRACER
cmaq_data.o: MAPPING.MECH

$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<
clean:
	rm -f $(OBJECTS) $(MODEL) *.mod


