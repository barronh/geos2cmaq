#MODEL = GC2CMAQ_SAPRC99.exe
 MODEL = GC2CMAQ.exe

#compiler = pgf
 compiler = ifc


ifeq ($(compiler),ifc)
# Compiler Definitions
 FC   = /usr/local/intel/ictce/3.2.2.013/fc/bin/intel64/ifort

# Include file subsitution definitions
 IOAPI_PATH = /home/wdx/lib/x86_64/ifc11/ioapi_3.1/Linux2_x86_64ifort
 IOAPI_INC = /home/wdx/lib/x86_64/ifc/ioapi_3.1/ioapi/fixed_src
 CPP_FLAGS = -DSUBST_GRID_ID= \
 -DSUBST_IOPARMS=\"$(IOAPI_INC)/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"$(IOAPI_INC)/FDESC3.EXT\" \
 -DSUBST_IODECL=\"$(IOAPI_INC)/IODECL3.EXT\"


#Libraries
 IOAPI = -L$(IOAPI_PATH) -lioapi
 NETCDF = -L/home/wdx/lib/x86_64/ifc11/netcdf/lib -lnetcdf
 LIBRARIES = $(IOAPI) $(NETCDF)

DEBUG = -CU -CB -traceback
# Compiler Flags
 F_FLAGS = -C $(DEBUG) -extend_source -FI -O2 -132 -I $(IOAPI_INC) -I $(IOAPI_PATH)
F_FLAGS    = -fixed -extend_source -132 -convert big_endian -O2 $(DEBUG) -I $(IOAPI_INC) -I $(IOAPI_PATH) -I .
f_FLAGS    = -fixed -extend_source -132 -convert big_endian -O2 $(DEBUG) -I $(IOAPI_INC) -I $(IOAPI_PATH)  -I .

 LINK_FLAGS = -i_static

else

FC    = /usr/local/pgi/linux86-64/10.5/bin/pgf90
LINKER = $(FC)


ioapi_path = /home/wdx/lib/x86_64/pgf/ioapi_3.1/Linux2_x86_64pg

IOAPI_INC =  /home/wdx/lib/src/ioapi_3.1/ioapi/fixed_src

 DEBUG = -g -C -traceback -Ktrap=divz,inv -Minform=warn -Mchkptr -Mchkfpstk
 DEBUG = -g -C -traceback -Ktrap=divz,inv -Minform=warn 
#DEBUG = 

F_FLAGS    = -Mfixed -Mextend -byteswapio -O2 -module . $(DEBUG) -I $(IOAPI_INC) -I $(ioapi_path) -I .
f_FLAGS    = -Mfixed -Mextend -byteswapio -O2 -module . $(DEBUG) -I $(IOAPI_INC) -I $(ioapi_path) -I .
F90_FLAGS    = -Mfree -O3 $(DEBUG)  -I $(IOAPI_INC) -I $(ioapi_path) -I .
f90_FLAGS    = -Mfree -O3  $(DEBUG) -I $(IOAPI_INC) -I $(ioapi_path) -I .
C_FLAGS    = -O2  -DFLDMN -I /home/wdx/lib/x86_64/pgf/mpich/include
LINKER_FLAGS = -Bstatic  -Bstatic_pgi

#   Library paths
LIBRARIES = \
-L$(ioapi_path)  -lioapi \
-L/home/wdx/lib/x86_64/pgf/netcdf/lib -lnetcdf \

 CPP_FLAGS = -DSUBST_GRID_ID= \
 -DSUBST_IOPARMS=\"$(IOAPI_INC)/PARMS3.EXT\" \
 -DSUBST_IOFDESC=\"$(IOAPI_INC)/FDESC3.EXT\" \
 -DSUBST_IODECL=\"$(IOAPI_INC)/IODECL3.EXT\"

endif

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


$(MODEL): $(OBJECTS)
	$(FC) $(LINK_FLAGS) $(OBJECTS) $(LIBRARIES) -o $@

chem_map.o: MAPPING.MAP BPCH2_DATA.o CMAQ_DATA.o
prof_map.o: MAPPING.PROFILE CMAQ_DATA.o prof_map.o
BPCH2_DATA.o: MAPPING.CSPEC MAPPING.TRACER
cmaq_data.o: MAPPING.MECH

.F.o:
	$(FC) -c $(F_FLAGS) $(CPP_FLAGS) $(INCLUDES) $<

.f.o:
	$(FC) $(F_FLAGS) -c $<
clean:
	rm -f $(OBJECTS) $(MODEL) *.mod


