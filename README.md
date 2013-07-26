# geos2cmaq

GEOS2CMAQ maps GEOS-Chem outputs to CMAQ boundary conditions for any chemical mechanism.

## Introduction

The GEOS2CMAQ tool is an advanced chemical mapping and interpolation tool which allows users to
convert hourly GEOS-Chem output into CMAQ-ready boundary condition files. The tool provides a
flexible mechanism for relating GEOS-Chem chemical species to a variety of chemical and aerosol
mechanisms.

Required input files

1.  GEOS-Chem output: Both GEOS-Chem “tracers” and “chemical species” are used in the mapping lists provided with this tool. The US EPA has archived several GEOS-Chem runs from 2001-2010. These files can be provided to users as necessary. Please contact Farhan Akhtar or Kristen Foley for more details on how to receive these files. (NOTE: GEOS-Chem by default does not write out chemical species concentrations. Please contact Farhan Akhtar for the code updates to output the chemical species from GEOS-Chem)
1.  CMAQ/MCIP METBDY3D file: The tool dynamically regrids the GEOS-Chem output to the desired subdomain by using the grid description provided by these files.
1.  CMAQ namelist files: Users must provide the *.nml files for the CMAQ application.
1.  Chemical Mechanism Mapping files: These files provide the mapping of GEOS-Chem tracers and species to a desired chemical mechanism’s species set. Several default cases are provided with the tool and can be specified at compile time. See re-compiling the code for more details on the provided chemical mappings and how to specify alternative mappings.

## Installation

### Download location and software requirements

The tool requires libraries for python (with numpy and matplotlib), netcdf and ioapi (with m3utilio) to be installed on the host system.

Users can download the code directory at the following location using wget:

1.  Clicking on https://github.com/barronh/geos2cmaq/archive/master.zip
2.  Using git clone

    git clone https://github.com/barronh/geos2cmaq

This package includes the FORTRAN-based regridder and python pre-processor.
Verify installation (recommended but optional)

Users can download a test run directory from the following address:
wget https://dl.dropbox.com/u/52906249/geos2cmaq_test/testrun.tar.gz

This test case provides a simple framework for compiling and running the tool on a two day period. It provides a default case for compiling the code and contains a default script for interacting with the tool
(see changing run options for more information).

Unzip the downloaded files into a common directory:

    tar -xvf geos2cmaq.tar.gz
    tar -xvf testrun.tar.gz

In the run directory (‘testrun’) open the Makefile.test and update any compile flags as necessary for
your system. Contact your computer support staff with help with this step if the code does not compile.
The tool requires flags for netcdf and ioapi (with m3utilio) to be set in the makefile.
Compile the code and run the test case conversion:

    make -f Makefile.test

If the code is installed properly, this command will compile both the python and FORTRAN portions of
the tool, producing a single executable and running the run.sh script for the default period (20050101-
20050102). Following successful completion, the following files will be available in the rundirectory
folder:

    geos2cmaq.20050101.ncf
    geos2cmaq.20050102.ncf

If these files are not produced or if any error messages are shown on screen, please revisit the compiler
options or contact Barron Henderson for support.

## Running the tool

### Re-compiling the code:

Recompilation of the code after installation verification is only necessary when changing the chemical
mechanism from the default case (CB05/AE6). Alternate chemical mechanisms can be specified after the
"-c" flag in the src section of the makefile. Several default mappings are included in the ./mapping folder
in the code directory. Users can also specify the full path to another mapping file, if desired.

### Changing Run Options
In the test run directory, the run.sh script can be used to create boundary condition files. The script has
several required and optional variables, as outlined below:

    Usage: START_DATE=CCYYMMDD STOP_DATE=CCYYMMDD [MECHINC=<PATH>] [PROFILE=<PATH>]
    [OUTPATH=<path>] run.sh
(see Makefile.test from testrun for an example)

    Required Variables:
        START_DATE First day of boundary condition file (+%Y%m%d format as defined by GNU coreutils)
        STOP_DATE Last day of boundary condition file (format same as START_DATE)
    Optional Variables:
        MECHINC Path to CMAQ chemical mechanism namelist files (*.NML). Defaults to ./
        PROFILE CMAQ formated chemical profile (i.e., BCON input). Defaults to ./profile.dat
        OUTPATH Full path (directory and file) for output file to be written to. Defaults to ./geos2cmaq.CCYYMMDD.ncf"

Users can create a wrapper script to iterate across multiple dates if necessary.

NOTE: The code will list any CMAQ species specified in the namelist file that cannot be found in the
chemical mapping file. If the species exists in the specified CMAQ profile file, it will be mapped and
placed into the output boundary conditions. Please review the lists of mapped species in the log files to
ensure that all species are correctly mapped.

For more information or questions, please contact developers.
