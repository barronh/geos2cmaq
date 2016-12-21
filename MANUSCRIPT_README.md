# Overview
This software was used in Henderson et al.[1]. In that manuscript, the GEOS-Chem outputs were created using v8-03-02 and v9-01-01 code. For subsequent work, we use the ND49 option instead of modifying the code. These files are provided for posterity.

# Code Specifics
v8-03-02 was at the following commit with edits described in MANUSCRIPT_CODE.diff
commit b27d73dc1ea51de3f9571b337408a93daf113c04 (aka tag v8-03-02-Patch-setemis)

v9-01-01 was at the following commit with edits described in MANUSCRIPT_CODE.diff
commit a03a26db8e453892f2f46d48f84c4a699d474432 (aka tag v9-01-01-Patch-CO2)

# Example Run Scripts:
ftp://data.as.essie.ufl.edu/pub/geos2cmaq/BCv8geos5_merra/work/MOD3EVAL/afd/GEOS-Chem_v8-03-02_v8-02-03CHEM_GEOS5/runarchive/GEOS-Chem.run.4-26-2012_SOA_v8-03-02_v8-2-3CHEM_BC/qscript.run

# Configuration Files:
ftp://data.as.essie.ufl.edu/pub/geos2cmaq/BCv8geos5_merra/work/MOD3EVAL/afd/GEOS-Chem_v8-03-02_v8-02-03CHEM_GEOS5/runarchive/GEOS-Chem.run.4-26-2012_SOA_v8-03-02_v8-2-3CHEM_BC/input.geos
ftp://data.as.essie.ufl.edu/pub/geos2cmaq/BCv8geos5_merra/work/MOD3EVAL/afd/GEOS-Chem_v8-03-02_v8-02-03CHEM_GEOS5/runarchive/GEOS-Chem.run.4-26-2012_SOA_v8-03-02_v8-2-3CHEM_BC/*.dat
ftp://data.as.essie.ufl.edu/pub/geos2cmaq/BCv8geos5_merra/work/MOD3EVAL/afd/GEOS-Chem_v8-03-02_v8-02-03CHEM_GEOS5/runarchive/GEOS-Chem.run.4-26-2012_SOA_v8-03-02_v8-2-3CHEM_BC/ratj.d

# Restarts (including 2011-01-01):
ftp://data.as.essie.ufl.edu/pub/geos2cmaq/BCv8geos5_merra/work/MOD3EVAL/afd/GEOS-Chem_v8-03-02_v8-02-03CHEM_GEOS5/restart


[1]Henderson, B. H., Akhtar, F., Pye, H. O. T., Napelenok, S. L., and Hutzell, W. T.: A database and tool for boundary conditions for regional air quality modeling: description and evaluation, Geosci. Model Dev., 7, 339-360, doi:10.5194/gmd-7-339-2014, 2014.
