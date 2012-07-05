#!/bin/bash

if [[ ! -n ${START_DATE} && ! -n ${STOP_DATE} ]]; then
echo "Usage: START_DATE=CCYYMMDD STOP_DATE=CCYYMMDD [MECHINC=<PATH>] [PROFILE=<PATH>] [OUTPATH=<path>] $0"
echo 
echo 
echo " Required:"
echo "  START_DATE     First day of boundary condition file \(+%Y%m%d format as defined by GNU coreutils\)"
echo "  STOP_DATE      Last day of boundary condition file \(format same as START_DATe\)"
echo 
echo " Optional:"
echo "  MECHINC        Path to CMAQ chemical mechanism namelist files \(*.NML\). Defaults to ./"
echo "  PROFILE        CMAQ formated chemical profile \(i.e., BCON input\). Defaults to ./profile.dat"
echo "  OUTPATH        Full path \(directory and file\) for output file to be written to. Defaults to geos2cmaq.CCYYDD.ncf"
echo
exit
fi
echo 
echo "Reporting variables used (error for first missing variable)"
echo 
echo "  START_DATE=${START_DATE?You must provide START_DATE in the format CCYYMMDD.}"
echo "  STOP_DATE=${STOP_DATE?You must provide STOP_DATE in the format CCYYMMDD.}"
echo
echo "  MECHINC=${MECHINC:=`pwd`/}"
echo "  PROFILE=${PROFILE:=`pwd`/profile.dat}"
echo "  OUTPATH=${OUTPATH:=`pwd`/geos2cmaq.${START_DATE}.ncf}"
OUTDIR=`dirname $OUTPATH`

# Set output directory to current working directory; Make directory if necessary
mkdir -p ${OUTDIR}

# Set output file environmental variable using a name recognized by the executable
export BC_FNAME=${OUTPATH}

# Set location of profile data using name recognized by executable
export BC_PROFILE=${PROFILE}

#############################################################
#  Set IOAPI environmental variables
#############################################################
export EXECUTION_ID=GC2CMAQ
export IOAPI_ISPH=19
export IOAPI_CHECK_HEADERS=Y


GC_NML=$MECHINC/GC_*.nml
AE_NML=$MECHINC/AE_*.nml
NR_NML=$MECHINC/NR_*.nml
TR_NML=$MECHINC/Species_Table_TR_0.nml

ln -s -f $GC_NML    gc_matrix.nml
ln -s -f $AE_NML    ae_matrix.nml
ln -s -f $NR_NML    nr_matrix.nml
ln -s -f $TR_NML    tr_matrix.nml
# test for existence of NML files
if [[ ! -e gc_matrix.nml || ! -e ae_matrix.nml || ! -e nr_matrix.nml || ! -e tr_matrix.nml ]]; then
   echo 'missing namelist file\(s\)'
   echo ${GC_NML} ${AE_NML} ${NR_NML} ${TR_NML}
   ls -lhg ${GC_NML} ${AE_NML} ${NR_NML} ${TR_NML}
   exit
fi


CURDATE=${START_DATE}

if [[ -e "${BC_FNAME}" ]]; then
    rm -f "${BC_FNAME}"
fi

  export START_DATE=`date -d "$CURDATE" +"%Y%j"`
  export START_TIME="000000"
  export STOP_DATE=`date -d "${STOP_DATE}" +"%Y%j"`
  export STOP_TIME="000000"
  export repair_date=`date -d "$EDATE -1 day" +"%Y%m%d"`

  while [ $CURDATE != $EDATE ]
  do
    thisdate=$CURDATE
    thismo=`date -d "$CURDATE" +%m`
    thisjdate=`date -d "${thisdate}" +"%Y%j"`  
    thisdate_nom=`date -d "${thisdate}" +"%y%m%d"`  
    export MET_BDY_3D=${MCIP}"/METBDY3D_"${thisdate_nom}
    if [[ ! -e $MET_BDY_3D ]]; then
        echo "MCIP file ${MET_BDY_3D} could not be found"
        echo "MCIP files should be retrieved from ASM prior to running"
        exit
    fi
    export GT_FILE="BC."${thisdate}
    export GS_FILE="BC.CSPEC."${thisdate}
    if [[ ! -e ../GEOS/${GT_FILE} ]]; then
      echo "GEOS file ../GEOS/${GT_FILE} could not be found"
      echo "GEOS BC files should be retrieved prior to running"
      exit()
    fi
    export GT_FILE="../GEOS/${GT_FILE}"
    if [[ ! -e ../GEOS/${GS_FILE} ]]; then
      echo "GEOS file ../GEOS/${GS_FILE} could not be found"
      echo "GEOS BC files should be retrieved prior to running"
      exit
    fi
    export GS_FILE="../GEOS/${GS_FILE}"
    export GEO_INPUT_PATH="../GEOS/"
    export REPAIR="F"
    if [[ $thisdate == ${repair_date} ]]; then
      export REPAIR="T"
    fi 
    echo $thisdate ${REPAIR}
    ${EXEC}>& ${SDATE}_${thisdate}.log
    CURDATE=`date -d "$CURDATE +1 day" +%Y%m%d`
  done
exit() 

