#!/bin/csh

#PBS -N GADJ_CDDM
#PBS -l walltime=12:00:00
#PBS -l nodes=1:ppn=1
#PBS -W group_list=climsim
#PBS -q climsim
#PBS -d /work/CLIMSIM/hoq/GADJ_CDDM/boundaries/
# BS -V
#PBS -m n
#PBS -j oe
#PBS -o /work/CLIMSIM/hoq/GADJ_CDDM/boundaries/runsample.log

# shell script to run 
#set echo

set  BASE = `pwd`

#set  MCIP_ARCHIVE  = /asm2/MOD3EVAL/met/MCIP_v4.0rel/WRF_2006_35aL/12US1/mcip_out
set  OUTDIR = ${BASE}/
if( ! ( -d ${OUTDIR} ) )mkdir -p ${OUTDIR}

set SDATE=20050101
set EDATE=20060101
#############################################################
#  Set env variables
#############################################################

 setenv EXECUTION_ID  GC2CMAQ
 setenv IOAPI_ISPH  19
 setenv IOAPI_CHECK_HEADERS  Y


 setenv BC_PROFILE  /work/MOD3DEV/hwo/GEOS-CHEM_bctool/supportfiles/bc_profile_v9_22september08.dat
 setenv BC_PROFILE  /home/hwo/data/icbcs/bc_profile_v9-CO2.dat

 #set NML = /work/MOD3EVAL/nsu/boundary/BLD_ddm_saprc07tc/
 set GC_NML = $NML/GC_*.nml
 set AE_NML = $NML/AE_*.nml
 set NR_NML = $NML/NR_*.nml

 #set NML_TR =/home/hwo/cmaq-v5.0/mechanisms
 set TR_NML = $NML/Species_Table_TR_0.nml

 ln -s -f $GC_NML    gc_matrix.nml
 ln -s -f $AE_NML    ae_matrix.nml
 ln -s -f $NR_NML    nr_matrix.nml
 ln -s -f $TR_NML    tr_matrix.nml

# test for existence of NML files
 if( ! -e $GC_NML || ! -e $AE_NML || ! -e $NR_NML || ! -e $TR_NML )then
    echo "missing namelist file(s)"
    exit()
 endif

  set CURDATE=$SDATE
  while ( $CURDATE != $EDATE )
    setenv NEXTDATE `date -d "$CURDATE +1 day" +"%Y%m%d"`
    setenv START_DATE `date -d "$CURDATE" +"%Y%j"`
    setenv START_TIME "000000"
    setenv STOP_DATE `date -d "$NEXTDATE" +"%Y%j"`
    setenv STOP_TIME "000000"

    setenv BC_FNAME  ${OUTDIR}"/geoschem_bc_cb05tucl_ae6_aq."${CURDATE}".ncf"
    echo $BC_FNAME
    if( -e ${BC_FNAME} )\rm -f ${BC_FNAME}
  
    foreach thisdate ($CURDATE $NEXTDATE)
      set thisjdate=`date -d "${thisdate}" +"%Y%j"`  
      set thisdate_nom=`date -d "${thisdate}" +"%y%m%d"`  
      setenv MET_BDY_3D  ${MCIP}"/METBDY3D_"${thisdate_nom}
      if( ! ( -e $MET_BDY_3D ) )then
          echo "Darn"
          exit()
          aget -a ${MCIP} ${MCIP_ARCHIVE}"/METBDY3D_"${thisdate_nom}
      endif
      
      setenv GEO_INPUT_PATH  ${GEOSCHEM}"/"
      setenv GT_FILE "BC."${thisdate}
      setenv GS_FILE "BC.CSPEC."${thisdate}
      if( ! ( -e ./${GT_FILE}) )then
        gunzip -c ${GEO_INPUT_PATH}${GT_FILE}.gz > ./${GT_FILE}
      endif
      if( ! ( -e ./${GS_FILE}) )then
        gunzip -c ${GEO_INPUT_PATH}${GS_FILE}.gz > ./${GS_FILE}
      endif
      setenv GEO_INPUT_PATH  "$PWD/"
      rm -f ${CURDATE}.log
      ${EXEC}>& ${CURDATE}_${thisdate}.log
      if ( $NEXTDATE == $EDATE ) then
        break
      endif
    end
    echo $thisdate $thisjdate $thisdate_nom
    set CURDATE=$NEXTDATE
  end

\rm -f gc_matrix.nml
\rm -f ae_matrix.nml
\rm -f nr_matrix.nml
\rm -f tr_matrix.nml
exit() 

