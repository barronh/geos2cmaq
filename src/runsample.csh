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
set echo

set  BASE = /work/CLIMSIM/hoq/GADJ_CDDM/
set  GEOSCHEM = /work/MOD3EVAL/afd/GEOS-Chem_v8/BC_2x25_NA/2008
set  MCIP = /work/MOD3EVAL/nsu/boundary/input/met

#set  MCIP_ARCHIVE  = /asm2/MOD3EVAL/met/MCIP_v4.0rel/WRF_2006_35aL/12US1/mcip_out
set  EXEC = ${BASE}/temp/GC2CMAQ_SAPRC07T.exe
set  OUTDIR = ${BASE}/boundaries/output
if( ! ( -d ${OUTDIR} ) )mkdir -p ${OUTDIR}


#############################################################
#  Set env variables
#############################################################

 set DOM01 = "01 02 03"
 set DOM28 = "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28"
 set DOM29 = ( ${DOM28} "29" )
 set DOM30 = ( ${DOM29} "30" )
 set DOM31 = ( ${DOM30} "31" )
 set DOMXX = "11 12 13 14 15 16 17 18 19 20 21 22 23 24 25"
 
 set JUL_START = ( "00" "31" "59" "90" "120" "151" "181" "212" "243" "273" "304" "334" )
 
 set DAYS_MONTH = ( "31" "28" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31" )
 
 set MONTHS = "04"
 
 set YEAR = 2008
 set BC_YEAR = ${YEAR}
 set YR = 08
 set LEAP_YR = "T"

 if( ${LEAP_YR} == "T" )then
     set JUL_START = ( "00" "31" "60" "91" "121" "152" "182" "213" "244" "274" "305" "335" )
     set DAYS_MONTH = ( "31" "29" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31" )
     set TOTAL_DAYS = 366
 else
     set JUL_START = ( "00" "31" "59" "90" "120" "151" "181" "212" "243" "273" "304" "334" )
     set DAYS_MONTH = ( "31" "28" "31" "30" "31" "30" "31" "31" "30" "31" "30" "31" )
     set TOTAL_DAYS = 365
 endif

 setenv EXECUTION_ID  GC2CMAQ
 setenv IOAPI_ISPH  19
 setenv IOAPI_CHECK_HEADERS  Y

 setenv BPCH_YEAR  ${YEAR}

 setenv BC_PROFILE  /work/MOD3DEV/hwo/GEOS-CHEM_bctool/supportfiles/bc_profile_v9_22september08.dat
 setenv BC_PROFILE  /home/hwo/data/icbcs/bc_profile_v9-CO2.dat

 set NML    = "."
 #set NML = /work/MOD3EVAL/nsu/boundary/BLD_ddm_saprc07tc/
 set GC_NML = $NML/GC_saprc07tc_ae6_aq.nml
 set AE_NML = $NML/AE_cb05cl_ae5_aq.nml
 set NR_NML = $NML/NR_saprc07tc_ae6_aq.nml

 set NML_TR = "."
 #set NML_TR =/home/hwo/cmaq-v5.0/mechanisms
 set TR_NML = $NML_TR/Species_Table_TR_0.nml

 ln -s -f $GC_NML    gc_matrix.nml
 ln -s -f $AE_NML    ae_matrix.nml
 ln -s -f $NR_NML    nr_matrix.nml
 ln -s -f $TR_NML    tr_matrix.nml

# test for existence of NML files
 set eflag = 'F'

 if( ! -e $GC_NML )then
     ls $GC_NML
     set eflag = 'T'
 endif

 if( ! -e $AE_NML )then
     ls $AE_NML
     set eflag = 'T'
 endif

 if( ! -e $NR_NML )then
     ls $NR_NML
     set eflag = 'T'
 endif

 if( ! -e $TR_NML  )then
     ls $TR_NML
     set eflag = 'T'
 endif

 if( $eflag ==  'T' )then
    echo "missing namelist file(s)"
    exit()
 endif

foreach MO ( ${MONTHS} )

  #foreach DAY ( $DOM30 )
  foreach DAY ( 30 )
    
    set day = `expr $DAY + 0 `
    set  mo = `expr $MO + 0 `
   if( $day > $DAYS_MONTH[$mo] )break

    setenv BC_FNAME  ${OUTDIR}"/geoschem_bc_saprc07tc_ae5_aq_34L."${BC_YEAR}${MO}${DAY}".ncf"
   if( -e ${BC_FNAME} )\rm -f ${BC_FNAME}
    
    @ start_date = $JUL_START[$mo] + $day + $BC_YEAR * 1000 

    setenv START_DATE ${start_date}
    setenv START_TIME "000000"

    if( ( $day + $JUL_START[$mo] ) == $TOTAL_DAYS )then
        @ stop_date = 1 + 1000 + $BC_YEAR * 1000
    else
        @ stop_date = $start_date + 1
    endif

    setenv STOP_DATE  ${stop_date}
    setenv STOP_TIME  "000000"
    echo ${STOP_DATE} ${STOP_TIME}
    \rm -f {BC_FNAME}
  
#    @ day = $DAY + 1
    
    set day = `expr $DAY + 1 `
    
   if( $day > $DAYS_MONTH[$mo] )then
        set next_day = "01"
   else
        set next_day = `echo $day | awk '{printf("%2.2d",$1)}'`
   endif      

    
     set previous_day = -1
    foreach day ( ${DAY} ${next_day} ) 

        set current_day = `expr $day + 0 `
      if( $previous_day > $current_day )then
            @ mo =  $mo + 1
            set month = `echo $mo | awk '{printf("%2.2d",$1)}'`
      else
            set month = $MO
      endif

      if( $mo == "13" )then
          echo "reached end of the calendar year"
             set month = "01"
#          exit()
      endif
        
      setenv MET_BDY_3D  ${MCIP}"/METBDY3D_"${YR}${month}${day}
      if( ! ( -e $MET_BDY_3D ) )then
          echo "Darn"
          exit()
          aget -a ${MCIP} ${MCIP_ARCHIVE}"/METBDY3D_"${YR}${month}${day}
#         set EXTN = ${YR}${month}${day}
#         qsub -v APPL=${EXTN} /work/MOD3DEV/hwo/GEOS-CHEM_bctool/get_met.csh 
      endif
      
      setenv GEO_INPUT_PATH  ${GEOSCHEM}"/"
      setenv GT_FILE "BC."${YEAR}${month}${day}
      setenv GS_FILE "BC.CSPEC."${YEAR}${month}${day}
      if( ! ( -e ./${GT_FILE}) )then
        gunzip -c ${GEO_INPUT_PATH}${GT_FILE}.gz > ./${GT_FILE}
      endif
      if( ! ( -e ./${GS_FILE}) )then
        gunzip -c ${GEO_INPUT_PATH}${GS_FILE}.gz > ./${GS_FILE}
      endif
      #setenv GT_FILE ${PWD}/${GT_FILE}
      #setenv GS_FILE ${PWD}/${GS_FILE}
      setenv GEO_INPUT_PATH  "$PWD/"
      ${EXEC}

      set previous_day = $current_day
    end
  end
end  

\rm -f gc_matrix.nml
\rm -f ae_matrix.nml
\rm -f nr_matrix.nml
\rm -f tr_matrix.nml
exit() 

