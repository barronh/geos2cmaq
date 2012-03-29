      SUBROUTINE CHEM_MAP( NBDY, BC1_GT_VERT, BC1_GS_VERT, BC2, 
     &                     JDATE, JTIME )

C***********************************************************************
C
C  FUNCTION: Converts GEO CTM species to CMAQ/SAPRC species 
C              concentrations  
C             
C  Apr 2008 -- Sergey L. Napelenok
C      Modified to describe GEOS-Chem files provided by Daven Henze
C  Sep 2008 -- Sergey L. Napelenok
C      Version for CB05CL_AE5_AQ mechanism
C
C  KEY SUBROURINES/FUNCTIONS CALLED:     
C
C***********************************************************************
      USE GEO_DATA
      USE CMAQ_DATA

      IMPLICIT NONE

C..INCLUDE FILES:
      INCLUDE SUBST_IOPARMS
      INCLUDE SUBST_IOFDESC
      INCLUDE SUBST_IODECL


C..ARGUMENTS:
      INTEGER      :: NBDY
      REAL         :: BC1_GT_VERT( NBDY, NLAYS3D, NSPC_GT )
      REAL         :: BC1_GS_VERT( NBDY, NLAYS3D, NSPC_GS )
      REAL         :: BC2( NBDY, NLAYS3D, NSPC_CMAQ )
      

C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'CHEM_MAP'

C..EXTERNAL FUNCTIONS:
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      INTEGER  C, R, L, N, S
      INTEGER  JDATE
      INTEGER  JTIME
      LOGICAL  LERROR

C SLN - add PRES and TA in addition to DENS
      REAL :: DENS( NBDY, NLAYS3D ) 
      REAL :: PRES( NBDY, NLAYS3D )
      REAL :: TA  ( NBDY, NLAYS3D )

C SLN - conversion factors
      REAL :: MOLTOPPM( NBDY, NLAYS3D )
      REAL :: MOLTOUGM
      REAL :: UGMTOPPM( NBDY, NLAYS3D )

      REAL FAC

C**********************************************************************
      N = NBDY
      L = NLAYS3D 
      S = NSPC_CMAQ  


C**********************************************************************
C  read air density from MET_BDY_3D file (use for unit conversion)
      IF(.NOT. INTERP3('MET_BDY_3D','DENS', PNAME, JDATE, JTIME,
     &                  N*L, DENS)) then
        IF(.NOT. Read3('MET_BDY_3D','DENS', ALLAYS3, JDATE, JTIME, DENS)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading DENS from MET_BDY_3D file', .TRUE.)
          endif
        Endif
C SLN - read PRES and TA also
      IF(.NOT. INTERP3('MET_BDY_3D','PRES', PNAME, JDATE, JTIME,
     &                  N*L, PRES)) then
        IF(.NOT. Read3('MET_BDY_3D','PRES', ALLAYS3, JDATE, JTIME, PRES)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading PRES from MET_BDY_3D file', .TRUE.)
          endif
        Endif
      IF(.NOT. INTERP3('MET_BDY_3D','TA', PNAME, JDATE, JTIME,
     &                  N*L, TA)) then
        IF(.NOT. Read3('MET_BDY_3D','TA', ALLAYS3, JDATE, JTIME, TA)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading TA from MET_BDY_3D file', .TRUE.)
          endif
        Endif



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Transform GEO species concs to CB05CL_AE_AQ species concs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c SLN - common conversion factors

      MOLTOPPM( 1:N, 1:L) = ( 1.0E+12 * 8.314 / 6.022E+23 ) 
     &                      * TA( 1:N, 1:L) / PRES( 1:N, 1:L) ! used in molecules/cm3 to ppm converstion 

      MOLTOUGM = 1.0E+12 / 6.022E+23                          ! used in molecules/cm3 to ug/m3 conversion (needs MW)

      UGMTOPPM( 1:N, 1:L) = 8.314 * TA( 1:N, 1:L) / PRES( 1:N, 1:L)
                                                              ! used in ug/m3 to ppm conversion (needs MW)
    
c SLN - gas
#include <MAPPING.MAP>
      RETURN

      End SUBROUTINE CHEM_MAP
