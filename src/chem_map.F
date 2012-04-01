      SUBROUTINE CHEM_MAP( NBDY, BC1_GT_VERT, BC1_GS_VERT, BC2, 
     &                     JDATE, JTIME )

C***********************************************************************
C
C  FUNCTION: Converts GEO CTM species to CMAQ/SAPRC species 
C              concentrations  
C             
C  Apr 2006 -- Sergey L. Napelenok
C      Modified to describe GEOS-Chem files provided by Daven Henze
C
C  KEY SUBROURINES/FUNCTIONS CALLED:     
C
C***********************************************************************
      
      USE UTILIO_DEFN
      USE GEO_DATA
      USE CMAQ_DATA

      IMPLICIT NONE

C..INCLUDE FILES:

C..ARGUMENTS:
      INTEGER, INTENT( IN ) :: NBDY
      REAL, INTENT( IN )    :: BC1_GT_VERT( :, :, : )
      REAL, INTENT( IN )    :: BC1_GS_VERT( :, :, : )
      REAL, INTENT( INOUT ) :: BC2( :, :, : )
      

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
      REAL, ALLOCATABLE, SAVE :: DENS( :, : ) 
      REAL, ALLOCATABLE, SAVE :: PRES( :, : )
      REAL, ALLOCATABLE, SAVE :: TA  ( :, : )

C SLN - conversion factors
      REAL                    :: MOLTOUGM
      REAL, ALLOCATABLE, SAVE :: MOLTOPPM( :, : )
      REAL, ALLOCATABLE, SAVE :: UGMTOPPM( :, : )

      REAL FAC

      LOGICAL, SAVE :: FIRSTCALL  = .TRUE.

C**********************************************************************
      N = NBDY
      L = NLAYS3D 
      S = NSPC_CMAQ  

      IF( FIRSTCALL )THEN
      
          FIRSTCALL = .FALSE.
          
          ALLOCATE( DENS( NBDY, NLAYS3D ), PRES( NBDY, NLAYS3D ), 
     &                TA( NBDY, NLAYS3D ) )
          
          ALLOCATE( MOLTOPPM( NBDY, NLAYS3D ), UGMTOPPM( NBDY, NLAYS3D ) )

      END IF

C**********************************************************************
C  read air density from MET_BDY_3D file (use for unit conversion)
      IF(.NOT. INTERPB('MET_BDY_3D','DENS', PNAME, JDATE, JTIME,
     &                  N*L, DENS)) then
        IF(.NOT. Read3('MET_BDY_3D','DENS', ALLAYS3, JDATE, JTIME, DENS)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading DENS from MET_BDY_3D file', .TRUE.)
          endif
        Endif
C SLN - read PRES and TA also
      IF(.NOT. INTERPB('MET_BDY_3D','PRES', PNAME, JDATE, JTIME,
     &                  N*L, PRES)) then
        IF(.NOT. Read3('MET_BDY_3D','PRES', ALLAYS3, JDATE, JTIME, PRES)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading PRES from MET_BDY_3D file', .TRUE.)
          endif
        Endif
      IF(.NOT. INTERPB('MET_BDY_3D','TA', PNAME, JDATE, JTIME,
     &                  N*L, TA)) then
        IF(.NOT. Read3('MET_BDY_3D','TA', ALLAYS3, JDATE, JTIME, TA)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading TA from MET_BDY_3D file', .TRUE.)
          endif
        Endif



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Transform GEO species concs to SAPRC species concs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c SLN - common conversion factors

      MOLTOPPM( 1:N, 1:L) = ( 1.0E+12 * 8.314 / 6.022E+23 ) 
     &                      * TA( 1:N, 1:L) / PRES( 1:N, 1:L) ! used in molecules/cm3 to ppm converstion 

      MOLTOUGM = 1.0E+12 / 6.022E+23                          ! used in molecules/cm3 to ug/m3 conversion (needs MW)

      UGMTOPPM( 1:N, 1:L) = 8.314 * TA( 1:N, 1:L) / PRES( 1:N, 1:L)
                                                              ! used in ug/m3 to ppm conversion (needs MW)
#include <MAPPING.MAP>

      RETURN

      End SUBROUTINE CHEM_MAP
