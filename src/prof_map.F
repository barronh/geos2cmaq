      SUBROUTINE CHEM_MAP_PROF( LOGDEV, NEDGES, NSPC_PROF, BC2P, BC3P,
     &                          PROF_SP_NAME )

C***********************************************************************
C
C  FUNCTION: Converts CMAQ "clean air" default species concentrations to
C            CMAQ/SAPRC concentrations  

C  Apr 2008 -- Sergey L. Napelenok
C      Modified to describe GEOS-Chem files provided by Daven Henze
C  Sep 2008 -- Sergey L. Napelenok
C      Version for CB05CL_AE5_AQ mechanism
C
C
C  PRECONDITIONS: Assumes species order listed in GEO_DATA module
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:     
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
      INTEGER      :: LOGDEV
      INTEGER      :: NEDGES
      INTEGER      :: NSPC_PROF

      CHARACTER( * ) :: PROF_SP_NAME( NSPC_PROF ) 

      REAL         :: BC2P( NEDGES, NLAYS3D, NSPC_PROF )
      REAL         :: BC3P( NEDGES, NLAYS3D, NSPC_DFLT )
      

C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'CHEM_MAP_PROF'

C..EXTERNAL FUNCTIONS:
      INTEGER   INDEX1
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 80 )   ::   MSG

      INTEGER  L, N, S
      INTEGER  JDATE
      INTEGER  JTIME
      LOGICAL  LERROR

C**********************************************************************


      JDATE = 0
      JTIME = 0


c..set-up indices to use for profile species
      LERROR = .FALSE.

        
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Transform PROFILE species concs to SAPRC species concs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      N = NEDGES
      L = NLAYS3D

#include <MAPPING.PROFILE>
      RETURN

92000 FORMAT( 'ERROR: Could not find species no. ', I2, ', ', A , 
     &        ' in default profile file' )


      END
