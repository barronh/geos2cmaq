      SUBROUTINE CHEM_MAP( NBDY, BC1_GT_VERT, BC1_GS_VERT, BC1_PF_VERT, 
     &                     BC2, JDATE, JTIME )

C***********************************************************************
C
C  FUNCTION: Converts GEO CTM species to CMAQ/SAPRC species 
C              concentrations  
C             
C  Apr 2006 -- Sergey L. Napelenok
C      Modified to describe GEOS-Chem files provided by Daven Henze
C  Jun 2013 -- Farhan H. Akhtar
C      Modified to produce initial conditions
C
C  KEY SUBROURINES/FUNCTIONS CALLED:     
C
C***********************************************************************
      
      USE UTILIO_DEFN
      USE GEO_DATA
      USE CMAQ_DATA
      USE PROFILE_DATA

      IMPLICIT NONE

C..INCLUDE FILES:

C..ARGUMENTS:
      INTEGER, INTENT( IN ) :: NBDY
      REAL, INTENT( IN )    :: BC1_GT_VERT( :, :, : )
      REAL, INTENT( IN )    :: BC1_GS_VERT( :, :, : )
      REAL, INTENT( IN )    :: BC1_PF_VERT( :, :, : )
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
      CHARACTER(  6 )   ::  XCON       ! Type of output
      CHARACTER( 16 )   ::  INFILE     ! Name of met file

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
C     Check if initial conditions are requested, set input file accordingly
      CALL getenv( 'XCON', XCON )

      IF ( XCON .EQ. 'ICON' ) THEN
          INFILE = 'MET_CRO_3D'
!         IF ( .NOT. OPEN3( INFILE, FSREAD3, PNAME ) ) THEN
!            MSG = 'Could not open ' // INFILE // ' file'
!         ENDIF
      ELSE
          INFILE = 'MET_BDY_3D'
!         IF ( .NOT. OPEN3( INFILE, FSREAD3, PNAME ) ) THEN
!            MSG = 'Could not open ' // INFILE // ' file'
!         ENDIF
      ENDIF

C**********************************************************************
C  read air density from INPUT file (use for unit conversion)
      IF(.NOT. INTERPB(INFILE,'DENS', PNAME, JDATE, JTIME,
     &                  N*L, DENS)) then
        IF(.NOT. Read3(INFILE,'DENS', ALLAYS3, JDATE, JTIME, DENS)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading DENS from ' // INFILE // ' file', .TRUE.)
          endif
        Endif
C SLN - read PRES and TA also
      IF(.NOT. INTERPB(INFILE,'PRES', PNAME, JDATE, JTIME,
     &                  N*L, PRES)) then
        IF(.NOT. Read3(INFILE,'PRES', ALLAYS3, JDATE, JTIME, PRES)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading PRES from ' // INFILE // ' file', .TRUE.)
          endif
        Endif
      IF(.NOT. INTERPB(INFILE,'TA', PNAME, JDATE, JTIME,
     &                  N*L, TA)) then
        IF(.NOT. Read3(INFILE,'TA', ALLAYS3, JDATE, JTIME, TA)) then
          Call m3err(pname, JDATE, JTIME,
     &      'Reading TA from ' // INFILE // ' file', .TRUE.)
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
