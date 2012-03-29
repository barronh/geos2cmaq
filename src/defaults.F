      SUBROUTINE DEFAULTS( LOGUNIT, NBDY, BC3D )

C***********************************************************************
C
C  FUNCTION: Gets BCs for species not in GEO files useing CMAQ 
C            "clean air" default profiles
C             
C  PRECONDITIONS: 
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:     
C
C  REVISION HISTORY: Created October, 2004 by J Gipson
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
      INTEGER      :: LOGUNIT
      REAL         :: BC3D( NBDY, NLAYS3D, NSPC_DFLT )
      
C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'DEFAULTS'
      CHARACTER( 16 ), PARAMETER :: BC_PROFILE = 'BC_PROFILE'

      INTEGER, PARAMETER         :: NEDGES = 4


C..EXTERNAL FUNCTIONS:
      INTEGER    GETEFILE
      INTEGER    INDEX1
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 256 ) LINEIN         ! Input line
      CHARACTER(  80 ) MSG            ! Log message
      CHARACTER(  16 ) SPEC_IN        !
      CHARACTER(   5 ) EDGE_IN        !

      CHARACTER(   5 ) ::    EDGE_NAME( NEDGES ) = ( /
     &                                                 'NORTH' ,
     &                                                 'EAST ' ,
     &                                                 'SOUTH' ,
     &                                                 'WEST '
     &                                                  / )

      INTEGER  ::  E, L, N, S
      INTEGER  ::  IND, IND2
      INTEGER  ::  PFILE
      INTEGER  ::  JDATE = 0
      INTEGER  ::  JTIME = 0

      INTEGER  ::  NLAYS_PROF
      INTEGER  ::  NSPC_PROF

      INTEGER  ::  NORTH, EAST, SOUTH, WEST

      INTEGER  ::  SW, SE, ES, EN, NW, NE, WS, WN

      LOGICAL  ::  LRDONLY
      LOGICAL  ::  LFORMTD
      LOGICAL  ::  LERROR

      CHARACTER( 16 ), ALLOCATABLE  :: PROF_SP_NAME( : )

      REAL, ALLOCATABLE :: PROF_LEVS( : )

      REAL, ALLOCATABLE :: BC1P( : , : , : )
      REAL, ALLOCATABLE :: BC2P( : , : , : )
      REAL, ALLOCATABLE :: BC3P( : , : , : )

C**********************************************************************

      


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open and read the input profile file
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LRDONLY = .TRUE.
      LFORMTD = .TRUE.
      PFILE = GETEFILE( BC_PROFILE, LRDONLY, LFORMTD, PNAME )
      IF( PFILE .LT. 0 ) THEN
         MSG = 'ERROR: Could not open BC_PROFILE'
         CALL M3ERR (PNAME, JDATE, JTIME, MSG, .TRUE. )
      ENDIF

      DO N = 1, 3
         READ( PFILE, 94000 ) LINEIN
      ENDDO

      READ( PFILE, 94000 ) LINEIN

      write( *, '(A)') LINEIN

      READ( LINEIN, * ) NLAYS_PROF, NSPC_PROF

      ALLOCATE( PROF_LEVS( NLAYS_PROF + 1 ) )

      READ( LINEIN, * ) NLAYS_PROF, NSPC_PROF, 
     &                 ( PROF_LEVS( L ), L = 1, NLAYS_PROF + 1 )

 
      READ( PFILE, 94000 ) LINEIN

      ALLOCATE( BC1P( NEDGES, NLAYS_PROF, NSPC_PROF ) )
      ALLOCATE( PROF_SP_NAME( NSPC_PROF ) )

      LERROR = .FALSE.
      DO E = 1, NEDGES

         READ( PFILE, * ) EDGE_IN

         CALL UPCASE( EDGE_IN )

         IND = INDEX1( EDGE_IN, NEDGES, EDGE_NAME )
         IF( IND .EQ. 0 ) THEN
            MSG = 'ERROR: Invalid edge name: ' //
     &       EDGE_IN( 1 : LEN_TRIM( EDGE_IN ) ) 
            CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. ) 
         ENDIF

         DO S = 1, NSPC_PROF
            READ( PFILE, * ) SPEC_IN, ( BC1P( IND, L, S ), L = 1, NLAYS_PROF ) 
            IF( E .EQ. 1 ) THEN
               PROF_SP_NAME( S ) = SPEC_IN
            ELSE
               IND2 = INDEX1( SPEC_IN, NSPC_PROF, PROF_SP_NAME )
               IF( IND2 .EQ. 0 ) THEN
                  WRITE( LOGUNIT, 92000 ) SPEC_IN
                  LERROR = .TRUE.
               ENDIF
            ENDIF

         ENDDO

      ENDDO
                   
      IF( LERROR ) THEN
         MSG = 'ERROR: Stopping because of inconsistent species names on ' //
     &         ' profile file '
         CALL M3ERR (PNAME, JDATE, JTIME, MSG, .TRUE. )
      ENDIF


      CLOSE( PFILE )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Vertically interpolate profile concs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( BC2P( NEDGES, NLAYS3D, NSPC_PROF  ) )

      CALL VINTERP_PROF( NEDGES, NLAYS_PROF, NSPC_PROF, PROF_LEVS, 
     &                   BC1P, BC2P )

      DEALLOCATE( BC1P )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Transform profile species to CMAQ species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( BC3P( NEDGES, NLAYS3D, NSPC_DFLT ) )

      CALL CHEM_MAP_PROF( LOGUNIT, NEDGES, NSPC_PROF, BC2P, BC3P, 
     &                    PROF_SP_NAME )

      DEALLOCATE( BC2P )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Assign the edge boundary concs to the cell boundary concs
c  Assumes NTHIK=1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SW = 1
      SE = NCOLS3D + 1

      ES = NCOLS3D + 2
      EN = NCOLS3D + NROWS3D + 2

      NW = NCOLS3D + NROWS3D + 3
      NE = 2 * NCOLS3D + NROWS3D + 3

      WS = 2 * NCOLS3D + NROWS3D + 4
      WN = 2 * ( NCOLS3D + NROWS3D ) + 4


      SOUTH = INDEX1( 'SOUTH', NEDGES, EDGE_NAME )
      EAST  = INDEX1( 'EAST ', NEDGES, EDGE_NAME )
      NORTH = INDEX1( 'NORTH', NEDGES, EDGE_NAME )
      WEST  = INDEX1( 'WEST ', NEDGES, EDGE_NAME )
 
      L = NLAYS3D
      S = NSPC_DFLT

      DO S = 1, NSPC_DFLT
         DO L = 1, NLAYS3D
            DO N = SW, SE
               BC3D( N, L, S ) = BC3P( SOUTH, L, S )
            ENDDO
            DO N = ES, EN
               BC3D( N, L, S ) = BC3P( EAST, L, S )
            ENDDO
            DO N = NW, NE
               BC3D( N, L, S ) = BC3P( NORTH, L, S )
            ENDDO
            DO N = WS, WN
               BC3D( N, L, S ) = BC3P( WEST, L, S )
            ENDDO
         ENDDO
      ENDDO

      RETURN

94000 FORMAT( A )

92000 FORMAT( 'ERROR: Default profile species out of order - ', A )
      END
