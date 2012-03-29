      SUBROUTINE VINTERP( LOGUNIT, NBDY, BDY_GEO, NSPC_GEO, BDY1 )

C***********************************************************************
C
C  FUNCTION: Does vertical interpolation of layer concentrations 
C             
C  PRECONDITIONS: Assumes species order listed in HGCTM_DATA module
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:     
C
C  REVISION HISTORY: Created October, 2004 by J Gipson using parts of
C                      the geos2cmaq BCON program originally created
C                      by Nankyoung Moon IMAQS, University of Houston,
C                      November, 2002; also uses parts of CMAQ BCON
C                      pre-processor
C
C***********************************************************************
      USE GEO_DATA
      USE CMAQ_DATA

      IMPLICIT NONE

C..INCLUDE FILES:
      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
      INCLUDE SUBST_IOFDESC     ! IOAPI file description
      INCLUDE SUBST_IODECL      ! IOAPI declarations

C..ARGUMENTS:

      INTEGER  LOGUNIT      ! Unit number for output log
      INTEGER  NBDY         ! No. of boundary cells

      INTEGER  NSPC_GEO

      REAL BDY_GEO( NBDY, N_GEO_LAYS, NSPC_GEO )  ! GEO conc bndy concs
      REAL BDY1(    NBDY, NLAYS3D,    NSPC_GEO )  ! Interpolated bndy concs

C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'VINTERP'       ! Program Name
      LOGICAL, PARAMETER         :: L_INT_BY_PRESS = .TRUE. ! Interp flag
      LOGICAL, PARAMETER         :: L_RATINT = .FALSE.      ! Rat Interp flag

      REAL, PARAMETER            :: PREF = 1000.0           ! Ref sfc press

C..EXTERNAL FUNCTIONS:
 
C..SAVED LOCAL VARIABLES:

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80  MSG            ! Log message

      INTEGER L              ! Loop index for vertical layers
      INTEGER N              ! Loop index for boundary cells
      INTEGER S              ! Loop index for species
      INTEGER NLAYS2USE      ! No. og GEO layers to use in interpolation

      LOGICAL LDEC           ! Flag for monotonic decreasing layer levels
      LOGICAL LINC           ! Flag for monotonic increasing layer levels

      REAL DELY  ! Error estimate for conc interpolated by rational func
      REAL PSTAR ! Ref press minus top press
      REAL X3    ! Vertical coordinate used in interpolation
      REAL Y     ! Interpolated concentration

      REAL CMAQ_PRESS( NLAYS3D + 1 )
      REAL GEO_PRESS( N_GEO_LAYS + 1 )

      REAL, ALLOCATABLE  :: WORKA(  : )   ! Work array for conc input
      REAL, ALLOCATABLE  :: X3_OLD( : )   ! GEO vertical levels
      REAL, ALLOCATABLE  :: X3_NEW( : )   ! CMAQ vertical levels

 
C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Set-up for vertical interpolation using approximated pressures
c  Only option currently available
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LDEC = .FALSE.
      LINC = .FALSE.

      IF( L_INT_BY_PRESS ) THEN

c..set CMAQ pressure levels assuming PREF
         PSTAR = PREF - VGTOP3D * 0.01
         DO L = 1, NLAYS3D + 1
            CMAQ_PRESS( L ) = VGLVS3D( L ) * PSTAR + VGTOP3D * 0.01
         ENDDO

      
c..set GEO pressure levels assuming PREF
         PSTAR = PREF - GEO_TOP
         DO L = 1, N_GEO_LAYS + 1
            GEO_PRESS( L ) = GEO_SIGMA( L ) * PSTAR + GEO_TOP
         ENDDO
      
c..Do the interpolation
         NLAYS2USE = N_GEO_LAYS
c        NLAYS2USE = 5
         ALLOCATE( WORKA( NLAYS2USE ) )
         ALLOCATE( X3_OLD( NLAYS2USE ) )
         ALLOCATE( X3_NEW( NLAYS3D ) )


         DO L = 1, NLAYS2USE
            X3_OLD( L ) = 0.5 * ( GEO_PRESS( L ) + GEO_PRESS( L + 1 ) )
         ENDDO

         DO L = 1, NLAYS3D
            X3_NEW( L ) = 0.5 * ( CMAQ_PRESS( L ) + CMAQ_PRESS( L + 1 ) )
         ENDDO

         LDEC = .TRUE.

      ENDIF      

c     WRITE( LOGUNIT, 92000 )  GEO_TOP, PREF, PSTAR
c     WRITE( LOGUNIT, 92010 )  NLAYS2USE
c     WRITE( LOGUNIT, 92080 )
c     DO L = 1, N_GEO_LAYS + 1
c       WRITE( LOGUNIT, 92020 ) L-1, GEO_SIGMA( L ), GEO_PRESS(L)
c     ENDDO


c     WRITE( LOGUNIT, 92040 )  0.01 * VGTOP3D, PREF, PSTAR
c     WRITE( LOGUNIT, 92080 )
c     DO L = 1, NLAYS3D + 1
c       WRITE( LOGUNIT, 92060 ) L-1, VGLVS3D( L ), CMAQ_PRESS( L )
c     ENDDO

      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Generic interpolation section; X3_OLD and X3_NEW must have been set
c    above; normally they would be set to pressures, heights, or sigma
c    levels
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO S = 1, NSPC_GEO

         DO N = 1, NBDY

            DO L = 1, NLAYS2USE
               WORKA( L ) = BDY_GEO( N, L, S )
            ENDDO

            DO L = 1, NLAYS3D

               IF( NLAYS2USE .EQ. 1 ) THEN

                  BDY1( N, L, S ) = WORKA( 1 )

               ELSE

                  X3 = X3_NEW( L )

                  IF( LINC .AND. X3 .LE. X3_OLD( 1 ) ) THEN
                     BDY1( N, L, S ) = WORKA( 1 )
                  ELSEIF( LDEC .AND. X3 .GE. X3_OLD( 1 ) ) THEN
                     BDY1( N, L, S ) = WORKA( 1 )
                  ELSEIF( LINC .AND. X3 .GE. X3_OLD( NLAYS2USE ) ) THEN
                     BDY1( N, L, S ) = WORKA( NLAYS2USE )
                  ELSEIF( LDEC .AND. X3 .LE. X3_OLD( NLAYS2USE ) ) THEN
                     BDY1( N, L, S ) = WORKA( NLAYS2USE )
                  ELSE
                     CALL LR_INTERP( L_RATINT, X3_OLD, WORKA, NLAYS2USE,
     &                                  X3, Y, DELY )
                     BDY1( N, L, S ) = Y
                  ENDIF

               ENDIF

            ENDDO
                  
         ENDDO
      ENDDO

      DEALLOCATE( WORKA )
      DEALLOCATE( X3_OLD )
      DEALLOCATE( X3_NEW )

      RETURN

92000 FORMAT( /'GEO Vertical Structure:'
     &        /2X, 'PTOP=', F10.2, '  PREF=', F10.2, '  PSTAR=', F10.2 )
92010 FORMAT(  2X, 'Number of GEO layers used in interpolation = ', I3 )
92080 FORMAT( '  Lev     Sigma  Pressure (mb)' )
92020 FORMAT( 2X, I3, F10.5, 5X, F10.2 )
92040 FORMAT( /'CMAQ Vertical Structure:'
     &        /2X, 'PTOP=', F10.2, '  PREF=', F10.2, '  PSTAR=', F10.2 )
92060 FORMAT( 2X, I3, F10.5, 5X, F10.2 )

      END
        
