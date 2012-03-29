      SUBROUTINE VINTERP_PROF( NEDGES, NLAYS_PROF, NSPC_PROF, 
     &         PROF_LEVS, BC1P, BC2P )

C***********************************************************************
C
C  FUNCTION: Does vertical interpolation of layer concentrations for
C             default profile 
C             
C  PRECONDITIONS: Assumes vertical cooredinate types are consistent
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:     
C
C  REVISION HISTORY: Created October, 2004 by J Gipson
C
C***********************************************************************
      USE CMAQ_DATA

      IMPLICIT NONE

C..INCLUDE FILES:
      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
      INCLUDE SUBST_IOFDESC     ! IOAPI file description
      INCLUDE SUBST_IODECL      ! IOAPI declarations

C..ARGUMENTS:

      INTEGER  NEDGES         ! No. of boundary edges
      INTEGER  NSPC_PROF      ! No. of species
      INTEGER  NLAYS_PROF     ! No. of layers

      REAL PROF_LEVS( * )

      REAL BC1P( NEDGES, NLAYS_PROF, NSPC_PROF )  ! GEO conc bndy concs
      REAL BC2P( NEDGES, NLAYS3D,    NSPC_PROF )  ! Interpolated bndy concs

C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'VINTERP_PROF'  ! Program Name
      LOGICAL, PARAMETER         :: L_RATINT = .FALSE.      ! Rat Interp flag

C..EXTERNAL FUNCTIONS:
 
C..SAVED LOCAL VARIABLES:

C..SCRATCH LOCAL VARIABLES:
      CHARACTER*80  MSG            ! Log message

      INTEGER L              ! Loop index for vertical layers
      INTEGER N              ! Loop index for boundary cells
      INTEGER S              ! Loop index for species
      INTEGER E

      LOGICAL LDEC           ! Flag for monotonic decreasing layer levels
      LOGICAL LINC           ! Flag for monotonic increasing layer levels

      REAL DELY  ! Error estimate for conc interpolated by rational func
      REAL X3    ! Vertical coordinate used in interpolation
      REAL Y     ! Interpolated concentration


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

      IF( NLAYS3D .EQ. NLAYS_PROF ) THEN

         DO E = 1, NEDGES
            DO L = 1, NLAYS3D
               DO N = 1, NSPC_PROF
                  BC2P( E, L, S ) = BC1P( E, L, S )
               ENDDO
            ENDDO

         ENDDO

         RETURN

      ENDIF
      
      ALLOCATE( X3_OLD( NLAYS_PROF ) )
      ALLOCATE( X3_NEW( NLAYS3D ) )


      DO L = 1, NLAYS3D
         X3_NEW( L ) = 0.5 * ( VGLVS3D ( L ) +  VGLVS3D ( L + 1 ) )
      ENDDO

      DO L = 1, NLAYS_PROF
         X3_OLD( L ) = 0.5 * ( PROF_LEVS ( L ) +  PROF_LEVS ( L + 1 ) )
      ENDDO

      LDEC = .TRUE.




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Generic interpolation section; X3_OLD and X3_NEW set above
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( WORKA( NLAYS_PROF ) ) 


      DO S = 1, NSPC_PROF

         DO N = 1, NEDGES

            DO L = 1, NLAYS_PROF
               WORKA( L ) = BC1P( N, L, S )
            ENDDO

            DO L = 1, NLAYS3D

               IF( NLAYS_PROF .EQ. 1 ) THEN

                  BC2P( N, L, S ) = WORKA( 1 )

               ELSE

                  X3 = X3_NEW( L )

                  IF( LINC .AND. X3 .LE. X3_OLD( 1 ) ) THEN
                     BC2P( N, L, S ) = WORKA( 1 )
                  ELSEIF( LDEC .AND. X3 .GE. X3_OLD( 1 ) ) THEN
                     BC2P( N, L, S ) = WORKA( 1 )
                  ELSEIF( LINC .AND. X3 .GE. X3_OLD( NLAYS_PROF ) ) THEN
                     BC2P( N, L, S ) = WORKA( NLAYS_PROF )
                  ELSEIF( LDEC .AND. X3 .LE. X3_OLD( NLAYS_PROF ) ) THEN
                     BC2P( N, L, S ) = WORKA( NLAYS_PROF )
                  ELSE
                     CALL LR_INTERP( L_RATINT, X3_OLD, WORKA, NLAYS_PROF,
     &                                  X3, Y, DELY )
                     BC2P( N, L, S ) = Y
                  ENDIF

               ENDIF

            ENDDO
                  
         ENDDO
      ENDDO

      DEALLOCATE( WORKA )
      DEALLOCATE( X3_OLD )
      DEALLOCATE( X3_NEW )

      RETURN

      END
        
