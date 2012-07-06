
      MODULE PROFILE_DATA
      IMPLICIT NONE
      REAL, ALLOCATABLE :: BDY_PF( :, :, :)
      INTEGER ::   NLEVS_IN       ! No. of layers in input conc file
      INTEGER ::   NSPCS_IN       ! No. of layers in input conc file

#include <MAPPING.PROFILE>
      CONTAINS

      SUBROUTINE LoadPROF(LOGUNIT, NBDY)
          USE CMAQ_DATA
          USE UTILIO_DEFN
          IMPLICIT NONE
          
          INTEGER, INTENT( IN  ) :: LOGUNIT, NBDY
          
          CHARACTER(16)  ENV_DFLT       ! Environment variable default value
          CHARACTER(80)  ENV_DESC       ! Environment variable description
          CHARACTER(256) LINEIN         ! Input line
          CHARACTER(80)  MSG            ! Log message
          CHARACTER(16)  PROF_UNITS     ! Environment variable for units
          CHARACTER(16)  SPEC_NAME      ! Species name
          CHARACTER(16)  EDGE_IN      ! Species name
          CHARACTER(16)  PROF_SP      ! Species name
          CHARACTER(256),             SAVE :: PROF_FL_NAME   ! Input profile file name

          CHARACTER(16), SAVE ::  PNAME       = 'LoadPROF' ! Program name
          CHARACTER(16), SAVE ::  BC_PROFILE  = 'BC_PROFILE'       ! Logical name of input profile file
          CHARACTER(16), SAVE ::  PUNITS      = 'PROF_UNITS'       ! Value of units Environment variable
          
          INTEGER, PARAMETER :: NEDGES = 4
          INTEGER  NW, NS, NE        ! North boundary loop indices
          INTEGER  SW, SS, SE        ! South boundary loop indices
          INTEGER  WN, WS, WE        ! West boundary loop indices
          INTEGER  EN, ES, EE        ! East boundary loop indices

          CHARACTER(   5 ), PARAMETER ::    EDGE_NAME( NEDGES ) = ( /
     &                                                 'NORTH' ,
     &                                                 'EAST ' ,
     &                                                 'SOUTH' ,
     &                                                 'WEST '
     &                                                  / )
          INTEGER, PARAMETER :: NORTH=1, EAST=2, SOUTH=3, WEST=4 ! Must be consistent with EDGE_NAME
          REAL VGLVS_IN( MXLAYS3 + 1 )              ! Input vertical levels
          INTEGER PFILE        ! Unit number of profile file    
          INTEGER :: N, STATUS, SPC, IND, L, C

          LOGICAL ::  LERROR, LNEG                     ! Error flag
          
          REAL INPROF( MXLAYS3, 4, MX_INFL_SP )     ! Input conc profiles  
          REAL, ALLOCATABLE, SAVE :: VIPROF( :, :, : )     ! Vertically interp profiles
          
          INTERFACE
           SUBROUTINE VINTERP_PROF( NEDGES, NLAYS_PROF, NSPC_PROF, 
     &                          PROF_LEVS, BC1P, BC2P )
             USE UTILIO_DEFN
             USE CMAQ_DATA
             IMPLICIT NONE
             INTEGER, INTENT( IN ) ::  NEDGES           ! No. of boundary edges
             INTEGER, INTENT( IN ) ::  NSPC_PROF        ! No. of species
             INTEGER, INTENT( IN ) ::  NLAYS_PROF       ! No. of layers
             REAL, INTENT( IN )    ::  PROF_LEVS(  : )
             REAL, INTENT( IN )    ::  BC1P( :, :, : )  ! profile conc bndy concs
             REAL, INTENT( OUT )   ::  BC2P( :, :, : )  ! Interpolated bndy concs
           END SUBROUTINE VINTERP_PROF
          END INTERFACE
          IF (NSPC_DFLT .EQ. 0) RETURN
          ALLOCATE( BDY_PF( NBDY, NLAYS3D, NSPC_PF ) )

          SS = 1
          SE = NCOLS3D + 1
    
          ES = NCOLS3D + 2
          EE = NCOLS3D + NROWS3D + 2
    
          NS = NCOLS3D + NROWS3D + 3
          NE = 2 * NCOLS3D + NROWS3D + 3
    
          WS = 2 * NCOLS3D + NROWS3D + 4
          WN = 2 * ( NCOLS3D + NROWS3D ) + 4
    
          SW = 1
          SE = NCOLS3D + 1
    
          ES = NCOLS3D + 2
          EN = NCOLS3D + NROWS3D + 2
    
          NW = NCOLS3D + NROWS3D + 3
          NE = 2 * NCOLS3D + NROWS3D + 3
    
          WS = 2 * NCOLS3D + NROWS3D + 4
          WN = 2 * ( NCOLS3D + NROWS3D ) + 4

          ENV_DFLT = ' '
          ENV_DESC = 'Input BC Profile file'        
          CALL ENVSTR( BC_PROFILE, ENV_DESC, ENV_DFLT, PROF_FL_NAME, STATUS)

          IF( STATUS .NE. 0 ) THEN
             MSG = 'No BC profile file assigned to BC_PROFILE'
             CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
          ENDIF

          PFILE = JUNIT()

#ifdef verbose
          WRITE( LOGUNIT, * )TRIM( PNAME ) // ' attempting to open BC_PROFILE '
     &    // TRIM( PROF_FL_NAME )
#endif
     
          OPEN(UNIT = PFILE, FILE = PROF_FL_NAME, ERR = 999)

          WRITE( LOGUNIT, 92020) PROF_FL_NAME( 1 : 100 )
          REWIND PFILE
          DO N = 1, 3
             READ( PFILE, 94000 ) LINEIN
#ifdef verbose
             WRITE( LOGUNIT, 92040 ) LINEIN
#endif
          ENDDO

          READ(PFILE, * )NLEVS_IN, NSPCS_IN
          BACKSPACE PFILE 
          READ(PFILE, * )N, C, (VGLVS_IN( L ), L = 1, NLEVS_IN+1)
#ifdef verbose
          WRITE(LOGUNIT, '(i4,1x,i4,50(1x,es12.4))' )NLEVS_IN, NSPCS_IN, (VGLVS_IN( L ), L = 1, NLEVS_IN+1)
#endif
          READ( PFILE, 94000 ) LINEIN
          WRITE( LOGUNIT, 92040 ) LINEIN
          
          LERROR = .FALSE.
          IF( NLEVS_IN .GT. MXLAYS3 + 1 ) THEN
              MSG = 'ERROR: No of layers on profile file exceeds MXLAYS + 1' 
              WRITE(LOGUNIT, 92080) MSG
              LERROR = .TRUE.
          ENDIF

          IF( NSPCS_IN .GT. MX_INFL_SP ) THEN
              MSG = 'ERROR: No of species profile file exceeds  MXINFL_SP' 
              WRITE( LOGUNIT, 92080  ) MSG
              LERROR = .TRUE.
          ENDIF

          IF( LERROR ) THEN
              MSG = 'Dimension problem in BC profile file'
              CALL M3ERR( PNAME, 0, 0, MSG, .TRUE.) 
          ENDIF


          DO N = 1, 4
             READ( PFILE, * )EDGE_IN
    
             CALL UPCASE( EDGE_IN )
    
             IND = INDEX1( EDGE_IN, NEDGES, EDGE_NAME )
    
             DO SPC = 1, NSPCS_IN
               READ(PFILE,*)PROF_SP, (INPROF(L, IND, SPC),L = 1, NLEVS_IN)
               IF (PROF_SP .NE. PF_SPNAME(SPC)) THEN
                  MSG = 'Species expected ' // PF_SPNAME(SPC) // '; received ' // PROF_SP
                  CALL M3ERR( PNAME, 0, 0, MSG, .TRUE.)                
               ENDIF
#ifdef verbose
               WRITE(LOGUNIT,'(i4,1x,a16,50(1x,es12.4))')SPC, PROF_SP, (INPROF(L, IND, SPC),L = 1, NLEVS_IN)
#endif
             ENDDO
          ENDDO
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c  Do the vertical interpolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
           REWIND PFILE
           close(PFILE)
           ALLOCATE( VIPROF( NLAYS3D, 4, MX_INFL_SP ) )
           CALL VINTERP_PROF( NEDGES, NLEVS_IN, NSPCS_IN, VGLVS_IN, 
     &                    INPROF, VIPROF )
           DO SPC = 1, NSPCS_IN
            DO L =1, NLAYS3D
c..South
            DO C = SW, SE
               BDY_PF( C, 1:NLAYS3D, SPC ) = VIPROF( 1:NLAYS3D, SOUTH, IND )
            ENDDO
c..East
            DO C = ES, EN
               BDY_PF( C, 1:NLAYS3D, SPC ) = VIPROF( 1:NLAYS3D, EAST, IND )
            ENDDO
c..North
            DO C = NW, NE
               BDY_PF( C, 1:NLAYS3D, SPC ) = VIPROF( 1:NLAYS3D, NORTH,IND )
            ENDDO
c..West
            DO C = WS, WN
               BDY_PF( C, 1:NLAYS3D, SPC ) = VIPROF( 1:NLAYS3D, WEST, IND )
            ENDDO
            ENDDO
           ENDDO

      RETURN

  999 CONTINUE

      
      MSG = 'Could not open file ' // BC_PROFILE
      STATUS = 2
      CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT( // 1X, 79( '#' ) 
     &         / 1X, '#  Input file section '
     &         / 1X, 79( '#' )
     &        // 5X, 'Boundary Concentrations from an input BC profile ',
     &               'file.' /
     &           5X, 'BCs do not vary with time.' )

92020 FORMAT( //5X, 'Profile file name: ', A 
     &        //5X, 'File contents: '
     &        //1X )


92040 FORMAT( 10X, A100 )

92050 FORMAT(  5X, '---> ', A100 )

92060 FORMAT( //5X, 'End of file contents.')

92080 FORMAT( //1X, A )

92200 FORMAT( //5X, 'Profile concentration units assumed to be ppm' ) 

92220 FORMAT( //5X, 'Profile concentration units assumed to be ppb' ) 

94000 FORMAT( A )

94020 FORMAT('Negative BC on ',A5,' Edge for ',A16,' at lay = ',I3)

      END SUBROUTINE
      END MODULE PROFILE_DATA