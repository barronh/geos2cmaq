
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/BCON/src/input/profile/prof_driver.F,v 1.6 2002/04/12 14:19:25 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE CMAQ_PROFILE_BDY( LOGUNIT, N_BDY, BDY_OUT )   

C*************************************************************************
C
C   FUNCTION: Opens the input profile data file, and calls the routine to 
C             compute the BCs and return for the output BC file  
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: GC_DPC_MAP
C                                    NGC_SPC_MAP
C                                    LST_SPC_MAP

C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 1998
C                    Modified April 1998 by JG to remove ppb option for 
C                       input profile file
C
C*************************************************************************

      USE UTILIO_DEFN
      USE CGRID_SPCS
      USE CMAQ_DATA
      
      IMPLICIT NONE     

C..INCLUDE FILES:

C..ARGUMENTS: 

      INTEGER, INTENT( IN  ) :: LOGUNIT
      INTEGER, INTENT( IN  ) :: N_BDY
      REAL,    INTENT( OUT ) :: BDY_OUT( :, :, :)

      INTEGER NHRS                    ! No. of hours for BC output
      INTEGER SDATE                   ! Date for BC Output
      INTEGER STIME                   ! Time for BC output

C..PARAMETERS:


C..EXTERNAL FUNCTIONS: 
!      INTEGER INDEX1               ! Looks up name in table
!      INTEGER JUNIT                ! Get Logical unit no. for file

      
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(16), ALLOCATABLE ::  EXT_IC_NAME( : )  ! Names of species in *.EXTs
      CHARACTER(16)  ENV_DFLT       ! Environment variable default value
      CHARACTER(80)  ENV_DESC       ! Environment variable description
      CHARACTER(256) LINEIN         ! Input line
      CHARACTER(80)  MSG            ! Log message
      CHARACTER(16)  PROF_UNITS     ! Environment variable for units
      CHARACTER(16)  SPEC_NAME      ! Species name
      CHARACTER(16)  PROF_SP        ! Species name on profile file
      CHARACTER(16)  VNAME          ! Scratch Species name 

      CHARACTER(16), SAVE ::  PNAME       = 'CMAQ_PROFILE_BDY' ! Program name
      CHARACTER(16), SAVE ::  BC_PROFILE  = 'BC_PROFILE'       ! Logical name of input profile file
      CHARACTER(16), SAVE ::  PUNITS      = 'PROF_UNITS'       ! Value of units Environment variable

      CHARACTER(256),             SAVE :: PROF_FL_NAME   ! Input profile file name

      CHARACTER(16),              SAVE ::  PROF_SP_NAME( MXCTMS * MXVARS3 )  ! Species names on
                                                                             ! profile file
      CHARACTER(16), ALLOCATABLE, SAVE ::  SPNAME_OUT( : )                   ! Output file species
                                                                             ! names
      CHARACTER(16), ALLOCATABLE, SAVE ::  MCNV_SPCS(  :, : )                ! Mech conv species
                                                                             ! names
      INTEGER IND, ISP     ! Array indices for species
      INTEGER N, SPC       ! Loop indices
      INTEGER PFILE        ! Unit number of profile file    
      INTEGER STATUS       ! Status code

      INTEGER, ALLOCATABLE, SAVE ::   SPMAP(     : ) ! Map to input file species from BC
                                                     ! species
      INTEGER, ALLOCATABLE, SAVE ::   N_MCNV_SP( : ) ! No. of mech input species for each
                                                     ! mech output species
      INTEGER,              SAVE ::   NSPCS_IN       ! CTM files species counter
      INTEGER,              SAVE ::   NLAYS_IN       ! No. of layers in input conc file
      INTEGER,              SAVE ::   N_GC_OUT       ! No. of GC species for conversion 
      INTEGER,              SAVE ::   NSPCS_OUT      ! Number of BC species on output file(s)
      INTEGER,              SAVE ::   NEDGE         ! Loop index for no. of edges (4)  

      LOGICAL,              SAVE ::  LCONV                      ! Flag for mech conversion
      LOGICAL,              SAVE ::  LERROR                     ! Error flag
      LOGICAL, ALLOCATABLE, SAVE ::  LCONVSP( : )               ! Flag for species mech conversion
      LOGICAL,              SAVE ::  LSCALE( MXVARS3 * MXCTMS)  ! Flag for scaling input concs 

      REAL, ALLOCATABLE,    SAVE ::  MCNV_COEFF( :, : )         ! Mech conv species coefficients
      REAL,                 SAVE ::  SFAC( MXVARS3 * MXCTMS )   ! Input species conc scale factor
      LOGICAL, ALLOCATABLE, SAVE ::  USE_PROFILE( : )

      INTEGER, PARAMETER         :: NEDGES = 4

      REAL VGLVS_IN( MXLAYS3 + 1 )              ! Input vertical levels
      REAL INPROF( MXLAYS3, 4, MX_INFL_SP )     ! Input conc profiles  
      REAL COEFF                                ! Conc coefficient

      REAL, ALLOCATABLE, SAVE :: VIPROF( :, :, : )     ! Vertically interp profiles

      INTEGER  NLOOPS            ! Loop parameter
      INTEGER  NW, NS, NE        ! North boundary loop indices
      INTEGER  SW, SS, SE        ! South boundary loop indices
      INTEGER  WN, WS, WE        ! West boundary loop indices
      INTEGER  EN, ES, EE        ! East boundary loop indices
      INTEGER  C, V, L       ! Loop indices
      INTEGER  NLEVS_IN      ! No. of layers in input conc file

      INTEGER          ::    NORTH, EAST, SOUTH, WEST
      CHARACTER(   5 ) ::    EDGE_IN      
      CHARACTER(   5 ) ::    EDGE_NAME( NEDGES ) = ( /
     &                                                 'NORTH' ,
     &                                                 'EAST ' ,
     &                                                 'SOUTH' ,
     &                                                 'WEST '
     &                                                  / )

      
      LOGICAL       :: LNEG  ! flag for negative BC's
      LOGICAL, SAVE :: FIRSTCALL = .TRUE.

      INTERFACE
       SUBROUTINE GC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, SPMAP, 
     &                       N_MCNV_SP, SFAC, MCNV_COEFF, LCONV, LSCALE,  
     &                       LCONVSP, EXT_IC_NAME, INFL_SP_NAME,   
     &                       MCNV_SPCS, SPNAME_OUT )
         USE UTILIO_DEFN
         USE CGRID_SPCS
         USE CMAQ_DATA
         IMPLICIT NONE
         INTEGER, INTENT( IN )            :: LOGUNIT             ! Output log unit number
         INTEGER, INTENT( IN )            :: NSPCS_IN            ! Total no. of species in input file
         CHARACTER( 16 ), INTENT( IN )    :: EXT_IC_NAME( : )    ! List of all IC species in .EXT files
         CHARACTER( 16 ), INTENT( IN )    :: INFL_SP_NAME( : )   ! List of species on the input file
         CHARACTER( 16 ), INTENT( INOUT ) :: SPNAME_OUT( : )     ! Output file species names
         CHARACTER( 16 ), INTENT( INOUT ) :: MCNV_SPCS(  :, : )  ! Mechanism species names convert from
         INTEGER, INTENT( INOUT )         :: N_GC_OUT            ! No. of GC species for mech. conversion 
         INTEGER, INTENT( INOUT )         :: SPMAP( : )          ! Map of output species to input species
         INTEGER, INTENT( INOUT )         :: N_MCNV_SP( : )      ! No. of mech species to convert from
         LOGICAL, INTENT( INOUT )         :: LCONV               ! Flag for mech conversion
         LOGICAL, INTENT( INOUT )         :: LCONVSP( : )        ! Flag for species mech conversion
         LOGICAL, INTENT( INOUT )         :: LSCALE( : )         ! Flag for scaling input concs 
         REAL, INTENT( INOUT )            :: MCNV_COEFF( :, : )  ! Mech input species coefficients
         REAL, INTENT( INOUT )            :: SFAC( : )           ! Input species conc scale factor
        END SUBROUTINE GC_SPC_MAP      
        SUBROUTINE NGC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, NSPCS_OUT, 
     &                        SPMAP, SFAC, LCONV, LSCALE, LCONVSP,
     &                        EXT_IC_NAME, INFL_SP_NAME, SPNAME_OUT )
         USE UTILIO_DEFN
         USE CGRID_SPCS
         USE CMAQ_DATA
         IMPLICIT NONE     
         CHARACTER( 16 ), INTENT( IN ) :: EXT_IC_NAME( : )   ! Names of species in *.EXTs
         CHARACTER( 16 ), INTENT( IN ) :: INFL_SP_NAME( : )  ! Species names on input file
         INTEGER, INTENT( IN ) :: LOGUNIT                    ! Unit number for output log
         INTEGER, INTENT( IN ) :: N_GC_OUT                   ! No. of GC species for conversion 
         INTEGER, INTENT( IN ) :: NSPCS_IN                   ! No. of species in the IC input file(s)
         LOGICAL, INTENT( IN ) :: LCONV                      ! Flag for mech conversion
         INTEGER,        INTENT( INOUT ) :: NSPCS_OUT       ! Number of BC species on output file(s)
         INTEGER,        INTENT( INOUT ) :: SPMAP( : )      ! Map to CTM species from IC species
         REAL,           INTENT( INOUT ) :: SFAC( : )       ! Input species conc scale factor
         LOGICAL,        INTENT( INOUT ) :: LCONVSP( : )    ! Flag for species mech conversion
         LOGICAL,        INTENT( INOUT ) :: LSCALE( : )     ! Flag for scaling input concs 
         CHARACTER( 16), INTENT( INOUT ) :: SPNAME_OUT( : ) ! Output file species names
        END SUBROUTINE NGC_SPC_MAP      
        SUBROUTINE LST_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, NSPCS_OUT,  
     &                        SPMAP, N_MCNV_SP, SFAC, MCNV_COEFF, LCONV, 
     &                        LSCALE, LCONVSP, EXT_IC_NAME, INFL_SP_NAME,  
     &                        MCNV_SPCS, SPNAME_OUT )
         USE UTILIO_DEFN
         USE CGRID_SPCS
         USE CMAQ_DATA
         IMPLICIT NONE     
         CHARACTER( 16 ), INTENT ( IN ) :: EXT_IC_NAME( : )      ! List of all species names
         CHARACTER( 16 ), INTENT ( IN ) :: INFL_SP_NAME( : )     ! List of species on the input file
         CHARACTER( 16 ), INTENT ( IN ) :: SPNAME_OUT( : )       ! Output file species names
         CHARACTER( 16 ), INTENT ( IN ) :: MCNV_SPCS(  :, : )    ! Mechanism species names to convert from
         INTEGER, INTENT ( IN ) :: LOGUNIT             ! Ouput log unit number
         INTEGER, INTENT ( IN ) :: N_GC_OUT            ! No. of GC species for mech. conversion 
         INTEGER, INTENT ( IN ) :: NSPCS_IN            ! Total no. of species in input file
         INTEGER, INTENT ( IN ) :: NSPCS_OUT           ! Number of BC species on output file(s)
         INTEGER, INTENT ( IN ) :: SPMAP( : )          ! Map of output species to input species
         INTEGER, INTENT ( IN ) :: N_MCNV_SP( : )      ! No. of mech species to convert from
         LOGICAL, INTENT ( IN ) :: LCONV               ! Flag for mech conversion
         LOGICAL, INTENT ( IN ) :: LCONVSP( : )        ! Flag for species mech conversion
         LOGICAL, INTENT ( IN ) :: LSCALE( : )         ! Flag for scaling input concs 
         REAL, INTENT ( IN )    :: MCNV_COEFF( :, : )  ! Mech input species coefficients
         REAL, INTENT ( IN )    :: SFAC( : )           ! Input species conc scale factor
       END SUBROUTINE LST_SPC_MAP      
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
C**********************************************************************


      WRITE( LOGUNIT, 92000 )

      NHRS = 0
      SDATE = 0
      STIME = 0

      IF( FIRSTCALL )THEN
          FIRSTCALL = .FALSE.

C Set CGRID mechanism
         IF ( .NOT. CGRID_SPCS_INIT() ) THEN
            MSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT1 )
         END IF

         ALLOCATE( SPNAME_OUT( NSPCSD ), MCNV_SPCS(  N_GC_SPCD, MXCVSP ) )

         ALLOCATE( SPMAP( NSPCSD ), N_MCNV_SP( N_GC_SPCD ), LCONVSP( NSPCSD ),
     &             MCNV_COEFF( N_GC_SPCD, MXCVSP ) )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Build one array containing the names of the species in *.EXT files 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          ALLOCATE(EXT_IC_NAME( NSPCSD ) )

          ISP = 0
          DO N = 1, N_GC_SPC
             ISP = ISP + 1
             EXT_IC_NAME( ISP ) = GC_SPC( N )
          ENDDO

          DO N = 1, N_AE_SPC
             ISP = ISP + 1
             EXT_IC_NAME( ISP ) = AE_SPC( N )
          ENDDO

          DO N = 1, N_NR_SPC
             ISP = ISP + 1
             EXT_IC_NAME( ISP ) = NR_SPC( N )
          ENDDO

          DO N = 1, N_TR_SPC
             ISP = ISP + 1
             EXT_IC_NAME( ISP ) = TR_SPC( N )
          ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the input profile file, read the number of species and their 
c  names, and list the file on the output log
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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

          DO N = 1, 3
             READ( PFILE, 94000 ) LINEIN
#ifdef verbose
             WRITE( LOGUNIT, 92040 ) LINEIN
#endif
          ENDDO

          READ( PFILE, * ) NLAYS_IN, NSPCS_IN
#ifdef verbose
          WRITE( LOGUNIT, * )' NLAYS_IN = ', NLAYS_IN,' NSPCS_IN = ', NSPCS_IN
#endif
          BACKSPACE PFILE 
          READ( PFILE, 94000 ) LINEIN
          WRITE( LOGUNIT, 92040 ) LINEIN

          READ( PFILE, 94000 ) LINEIN
          WRITE( LOGUNIT, 92040 ) LINEIN

          LERROR = .FALSE.
          IF( NLAYS_IN .GT. MXLAYS3 + 1 ) THEN
              MSG = 'ERROR: No of layers on profile file exceeds MXLAYS + 1' 
              WRITE(LOGUNIT, 92080) MSG
              LERROR = .TRUE.
          ENDIF

          IF( NSPCS_IN .GT. MX_INFL_SP ) THEN
              MSG = 'ERROR: No of layers on profile file exceeds  MXINFL_SP' 
              WRITE( LOGUNIT, 92080  ) MSG
              LERROR = .TRUE.
          ENDIF

          IF( LERROR ) THEN
              MSG = 'Dimension problem in BC profile file'
              CALL M3ERR( PNAME, 0, 0, MSG, .TRUE.) 
          ENDIF

          LERROR = .FALSE.
          DO NEDGE = 1, 4

             READ( PFILE, 94000 ) LINEIN
             WRITE( LOGUNIT, 92040 ) LINEIN
             DO SPC = 1, NSPCS_IN
                READ( PFILE, * ) SPEC_NAME
                BACKSPACE PFILE
                READ( PFILE, 94000 ) LINEIN
                IF( NEDGE .EQ. 1 ) THEN
                   PROF_SP_NAME( SPC ) = SPEC_NAME
                   WRITE( LOGUNIT, 92040 ) LINEIN
                ELSE
                   IND = INDEX1( SPEC_NAME, NSPCS_IN, PROF_SP_NAME( SPC ) )
                   IF( IND. EQ. 0 ) THEN
                       LERROR = .TRUE.
                       WRITE( LOGUNIT, 92050 ) LINEIN
                   ELSE
                       WRITE( LOGUNIT, 92040 ) LINEIN
                   ENDIF
                ENDIF
             ENDDO
                  
          ENDDO

          IF( LERROR ) THEN
              MSG = 'Inconsistent species names in BC profile file'
              CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. ) 
          ENDIF
      
          WRITE( LOGUNIT, 92060 ) 

          CLOSE( PFILE )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Build a species map and report
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          CALL GC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, SPMAP, N_MCNV_SP,
     &                     SFAC, MCNV_COEFF, LCONV, LSCALE, LCONVSP, 
     &                     EXT_IC_NAME, PROF_SP_NAME, MCNV_SPCS,  
     &                     SPNAME_OUT )

          CALL NGC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, NSPCS_OUT, SPMAP, 
     &                     SFAC, LCONV, LSCALE, LCONVSP, EXT_IC_NAME,
     &                     PROF_SP_NAME, SPNAME_OUT )


          CALL LST_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, NSPCS_OUT, SPMAP, 
     &                      N_MCNV_SP, SFAC, MCNV_COEFF, LCONV, LSCALE,  
     &                      LCONVSP, EXT_IC_NAME, PROF_SP_NAME, MCNV_SPCS, 
     &                      SPNAME_OUT )


          ALLOCATE( USE_PROFILE( NSPCS_OUT ))

          USE_PROFILE = .FALSE.
          NSPC_DFLT   = 0

          WRITE(LOGUNIT,*)'The below species from the CMAQ bc_profile.dat'

          DO V = 1, NSPCS_OUT
             IF( INDEX1( SPNAME_OUT(V), NSPC_CMAQ, CMAQ_SNAME) .LT. 1 )THEN
                 NSPC_DFLT = NSPC_DFLT + 1
                 USE_PROFILE(V) = .TRUE.
             ENDIF
          ENDDO

          ALLOCATE( DFLT_UNITS(  NSPC_DFLT ) )
          ALLOCATE( DFLT_LNAME( NSPC_DFLT ) )
          ALLOCATE( DFLT_SNAME( NSPC_DFLT ) )

          IND = 0
          DO V = 1, NSPCS_OUT
             IF( .NOT. USE_PROFILE( V ))CYCLE
             IND = IND + 1 
             DFLT_UNITS(  IND ) = 'ppmV'
             DFLT_LNAME( IND ) =  SPNAME_OUT( V )
             DFLT_SNAME( IND ) =  SPNAME_OUT( V )
            IF( INDEX1( SPNAME_OUT( V ), N_AE_SPC, AE_SPC ) .GT. 0 )THEN
               IF ( SPNAME_OUT( V )( 1:3 ) .EQ. 'NUM' ) THEN
                  DFLT_UNITS(  IND ) = '#/m**3'
               ELSEIF ( SPNAME_OUT( V )( 1:3 ) .EQ. 'SRF' ) THEN
                  DFLT_UNITS(  IND ) = 'm**2/m**3'
               ELSE
                  DFLT_UNITS(  IND ) = 'micrograms/m**3'
               ENDIF
            ENDIF
            WRITE(LOGUNIT,'(I3,1X,A16,1X,A16)')IND,DFLT_LNAME( IND ),
     &                                         DFLT_UNITS(  IND )
         ENDDO

      PFILE = JUNIT()

#ifdef verbose
       WRITE( LOGUNIT, * )TRIM( PNAME ) // ' attempting to open BC_PROFILE '
     & // TRIM( PROF_FL_NAME )
#endif

      OPEN( UNIT = PFILE, FILE = PROF_FL_NAME , ERR = 999 )
           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read BCs from the input profile
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO N = 1, 3
         READ( PFILE, 94000 ) 
      ENDDO

      READ(PFILE, * )NLEVS_IN, SPC, (VGLVS_IN( L ), L = 1, NLEVS_IN+1)
#ifdef verbose
      WRITE(LOGUNIT, '(i4,1x,i4,50(1x,es12.4))' )NLEVS_IN, SPC, (VGLVS_IN( L ), L = 1, NLEVS_IN+1)
#endif

      READ( PFILE, * ) 

      DO N = 1, 4
         READ( PFILE, * )EDGE_IN

         CALL UPCASE( EDGE_IN )

         IND = INDEX1( EDGE_IN, NEDGES, EDGE_NAME )

         DO SPC = 1, NSPCS_IN
           READ(PFILE,*)PROF_SP, (INPROF(L, IND, SPC),L = 1, NLEVS_IN)
#ifdef verbose
           WRITE(LOGUNIT,'(i4,1x,a16,50(1x,es12.4))')SPC, PROF_SP, (INPROF(L, IND, SPC),L = 1, NLEVS_IN)
#endif
         ENDDO
      ENDDO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do the vertical interpolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       ALLOCATE( VIPROF( NLAYS3D, 4, MX_INFL_SP ) )

       CALL VINTERP_PROF( NEDGES, NLEVS_IN, NSPCS_IN, VGLVS_IN, 
     &                    INPROF, VIPROF )

#ifdef verbose     
       DO N = 1, NEDGES
          DO SPC = 1, NSPCS_IN
                WRITE(LOGUNIT,'(I3,1X,I3,1X,A16,1X,100(1x,ES12.4))')
     &          N, SPC, PROF_SP_NAME( SPC ), VIPROF( 1:NLEVS_IN, N, SPC )
          END DO
       END DO
#endif

       RETURN

      ENDIF ! FIRSTCALL

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Compute the output BC concentrations
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
      LNEG = .FALSE.
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

      SOUTH = INDEX1( 'SOUTH', NEDGES, EDGE_NAME )
      EAST  = INDEX1( 'EAST ', NEDGES, EDGE_NAME )
      NORTH = INDEX1( 'NORTH', NEDGES, EDGE_NAME )
      WEST  = INDEX1( 'WEST ', NEDGES, EDGE_NAME )

      IND = 0
      BDY_OUT = 0.0
      
      LOOP_SPC_OUT: DO N = 1, NSPCS_OUT
      
         WRITE( LOGUNIT, * )

         VNAME = SPNAME_OUT( N )
         V = INDEX1( VNAME, NSPC_DFLT, DFLT_LNAME)
         IF( V .LT. 1 )CYCLE LOOP_SPC_OUT

!         IF( .NOT. USE_PROFILE( N ) )CYCLE
    
         LOOP_LAYS: DO L = 1, NLAYS3D

            IF( LCONVSP( N ) ) THEN 
                NLOOPS = N_MCNV_SP( N )
            ELSE
                NLOOPS = 1
                IND = SPMAP( N )
            ENDIF

            LOOP_CONVERT: DO SPC = 1, NLOOPS

               IF( LCONVSP( N ) ) THEN                      
                  IND = INDEX1( MCNV_SPCS( N, SPC ), NSPCS_IN, PROF_SP_NAME)
                  COEFF = MCNV_COEFF( N, SPC )
               ELSE
                  COEFF = 1.0
               ENDIF
c..South
                DO C = SW, SE
                   BDY_OUT( C, L, V ) = BDY_OUT( C, L, V ) + COEFF *
     &                                  VIPROF( L, SOUTH, IND )
                   IF( BDY_OUT( C, L, V ) .LT. 0.0 )THEN
                       LNEG = .TRUE.
                       WRITE( LOGUNIT,94020)'SW-SE', VNAME, L
                   ENDIF
                ENDDO
c..East
                DO C = ES, EN
                   BDY_OUT( C, L, V ) = BDY_OUT( C, L, V ) + COEFF *
     &                                  VIPROF( L, EAST, IND )
                   IF( BDY_OUT( C, L, V ) .LT. 0.0 ) THEN
                       LNEG = .TRUE.
                       WRITE( LOGUNIT,94020)'ES-EN', VNAME, L
                   ENDIF
                ENDDO
c..North
                DO C = NW, NE
                   BDY_OUT( C, L, V ) = BDY_OUT( C, L, V ) + COEFF *
     &                                  VIPROF( L, NORTH,IND )
                   IF( BDY_OUT( C, L, V ) .LT. 0.0 ) THEN
                       LNEG = .TRUE.
                       WRITE( LOGUNIT,94020)'NW-NE', VNAME, L
                   ENDIF
                ENDDO
c..West
                DO C = WS, WN
                   BDY_OUT( C, L, V ) = BDY_OUT( C, L, V ) + COEFF *
     &                                  VIPROF( L, WEST, IND )
                   IF( BDY_OUT( C, L, V ) .LT. 0.0 ) THEN
                       LNEG = .TRUE.
                       WRITE( LOGUNIT,94020)'WS-WN', VNAME, L
                   ENDIF
                ENDDO

            ENDDO LOOP_CONVERT
            
         ENDDO LOOP_LAYS

#ifdef verbose         
         WRITE(LOGUNIT,'(I3,1X,A16,1X,100(1x,ES12.4))')V, VNAME,
     &   BDY_OUT( 1, 1:NLAYS3D, V )
#endif

      ENDDO LOOP_SPC_OUT

!      CLOSE( PFILE )

      IF( LNEG ) THEN
         MSG = 'Negative ICs output'
         STATUS = 1
         CALL M3EXIT( PNAME, 0, 0,' ', STATUS) 
      ENDIF

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

      END
