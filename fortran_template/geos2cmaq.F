C***********************************************************************
C    GEOS2CMAQ.F
C
C  FUNCTION:   
C             
C  PRECONDITIONS: Assumes METBDY3D File is available
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED:     
C
C  July 2011 -- HOT Pye
C     Changed GEOS-Chem lat/lon information from corners to centers
C
C***********************************************************************
      USE GEO_DATA
      USE CMAQ_DATA

      IMPLICIT NONE

C..INCLUDE FILES:
      INCLUDE SUBST_IOPARMS
      INCLUDE SUBST_IOFDESC
      INCLUDE SUBST_IODECL


C..ARGUMENTS: None

C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'BC_GEOCTM'
      CHARACTER( 16 ), PARAMETER :: MET_BDY_3D = 'MET_BDY_3D'

C..EXTERNAL FUNCTIONS:
      INTEGER  GETEFILE
      INTEGER  INDEX1
      INTEGER  ENVINT
 
C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER(  4 )   ::  GEO_MONTH        ! Month on GEO file
      CHARACTER(  4 )   ::  GEO_YEAR         ! Year on GEO file
      CHARACTER(  4 )   ::  GEO_FL_SPNAM     ! Species name on GEO file
      CHARACTER(  5 )   ::  LIST_EDGE
      CHARACTER( 80 )   ::  MSG              ! Log message

      CHARACTER( 16 )   ::  BC_FNAME       = 'BC_FNAME'
      CHARACTER( 80 )   ::  TITLE


      INTEGER            ::  C, R, L, S, N, V
      INTEGER            ::  IND
      INTEGER            ::  JDATE = 0
      INTEGER            ::  JTIME = 0
      INTEGER            ::  LOGDEV           ! Unit number for output log
      INTEGER            ::  NBDY
      INTEGER            ::  NCELL
      INTEGER            ::  GEO_UNIT
      INTEGER            ::  NREAD
      INTEGER            ::  STRT
      INTEGER            ::  FINI
      INTEGER            ::  STATUS
      INTEGER            ::  NSTEPS
      INTEGER            ::  tStep

      INTEGER, ALLOCATABLE :: COL_LOC( : )
      INTEGER, ALLOCATABLE :: ROW_LOC( : )

      LOGICAL            :: RDONLY
      LOGICAL            :: FORMTD
      LOGICAL            :: LZERO

      REAL               :: DMIN
      REAL               :: LAT
      REAL               :: LON
      REAL               :: X1
      REAL               :: Y1
      REAL               :: MAXC
      REAL               :: MINC
      REAL               :: FAC

      REAL, ALLOCATABLE  :: LON_GEO(   : , : )
      REAL, ALLOCATABLE  :: LAT_GEO(   : , : )
      REAL, ALLOCATABLE  :: LON_CMAQ(  :  )
      REAL, ALLOCATABLE  :: LAT_CMAQ(  : )
      REAL, ALLOCATABLE  :: CGRID_GT( : , :, :, :, : )
      REAL, ALLOCATABLE  :: CGRID_GS( : , :, :, :, : )

      REAL, ALLOCATABLE  :: BDY_GS( : , : , : )
      REAL, ALLOCATABLE  :: BDY_GT( : , : , : )
      REAL, ALLOCATABLE  :: BDY_GT_VERT( : , : , : )
      REAL, ALLOCATABLE  :: BDY_GS_VERT( : , : , : )
      REAL, ALLOCATABLE  :: BDY2( : , : , : )
      REAL, ALLOCATABLE  :: BDY3( : , : , : )
      REAL, ALLOCATABLE  :: BCOUT( : , : )

c     integer :: nc=55
c     integer :: hsp=3
c     integer :: csp=1

C**********************************************************************


      LOGDEV = INIT3()

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open the METBDY3D file & get grid description and date/time
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( .NOT. OPEN3( MET_BDY_3D, FSREAD3, PNAME ) ) THEN
         MSG = 'Could not open ' // MET_BDY_3D // ' file'
         CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. )
      ENDIF

      IF( .NOT. DESC3( MET_BDY_3D ) ) THEN
         MSG = 'Could not read DESC of  ' // MET_BDY_3D  // ' file'
         CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. )
      ENDIF


C...get starting date and time
c      jdate = ENVINT('START_DATE','Start date for period', SDATE3D, STATUS)

      jdate = SDATE3D

      !jtime = ENVINT('START_TIME','Start time for period', STIME3D, STATUS)
      !nsteps= ENVINT('NSTEPS','Number of 3-hours steps', 9, STATUS)
      jtime = 0
      nsteps = 24

      bpchYear = ENVINT('BPCH_YEAR','Reset year for BPCH data',0,status) 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Define the lat/lons for the GEOCTM model grid
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Longitude sequence: -181.25 -178.75 .... 176.25 178.75     
c Latitude sequence:   -90.00 -98.00 ....  87.0 89.0 90.0

      ALLOCATE( LON_GEO( NCOLS_GEO, NROWS_GEO ) )
      ALLOCATE( LAT_GEO( NCOLS_GEO, NROWS_GEO ) )

c..temporarily set latitude of bottom row to -90.0
      DO C = 1, NCOLS_GEO
c        LAT_GEO( C, 1 ) = -91.0
         !LAT_GEO( C, 1 ) = 9.0
         ! now use center (hotp 6/17/2011)
         LAT_GEO( C, 1 ) = 10.0E0
      ENDDO

c..set remaining rows by incrementing by delta latitude
      DO R = 2, NROWS_GEO
         DO C = 1, NCOLS_GEO
            LAT_GEO( C, R ) =  LAT_GEO( C, R - 1 ) + DEL_LAT
         ENDDO
      ENDDO

c..reset top & bottom rows to 89.0 & -89.0
c     DO C = 1, NCOLS_GEO
c        LAT_GEO( C, NROWS_GEO ) =  90.0
c        LAT_GEO( C, 1         ) = -90.0
c     ENDDO


c..set left-most column to -181.25
      DO R = 1, NROWS_GEO
c        LON_GEO( 1, R ) = -181.25
         ! Updated, hotp 6/17/2011
         !LON_GEO( 1, R ) = -141.250
         ! now use center of grid box
         LON_GEO( 1, R ) = -137.5E0
      ENDDO

c..set remaining rows by incrementing by delta latitude
      DO R = 1, NROWS_GEO
         DO C = 2, NCOLS_GEO
            LON_GEO( C, R ) = LON_GEO( C-1, R ) + DEL_LON
         ENDDO
      ENDDO

      ! hotp
      print*,'Dimensions of GEOS-Chem data: ', NCOLS_GEO, NROWS_GEO
      print*,'Latitudes of GEOS-Chem grid: ',LAT_GEO
      print*,'Longitudes of GEOS-Chem grid: ', LON_GEO


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get the Lat/lons of the CMAQ boundary cells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NBDY = 2 * NCOLS3D + 2 * NROWS3D + 4
      ALLOCATE( LON_CMAQ( NBDY ) )
      ALLOCATE( LAT_CMAQ( NBDY ) )

c..South edge
      NCELL = 0
      DO C = 1, NCOLS3D + 1
         R = 0
         NCELL = NCELL + 1
         CALL LAT_LON( C, R, GDTYP3D, XORIG3D, YORIG3D, 
     &                 XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                 P_ALP3D, P_BET3D, P_GAM3D, LAT_CMAQ( NCELL ),
     &                 LON_CMAQ( NCELL ) )
      ENDDO

c..East Edge
      DO R = 1, NROWS3D + 1
         C = NCOLS3D + 1
         NCELL = NCELL + 1
         CALL LAT_LON( C, R, GDTYP3D, XORIG3D, YORIG3D, 
     &                 XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                 P_ALP3D, P_BET3D, P_GAM3D, LAT_CMAQ( NCELL ),
     &                 LON_CMAQ( NCELL ) )
      ENDDO

c..North Edge
      DO C = 0, NCOLS3D
         R = NROWS3D + 1
         NCELL = NCELL + 1
         CALL LAT_LON( C, R, GDTYP3D, XORIG3D, YORIG3D, 
     &                 XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                 P_ALP3D, P_BET3D, P_GAM3D, LAT_CMAQ( NCELL ),
     &                 LON_CMAQ( NCELL ) )
      ENDDO

c..West Edge
      DO R = 0, NROWS3D
         C = 0
         NCELL = NCELL + 1
         CALL LAT_LON( C, R, GDTYP3D, XORIG3D, YORIG3D, 
     &                 XCELL3D, YCELL3D, XCENT3D, YCENT3D,
     &                 P_ALP3D, P_BET3D, P_GAM3D, LAT_CMAQ( NCELL ),
     &                 LON_CMAQ( NCELL ) )
      ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Map the CTM file LAT/LONs to the BC file LAT/LONs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( COL_LOC( NBDY ) )
      ALLOCATE( ROW_LOC( NBDY ) )

      DO N = 1, NBDY
         LAT = LAT_CMAQ( N )
         LON = LON_CMAQ( N )
         DMIN = 1.0E+30
         DO C = 1, NCOLS_GEO
            DO R = 1, NROWS_GEO
               Y1 = ( LAT - LAT_GEO( C, R ) ) ** 2
               X1 = ( LON - LON_GEO( C, R ) ) ** 2
               IF( ( X1 + Y1 ) .LT. DMIN ) THEN           
                  DMIN = X1 + Y1
                  COL_LOC( N ) = C
                  ROW_LOC( N ) = R
               ENDIF
             ENDDO
         ENDDO
      ENDDO


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open output file
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NVARS3D = NSPC_CMAQ + NSPC_DFLT
c SLN - replace TSTEP3D to 10000 from 30000
      TSTEP3D = 10000
      SDATE3D = jdate
      STIME3D = jtime
      DO V = 1, NSPC_CMAQ
         VNAME3D( V ) = CMAQ_SNAME( V )
         VDESC3D( V ) = CMAQ_LNAME( V )
         UNITS3D( V ) = CMAQ_UNITS( V )
         VTYPE3D( V ) = M3REAL
      ENDDO
                                                                
      N = 0                                                     
      DO V = NSPC_CMAQ + 1, NVARS3D                             
         N = N + 1                                              
         VNAME3D( V ) = DFLT_SNAME( N )                         
         VDESC3D( V ) = DFLT_LNAME( N )                         
         UNITS3D( V ) = DFLT_UNITS( N )                         
         VTYPE3D( V ) = M3REAL                                  
      ENDDO                                                     
                                                                
      FTYPE3D = BNDARY3                                         
      NTHIK3D = 1                                               

      IF( .NOT. OPEN3( BC_FNAME, FSNEW3, PNAME ) ) THEN         
        IF( .NOT. OPEN3( BC_FNAME, FSRDWR3, PNAME ) ) THEN      
           MSG = 'Could not open nor create ' //  BC_FNAME // ' file '        
           CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )               
           ENDIF                                                
        ENDIF                                                   

      ! allocate arrays for output data
      ALLOCATE( BDY_GT_VERT( NBDY, NLAYS3D, NSPC_GT ) )
      ALLOCATE( BDY_GS_VERT( NBDY, NLAYS3D, NSPC_GS ) )
      ALLOCATE( BDY2( NBDY, NLAYS3D, NSPC_CMAQ ) )
      ALLOCATE( BDY3( NBDY, NLAYS3D, NSPC_DFLT ) )
      ALLOCATE( BCOUT( NBDY, NLAYS3D ) )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read in the GEO 3 hour data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( CGRID_GT( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS, NSPC_GT, 24 ) )
      ALLOCATE( CGRID_GS( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS, NSPC_GS, 24 ) )

      Call LoadGEO( CGRID_GT, CGRID_GS, jdate, jtime, status )
c     if( status.ne.0 ) EXIT

C SLN - hack this in to read 24 hour GC files cuz there is no time for better

      ALLOCATE( BDY_GT( NBDY, N_GEO_LAYS, NSPC_GT ) )
      ALLOCATE( BDY_GS( NBDY, N_GEO_LAYS, NSPC_GS ) )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  start time loop to read and process each timestep  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Do tStep=1,NSTEPS ! time loop

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set boundary concs using GEO species & GEO vertical structure
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       DO L = 1, N_GEO_LAYS
          DO N = 1, NBDY
             C = COL_LOC( N )
             R = ROW_LOC( N )
               DO S = 1, NSPC_GT
                 BDY_GT( N, L, S ) = CGRID_GT( C, R, L, S, tStep )
               ENDDO
               DO S = 1, NSPC_GS
                 BDY_GS( N, L, S ) = CGRID_GS( C, R, L, S, tStep )
               ENDDO
           ENDDO
       ENDDO

c     DEALLOCATE ( CGRID_GT )
c     DEALLOCATE ( CGRID_GS )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Vertically interpolate GEO species concs to CMAQ layers
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CALL VINTERP( LOGDEV, NBDY, BDY_GT, NSPC_GT, BDY_GT_VERT )
c     DEALLOCATE( BDY_GT )

      CALL VINTERP( LOGDEV, NBDY, BDY_GS, NSPC_GS, BDY_GS_VERT )
c     DEALLOCATE( BDY_GS )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Convert GEO species to CMAQ species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      CALL CHEM_MAP( NBDY, BDY_GT_VERT, BDY_GS_VERT, BDY2, jdate, jtime)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Fix any negative concs
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( LOGDEV, 92000 ) 
      LZERO = .FALSE.
      DO S = 1, NSPC_CMAQ
         DO L = 1, NLAYS3D
            DO N = 1, NBDY
                IF( BDY2( N, L, S ) .LT. 0.0 ) THEN
                   BDY2( N, L, S ) = 0.0
                   LZERO = .TRUE.
                ENDIF
            ENDDO
         ENDDO
      ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Add defaults for species not available from GEO files
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if(NSPC_DFLT.gt.0) CALL DEFAULTS( LOGDEV, NBDY, BDY3 )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write to output file 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      N = NBDY
      L = NLAYS3D

      S = 0
      DO V = 1, NVARS3D

         IF( V .LE. NSPC_CMAQ ) THEN

             BCOUT( 1:N, 1:L ) = BDY2( 1:N, 1:L, V )
         ELSE

             S = S + 1
             BCOUT( 1:N, 1:L ) = BDY3( 1:N, 1:L, S )

         ENDIF

         IF( .NOT. WRITE3( BC_FNAME, VNAME3D( V ), JDATE, JTIME,
     &                     BCOUT( 1, 1 ) ) ) THEN
            MSG =  'Could not WRITE species ' //  VNAME3D( V ) // 
     &             'to file ' // BC_FNAME 
            CALL M3ERR( PNAME, JDATE, JTIME, MSG, .TRUE. )
         ENDIF
      
      ENDDO

      ! advance to next time step
      Call Nextime( jdate, jtime, TSTEP3D )

      EndDo  ! time loop

      DEALLOCATE( BDY_GT_VERT )
      DEALLOCATE( BDY_GS_VERT )
      DEALLOCATE( BDY2 )
      DEALLOCATE( BDY3 )
      DEALLOCATE( BCOUT )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  call repair routine
C    repair the number and aerosol surface area concentrations to be
c    consistent with assumed background aerosol distributions,
c    i.e. the standard deviation and geometric mean diameter around
c    the boundaries will be the same
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      Call REPAIR(BC_FNAME)


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Finish up 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      IF ( SHUT3() ) THEN
         WRITE ( *,91000 )
     &   '>>---->  Program ', PNAME( 1: LEN_TRIM( PNAME ) ), 
     &   ' completed successfully  <----<< '
      ELSE
         WRITE ( *,91020 ) PNAME( 1: LEN_TRIM( PNAME ) ), 
     &   'FATAL ERROR shutting down Models-3 I/O'
      ENDIF

      STOP


91000 FORMAT ( //5X, A, A, A // )

91020 FORMAT ( //5X , '*** ERROR ABORT in program ', A, ' ***',
     &         /5X , A , // )        !  generic error message format
 
92000 FORMAT( //'Negative boundary concentrations were changed to zero ',
     &          'for the following:' )
92020 FORMAT( 5X, A, ' BDY( ', I4, ', ', I2, ', ', I3, ' ) = ', E15.6 )

92100 FORMAT( / 'Maximum & Minimum BCs for species ', A, ' (ppm) at ',
     &  A, ' Edge' /
     &          'Level', 10X, 'Minimum', 7X, 'Maximum' )
92110 FORMAT( / 'Maximum & Minimum BCs for species ', A, ' (',A,') at ',
     &  A, ' Edge' /
     &          'Level', 10X, 'Minimum', 7X, 'Maximum' )
92120 FORMAT( I3, 7X, E15.7, 2X, E15.7 )

92140 FORMAT( I3, 7X, E12.4, 2X, E12.4 )


      END

