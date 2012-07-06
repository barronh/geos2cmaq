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
      USE UTILIO_DEFN
      USE GEO_DATA
      USE CGRID_SPCS
      USE CMAQ_DATA
      USE PROFILE_DATA

      IMPLICIT NONE

C..INCLUDE FILES:
!      INCLUDE SUBST_IOPARMS
!      INCLUDE SUBST_IOFDESC
!      INCLUDE SUBST_IODECL


C..ARGUMENTS: None

C..PARAMETERS:
      CHARACTER( 16 ), PARAMETER :: PNAME = 'BC_GEOCTM'
      CHARACTER( 16 ), PARAMETER :: MET_BDY_3D = 'MET_BDY_3D'

C..EXTERNAL FUNCTIONS:
!      INTEGER  GETEFILE
!     INTEGER  INDEX1
!      INTEGER  ENVINT
 
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
      INTEGER            ::  JDATE_CHECK = 0
      INTEGER            ::  JTIME = 0
      INTEGER            ::  EDATE = 0
      INTEGER            ::  EDATE_CHECK = 0
      INTEGER            ::  ETIME = 0
      INTEGER            ::  IGNORE_YEAR = 0
      INTEGER            ::  MDATE 
      INTEGER            ::  MTIME 


      INTEGER            ::  START_DATE = 0
      INTEGER            ::  START_TIME = 0
      INTEGER            ::  STOP_DATE  = 0
      INTEGER            ::  STOP_TIME  = 0
      INTEGER            ::  LOGDEV           ! Unit number for output log
      INTEGER            ::  NBDY
      INTEGER            ::  NCELL
      INTEGER            ::  GEO_UNIT
      INTEGER            ::  NREAD
      INTEGER            ::  STRT
      INTEGER            ::  FINI
      INTEGER            ::  STATUS
!      INTEGER            ::  NSTEPS  = 24
      INTEGER            ::  NT
      INTEGER            ::  MET_TSTEP
      INTEGER            ::  BC_TSTEP

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
      
      INTEGER            :: DAYS_PER_YEAR


      REAL, ALLOCATABLE  :: LON_CMAQ(  :  )
      REAL, ALLOCATABLE  :: LAT_CMAQ(  : )

      REAL, ALLOCATABLE  :: BDY_GS( : , : , : )
      REAL, ALLOCATABLE  :: BDY_GT( : , : , : )
      REAL, ALLOCATABLE  :: BDY_GT_VERT( : , : , : )
      REAL, ALLOCATABLE  :: BDY_GS_VERT( : , : , : )
      REAL, ALLOCATABLE  :: BDY_PF_VERT( : , : , : )
      REAL, ALLOCATABLE  :: BDY2( : , : , : )
      REAL, ALLOCATABLE  :: BCOUT( : , : )

      INTERFACE
        SUBROUTINE CHEM_MAP( NBDY, BC1_GT_VERT, BC1_GS_VERT, BC1_PF_VERT, 
     &                     BC2, JDATE, JTIME )
           USE UTILIO_DEFN
           USE GEO_DATA
           USE CMAQ_DATA
           IMPLICIT NONE
           INTEGER, INTENT( IN ) :: NBDY
           Integer, Intent( IN  ) :: JDATE, JTIME
           REAL, INTENT( IN )    :: BC1_GT_VERT( :, :, : )
           REAL, INTENT( IN )    :: BC1_GS_VERT( :, :, : )
           REAL, INTENT( IN )    :: BC1_PF_VERT( :, :, : )
           REAL, INTENT( INOUT ) :: BC2( :, :, : )
        END SUBROUTINE CHEM_MAP
      END INTERFACE 

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

      jdate      = SDATE3D
      jtime       = STIME3D

      edate      = SDATE3D
      etime      = STIME3D
      
      MET_TSTEP = TSTEP3D
      DO NT = 1, MXREC3D - 1
         Call Nextime( edate, etime, MET_TSTEP )
      END Do
      WRITE(LOGDEV, *) 'NEXTTIME', NT, MXREC3D, EDATE, ETIME

      IF( MOD( INT(jdate/1000), 4) .EQ. 0 )THEN
          DAYS_PER_YEAR = 366
      ELSE
          DAYS_PER_YEAR = 365
      END IF
         
C...get starting date and time

       START_DATE = ENVINT('START_DATE','Start Date for period', START_DATE, STATUS)
       START_TIME  = ENVINT('START_TIME','Start Time for period', START_TIME, STATUS)
       IGNORE_YEAR  = ENVINT('IGNORE_YEAR','Positive to indicate surrogate year', IGNORE_YEAR, STATUS)
       IF( START_DATE .LE. 0 )THEN
           MSG = 'BAD or UNDEFINED value for environment variable START_DATE'
           CALL M3EXIT(PNAME, JDATE, JTIME, MSG, XSTAT1 )
       END IF

       STOP_DATE = ENVINT('STOP_DATE','Stop Date for period', STOP_DATE, STATUS)
       STOP_TIME = ENVINT('STOP_TIME', 'Stop Time for period', STOP_TIME, STATUS)

       IF( STOP_DATE .LE. 0 )THEN
           MSG = 'BAD or UNDEFINED value for environment variable STOP_DATE'
           CALL M3EXIT(PNAME, JDATE, JTIME, MSG, XSTAT1 )
       END IF
       
       IF ( IGNORE_YEAR .GT. 0) THEN
           WRITE(LOGDEV, *)'METBDY3D date (', jdate, ') and start date (', START_DATE, ') have different years.'
           WRITE(LOGDEV, *)' *** PROCEEDING with surrogate year *** '           
           JDATE_CHECK = MOD(JDATE, 1000) + START_DATE / 1000
           EDATE_CHECK = MOD(EDATE, 1000) + START_DATE / 1000
           IF (((STOP_DATE / 1000) .GT. (START_DATE / 1000)) .AND.
     &         (JDATE_CHECK .LT. START_DATE)) THEN
               JDATE_CHECK = MOD(JDATE, 1000) + STOP_DATE / 1000
               EDATE_CHECK = MOD(EDATE, 1000) + STOP_DATE / 1000
           ENDIF
       ELSE
           JDATE_CHECK = JDATE
           EDATE_CHECK = EDATE
       ENDIF
       
      IF( (JDATE_CHECK .GT. STOP_DATE) .OR.
     &    (JDATE_CHECK .EQ. STOP_DATE .AND.  jtime .GT. STOP_TIME))THEN
              WRITE(LOGDEV, *)'METBDY3D file ', JDATE_CHECK,' begins after STOP_DATE ',
     &        STOP_DATE,' and STOP_TIME ', STOP_TIME
              WRITE(LOGDEV, *)' *to ignore year set IGNORE_YEAR to any positive integer' 
              STOP
      ELSEIF( (EDATE_CHECK .LT. START_DATE) .OR.
     &        (EDATE_CHECK .EQ. START_DATE .AND. etime .LT. START_TIME)) THEN
              WRITE(LOGDEV, *)'METBDY3D file ends prior to START_DATE ',
     &          START_DATE,' and START_TIME ', START_TIME
              WRITE(LOGDEV, *)' *to ignore year set IGNORE_YEAR to any positive integer' 
              STOP
      END IF

      bpchYear = ENVINT('BPCH_YEAR','Reset year for BPCH data',0,status) 



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
c  Read in the GEO 3 hour data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Call LoadPROF(LOGDEV, NBDY)
      Call LoadGEO( jdate, jtime, status )

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

C Set CGRID mechanism
      IF ( .NOT. CGRID_SPCS_INIT() ) THEN
          MSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
          CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1  )
      END IF
      CALL DEFINE_CMAQ_UNITS

      NVARS3D = NSPC_CMAQ
c SLN - replace TSTEP3D to 10000 from 30000
C WTH assume BC step same as METBDY3D file
      BC_TSTEP = MET_TSTEP
      DO V = 1, NSPC_CMAQ
         VNAME3D( V ) = CMAQ_SNAME( V )
         VDESC3D( V ) = CMAQ_LNAME( V )
         UNITS3D( V ) = CMAQ_UNITS( V )
         VTYPE3D( V ) = M3REAL
#ifdef verbose
         write( logdev, '(i3,1x,3(a16,1x),i3)')V,CMAQ_SNAME( V ),CMAQ_LNAME( V ),
     &      CMAQ_UNITS( V ), M3REAL
#endif
      ENDDO
                                                                      
#ifdef verbose
      DO V = 1, NVARS3D
         write( logdev, '(i3,1x,3(a16,1x),i3)')V,VNAME3D( V ),VDESC3D( V ),
     &                                  UNITS3D( V ),VTYPE3D( V )
      END DO                                                          
#endif
      IF( MOD(jdate, 1000) .EQ. MOD(STOP_DATE, 1000)  .AND.  jtime .GT. STOP_TIME )THEN
              WRITE(LOGDEV, *)'GEOSCHEM files begin after STOP_DATE ',
     &        STOP_DATE,' and STOP_TIME ', STOP_TIME
              STOP
       END IF

      IF( MOD(MOD(jdate, 1000), DAYS_PER_YEAR) .GT. MOD(STOP_DATE, 1000) )THEN
             WRITE(LOGDEV, *)'GEOSCHEM files begin after STOP_DATE ', STOP_DATE
             STOP
      ENDIF

       WRITE(LOGDEV, *)'GEOSCHEM files begin before or on  STOP_DATE ',
     & STOP_DATE,' and STOP_TIME ', STOP_TIME

      
      edate      = jdate
      etime      = jtime

      DO NT = 1, MXREC3D - 1
         Call Nextime( edate, etime, TSTEP3D )
      END Do

      IF( MOD(edate, 1000) .EQ. MOD(START_DATE, 1000)  .AND.  etime .LT. START_TIME )THEN
              WRITE(LOGDEV, *)'GEOSCHEM files end prior to START_DATE ',
     &        START_DATE,' and START_TIME ', START_TIME
              STOP
       END IF

      IF( MOD(edate, 1000) .LT. MOD(MOD(START_DATE, 1000), DAYS_PER_YEAR)  .AND.
     &        INT(jdate/1000) .EQ.  INT(edate/1000) )THEN
             WRITE(LOGDEV, *)'GEOSCHEM files end prior to START_DATE ', START_DATE
             STOP
      ENDIF

       WRITE(LOGDEV, *)'GEOSCHEM files end before START_DATE ',
     & START_DATE,' and START_TIME ', START_TIME


      IF(  OPEN3( BC_FNAME, FSRDWR3, PNAME )  ) THEN      
              MDATE    = SDATE3D
              MTIME    = STIME3D
              BC_TSTEP = TSTEP3D
              IF( MOD( MDATE, 1000) .EQ. 1 .AND. MTIME .EQ. 0 )THEN 
!       weird fix when MDATE start at new years
                 IF( MXREC3D .GT. 0 )THEN
                     MDATE = MDATE + 1000
                     WRITE(LOGDEV, *)'Adjusting OUTPUT DATE from ', MDATE - 1000,
     &               ' to ',MDATE
                 END IF
              END IF
              MSG = 'Found and updating existing BCFILE ' //  TRIM( BC_FNAME )
              WRITE( LOGDEV, * )MSG 
              WRITE(LOGDEV, '(a,i8,a,i8)')'Updating from MDATE = ', MDATE,' at MTIME = ',
     &                                     MTIME
              IF( ( MDATE .GT. STOP_DATE ) .OR. (MDATE .EQ. STOP_DATE .AND. MTIME .GT. STOP_TIME) )THEN
                 WRITE( LOGDEV, '(a,i8,a,i8)')'ERROR: exceeds STOP_DATE ', STOP_DATE, ' and STOP_TIME ', STOP_TIME
                 STOP
              END IF
      ELSE
              FTYPE3D = BNDARY3                                         
              NTHIK3D = 1                                               
              SDATE3D = START_DATE
              STIME3D = START_TIME
              WRITE(LOGDEV, * ) ' OPENING BC_FILE for ', SDATE3D, STIME3D
              WRITE( LOGDEV, * )TRIM( BC_FNAME ), ' not found. Attempt to create new file.'
              WRITE(LOGDEV, '(a,i8,a,i8)')'Starting from START_DATE = ', SDATE3D,' at START_TIME = ',
     &                                     STIME3D
              TSTEP3D = BC_TSTEP
              MDATE   = START_DATE
              MTIME   = START_TIME
              IF( .NOT. OPEN3( BC_FNAME, FSNEW3, PNAME ) ) THEN         
                  MSG = 'Could not open nor create ' // TRIM( BC_FNAME ) // ' file '        
                  CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )               
              END IF
      END IF                                                   

      ! allocate arrays for output data
      ALLOCATE( BDY_GT_VERT( NBDY, NLAYS3D, NSPC_GT ) )
      ALLOCATE( BDY_GS_VERT( NBDY, NLAYS3D, NSPC_GS ) )
      ALLOCATE( BDY_PF_VERT( NBDY, NLAYS3D, NSPC_PF ) )
      ALLOCATE( BDY2( NBDY, NLAYS3D, NVARS3D ) )
      ALLOCATE( BCOUT( NBDY, NLAYS3D ) )

c     if( status.ne.0 ) EXIT

C SLN - hack this in to read 24 hour GC files cuz there is no time for better

      ALLOCATE( BDY_GT( NBDY, N_GEO_LAYS, NSPC_GT ) )
      ALLOCATE( BDY_GS( NBDY, N_GEO_LAYS, NSPC_GS ) )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  start time loop to read and process each timestep  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LOOP_TIME : Do NT= 1, MXREC3D ! time loop

       IF( MOD(jdate, 1000) .LT. MOD(MDATE, 1000) )THEN
         Call Nextime( jdate, jtime, MET_TSTEP )
         WRITE(MSG, * )'Skipping DATE on GEOSCHEM files', jdate,mdate
         CALL M3WARN(PNAME, JDATE, JTIME, MSG )
         CYCLE LOOP_TIME
       END IF
       
       IF( MOD(jdate, 1000) .EQ. MOD(MDATE, 1000) .AND. jtime .LT. mtime )THEN
         Call Nextime( jdate, jtime, MET_TSTEP )
         WRITE(MSG, * )'Skipping DATE and TIME on GEOSCHEM files', jdate, jtime
         CALL M3WARN(PNAME, JDATE, JTIME, MSG )
         CYCLE LOOP_TIME
       END IF

        WRITE(MSG, * )'Writing DATE from GEOSCHEM files'
        CALL M3WARN(PNAME, JDATE, JTIME, MSG )
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set boundary concs using GEO species & GEO vertical structure
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       DO L = 1, N_GEO_LAYS
          DO N = 1, NBDY
             C = COL_LOC( N )
             R = ROW_LOC( N )
               DO S = 1, NSPC_GT
                 BDY_GT( N, L, S ) = CGRID_GT( C, R, L, S, NT )
               ENDDO
               DO S = 1, NSPC_GS
                 BDY_GS( N, L, S ) = CGRID_GS( C, R, L, S, NT )
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

      CALL CHEM_MAP( NBDY, BDY_GT_VERT, BDY_GS_VERT, BDY_PF_VERT, 
     &               BDY2, jdate, jtime)

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
c  Write to output file 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      N = NBDY
      L = NLAYS3D

      S = 0
      LOOP_VAR: DO V = 1, NSPC_CMAQ

         BCOUT( 1:NBDY, 1:NLAYS3D ) = BDY2( 1:NBDY, 1:NLAYS3D, V )


         IF( .NOT. WRITE3( BC_FNAME, VNAME3D( V ), MDATE, MTIME,
     &                     BCOUT( 1, 1 ) ) ) THEN
            MSG =  'Could not WRITE species ' //  TRIM( VNAME3D( V ) ) // 
     &                 ' to file ' // TRIM( BC_FNAME )
            CALL M3ERR( PNAME, MDATE, MTIME, MSG, .TRUE. )
         END IF
      
      END DO LOOP_VAR

      ! advance to next time step
         CALL NEXTIME( MDATE, MTIME, BC_TSTEP)
         Call Nextime( jdate, jtime, MET_TSTEP )

          IF( MOD(jdate, 1000) .EQ. MOD(STOP_DATE, 1000) .AND. jtime .GT. STOP_TIME )THEN
               MSG = 'Stopping at MET and GEOSCHEM data.'
               CALL M3WARN(PNAME, JDATE, JTIME, MSG )
               EXIT LOOP_TIME
           END IF

           IF( MOD(jdate, 1000) .GT. STOP_DATE )THEN
               MSG = 'Stopping at MET and GEOSCHEM data.'
               CALL M3WARN(PNAME, JDATE, JTIME, MSG )
               EXIT LOOP_TIME
           END IF

  
      END DO LOOP_TIME ! time loop

      DEALLOCATE( BDY_GT_VERT )
      DEALLOCATE( BDY_GS_VERT )
      DEALLOCATE( BDY_PF_VERT )
      DEALLOCATE( BDY2 )
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

