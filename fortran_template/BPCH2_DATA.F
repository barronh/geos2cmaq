      MODULE GEO_DATA

C************************************************************************
C
C     FUNCTION: Defines Harvard CTM data BPCH2 format
C
C  Apr 2006 -- Sergey L. Napelenok
C      Modified to describe GEOS-Chem files provided by Daven Henze
C  Mar 2011 -- Sergey L. Napelenok
C      Modified for AQMEII project
C      Use both tracer file and species file
C
C....................................................................

c..Data file variable
      CHARACTER( 256 )   ::  GEO_INPUT_PATH
      CHARACTER( 256 )   ::  GEO_INPUT_FILE
      CHARACTER( 16 )    ::  GT_FILE = 'GT_FILE'
      CHARACTER( 16 )    ::  GS_FILE = 'GS_FILE'
c..Horizontal Grid data
      INTEGER :: NCOLS_GEO
      INTEGER :: NROWS_GEO

      REAL, ALLOCATABLE  :: LON_GEO(   : , : )
      REAL, ALLOCATABLE  :: LAT_GEO(   : , : )
      REAL, ALLOCATABLE  :: CGRID_GT( : , :, :, :, : )
      REAL, ALLOCATABLE  :: CGRID_GS( : , :, :, :, : )

c..Redefinition of record's year
      INTEGER   bpchYear, SKIP_COLS, SKIP_ROWS
      integer :: i, j, k

#include <GEOS_DOMAIN.INC>

c SLN (14Mar2011) species list in GEOSChem tracer file
#include <MAPPING.TRACER>
#include <MAPPING.CSPEC>

      CONTAINS

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      !  Subroutine to read the GEO monthly average data
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Subroutine LoadGEO(JDATE, JTIME, status )
      
      USE UTILIO_DEFN

      Implicit None

      Integer, Intent( IN  ) :: JDATE, JTIME
      Integer, Intent( OUT ) :: status
      ! array argument
!     Real, Intent( OUT ) :: CGRID_GT( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS, NSPC_GT, 24 )
!     Real, Intent( OUT ) :: CGRID_GS( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS, NSPC_GS, 24 )

      ! external functions
!      Integer GETEFILE
!      Integer SECSDIFF
!      Integer ENVINT   
      
      ! local variables
      Integer GEO_UNIT
      Character*(80) MSG      
      Integer recDate, recTime, tDiff
      Logical found
      Character*(128) fname
      Character*(40) ftype
      Character*(80) toptitle
 
      Character*(20) modelName
      Real modelRes(2)
      Integer halfpolar
      Integer center180
      Integer myear,mnth, mday
 
      Character*40 category
      Integer  tracer
      Character*40 unit
      Real*8   tau0, tau1, ntau0, ntau1
      Character*40 reserved
      Integer dim(6)
      Integer skip
      Real blank
      Logical, save :: first = .true.
      Logical, save :: firstread = .true.

      INTEGER  :: NCOUNT, R, C


      Real, ALLOCATABLE :: buffer( :, :, :)

      integer timestep

      status = 0

      ! on first call, get path of input data file
      if( first ) then
        Call ENVSTR('GEO_INPUT_PATH','Path of GEO_INPUT_FILE', ' ', GEO_INPUT_PATH, status)
        if( status.ne.0 ) then
          MSG = 'ERROR: GEO_INPUT_PATH not defined'
          CALL M3ERR ('BPCH_BCON', JDATE, JTIME, MSG, .TRUE. )
          endif
        first = .false.
        endif

c - TRACER file

      ! build GEO_INPUT_FILE
      myear = JDATE / 1000
      Call DAYMON(JDATE, mnth, mday)
c     mhour = JTIME / 10000
      write(GEO_INPUT_FILE,'(a,''BC.'',i4.4,2i2.2)') TRIM(GEO_INPUT_PATH),
     &      myear, mnth, mday
     !Write out filename to terminal for debugging - FHA 6-18-2013
      write(*,'(''Reading GEOS input file: '',a,''BC.'',i4.4,2i2.2)') 
     &      TRIM(GEO_INPUT_PATH), myear, mnth, mday

      ! open data file 
      !GEO_UNIT=GETEFILE( GT_FILE, .TRUE., .FALSE., 'BPCH_BCON' )
      GEO_UNIT = 98
      OPEN( GEO_UNIT,FILE=GEO_INPUT_FILE,STATUS='OLD',FORM='UNFORMATTED',
     &      ACCESS='SEQUENTIAL',iostat=status)

      if( GEO_UNIT .lt. 0 ) then
        MSG =  'ERROR: Cannot open GEO_INPUT_FILE '
     &      // GT_FILE
     &      // ' not defined '
        CALL M3ERR ('BPCH_BCON', JDATE, JTIME, MSG, .TRUE. )
      else
        MSG =  'Opening GEO_INPUT_FILE '
     &      // GT_FILE
        CALL M3MESG ( MSG )
      endif

C  read header record  
      read(GEO_UNIT, iostat=status) ftype
      if( status.ne.0 ) then
        write(*,'(''**ERROR** cannot read header record ftype'')')
        found = .false.
        goto 100
        endif

      read(GEO_UNIT, iostat=status) toptitle
      if( status.ne.0 ) then
        write(*,'(''**ERROR** cannot read header record toptitle'')')
        found = .false.
        goto 100
        endif

C  read record of data values
      IF (.not. FIRSTREAD) THEN
         CGRID_GT = -0.00001
      ENDIF

C  read record of data values and fill array
      found = .false.

      do timestep = 1, 24

      do NCOUNT = 1, NSPC_GT

        read(GEO_UNIT,iostat=status,end=100) modelName, modelRes, halfpolar, center180   
        if( status.ne.0 ) then
          write(*,'(''**ERROR** cannot read data model header record'')')
          found = .false.
          goto 100
          endif

        read(GEO_UNIT,iostat=status) category, tracer, unit, tau0, tau1,         
     &                         reserved, dim, skip
        IF (tracer.ne.NCOUNT) THEN
          write(*,'(''ERROR Tracer expected'' (i4)''; tracer found'' (i4))'), NCOUNT, tracer
          STOP
        ENDIF
        IF (FIRSTREAD) THEN
          SKIP_COLS = dim(4)
          SKIP_ROWS = dim(5)
          NCOLS_GEO = dim(1)
          NROWS_GEO = dim(2)
          print*,'Dimensions of GEOS-Chem data: ', NCOLS_GEO, NROWS_GEO
          print*,'Output excludes (NCOLS, NROWS): ', SKIP_COLS, SKIP_ROWS
          print*,'Resolution (hard coded lat/lon): ', DEL_LAT, DEL_LON
          print*,'Poles are at half resolution: ', halfpolar
!         N_GEO_LAYS = dim(3)
          ALLOCATE( BUFFER( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS) )
          ALLOCATE( CGRID_GT( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS, NSPC_GT, 25 ) )
          ALLOCATE( CGRID_GS( NCOLS_GEO, NROWS_GEO, N_GEO_LAYS, NSPC_GS, 25 ) )
          CGRID_GT = -0.00001
          CGRID_GS = -0.00001
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Define the lat/lons for the GEOCTM model grid
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Longitude sequence: -181.25 -178.75 .... 176.25 178.75     
c Latitude sequence:   -90.00 -98.00 ....  87.0 89.0 90.0
    
          ALLOCATE( LON_GEO( NCOLS_GEO, NROWS_GEO ) )
          ALLOCATE( LAT_GEO( NCOLS_GEO, NROWS_GEO ) )
    
c..temporarily set latitude of bottom row to -90.0
          DO C = 1, NCOLS_GEO
             IF (HALFPOLAR.eq.1) THEN
                 IF (SKIP_ROWS.eq.1) THEN
                   LAT_GEO( C, 1 ) = -89.5
                 ELSE
                   LAT_GEO( C, 1 ) = (SKIP_ROWS-1)*DEL_LAT - 0.25 * DEL_LAT - 89.5
                 ENDIF
             ELSE
                 LAT_GEO( C, 1 ) = (SKIP_ROWS-1)*DEL_LAT - 89
             ENDIF
          ENDDO
    
c..set remaining rows by incrementing by delta latitude
          DO R = 2, NROWS_GEO
             DO C = 1, NCOLS_GEO
                LAT_GEO( C, R ) =  LAT_GEO( C, R - 1 ) + DEL_LAT
             ENDDO
          ENDDO
        
    
c..set remaining rows by incrementing by delta latitude
          DO R = 1, NROWS_GEO
             LON_GEO( 1, R ) = (SKIP_COLS-1) * DEL_LON - 180.
          ENDDO
    
c..set remaining rows by incrementing by delta longitude
          DO R = 1, NROWS_GEO
             DO C = 2, NCOLS_GEO
                LON_GEO( C, R ) = LON_GEO( C-1, R ) + DEL_LON
             ENDDO
          ENDDO

c.. Printing for testing purposes    
          print*,'Latitudes of GEOS-Chem grid: ',(LAT_GEO(1,R),R=1,NROWS_GEO)
          print*,'Longitudes of GEOS-Chem grid: ', (LON_GEO(C, 1),C=1,NCOLS_GEO)
          
          FIRSTREAD = .false. 
        ENDIF
        if( status.ne.0 ) then
          write(*,'(''**ERROR** cannot read simulation header'')')
          found = .false.
          goto 100
          endif

        ! skip species with less than N_GEO_LAYS layers
        if( dim(3) .gt. N_GEO_LAYS ) then
          read(GEO_UNIT) blank
          cycle
          endif
        
        read(GEO_UNIT,iostat=status) (((buffer(i, j, k), i=1,dim(1)), j=1,dim(2)), k=1,dim(3))
        if( status.ne.0 ) then
         write(*,'(''**ERROR** cannot read data record for tracer'',i5)') NCOUNT
         found = .false.
         goto 100
         endif

        found = .true.
        CGRID_GT(:,:,1:MIN(dim(3), N_GEO_LAYS),NCOUNT,timestep) =  buffer(:,:,1:MIN(dim(3), N_GEO_LAYS))

        enddo

        enddo

  100 Close (GEO_UNIT)
      if( .NOT.found) then
        print *, NCOUNT
        MSG = 'ERROR: Could not read GEO_INPUT_FILE'                           
        CALL M3ERR ('BPCH_BCON', JDATE, JTIME, MSG, .TRUE. ) 
        endif

c - SPECIES file

      ! build GEO_INPUT_FILE
      myear = JDATE / 1000
      Call DAYMON(JDATE, mnth, mday)
c     mhour = JTIME / 10000
      write(GEO_INPUT_FILE,'(a,''BC.CSPEC.'',i4.4,2i2.2)') TRIM(GEO_INPUT_PATH),
     &      myear, mnth, mday

      ! open data file 


!      GEO_UNIT=GETEFILE( GS_FILE, .TRUE., .FALSE., 'BPCH_BCON' )
      Call ENVSTR('GEO_INPUT_PATH','Path of GEO_INPUT_FILE', ' ', GEO_INPUT_PATH, status)

      OPEN( GEO_UNIT,FILE=GEO_INPUT_FILE,STATUS='OLD',FORM='UNFORMATTED',
     &      ACCESS='SEQUENTIAL',iostat=status)

      if( GEO_UNIT .lt. 0 ) then
        MSG =  'ERROR: Cannot open GEO_INPUT_FILE '
     &      // GS_FILE
     &      // ' not defined '
        CALL M3ERR ('BPCH_BCON', JDATE, JTIME, MSG, .TRUE. )
      else
        MSG =  'Opening GEO_INPUT_FILE '
     &      // TRIM( GEO_INPUT_FILE )
        CALL M3MESG ( MSG )
      endif

C  read header record  
      read(GEO_UNIT, iostat=status) ftype
      if( status.ne.0 ) then
        write(*,'(''**ERROR** cannot read header record 1'')')
        goto 101
        endif

      read(GEO_UNIT, iostat=status) toptitle
      if( status.ne.0 ) then
        write(*,'(''**ERROR** cannot read header record 2'')')
        goto 101
        endif

C  read record of data values
      CGRID_GS = -0.00001

C  read record of data values and fill array
      found = .false.


1024  timeloop : do timestep = 1, 24

      specloop: do NCOUNT = 1, NSPC_GS

        if( NCOUNT.gt.NSPC_GS ) EXIT

        read(GEO_UNIT,iostat=status,end=101) modelName, modelRes, halfpolar, center180
        if( status.ne.0 ) then
          write(*,'(''**ERROR** cannot read data header record 1'')')
          goto 101
          endif

        read(GEO_UNIT,iostat=status) category, tracer, unit, tau0, tau1,
     &                         reserved, dim, skip
        IF (tracer.ne.NCOUNT) THEN
          write(*,'(''**ERROR** Tracer expected (i4); tracer found (i4)'')')
          write(*,*), NCOUNT, tracer, timestep
          BACKSPACE(GEO_UNIT)
          BACKSPACE(GEO_UNIT)
          CYCLE timeloop
        ENDIF
        if( status.ne.0 ) then
          write(*,'(''**ERROR** cannot read data header record 2'')')
          goto 101
          endif

        ! skip species with less than N_GEO_LAYS layers
        if( dim(3) .gt. N_GEO_LAYS ) then
          read(GEO_UNIT) blank
          cycle specloop
          endif

        read(GEO_UNIT,iostat=status) (((buffer(i, j, k), i=1,dim(1)), j=1,dim(2)), k=1,dim(3))
        if( status.ne.0 ) then
         write(*,'(''**ERROR** cannot read data record for tracer'',i5)') NCOUNT
         goto 101
         endif

        found = .true.
        CGRID_GS(:,:,:,NCOUNT,timestep) =  buffer

        enddo specloop
        ntau0 = tau0
        if (timestep.ne.24) then
        DO WHILE ( ntau0.eq.tau0 )
        read(GEO_UNIT,iostat=status)
        read(GEO_UNIT,iostat=status) category, tracer, unit, ntau0, ntau1
        read(GEO_UNIT,iostat=status)
        enddo
        BACKSPACE(GEO_UNIT)
        BACKSPACE(GEO_UNIT)
        BACKSPACE(GEO_UNIT)
        endif
        enddo timeloop

  101 Close (GEO_UNIT)
      if( .NOT.found) then
        MSG = 'ERROR: Could not read GEO_INPUT_FILE'                           
        CALL M3ERR ('BPCH_BCON', JDATE, JTIME, MSG, .TRUE. )
        endif


      Return 
      End Subroutine LoadGEO

      END MODULE GEO_DATA
