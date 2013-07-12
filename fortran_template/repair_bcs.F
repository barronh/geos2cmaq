C-----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Read existing boundary condition file and repair the number
C    and aerosol surface area concentrations to be consistent with
C    assumed background aerosol distributions, i.e. the standard
C    deviation and geometric mean diameter around the boundaries
C    will be the same
C    Number and surface area variables must already be in the file
C
C 
C  Sep 2008 -- Sergey L. Napelenok
C      Modified for AE5
C  Jun 2011 -- HOT Pye
C      Modified for AE6 (Ca, K, Mg, Fe, Al, Si, APNCOM, ASEACATK)
C      Updated organic aerosol density from 2.0e3 to 1.3e3
C  July 2011 -- HOT Pye
C      Added ATIJ and AMNJ
C  Aug 2011 -- HOT Pye
C      Replaced AORGPA with APOC for AERO6
C  Apr 2012 -- HOT Pye (with constributions from SJR)
C      Updated for full AERO6 consistency
C      Added coarse mode surface area
C      Organized and checked species (included some species that are currently zero from GEOS-Chem)
C  Jun 2013 -- F. Akhtar
C      Added ICON test in NCELLS calculation
C
C-----------------------------------------------------------------------
      SUBROUTINE REPAIR( BC_FILE )

      IMPLICIT NONE

      INCLUDE "PARMS3.EXT"
      INCLUDE "FDESC3.EXT"
      INCLUDE "IODECL3.EXT"

C...........PARAMETERS and their descriptions:

      REAL(8), PARAMETER :: PI      = 3.14159265358979324  ! PI (single precision 3.141593)
      REAL,    PARAMETER :: RHOSO4  = 1.8e3 ! bulk density of aerosol sulfate
      REAL,    PARAMETER :: RHONH4  = 1.8e3 ! bulk density of aerosol ammonium
      REAL,    PARAMETER :: RHONO3  = 1.8e3 ! bulk density of aerosol nitrate
      REAL,    PARAMETER :: RHOORG  = 1.3e3 ! bulk density for aerosol organics following Carlton et al. 2010
      !REAL,    PARAMETER :: RHOORG  = 2.0e3 ! bulk density for aerosol organics following Carlton et al. 2010
      REAL,    PARAMETER :: RHOSOIL = 2.6e3 ! bulk density for aerosol soil dust
      REAL,    PARAMETER :: RHOSEAS = 2.2e3 ! bulk density for marine aerosol
      REAL,    PARAMETER :: RHOANTH = 2.2e3 ! bulk density for anthropogenic aerosol
      REAL,    PARAMETER :: SGINIAT = 1.7   ! initial sigma-G for Aitken mode
      REAL,    PARAMETER :: SGINIAC = 2.0   ! initial sigma-G for accumulation mode
      REAL,    PARAMETER :: SGINICO = 2.2   ! initial sigma-G for coarse mode
      REAL,    PARAMETER :: DGINIAT = 0.01E-6  ! geometric mean diameter for Aitken mode [ m ]
      REAL,    PARAMETER :: DGINIAC = 0.07E-6  ! geometric mean diameter for accum  mode [ m ]
      REAL,    PARAMETER :: DGINICO = 1.0E-6   ! geometric mean diameter for coarse mode [ m ]
      REAL,    PARAMETER :: CONMIN  = 1.0E-30  ! minimum concentration [ ug/m**3 ]
      integer, parameter :: nspecies = 57      ! number of aerosol species treated

C...........Argument VARIABLES
      CHARACTER(  16 ) :: BC_FILE

C...........LOCAL VARIABLES (scalars) and their descriptions:

      CHARACTER(  16 ) :: PNAME = 'REPAIR_BCS'  ! program name
      CHARACTER(  16 ) :: VNAME                 ! variable name
      CHARACTER( 220 ) :: MSG = ' '             ! status message string
      CHARACTER(  6 )  :: XCON                  ! Type of output

      INTEGER  JDATE      ! current model date, coded YYYYDDD
      INTEGER  JTIME      ! current model time, coded HHMMSS
      INTEGER VVERSION    ! variable version
      INTEGER NOTFOUND    ! integer of last missing variables version

      Type speciesstruct
           Character(16) :: name    ! species name in BC file
           INTEGER       :: ind     ! index in array
           INTEGER       :: mode    ! 1: Aitken, 2: accumulation, 3: coarse
           REAL          :: density ! species density
           INTEGER       :: version ! 0: All, 5: AE5, 6: AE6
           LOGICAL       :: found   ! In file: true
      end type speciesstruct

C...identification of aerosol species, mode, and density
C   that contribute to Aitken, Accumulation, and Coarse aerosol
      Type( speciesstruct ), save :: bcspecies( nspecies ) = (/
C                           name, ind, mode, density, version, found
     & speciesstruct (    'ACLI',   0,    1, RHOSEAS, 0, .false.),
     & speciesstruct (    'AECI',   0,    1, RHOANTH, 0, .false.),
     & speciesstruct (    'ANAI',   0,    1, RHOSEAS, 0, .false.),
     & speciesstruct (   'ANH4I',   0,    1,  RHONH4, 0, .false.),
     & speciesstruct (   'ANO3I',   0,    1,  RHONO3, 0, .false.),
     & speciesstruct (   'ASO4I',   0,    1,  RHOSO4, 0, .false.),
     & speciesstruct (   'AALKJ',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ABNZ1J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ABNZ2J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ABNZ3J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (    'ACLJ',   0,    2, RHOSEAS, 0, .false.),
     & speciesstruct (    'AECJ',   0,    2, RHOANTH, 0, .false.),
     & speciesstruct (  'AISO1J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AISO2J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AISO3J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (    'ANAJ',   0,    2, RHOSEAS, 0, .false.),
     & speciesstruct (   'ANH4J',   0,    2,  RHONH4, 0, .false.),
     & speciesstruct (   'ANO3J',   0,    2,  RHONO3, 0, .false.),
     & speciesstruct (  'AOLGAJ',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AOLGBJ',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AORGCJ',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (   'ASO4J',   0,    2,  RHOSO4, 0, .false.),
     & speciesstruct (   'ASQTJ',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ATOL1J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ATOL2J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ATOL3J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ATRP1J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'ATRP2J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AXYL1J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AXYL2J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (  'AXYL3J',   0,    2,  RHOORG, 0, .false.),
     & speciesstruct (    'ACLK',   0,    3, RHOSEAS, 0, .false.),
     & speciesstruct (   'ACORS',   0,    3, RHOANTH, 0, .false.),
     & speciesstruct (   'ANH4K',   0,    3,  RHONH4, 0, .false.),
     & speciesstruct (   'ANO3K',   0,    3,  RHONO3, 0, .false.),
     & speciesstruct (   'ASO4K',   0,    3,  RHOSO4, 0, .false.),
     & speciesstruct (   'ASOIL',   0,    3, RHOSOIL, 0, .false.),
     & speciesstruct (    'A25I',   0,    1, RHOANTH, 5, .false.),
     & speciesstruct (    'A25J',   0,    2, RHOANTH, 5, .false.),
     & speciesstruct (    'ANAK',   0,    3, RHOSEAS, 5, .false.),
     & speciesstruct ( 'AORGPAI',   0,    1,  RHOORG, 5, .false.),
     & speciesstruct ( 'AORGPAJ',   0,    2,  RHOORG, 5, .false.),
     & speciesstruct (    'AALJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct (    'ACAJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct (    'AFEJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct (     'AKJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct (    'AMGJ',   0,    2, RHOSEAS, 6, .false.),
     & speciesstruct (    'AMNJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct (  'AOTHRI',   0,    1, RHOANTH, 6, .false.),
     & speciesstruct (  'AOTHRJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct ( 'APNCOMI',   0,    1,  RHOORG, 6, .false.),
     & speciesstruct ( 'APNCOMJ',   0,    2,  RHOORG, 6, .false.),
     & speciesstruct (   'APOCI',   0,    1,  RHOORG, 6, .false.),
     & speciesstruct (   'APOCJ',   0,    2,  RHOORG, 6, .false.),
     & speciesstruct ( 'ASEACAT',   0,    3, RHOSEAS, 6, .false.),
     & speciesstruct (    'ASIJ',   0,    2, RHOSOIL, 6, .false.),
     & speciesstruct (    'ATIJ',   0,    2, RHOSOIL, 6, .false.)
     & /)

      INTEGER  NSTEPS      ! time, space, species dimensions/counters
      INTEGER  TSTEP 
      INTEGER  NVARS       
      INTEGER  NLAYS 
      INTEGER  NCELLS
      INTEGER  IVAR
      INTEGER  ILAY
      INTEGER  ISTEP
      INTEGER  ICELL

      INTEGER VAT0, VAC0, VCOR0, VSURFAT, VSURFAC, VSURFCO ! array indices

      REAL  M3ATKN       ! Aitken mode 3rd moment
      REAL  M3ACC        ! accumulation mode 3rd moment
      REAL  M3COR        ! coarse mode 3rd moment
      REAL  NUMFAC_ATKN  ! number conversion factor
      REAL  NUMFAC_ACC 
      REAL  NUMFAC_COR 
      REAL  SRFFAC_ATKN  ! surface area conversion factor
      REAL  SRFFAC_ACC 
      REAL  SRFFAC_COR 

      REAL, ALLOCATABLE :: BC_VALUES( :,:,: ) 

C...........EXTERNAL FUNCTIONS and their descriptions:

      INTEGER, EXTERNAL :: INDEX1

C*********************************************************************
C     begin body of program REPAIR_BCS

C...conversion factors for number and surface area
      NUMFAC_ATKN = 1.0 / ( ( DGINIAT ** 3.0 ) * EXP( ( 9.0 / 2.0 ) * ( ( LOG( SGINIAT ) ) ** 2.0 ) ) )
      NUMFAC_ACC  = 1.0 / ( ( DGINIAC ** 3.0 ) * EXP( ( 9.0 / 2.0 ) * ( ( LOG( SGINIAC ) ) ** 2.0 ) ) )
      NUMFAC_COR  = 1.0 / ( ( DGINICO ** 3.0 ) * EXP( ( 9.0 / 2.0 ) * ( ( LOG( SGINICO ) ) ** 2.0 ) ) )
      SRFFAC_ATKN = PI / ( DGINIAT * EXP( ( 5.0 / 2.0 ) * ( ( LOG( SGINIAT ) ) ** 2.0 ) ) )
      SRFFAC_ACC  = PI / ( DGINIAC * EXP( ( 5.0 / 2.0 ) * ( ( LOG( SGINIAC ) ) ** 2.0 ) ) )
      SRFFAC_COR  = PI / ( DGINICO * EXP( ( 5.0 / 2.0 ) * ( ( LOG( SGINICO ) ) ** 2.0 ) ) )

C...get the file description from the input boundary condition file

      IF ( .NOT. DESC3( BC_FILE ) ) THEN
      	 MSG = 'Could not get ' // BC_FILE // ' file description'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      NSTEPS = MXREC3D
      JDATE  = SDATE3D
      JTIME  = STIME3D
      TSTEP  = TSTEP3D
      NVARS  = NVARS3D
      NLAYS  = NLAYS3D

C...get indices for aerosols

      bcspecies%ind = 0
      do ivar= 1, nspecies
        vname            = bcspecies(ivar)%name
        bcspecies(ivar)%ind = INDEX1( VNAME, NVARS, VNAME3D )
      enddo
      notfound = -1
      do ivar= 1, nspecies
        if ( bcspecies(ivar)%ind == 0 ) THEN
          vname            = bcspecies(ivar)%name
          vversion         = bcspecies(ivar)%version
          WRITE(MSG, '(A,I1,A)') 'Could not find ' // VNAME // ' AE', vversion, ' in ' // BC_FILE //' file'
          CALL M3WARN ( PNAME, 0, 0, MSG )
          if (notfound .le. 0) then
              notfound  = vversion
          else
              if (notfound .ne. vversion) then
                WRITE(MSG, '(A,I2,A, I2,A)') 'Aerosols from both ', notfound, ' and ', vversion, ' were not found'
                CALL M3WARN( PNAME, 0, 0, MSG )
              endif
          endif
        endif
      enddo
    
C...get indices for the number concentration (add index if it does not exist)

      VNAME = 'NUMATKN'
      VAT0 = INDEX1( VNAME, NVARS, VNAME3D )
      IF ( VAT0 .EQ. 0 ) THEN
         MSG = 'Could not find ' // VNAME // '...there is a problem'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      VNAME = 'NUMACC'
      VAC0 = INDEX1( VNAME, NVARS, VNAME3D )
      IF ( VAC0 .EQ. 0 ) THEN
         MSG = 'Could not find ' // VNAME // '...there is a problem'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      VNAME = 'NUMCOR'
      VCOR0 = INDEX1( VNAME, NVARS, VNAME3D )
      IF ( VCOR0 .EQ. 0 ) THEN
         MSG = 'Could not find ' // VNAME // '...there is a problem'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

C...get indices for the surface area concentration

      VNAME = 'SRFATKN'
      VSURFAT = INDEX1( VNAME, NVARS, VNAME3D )
      IF ( VSURFAT .EQ. 0 ) THEN
         MSG = 'Could not find ' // VNAME // '...there is a problem'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      VNAME = 'SRFACC'
      VSURFAC = INDEX1( VNAME, NVARS, VNAME3D )
      IF ( VSURFAC .EQ. 0 ) THEN
         MSG = 'Could not find ' // VNAME // '...there is a problem'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      VNAME = 'SRFCOR'
      VSURFCO = INDEX1( VNAME, NVARS, VNAME3D )
      IF ( VSURFCO .EQ. 0 ) THEN
         MSG = 'Could not find ' // VNAME // '...there is a problem'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

C...allocate arrays
C...define full array in case of icon files
      CALL getenv( 'XCON', XCON )

      IF ( XCON == 'ICON' ) THEN
         NCELLS = NCOLS3D * NROWS3D
      ELSE
         NCELLS = 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
      END IF

      ALLOCATE( BC_VALUES(  NCELLS, NLAYS, NVARS ) )

C...begin loop through all timesteps on input file

      DO ISTEP = 1, NSTEPS

         IF ( .NOT. READ3( BC_FILE, ALLVAR3, ALLAYS3, JDATE,
     &                     JTIME, BC_VALUES ) ) THEN
            MSG = 'Could not read all variables from ' // BC_FILE
            CALL M3ERR ( PNAME, JDATE, JTIME, MSG, .TRUE. )
         END IF

C...process data in all layers and cells

         DO ILAY = 1, NLAYS
            DO ICELL = 1, NCELLS

C...Calculate aerosol 3rd moment concentrations [ m**3 / m**3 ]
               m3atkn = 0
               m3acc  = 0
               m3cor  = 0

               do ivar= 1, nspecies
                  if ( bcspecies(ivar)%ind > 0 ) THEN
                     if ( bcspecies(ivar)%mode == 1 ) THEN
                        m3atkn = m3atkn + 1.0e-9*6.0/( pi*bcspecies(ivar)%density ) * bc_values(icell,ilay,bcspecies(ivar)%ind)
                     elseif ( bcspecies(ivar)%mode == 2 ) THEN
                        m3acc  = m3acc  + 1.0e-9*6.0/( pi*bcspecies(ivar)%density )* bc_values(icell,ilay,bcspecies(ivar)%ind)   
                     elseif ( bcspecies(ivar)%mode == 3 ) THEN
                        m3cor  = m3cor  + 1.0e-9*6.0/( pi*bcspecies(ivar)%density )* bc_values(icell,ilay,bcspecies(ivar)%ind)
                     endif
                  endif
               enddo

C...Calculate number concentrations [ # / m**3 ] and fill/overwrite position

               bc_values( ICELL, ILAY, VAT0  ) = NUMFAC_ATKN * M3ATKN
               bc_values( ICELL, ILAY, VAC0  ) = NUMFAC_ACC  * M3ACC
               bc_values( ICELL, ILAY, VCOR0 ) = NUMFAC_COR  * M3COR

C...Calculate surface area concentrations [ m**2 / m**3 ]

               bc_values( ICELL, ILAY, VSURFAT ) = SRFFAC_ATKN * M3ATKN
               bc_values( ICELL, ILAY, VSURFAC ) = SRFFAC_ACC  * M3ACC
               bc_values( ICELL, ILAY, VSURFCO ) = SRFFAC_COR  * M3COR

            END DO
         END DO

C...write updated boundary conditions to the output file

         IF ( .NOT. WRITE3( BC_FILE, ALLVAR3, JDATE, JTIME,
     &                      bc_values( 1, 1, 1 ) ) ) THEN
            MSG =  'Could not WRITE all species to file ' // BC_FILE 
            CALL M3ERR ( PNAME, JDATE, JTIME, MSG, .TRUE. )
         ENDIF

         CALL NEXTIME ( JDATE, JTIME, TSTEP )

      END DO

      DEALLOCATE ( BC_VALUES  )
      RETURN

      END SUBROUTINE REPAIR
