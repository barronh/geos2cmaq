C-----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Read existing boundary condition file and repair the number
C    and aerosol surface area concentrations to be consistent with
C    assumed background aerosol distributions, i.e. the standard
C    deviation and geometric mean diameter around the boundaries
C    will be the same
C
C 
C  Sep 2008 -- Sergey L. Napelenok
C      Modified for AE5
C
C-----------------------------------------------------------------------
      SUBROUTINE REPAIR( BC_FILE )

      IMPLICIT NONE

      INCLUDE SUBST_IOPARMS     ! IOAPI parameters
      INCLUDE SUBST_IOFDESC     ! IOAPI file description
      INCLUDE SUBST_IODECL      ! IOAPI declarations

C...........PARAMETERS and their descriptions:

      REAL(8), PARAMETER :: PI      = 3.14159265358979324  ! PI (single precision 3.141593)
      REAL,   PARAMETER :: F6DPI   = 6.0 / PI
      REAL,   PARAMETER :: F6DPIM9 = 1.0E-9 * F6DPI
      REAL,   PARAMETER :: RHOSO4  = 1.8e3 !  bulk density of aerosol sulfate
      REAL,   PARAMETER :: RHONH4  = 1.8e3 !  bulk density of aerosol ammonium
      REAL,   PARAMETER :: RHONO3  = 1.8e3 ! bulk density of aerosol nitrate
      REAL,   PARAMETER :: RHOH2O  = 1.0e3 !  bulk density of aerosol water
      REAL,   PARAMETER :: RHOORG  = 2.0e3 ! bulk density for aerosol organics
      REAL,   PARAMETER :: RHOSOIL = 2.6e3 ! bulk density for aerosol soil dust
      REAL,   PARAMETER :: RHOSEAS = 2.2e3 ! bulk density for marine aerosol
      REAL,   PARAMETER :: RHOANTH = 2.2e3 ! bulk density for anthropogenic aerosol
      REAL,   PARAMETER :: RHOPHG  = 2.0e3 ! bulk density for mercury aerosol
      REAL,   PARAMETER :: SO4FAC  = F6DPIM9 / RHOSO4 ! Factor to convert mass conc [ug m**-3]
      REAL,   PARAMETER :: NH4FAC  = F6DPIM9 / RHONH4 !  to 3rd moment conc [m**3 m^-3]
      REAL,   PARAMETER :: H2OFAC  = F6DPIM9 / RHOH2O
      REAL,   PARAMETER :: NO3FAC  = F6DPIM9 / RHONO3
      REAL,   PARAMETER :: ORGFAC  = F6DPIM9 / RHOORG
      REAL,   PARAMETER :: SOILFAC = F6DPIM9 / RHOSOIL
      REAL,   PARAMETER :: SEASFAC = F6DPIM9 / RHOSEAS
      REAL,   PARAMETER :: ANTHFAC = F6DPIM9 / RHOANTH
      REAL,   PARAMETER :: PHGFAC  = F6DPIM9 / RHOPHG
      REAL,   PARAMETER :: SGINIAT = 1.7  ! initial sigma-G for Aitken mode
      REAL,   PARAMETER :: SGINIAC = 2.0  ! initial sigma-G for accumulation mode
      REAL,   PARAMETER :: SGINICO = 2.2  ! initial sigma-G for coarse mode
      REAL,   PARAMETER :: DGINIAT = 0.01E-6  ! geometric mean diameter for Aitken mode [ m ]
      REAL,   PARAMETER :: DGINIAC = 0.07E-6  ! geometric mean diameter for accum  mode [ m ]
      REAL,   PARAMETER :: DGINICO = 1.0E-6   ! geometric mean diameter for coarse mode [ m ]
      REAL,   PARAMETER :: CONMIN  = 1.0E-30  ! minimum concentration [ ug/m**3 ]

C...........Argument VARIABLES
      CHARACTER(  16 ) :: BC_FILE

C...........LOCAL VARIABLES (scalars) and their descriptions:

      CHARACTER(  16 ) :: PNAME = 'REPAIR_BCS'  ! program name
      CHARACTER( 120 ) :: MSG = ' '             ! status message string

      INTEGER  JDATE      ! current model date, coded YYYYDDD
      INTEGER  JTIME      ! current model time, coded HHMMSS
      INTEGER  VSO4AK     ! coarse mode sulfate aerosol conc
      INTEGER  VSO4AJ     ! Accumulation mode sulfate aerosol conc
      INTEGER  VSO4AI     ! Aitken mode sulfate conc
      INTEGER  VNH4AJ     ! Accumulation mode aerosol ammonium conc
      INTEGER  VNH4AI     ! Aitken mode ammonium conc
      INTEGER  VNO3AK     ! coarse mode aerosol nitrate conc
      INTEGER  VNO3AJ     ! Accumulation mode aerosol nitrate conc
      INTEGER  VNO3AI     ! Aitken mode nitrate conc
      INTEGER  VNAK       ! coarse mode sodium aerosol conc
      INTEGER  VNAJ       ! Accumulation mode sodium aerosol conc
      INTEGER  VNAI       ! Aitken mode sodium aerosol conc
      INTEGER  VCLK       ! coarse mode chloride aerosol conc
      INTEGER  VCLJ       ! Accumulation mode chloride aerosol conc
      INTEGER  VCLI       ! Aitken mode chloride aerosol conc
      INTEGER  VORGPAJ    ! Accumulation mode primary anthropogenic organic aerosol conc
      INTEGER  VORGPAI    ! Aitken mode primary anthropogenic organic aerosol conc
c     INTEGER  VORGAJ     ! Accumulation mode anthropogenic organic aerosol conc
c     INTEGER  VORGAI     ! Aitken mode anthropogenic organic aerosol conc
c     INTEGER  VORGBJ     ! Accumulation mode biogenic organic aerosol conc
c     INTEGER  VORGBI     ! Aitken mode biogenic organic aerosol conc
      INTEGER  VECJ       ! Accumulation mode elemental carbon
      INTEGER  VECI       ! Aitken mode elemental carbon
      INTEGER  VP25AJ     ! Accumulation mode primary PM2.5 conc
      INTEGER  VP25AI     ! Aitken mode primary PM2.5 conc
      INTEGER  VANTHA     ! coarse mode anthropogenic aerosol conc
      INTEGER  VSEAS      ! coarse mode marine aerosol conc
      INTEGER  VSOILA     ! coarse mode soil-derived aerosol conc
      INTEGER  VAT0       ! Aitken mode number conc
      INTEGER  VAC0       ! accum  mode number conc
      INTEGER  VCOR0      ! coarse mode number conc
      INTEGER  VSURFAT    ! Index for Aitken mode surface area
      INTEGER  VSURFAC    ! Index for accumulatin mode surface area
      INTEGER  VPHGJ      ! Accumulation mode particulate mercury
      INTEGER  VPHGI      ! Aitken mode particulate mercury

      INTEGER   VALKJ     !   Accumulation mode SOA from "long" alkanes
      INTEGER   VXYL1J    !   Accumulation mode SOA from low-yield arom 
      INTEGER   VXYL2J    !   Accumulation mode SOA from low-yield arom 
      INTEGER   VXYL3J    !   Accumulation mode SOA from low-yield arom 
      INTEGER   VTOL1J    !   Accumulation mode SOA from high-yield arom
      INTEGER   VTOL2J    !   Accumulation mode SOA from high-yield arom  
      INTEGER   VTOL3J    !   Accumulation mode SOA from high-yield arom
      INTEGER   VBNZ1J    !   Accumulation mode SOA from benzene
      INTEGER   VBNZ2J    !   Accumulation mode SOA from benzene
      INTEGER   VBNZ3J    !   Accumulation mode SOA from benzene
      INTEGER   VTRP1J    !   Accumulation mode SOA from monoterpenes
      INTEGER   VTRP2J    !   Accumulation mode SOA from monoterpenes  
      INTEGER   VISO1J    !   Accumulation mode SOA from isoprene
      INTEGER   VISO2J    !   Accumulation mode SOA from isoprene  
      INTEGER   VISO3J    !   Accumulation mode SOA from isoprene
      INTEGER   VSQTJ     !   Accumulation mode SOA from sesquiterpenes
      INTEGER   VOLGAJ    !   Accum mode anthropogenic organic oligomers
      INTEGER   VOLGBJ    !   Accum mode biogenic organic oligomers
      INTEGER   VORGCJ    !   Accumulation mode SOA produced in clouds



      INTEGER  NSTEPS
      INTEGER  TSTEP 
      INTEGER  NVARS 
      INTEGER  NLAYS 
      INTEGER  NCELLS
      INTEGER  IVAR
      INTEGER  ILAY
      INTEGER  ISTEP
      INTEGER  ICELL

      REAL  M3ATKN       ! Aitken mode 3rd moment
      REAL  M3ACC        ! accumulation mode 3rd moment
      REAL  M3COR        ! coarse mode 3rd moment
      REAL  NUMFAC_ATKN
      REAL  NUMFAC_ACC 
      REAL  NUMFAC_COR 
      REAL  SRFFAC_ATKN
      REAL  SRFFAC_ACC 

      REAL, ALLOCATABLE :: BC_VALUES  ( :,:,: ) ! boundary conditions

C...........EXTERNAL FUNCTIONS and their descriptions:

      INTEGER, EXTERNAL :: INDEX1

C*********************************************************************
C     begin body of program REPAIR_BCS

      NUMFAC_ATKN = 1.0 / ( ( DGINIAT ** 3.0 ) * EXP( ( 9.0 / 2.0 ) * ( ( LOG( SGINIAT ) ) ** 2.0 ) ) )
      NUMFAC_ACC  = 1.0 / ( ( DGINIAC ** 3.0 ) * EXP( ( 9.0 / 2.0 ) * ( ( LOG( SGINIAC ) ) ** 2.0 ) ) )
      NUMFAC_COR  = 1.0 / ( ( DGINICO ** 3.0 ) * EXP( ( 9.0 / 2.0 ) * ( ( LOG( SGINICO ) ) ** 2.0 ) ) )
      SRFFAC_ATKN = PI / ( DGINIAT * EXP( ( 5.0 / 2.0 ) * ( ( LOG( SGINIAT ) ) ** 2.0 ) ) )
      SRFFAC_ACC  = PI / ( DGINIAC * EXP( ( 5.0 / 2.0 ) * ( ( LOG( SGINIAC ) ) ** 2.0 ) ) )

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

C...get indices for the Aitken mode aerosols

      VSO4AI = INDEX1( 'ASO4I', NVARS, VNAME3D )

      VNH4AI = INDEX1( 'ANH4I', NVARS, VNAME3D )

      VNO3AI = INDEX1( 'ANO3I', NVARS, VNAME3D )

      VORGPAI = INDEX1( 'AORGPAI', NVARS, VNAME3D )

c     VORGAI = INDEX1( 'AORGAI', NVARS, VNAME3D )

c     VORGBI = INDEX1( 'AORGBI', NVARS, VNAME3D )

      VECI = INDEX1( 'AECI', NVARS, VNAME3D )

      VP25AI = INDEX1( 'A25I', NVARS, VNAME3D )

      VNAI = INDEX1( 'ANAI', NVARS, VNAME3D )

      VCLI = INDEX1( 'ACLI', NVARS, VNAME3D )

      VPHGI = INDEX1( 'APHGI', NVARS, VNAME3D )

C...get indices for the accumulation mode aerosols

      VSO4AJ = INDEX1( 'ASO4J', NVARS, VNAME3D )

      VNH4AJ = INDEX1( 'ANH4J', NVARS, VNAME3D )

      VNO3AJ = INDEX1( 'ANO3J', NVARS, VNAME3D )

      VORGPAJ = INDEX1( 'AORGPAJ', NVARS, VNAME3D )

c     VORGAJ = INDEX1( 'AORGAJ', NVARS, VNAME3D )

c     VORGBJ = INDEX1( 'AORGBJ', NVARS, VNAME3D )

      VECJ = INDEX1( 'AECJ', NVARS, VNAME3D )

      VP25AJ = INDEX1( 'A25J', NVARS, VNAME3D )

      VNAJ = INDEX1( 'ANAJ', NVARS, VNAME3D )

      VCLJ = INDEX1( 'ACLJ', NVARS, VNAME3D )

      VPHGJ = INDEX1( 'APHGJ', NVARS, VNAME3D )

      VALKJ   = INDEX1( 'VALKJ',  NVARS, VNAME3D )
      VXYL1J  = INDEX1( 'VXYL1J', NVARS, VNAME3D )
      VXYL2J  = INDEX1( 'VXYL2J', NVARS, VNAME3D )
      VXYL3J  = INDEX1( 'VXYL3J', NVARS, VNAME3D )
      VTOL1J  = INDEX1( 'VTOL1J', NVARS, VNAME3D )
      VTOL2J  = INDEX1( 'VTOL2J', NVARS, VNAME3D ) 
      VTOL3J  = INDEX1( 'VTOL3J', NVARS, VNAME3D )
      VBNZ1J  = INDEX1( 'VBNZ1J', NVARS, VNAME3D )
      VBNZ2J  = INDEX1( 'VBNZ2J', NVARS, VNAME3D )
      VBNZ3J  = INDEX1( 'VBNZ3J', NVARS, VNAME3D )
      VTRP1J  = INDEX1( 'VTRP1J', NVARS, VNAME3D )
      VTRP2J  = INDEX1( 'VTRP2J', NVARS, VNAME3D )
      VISO1J  = INDEX1( 'VISO1J', NVARS, VNAME3D )
      VISO2J  = INDEX1( 'VISO2J', NVARS, VNAME3D )
      VISO3J  = INDEX1( 'VISO3J', NVARS, VNAME3D )
      VSQTJ   = INDEX1( 'VSQTJ',  NVARS, VNAME3D )
      VOLGAJ  = INDEX1( 'VOLGAJ', NVARS, VNAME3D )
      VOLGBJ  = INDEX1( 'VOLGBJ', NVARS, VNAME3D )
      VORGCJ  = INDEX1( 'VORGCJ', NVARS, VNAME3D )

C...get indices for the coarse mode aerosols

      VANTHA = INDEX1( 'ACORS', NVARS, VNAME3D )

      VSEAS = INDEX1( 'ASEAS', NVARS, VNAME3D )

      VSOILA = INDEX1( 'ASOIL', NVARS, VNAME3D )

      VNAK = INDEX1( 'ANAK', NVARS, VNAME3D )

      VCLK = INDEX1( 'ACLK', NVARS, VNAME3D )

      VSO4AK = INDEX1( 'ASO4K', NVARS, VNAME3D )

      VNO3AK = INDEX1( 'ANO3K', NVARS, VNAME3D )

C...get indices for the number concentration

      VAT0 = INDEX1( 'NUMATKN', NVARS, VNAME3D )

      VAC0 = INDEX1( 'NUMACC', NVARS, VNAME3D )

      VCOR0 = INDEX1( 'NUMCOR', NVARS, VNAME3D )

C...get indices for the surface area concentration

      VSURFAT = INDEX1( 'SRFATKN', NVARS, VNAME3D )

      VSURFAC = INDEX1( 'SRFACC', NVARS, VNAME3D )

C...allocate arrays

      NCELLS = 2 * NTHIK3D * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
      ALLOCATE( BC_VALUES( NCELLS, NLAYS, NVARS ) )

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

C... Aitken mode

               M3ATKN = 0.0
               IF ( VSO4AI  .NE. 0 ) M3ATKN = M3ATKN + SO4FAC  * BC_VALUES( ICELL, ILAY, VSO4AI  )
               IF ( VNH4AI  .NE. 0 ) M3ATKN = M3ATKN + NH4FAC  * BC_VALUES( ICELL, ILAY, VNH4AI  )
               IF ( VNO3AI  .NE. 0 ) M3ATKN = M3ATKN + NO3FAC  * BC_VALUES( ICELL, ILAY, VNO3AI  )
               IF ( VORGPAI .NE. 0 ) M3ATKN = M3ATKN + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGPAI )
c              IF ( VORGAI  .NE. 0 ) M3ATKN = M3ATKN + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGAI  )
c              IF ( VORGBI  .NE. 0 ) M3ATKN = M3ATKN + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGBI  )
               IF ( VP25AI  .NE. 0 ) M3ATKN = M3ATKN + ANTHFAC * BC_VALUES( ICELL, ILAY, VP25AI  )
               IF ( VECI    .NE. 0 ) M3ATKN = M3ATKN + ANTHFAC * BC_VALUES( ICELL, ILAY, VECI    )
               IF ( VNAI    .NE. 0 ) M3ATKN = M3ATKN + SEASFAC * BC_VALUES( ICELL, ILAY, VNAI    )
               IF ( VCLI    .NE. 0 ) M3ATKN = M3ATKN + SEASFAC * BC_VALUES( ICELL, ILAY, VCLI    )
               IF ( VPHGI   .NE. 0 ) M3ATKN = M3ATKN + PHGFAC  * BC_VALUES( ICELL, ILAY, VPHGI   )

C... accumulation mode

               M3ACC = 0.0
               IF ( VSO4AJ  .NE. 0 ) M3ACC = M3ACC + SO4FAC  * BC_VALUES( ICELL, ILAY, VSO4AJ  )
               IF ( VNH4AJ  .NE. 0 ) M3ACC = M3ACC + NH4FAC  * BC_VALUES( ICELL, ILAY, VNH4AJ  )
               IF ( VNO3AJ  .NE. 0 ) M3ACC = M3ACC + NO3FAC  * BC_VALUES( ICELL, ILAY, VNO3AJ  )
               IF ( VORGPAJ .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGPAJ )
c              IF ( VORGAJ  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGAJ  )
c              IF ( VORGBJ  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGBJ  )
               IF ( VP25AJ  .NE. 0 ) M3ACC = M3ACC + ANTHFAC * BC_VALUES( ICELL, ILAY, VP25AJ  )
               IF ( VECJ    .NE. 0 ) M3ACC = M3ACC + ANTHFAC * BC_VALUES( ICELL, ILAY, VECJ    )
               IF ( VNAJ    .NE. 0 ) M3ACC = M3ACC + SEASFAC * BC_VALUES( ICELL, ILAY, VNAJ    )
               IF ( VCLJ    .NE. 0 ) M3ACC = M3ACC + SEASFAC * BC_VALUES( ICELL, ILAY, VCLJ    )
               IF ( VPHGJ   .NE. 0 ) M3ACC = M3ACC + PHGFAC  * BC_VALUES( ICELL, ILAY, VPHGJ   )
               IF ( VALKJ   .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VALKJ   )
               IF ( VXYL1J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VXYL1J  )
               IF ( VXYL2J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VXYL2J  )
               IF ( VXYL3J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VXYL3J  )
               IF ( VTOL1J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VTOL1J  )
               IF ( VTOL2J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VTOL2J  )
               IF ( VTOL3J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VTOL3J  )
               IF ( VBNZ1J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VBNZ1J  )
               IF ( VBNZ2J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VBNZ2J  )
               IF ( VBNZ3J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VBNZ3J  )
               IF ( VTRP1J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VTRP1J  )
               IF ( VTRP2J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VTRP2J  )
               IF ( VISO1J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VISO1J  )
               IF ( VISO2J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VISO2J  )
               IF ( VISO3J  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VISO3J  )
               IF ( VSQTJ   .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VSQTJ   )
               IF ( VOLGAJ  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VOLGAJ  )
               IF ( VOLGBJ  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VOLGBJ  )
               IF ( VORGCJ  .NE. 0 ) M3ACC = M3ACC + ORGFAC  * BC_VALUES( ICELL, ILAY, VORGCJ  )

C... coarse mode

               M3COR = 0.0
               IF ( VSOILA  .NE. 0 ) M3COR = M3COR + SOILFAC * BC_VALUES( ICELL, ILAY, VSOILA )
               IF ( VSEAS   .NE. 0 ) M3COR = M3COR + SEASFAC * BC_VALUES( ICELL, ILAY, VSEAS  )
               IF ( VANTHA  .NE. 0 ) M3COR = M3COR + ANTHFAC * BC_VALUES( ICELL, ILAY, VANTHA )
               IF ( VNAK    .NE. 0 ) M3COR = M3COR + SEASFAC * BC_VALUES( ICELL, ILAY, VNAK   )
               IF ( VCLK    .NE. 0 ) M3COR = M3COR + SEASFAC * BC_VALUES( ICELL, ILAY, VCLK   )
               IF ( VSO4AK  .NE. 0 ) M3COR = M3COR + SO4FAC  * BC_VALUES( ICELL, ILAY, VSO4AK )
               IF ( VNO3AK  .NE. 0 ) M3COR = M3COR + NO3FAC  * BC_VALUES( ICELL, ILAY, VNO3AK )

C...Calculate number concentrations [ # / m**3 ]

               IF ( VAT0  .NE. 0 ) BC_VALUES( ICELL, ILAY, VAT0  ) = NUMFAC_ATKN * M3ATKN
               IF ( VAC0  .NE. 0 ) BC_VALUES( ICELL, ILAY, VAC0  ) = NUMFAC_ACC  * M3ACC
               IF ( VCOR0 .NE. 0 ) BC_VALUES( ICELL, ILAY, VCOR0 ) = NUMFAC_COR  * M3COR

C...Calculate surface area concentrations [ m**2 / m**3 ]

               IF ( VSURFAT .NE. 0 ) BC_VALUES( ICELL, ILAY, VSURFAT ) = SRFFAC_ATKN * M3ATKN
               IF ( VSURFAC .NE. 0 ) BC_VALUES( ICELL, ILAY, VSURFAC ) = SRFFAC_ACC  * M3ACC

            END DO
         END DO

C...write updated boundary conditions to the output file

         IF ( .NOT. WRITE3( BC_FILE, ALLVAR3, JDATE, JTIME,
     &                      BC_VALUES( 1, 1, 1 ) ) ) THEN
            MSG =  'Could not WRITE all species to file ' // BC_FILE 
            CALL M3ERR ( PNAME, JDATE, JTIME, MSG, .TRUE. )
         ENDIF

         CALL NEXTIME ( JDATE, JTIME, TSTEP )

      END DO

      DEALLOCATE ( BC_VALUES )
      RETURN
      END SUBROUTINE REPAIR
