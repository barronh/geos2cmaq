
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


      SUBROUTINE GC_SPC_MAP( LOGUNIT, NSPCS_IN, N_GC_OUT, SPMAP, 
     &                       N_MCNV_SP, SFAC, MCNV_COEFF, LCONV, LSCALE,  
     &                       LCONVSP, EXT_IC_NAME, INFL_SP_NAME,   
     &                       MCNV_SPCS, SPNAME_OUT )

C*************************************************************************
C
C  FUNCTION: Load arrays that are used in subsequent operations 
C            to convert RADM2 species to SAPRC07T species             
C             
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: None  
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, May, 2001
C                    
C  01/2009: B Hutzell: created based on saprc99 to radm2 conversion
C*************************************************************************

      USE UTILIO_DEFN
      USE CGRID_SPCS
      USE CMAQ_DATA

      IMPLICIT NONE

C..INCLUDE FILES:


C..ARGUMENTS:
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
      
      
C..PARAMETERS:
C..PARAMETERS:
      INTEGER, PARAMETER ::  NSAPRC = 54     ! No. of SAPRC species defined

      INTEGER, PARAMETER ::  MAXSP  =  1     ! Max no. of RADM species in a SAPRC species

                                 
C..EXTERNAL FUNCTIONS:

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 80 ) MSG                         ! Log message
      CHARACTER( 16 ) PNAME                       ! Program Name
      CHARACTER( 16 ) SAPRC_SPC( NSAPRC )         ! Hard coded SAPRC spec names
      CHARACTER( 16 ) RADM2_SPC( NSAPRC )         ! Hard coded RADM2 spec names

      CHARACTER( 16 ) MECHFL_IC_NF( MX_MECHFL_IC ) ! Default SAPRC species 
                                                   ! not in the *.EXT file
  
      CHARACTER( 16 ) MECHFL_SP_NF( MX_INFL_SP )   ! Default RADM2 species
                                                   ! not in the input file

      CHARACTER( 16 ), ALLOCATABLE :: MECHFL_GC_NF( : ) ! Non-default *.EXT SAPRC species names 

      INTEGER IND       ! Array index
      INTEGER N, SPC    ! Loop indices
      INTEGER NSP       ! Species counter

      INTEGER N_RADM_SPC( NSAPRC )  ! Hard-coded no. of RADM2 species 
                                    ! defining each SAPRC species
      REAL COEFF( NSAPRC )          ! Hard coded RADM2 species coefficients

      LOGICAL :: LFIRST = .TRUE.    ! Flag for first pass
      LOGICAL :: LUSED( NSAPRC )    ! Flag for SAPRC species for IC output

C**********************************************************************

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  The data statements below define the RADM2 to SAPRC07T Conversion as
c  follows:  ( [SAPRC07T species] = [RADM2 species] )
c
c   1.   [NO2 ] =       [NO2 ]
c   2.   [NO  ] =       [NO  ]
c   3.   [O3P ] =       [O3P ]
c   4.   [O3  ] =       [O3  ]
c   5.   [NO3 ] =       [NO3 ]
c   6.   [N2O5] =       [N2O5]
c   7.   [HNO3] =       [HNO3]
c   8.   [O1D2] =       [O1D ]
c   9.   [OH  ] =       [HO  ]
c  10.   [HONO] =       [HONO]
c  11.   [HO2 ] =       [HO2 ]
c  12.   [CO  ] =       [CO  ]
c  13.   [HNO4] =       [HNO4]
c  14.   [HO2H] =       [H2O2]
c  15.   [SO2 ] =       [SO2 ]
c  16.   [SULF] =       [SULF]
c  17.   [MEO2] =       [MO2 ]
c  18.   [HCHO] =       [HCHO]
c  19.   [COOH] =       [OP1 ] 
c  20.   [ROOH] =       [OP2 ] 
c  21.   [RNO3] =       [ONIT] 
c  22.   [MEK ] =       [KET ] 
c  23.   [MECO3] =      [ACO3] 
c  24.   [PAN ] =       [PAN ] 
c  25.   [CCOOOH] =     [PAA ] 
c  26.   [CCOOH] =      [ORA2] 
c  27.   [PAN2] =       [TPAN] 
c  28.   [CCHO] =       [ALD ]  
c  29.   [HCOOH] =      [ORA1] 
c  30.   [GLY ] =       [GLY ] 
c  31.   [MGLY] =       [MGLY] 
c  32.   [CRES] =       [CSL ] 
c  33.   [MACR] =       [MACR] 
c  34.   [MVK ] =       [MVK ] 
c  35.   [IPRD] =    [ISOPROD] 
c  36.   [AFG2] =       [DCB ] 
c  37.   [ETHENE] =     [OL2 ]
c  38.   [ISOPRENE] =   [ISO ]
c  39.   [TERP] =       [TERP]
c  40.   [ALK1] =       [ETH ]
c  41.   [ALK2] =   0.5*[HC3 ]
c  42.   [ALK3] =   0.5*[HC3 ]
c  43.   [ALK4] =       [HC5 ]
c  44.   [ALK5] =       [HC8 ]
c  45.   [TOLUENE] =       [TOL ]
c  46.   [MXYL] =      0.34* [XYL ]
c  47.   [OXYL] =      0.33* [XYL ]
c  48.   [PXYL] =      0.33* [XYL ]
c  49.   [OLE1] =       [OLT ]
c  50.   [OLE2] =       [OLI ]
c  51.   [BENZENE] =       [BENZENE]
c  52.   [HG] =       [HG]
c  53.   [HGIIGAS] =       [HGIIGAS]
C  54.   [CO2]     =   [CO2]
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c..Number of RADM2 species used to compute the SAPRC species concentration
      DATA N_RADM_SPC / 54*1 /

c..Names of SAPRC species
      DATA SAPRC_SPC / 'NO2             ', 'NO              ',
     &                 'O3P             ', 'O3              ',
     &                 'NO3             ', 'N2O5            ',
     &                 'HNO3            ', 'O1D2            ',
     &                 'OH              ', 'HONO            ',
     &                 'HO2             ', 'CO              ',
     &                 'HNO4            ', 'HO2H            ',
     &                 'SO2             ', 'SULF            ',
     &                 'MEO2            ', 'HCHO            ',
     &                 'COOH            ', 'ROOH            ',
     &                 'RNO3            ', 'MEK             ',
     &                 'MECO3           ', 'PAN             ',
     &                 'CCOOOH          ', 'CCOOH           ',
     &                 'PAN2            ', 'CCHO            ',
     &                 'HCOOH           ', 'GLY             ',
     &                 'MGLY            ', 'CRES            ',
     &                 'MACR           ', 'MVK             ',
     &                 'IPRD            ', 'AFG2            ',
     &                 'ETHENE          ', 'ISOPRENE        ',
     &                 'TERP            ', 'ALK1            ',
     &                 'ALK2            ', 'ALK3            ',
     &                 'ALK4            ', 'ALK5            ',
     &                 'TOLUENE         ', 'MXYL            ',
     &                 'OXYL            ', 'PXYL            ' ,
     &                 'OLE1            ', 'OLE2            ' ,
     &                 'BENZENE         ', 'HG              ' ,
     &                 'HGIIGAS         ', 'CO2             ' /


c..Names of the RADM2 species used to compute SAPRC species concentrations
 
      DATA RADM2_SPC / 'NO2             ', 'NO              ',
     &                 'O3P             ', 'O3              ',
     &                 'NO3             ', 'N2O5            ',
     &                 'HNO3            ', 'O1D             ',
     &                 'HO              ', 'HONO            ',
     &                 'HO2             ', 'CO              ',
     &                 'HNO4            ', 'H2O2            ',
     &                 'SO2             ', 'SULF            ',
     &                 'MO2             ', 'HCHO            ',
     &                 'OP1             ', 'OP2             ',
     &                 'ONIT            ', 'KET             ',
     &                 'ACO3            ', 'PAN             ',
     &                 'PAA             ', 'ORA2            ',
     &                 'TPAN            ', 'ALD             ',
     &                 'ORA1            ', 'GLY             ',
     &                 'MGLY            ', 'CSL             ',
     &                 'MACR            ', 'MVK             ',
     &                 'ISOPROD         ', 'DCB             ',
     &                 'OL2             ', 'ISO             ',
     &                 'TERP            ', 'ETH             ',
     &                 'HC3             ', 'HC3             ',
     &                 'HC5             ', 'HC8             ',
     &                 'TOL             ', 'XYL             ',
     &                 'XYL             ', 'XYL             ' ,
     &                 'OLT             ', 'OLI             ' ,
     &                 'BENZENE         ', 'HG              ' ,
     &                 'HGIIGAS         ', 'CO2             ' /



c..Coefficients of the RADM2 species
      DATA COEFF / 40 * 1.0, 0.5, 0.5, 3*1.0, 0.34, 2*0.33, 6*1.0 / ! 9 * 1.0 /



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write appropriate log data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      WRITE( LOGUNIT, 92000 ) 

      LCONV = .TRUE.

      IF( LFIRST )THEN
      
          ALLOCATE( MECHFL_GC_NF( N_GC_SPCD ) )

          LFIRST = .FALSE.
          
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check that the number of RADM2 species defining a SAPRC species does
c  not exceed the MXCVSP parameter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO N = 1, NSAPRC

         IF( N_RADM_SPC( N ) .GT. MXCVSP ) THEN
            MSG = 'Max no. conversion species exceeded -- increase MXCVSP'
            CALL M3ERR( PNAME, 0, 0, MSG, .TRUE. )
         ENDIF

      ENDDO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write messages about undefined species
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c..Default SAPRC species not in GC_SPC.EXT
      NSP = 0
      DO N = 1, NSAPRC

         LUSED( N ) = .TRUE.

         IF( INDEX1( SAPRC_SPC( N ), N_GC_SPC, GC_SPC ) .EQ. 0 ) THEN
            LUSED( N ) = .FALSE.
            NSP = NSP + 1
            MECHFL_IC_NF( NSP ) = SAPRC_SPC( N )
         ENDIF  

      ENDDO

      IF( NSP .GT. 0 ) THEN
         WRITE( LOGUNIT, 92020 ) 
         WRITE( LOGUNIT, 92040 ) ( MECHFL_IC_NF( N ), N = 1, NSP )
      ENDIF

c..RADM2 species not in the input file
      NSP = 0
      DO N = 1, NSAPRC
            
         IND = INDEX1( RADM2_SPC( N ), NSPCS_IN, INFL_SP_NAME ) 

         IF( IND .EQ. 0 ) THEN
            LUSED( N ) = .FALSE.
            NSP = NSP + 1
            MECHFL_SP_NF( NSP ) = RADM2_SPC( N )
         ENDIF

      ENDDO

      IF( NSP .NE. 0 ) THEN
         WRITE( LOGUNIT, 92060 ) 
         WRITE( LOGUNIT, 92040 ) ( MECHFL_SP_NF( N ), N = 1, NSP )
      ENDIF

c..Species in G_SPC.EXT that are not SAPRC default names
      NSP = 0
      DO N = 1, N_GC_SPC

         IND = INDEX1( GC_SPC( N ), NSAPRC, SAPRC_SPC ) 

         IF( IND .EQ. 0 ) THEN
            NSP = NSP + 1
            MECHFL_GC_NF( NSP ) = GC_SPC( N )
         ENDIF

      ENDDO

      IF( NSP .NE. 0 ) THEN
         WRITE( LOGUNIT, 92100 ) 
         WRITE( LOGUNIT, 92040 ) ( MECHFL_GC_NF( N ), N = 1, NSP )
      ENDIF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Load arrays and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NSP = 0

      DO N = 1, NSAPRC

         IF( LUSED( N ) ) THEN

            NSP = NSP + 1

            LSCALE( NSP ) = .FALSE.
            SFAC( NSP ) = 1.0

            SPNAME_OUT( NSP )     = SAPRC_SPC( N )
            N_MCNV_SP(  NSP )     = 1
            MCNV_SPCS(  NSP, 1 )  = RADM2_SPC( N )
            MCNV_COEFF( NSP, 1 )  = COEFF( N )

            IF( N_MCNV_SP( NSP ) .EQ. 1 .AND. MCNV_COEFF( NSP, 1 ) .EQ. 1.0 ) 
     &            THEN
               IND = INDEX1( MCNV_SPCS( NSP, 1 ), NSPCS_IN, INFL_SP_NAME )
               SPMAP( NSP )   = IND
               LCONVSP( NSP ) = .FALSE.
            ELSE
               SPMAP( NSP )   = 0
               LCONVSP( NSP ) = .TRUE.
            ENDIF

         ENDIF

      ENDDO 

      N_GC_OUT = NSP         
           
      RETURN

C************************* FORMAT STATEMENTS ***************************

92000 FORMAT(  /1X, 79 ( '#' ) 
     &         /1X, '#   Species map section'
     &         /1X, 79 ( '#' )
     &        //5X, 'Input IC species converted from RADM2 to SAPRC99. '
     &        / 5X, 'All IC surrogate names for gas phase ',
     &              'species ignored and default SAPRC99 and RADM2 ',
     &              'species names expected.' )


92020 FORMAT( // 5X, 'WARNING: The following default SAPRC99 species were ',
     &               'not found in the GC_SPC.EXT file.' 
     &        /  5X, '         ICs were NOT generated for them. ' )

92040 FORMAT(   (15X, 4( A, 2X ) ) )

92060 FORMAT( // 5X, 'WARNING: The following default RADM2 species were ',
     &               'not found in the input file.'
     &        /  5X, '         ICs were NOT generated for any ',
     &               'SAPRC99 species that reference them.' )

92080 FORMAT(   20X, A , 5X, A )

92100 FORMAT(//5X, 'WARNING: The following species in the GC_SPC.EXT ',
     &             'file are not in the list of SAPRC99 species ',
     &             'to be converted. ' 
     &        /  5X, '         ICs were NOT generated for them. ' )

      END

