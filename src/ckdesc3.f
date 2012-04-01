
        LOGICAL FUNCTION CKDESC3( FNAME )

C***********************************************************************
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C 2003-2008 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  105
C
C  RETURNS:
C       If environment variable IOAPI_CHECK_HEADERS begins with 'Y' or 'y',
C	checks whether file attributes in FDESC3.EXT commons fit into
C       standard valid ranges, and returns TRUE or FALSE accordingly.
C       Always checks for duplicate variable-names:  the error messages
C       from netCDF for this condition are quite obscure.
C	Returns TRUE otherwise.
C
C  PRECONDITIONS REQUIRED:
C       FDESC3.EXT commons set by user
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       m3err()
C
C  REVISION  HISTORY:
C       Prototype 9/1995 by CJC
C	Revised   7/1996 by CJC:  UNITS3D must be nonblank.
C       Revised  10/1996 by CJC:  new file type TSERIES3 for hydrology work.
C       Modified  2/1997 by CJC:  check for legality of variable-names
C       Modified  5/2003 by CJC:  corrected error-message
C       Modified 10/2003 by CJC for I/O API version 3:  corrected behavior
C       for STEGRD3 coordinate systems.
C       Modified  3/2004 by D.Yin: add check for POLGRD3
C       Modified  9/2004 by CJC add check for duplicate vble names
C       Modified  4/2005 by CJC:  Revision of duplicate vble name check,
C       to fix bug reported by Dr. Michael Bane, U Manchester, UK
C       Modified  5/2006 by CJC:  Support for VGTYP = VGWRFEM, as reported
C       by Tanya Otte, US EPA
C       Modified  6/2006 by CJC:  restructuring; support for VGTYP = VGWRFNM;
C       format problem reported by Tanya Otte.
C       Modified  7/2008 by CJC:  add support for EQMGRD3, TRMGRD3, 
C       ALBGRD3, LEQGRD3
C***********************************************************************

      USE UTILIO_DEFN
      USE CMAQ_DATA

      IMPLICIT NONE

C...........   INCLUDES:


C...........   ARGUMENTS and their descriptions:

        CHARACTER(16)    FNAME


C...........   EXTERNAL FUNCTIONS and their descriptions:

C...........   PARAMETER

        CHARACTER(16) BLANK, AIR_LAT, AIR_LON, AIR_ELV
        PARAMETER     ( BLANK    = ' ' ,
     &                  AIR_LAT  = 'AIR_LAT' ,
     &                  AIR_LON  = 'AIR_LON' ,
     &                  AIR_ELV  = 'AIR_ELV' )


C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.
        
        LOGICAL         CHKHDR
        LOGICAL         FIRSTIME
        DATA            FIRSTIME / .TRUE. /
        CHARACTER(19)     ENVCHK
        DATA            ENVCHK   / 'IOAPI_CHECK_HEADERS' /
        SAVE            CHKHDR, FIRSTIME, ENVCHK


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         ENVSTAT         !  return value for ENVYN()
        INTEGER         N, L, U, V         !  loop counters
        LOGICAL		INCREASING
        LOGICAL		EFLAG
        CHARACTER(256)   MESG


C***********************************************************************
C   begin body of function  CKDESC3
            
        IF ( FIRSTIME ) THEN

            FIRSTIME = .FALSE.

            CALL M3MESG( BLANK )
            CHKHDR   = ENVYN( ENVCHK, 'Perform file-header checks?',
     &                       .FALSE., ENVSTAT )
         
            IF ( ENVSTAT .GT. 0 ) THEN
                MESG = 'Invalid value for environment vble "' //
     &                 ENVCHK // '"'
                CALL M3WARN( 'CKDESC3', 0, 0, MESG )
            END IF

        END IF          !  if firstime


C...........   Checks for duplicates in the variable-list

        IF ( NVARS3D .LT. 0 ) THEN

            WRITE( MESG, 94010 ) 
     &          'Illegal negative number of variables:', NVARS3D,
     &          'for file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.

        ELSE

            DO  U = 2, NVARS3D
            DO  V = 1, U-1
                IF( VNAME3D( U ) .EQ. VNAME3D( V ) ) THEN

                    WRITE( MESG, 94030 ) 
     &              'Variable name VNAME3D(', U, ') = "' //
     &              VNAME3D( U )( 1:LEN_TRIM( VNAME3D( U ) ) ) //
     &              '" duplicates VNAME3D(', V, ') = "' //
     &              VNAME3D( V )( 1:LEN_TRIM( VNAME3D( V ) ) ) // 
     &              '" in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'

                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.

                END IF

            END DO
            END DO
 
        END IF          !  if nvars3d < 0


C.......   If not chkhdr, just return TRUE:
        
        IF ( .NOT. CHKHDR ) THEN
            CKDESC3 = .TRUE.
            RETURN
        END IF          !  if not chkhdr


C.......   Else perform checks:
C...........   First:  file type and type-specific dimension checks:

        EFLAG = .FALSE.
        IF ( FTYPE3D .EQ. DGRAPH3 ) THEN

            CKDESC3 = .TRUE.
            RETURN

        ELSE IF ( FTYPE3D .EQ. DCTNRY3 ) THEN

            CKDESC3 = .TRUE.
            RETURN

        ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad blob-size NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. GRDDED3  .OR. 
     &            FTYPE3D .EQ. TSRIES3  ) THEN

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. PTRFLY3 ) THEN	! "exotic" grdded3

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( INDEX1( AIR_LAT, NVARS3D, VNAME3D ) .LE. 0 ) THEN
                MESG = 'Variable AIR_LAT not found in ' //
     &                  'PTRFLY3-type file "' // 
     &                  FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( INDEX1( AIR_LON, NVARS3D, VNAME3D ) .LE. 0 ) THEN
                MESG = 'Variable AIR_LON not found in ' //
     &                  'PTRFLY3-type file "' // 
     &                  FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( INDEX1( AIR_ELV, NVARS3D, VNAME3D ) .LE. 0 ) THEN
                MESG = 'Variable AIR_ELV not found in ' //
     &                  'PTRFLY3-type file "' // 
     &                  FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( ABS( NTHIK3D ) .GT. 1+MIN( NCOLS3D, NROWS3D ) ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad boundary width NTHIK', NTHIK3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. IDDATA3 ) THEN

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad max site count NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. PROFIL3 ) THEN

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad max site count NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad max level count NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. GRNEST3 ) THEN

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad max cell-count NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad max nest count NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. SMATRX3 ) THEN

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad max matrix coeff-count NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad matrux NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NTHIK3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad full-matrix col-count NTHIK', NTHIK3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

            IF ( NCOLS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NCOLS', NCOLS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS3D .LE. 0 ) THEN
                WRITE( MESG, 94010 ) 
     &              'Bad NROWS', NROWS3D,
     &              'for file "' // 
     &              FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE

            WRITE( MESG, 94010 ) 
     &          'Illegal file type:', FTYPE3D,
     &          'for file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
            CALL M3WARN( 'CKDESC3', 0, 0, MESG )
            CKDESC3 = .FALSE.
            RETURN

        END IF


C...........   Next, checks on the variable-list

        NVARS3D = NSPC_CMAQ + NSPC_DFLT
        DO V = 1, NSPC_CMAQ
         VNAME3D( V ) = CMAQ_SNAME( V )
         VDESC3D( V ) = CMAQ_LNAME( V )
         UNITS3D( V ) = CMAQ_UNITS( V )
         VTYPE3D( V ) = M3REAL
            WRITE( MESG, '(i3,1x,3(a16,1x),i3)')V,CMAQ_SNAME( V ),CMAQ_LNAME( V ),
     &                                         CMAQ_UNITS( V ), M3REAL
            CALL M3MESG( MESG )
        ENDDO
                                                                
        N = 0                                                     
        DO V = NSPC_CMAQ + 1, NVARS3D                             
         N = N + 1                                              
         VNAME3D( V ) = DFLT_SNAME( N )                         
         VDESC3D( V ) = DFLT_LNAME( N )                         
         UNITS3D( V ) = DFLT_UNITS( N )                         
         VTYPE3D( V ) = M3REAL                                  
            WRITE( MESG, '(i3,1x,3(a16,1x),i3)')N,DFLT_SNAME( N ),DFLT_LNAME( N ),
     &                                   DFLT_UNITS( N ),M3REAL
            CALL M3MESG( MESG )
        ENDDO                                                     
      
        DO V = 1, NVARS3D
            WRITE( MESG, '(i3,1x,3(a16,1x),i3)')V,VNAME3D( V ),VDESC3D( V ),
     &                                  UNITS3D( V ),VTYPE3D( V )
            CALL M3MESG( MESG )
        END DO                                                          

        IF ( NVARS3D .EQ. 0 ) THEN         !  _is_ legal, but unusual

            WRITE( MESG, 94010 ) 
     &          'WARNING:  number of variables:', NVARS3D,
     &          'for file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
            CALL M3MESG( MESG )

        END IF

 
        DO  22  U = 1, NVARS3D

!           IF ( .NOT. CKNAME( VNAME3D( U ) ) ) THEN
!               WRITE( MESG, 94005 )U,
!    &              'Illegal variable name "' ,
!    &              VNAME3D( U )( 1:LEN_TRIM( VNAME3D( U ) ) ), 
!    &              '" in file ', FNAME( 1:LEN_TRIM( FNAME ) ), '"'
!               CALL M3MESG( MESG )
!               EFLAG = .TRUE.
!           END IF

            IF ( VTYPE3D( U ) .LT. M3INT  .OR.
     &           VTYPE3D( U ) .GT. M3DBLE ) THEN
        
                WRITE( MESG, 94010 )
     &              'Illegal data type ', VTYPE3D( U ),
     &              'for variable "' //
     &              VNAME3D( U )( 1:LEN_TRIM( VNAME3D( U ) ) ) // 
     &              '" in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.

            END IF      !  end check on variable-type

            WRITE( MESG, * )U,' ', VNAME3D( U )( 1:LEN_TRIM( VNAME3D( U ) ) ),
     &      ' ',UNITS3D( U )( 1:LEN_TRIM( UNITS3D( U ) ) )
            CALL M3MESG( MESG )

            IF ( UNITS3D( U )        .EQ. BLANK  .OR.
     &           UNITS3D( U )( 1:1 ) .EQ. CHAR( 0 ) ) THEN

                WRITE( MESG, 94005 )U,
     &              'No UNITS specifier for variable "',
     &              VNAME3D( U )( 1:LEN_TRIM( VNAME3D( U ) ) ),
     &              '" in file "', FNAME( 1:LEN_TRIM( FNAME ) ), '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.

            END IF      !  end check on variable-type

22      CONTINUE        !  end loop on variables U


C...........   Checks on the horizontal coordinate description:

        IF ( GDTYP3D .EQ. LATGRD3 ) THEN
        
            IF ( XORIG3D .LT. -180.0D0 .OR.
     &           XORIG3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad grid origin', XORIG3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YORIG3D .LT. -90.0D0 .OR.
     &           YORIG3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad grid origin', YORIG3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. LAMGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_ALP3D .LT. -90.0D0 .OR.
     &           P_ALP3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_BET3D .LT. P_ALP3D .OR.
     &           P_BET3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. MERGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_ALP3D .LT. -90.0D0 .OR.
     &           P_ALP3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_BET3D .LT. -180.0D0 .OR.
     &           P_BET3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. STEGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_ALP3D .LT. -90.0D0 .OR.
     &           P_ALP3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_BET3D .LT. -180.0D0 .OR.
     &           P_BET3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. POLGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( ABS( ABS( P_ALP3D ) - 1.D0 ) .GT. 1.0E-5 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_BET3D .LT. -90.0D0 .OR.
     &           P_BET3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN

            IF ( P_ALP3D .LT.  0.9D0 .OR.
     &           P_ALP3D .GT. 36.1D0 .OR.
     &           ABS( P_ALP3D - 
     &                DBLE( NINT( P_ALP3D ) ) ) .GT. 0.01 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. EQMGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_ALP3D .LT. -90.0D0 .OR.
     &           P_ALP3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. TRMGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. ALBGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_ALP3D .LT. -90.0D0 .OR.
     &           P_ALP3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_BET3D .LT. P_ALP3D .OR.
     &           P_BET3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. LEQGRD3 ) THEN

            IF ( XCENT3D .LT. -180.0D0 .OR.
     &           XCENT3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( YCENT3D .LT. -90.0D0 .OR.
     &           YCENT3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_ALP3D .LT. -90.0D0 .OR.
     &           P_ALP3D .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( P_GAM3D .LT. -180.0D0 .OR.
     &           P_GAM3D .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3D,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        ELSE IF ( GDTYP3D .EQ. IMISS3  ) THEN   !  "other" -- legal but unusual

            WRITE( MESG, 94010 ) 
     &          'WARNING:  Horizontal grid/coordinate type:', GDTYP3D, 
     &          '"MISSING"  in file "' // 
     &          FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )

        ELSE    !  illegal grid type

            WRITE( MESG, 94010 ) 
     &         'Illegal horizontal grid/coordinate type:', GDTYP3D,
     &         'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.

        END IF  !  if  gdtyp3d = lamgrd3, etc.


C...........   Checks on the vertical coordinate description:

        IF ( NLAYS3D .LT. 1 .AND. FTYPE3D .GE. CUSTOM3 ) THEN

            WRITE( MESG, 94010 ) 
     &         'Illegal vertical layer dimension:', NLAYS3D,
     &         'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.

        ELSE IF ( NLAYS3D .EQ. 1 ) THEN

            CONTINUE    ! do nothing:  vertical grid irrelevant

        ELSE IF ( VGTYP3D .EQ. VGSGPH3 .OR.     !  supported types...
     &            VGTYP3D .EQ. VGSGPN3 .OR.
     &            VGTYP3D .EQ. VGSIGZ3 .OR.
     &            VGTYP3D .EQ. VGPRES3 .OR.
     &            VGTYP3D .EQ. VGZVAL3 .OR.
     &            VGTYP3D .EQ. VGHVAL3 .OR.
     &            VGTYP3D .EQ. VGWRFEM .OR.
     &            VGTYP3D .EQ. VGWRFNM ) THEN

            INCREASING = ( VGLVS3D( 2 ) .GT. VGLVS3D( 1 ) )

            DO  188  L = 2, MIN( NLAYS3D, MXLAYS3 )

                IF ( INCREASING .NEQV.
     &               ( VGLVS3D( L+1 ) .GT. VGLVS3D( L ) ) ) THEN

                    WRITE( MESG, 94010 )
     &              'Bad layer monotonicity at layer', L,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'

                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.

                END IF

188         CONTINUE

        ELSE IF ( VGTYP3D .EQ. IMISS3  ) THEN   !  "other" -- legal but unusual

            WRITE( MESG, 94010 ) 
     &          'WARNING:  Vertical grid/coordinate type:', VGTYP3D,
     &          '"MISSING" in file "' // 
     &          FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
                CALL M3MESG( MESG )

            INCREASING = ( VGLVS3D( 2 ) .GT. VGLVS3D( 1 ) )

            DO  199  L = 2, MIN( NLAYS3D, MXLAYS3 )

                IF ( INCREASING .NEQV.
     &               ( VGLVS3D( L+1 ) .GT. VGLVS3D( L ) ) ) THEN

                    WRITE( MESG, 94010 )
     &              'Bad layer monotonicity at layer', L,
     &              'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
   
                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.

                END IF

199         CONTINUE

        ELSE    !  illegal grid type

            WRITE( MESG, 94010 ) 
     &         'Unknown vertical grid/coordinate type:', VGTYP3D,
     &         'in file "' // FNAME( 1:LEN_TRIM( FNAME ) ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.

        END IF  !  if  vgtyp3d = vgsgph3, etc.
      
           
C...........   Set function value and return:

        IF ( EFLAG ) THEN
            CALL M3WARN( 'CKDESC', 0, 0, 'Bad file description found' )
            CKDESC3 = .FALSE.
        ELSE
            CKDESC3 = .TRUE.
        END IF

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94000   FORMAT( 5 ( A, : ) )

94005   FORMAT( I4, 1X, 5 ( A, : ) )

94010   FORMAT( A, I10, :, 2X, A )

94020   FORMAT( A, 1PG14.7, :, 2X, A )

94030   FORMAT( 4 ( A, I2, :, 2X ) )


        END

