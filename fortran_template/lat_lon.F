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
C $Header: /project/work/rep/BCON/src/driver/bcon/lat_lon.F,v 1.3 2002/04/12 14:19:19 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE LAT_LON( COL, ROW, GDTYP, XORIG, YORIG, XCELL, YCELL, 
     &                    XCENT, YCENT, P_ALP, P_BET, P_GAM, LAT, LON )
 

C*************************************************************************
C
C  FUNCTION: Computes latitude and longitude of center of grid cells 
C
C  PRECONDITIONS: None
C 
C  KEY SUBROUTINES/FUNCTIONS CALLED: SETLAM
C                                    LAM2LL
C                                    UTM2LL
C
C  REVISION HISTORY: Modified form of LAT_LON program originally created
C                       by C. J. Coats  -- Jerry Gipson, January 1998    
C*************************************************************************
      IMPLICIT NONE     

C..INCLUDE FILES:
      INCLUDE "PARMS3.EXT"

C..ARGUMENTS:
      INTEGER GDTYP     ! Grid type
      INTEGER COL       ! No. of columns 
      INTEGER ROW       ! No. of rows

      REAL( 8 ) P_ALP      ! 1st map projection parameter
      REAL( 8 ) P_BET      ! 2nd map projection parameter
      REAL( 8 ) P_GAM      ! 3rd map projection parameter

      REAL( 8 ) XCELL      ! X-dimension of cell (m)
      REAL( 8 ) XCENT      ! Longitude of coordinate system center
      REAL( 8 ) XORIG      ! X-origin of grid
      REAL( 8 ) YCELL      ! Y-dimension of cell (m)
      REAL( 8 ) YCENT      ! Latitude of coordinate system center
      REAL( 8 ) YORIG      ! Y-origin of grid

      REAL LAT          ! Output latitude
      REAL LON          ! Output longitude

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS:
      LOGICAL SETLAM         ! Sets up Lambert projection
      LOGICAL LAM2LL         ! Gets Lat/lons of Lambert projection

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
      CHARACTER( 16  )  PNAME    ! Program Name
      CHARACTER( 132 )  MSG      ! Log message

      INTEGER ZONE           ! UTM zone

      REAL X, X0             ! X-dimension origin
      REAL Y, Y0             ! Y-dimension origin

C**********************************************************************
      DATA PNAME /'LAT_LON' /

      X0 = XORIG - 0.5D+00 * XCELL
      Y0 = YORIG - 0.5D+00 * YCELL

      IF( GDTYP .EQ. LATGRD3 ) THEN      ! LAT_LON Coordinates

         LAT = Y0 + FLOAT( ROW ) * YCELL
         LON = X0 + FLOAT( COL ) * XCELL
  
      ELSEIF( GDTYP .EQ. LAMGRD3 ) THEN  ! Lambert Coordinates

         IF( .NOT. SETLAM( SNGL( P_ALP ),        !  first, initialize
     &                     SNGL( P_BET ),        !  for LAM2LL()
     &                     SNGL( P_GAM ),
     &                     SNGL( XCENT ), 
     &                     SNGL( YCENT ) ) ) THEN
            MSG = 'Lambert projection setup error for CTM CONC file'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
         ENDIF
      
         X = X0 + FLOAT( COL ) * XCELL
         Y = Y0 + FLOAT( ROW ) * YCELL
         IF( .NOT. LAM2LL( X, Y, LON, LAT ) ) THEN
            MSG = 'Lambert conversion error for CTM CONC file'
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )
         ENDIF                          

      ELSEIF ( GDTYP .EQ. UTMGRD3 ) THEN   ! UTM Coordinates

         ZONE = NINT( P_ALP )
         X = X0 + FLOAT( COL ) * XCELL
         Y = Y0 + FLOAT( ROW ) * YCELL
  
         CALL UTM2LL( X, Y, ZONE, LON, LAT )

      ELSE                                   !  Unsupported Coordinates

         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )

      ENDIF 

      RETURN         

C************************* FORMAT STATEMENTS ***************************

94000 FORMAT( 'LAT/LON calculations for GDTYP3D ',I1, ' not supported' )
    
      END

