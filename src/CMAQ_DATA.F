      MODULE CMAQ_DATA


C************************************************************************
C
C     FUNCTION: Holds CMAQ data for generating BCs for CMAQ from the 
C               Harvard Global CTM model
C
C  Apr 2006 -- Sergey L. Napelenok
C      Modified to describe GEOS-Chem files provided by Daven Henze
C
C....................................................................

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Data for SAPRC species derived from GEO fila data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#include <MAPPING.MECH>
                                                                                             
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                
c  Data for SAPRC species derived from default clean air profile file                  
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc                
C  INCLUDE FILE: BC_PARMS.EXT
C
C  CONTAINS: Dimensioning parameters and data for BCON 
C
C  DEPENDENT UPON: None 
C
C  REVISION HISTORY: Prototype created by Jerry Gipson, January, 1998
C                    Modified by JG May, 1999 to treat PinG concs
C
C......................................................................


c..Dimensioning parameters
      INTEGER, PARAMETER :: NROOTS       = 5

      INTEGER, PARAMETER :: MXCTMS       = 3   ! Max no. of input CTM files

      INTEGER, PARAMETER :: MX_MECHFL_IC = 500 ! Max no. of species
      
      INTEGER, PARAMETER :: MXCVSP       = 20  ! Max no. of terms per species for

      INTEGER, PARAMETER :: MX_INFL_SP   = 500 ! Max no. of species in an input file

      INTEGER, PARAMETER :: NTRACERS     = 8  ! No. of catalogued tracers
 
      INTEGER, PARAMETER :: NPILLARS     = 10 ! No. of pillars in PinG plume


C...........................BC_PARMS.EXT.................................


      CHARACTER( 16 ), ALLOCATABLE, SAVE ::  DFLT_LNAME( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE ::  DFLT_SNAME( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE ::  DFLT_UNITS( : )
      CHARACTER( 16 ), ALLOCATABLE, SAVE ::  RQD_SP(     : )
      CONTAINS 
      
        SUBROUTINE DEFINE_CMAQ_UNITS()

          USE UTILIO_DEFN
          USE CGRID_SPCS

          IMPLICIT NONE

          INTEGER          :: IND, V
          CHARACTER( 132 ) :: XMSG
          CHARACTER(  18 ) :: PNAME = 'DEFINE_CMAQ_UNITS'
          LOGICAL          :: FOUND_SPCS, EFLAG

          IND = 0
          CMAQ_LNAME( 1: NSPC_CMAQ ) =  CMAQ_SNAME( 1:NSPC_CMAQ )
          
          EFLAG = .FALSE.

          XMSG = 'IND    CMAQ_LNAME      CMAQ_UNITS   '
          CALL M3MESG( XMSG ) 
          
          DO V = 1, NSPC_CMAQ

             IND = IND + 1 

             CMAQ_UNITS( IND ) = 'ppmV'

             FOUND_SPCS = .TRUE. 

             IF(INDEX1(CMAQ_LNAME( V ), N_GC_SPC, GC_SPC) .GT. 0)THEN
                 WRITE( XMSG, 5001 )IND, CMAQ_LNAME( IND ), CMAQ_UNITS(  IND )
                 CALL M3MESG( XMSG ) 
                 CYCLE
             ELSEIF(INDEX1(CMAQ_LNAME( V ), N_NR_SPC, NR_SPC) .GT. 0)THEN
                 WRITE( XMSG, 5001 )IND, CMAQ_LNAME( IND ), CMAQ_UNITS(  IND )
                 CALL M3MESG( XMSG ) 
                 CYCLE
             ELSEIF(INDEX1( CMAQ_LNAME( V ), N_TR_SPC, TR_SPC ) .GT. 0)THEN
                 WRITE( XMSG, 5001 )IND, CMAQ_LNAME( IND ), CMAQ_UNITS(  IND )
                 CALL M3MESG( XMSG ) 
                 CYCLE
             END IF
             
             IF( INDEX1( CMAQ_LNAME( V ), N_AE_SPC, AE_SPC ) .GT. 0 )THEN
                IF ( CMAQ_LNAME( V )( 1:3 ) .EQ. 'NUM' ) THEN
                   CMAQ_UNITS(  IND ) = '#/m**3'
                ELSEIF ( CMAQ_LNAME( V )( 1:3 ) .EQ. 'SRF' ) THEN
                  CMAQ_UNITS(  IND ) = 'm**2/m**3'
                ELSE
                  CMAQ_UNITS(  IND ) = 'micrograms/m**3'
                END IF
             ELSE
                FOUND_SPCS = .FALSE.
                XMSG = 'CMAQ_SNAME ' // TRIM( CMAQ_SNAME( V ) )
     &              // ' not found in mechanism namelists.'
                CALL M3MESG( XMSG ) 
             END IF

             IF( .NOT. FOUND_SPCS )THEN
                 EFLAG = .TRUE.
             ELSE             
                 WRITE( XMSG, 5001 )IND, CMAQ_LNAME( IND ), CMAQ_UNITS(  IND )
                 CALL M3MESG( XMSG ) 
             END IF
            
           END DO
           
           IF( EFLAG )THEN
              XMSG =  'Inconsistencies between CMAQ Species '
     &             // 'and Namelists'
              CALL M3ERR( PNAME, 0, 0, XMSG, .TRUE. )
           END IF
          
          RETURN
5001      FORMAT(I3,1X,A16,1X,A16)          
        END SUBROUTINE DEFINE_CMAQ_UNITS

      END MODULE CMAQ_DATA
