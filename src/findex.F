
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
C $Header: /project/work/rep/BCON/src/driver/bcon/findex.F,v 1.3 2002/04/12 14:19:19 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      INTEGER FUNCTION FINDEX (INDX, N, NLIST)

C Searches for INDX in list NLIST and returns the subscript
C (1...N) at which it is found, or returns 0 when NAME not
C found in NLIST

      IMPLICIT NONE
 
      INTEGER INDX        !  index being searched for
      INTEGER N           !  Length of array to be searched
      INTEGER NLIST(*)    !  array to be searched

      INTEGER I

      DO I = 1, N
         IF ( INDX .EQ. NLIST( I ) ) THEN  ! found
            FINDEX = I
            RETURN
            END IF
         END DO
      FINDEX = 0        !  not found
      RETURN

      END

