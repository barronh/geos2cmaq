There are two sections of code. One for v8-03-02 and one for v9-01-01. It would be better to simply use the ND49 outputs for future efforts.

# v8-03-02 Differences

diff -r code_2-15-2012.v8-03-02/GeosCore/main.f code_2-15-2012.v8-03-02.org/GeosCore/main.f
182,184d181
<       !for better CSPEC tracerinfos (fha, 10/28/11)
<       USE GAMAP_MOD,         ONLY : DO_GAMAP
<
207c204
<       CHARACTER(LEN=255) :: ZTYPE, DIAGINFO, TRACERINFO
---
>       CHARACTER(LEN=255) :: ZTYPE
978,983d974
<          ! fha - 10-28-11: Redo this after the chem initialization for CSPEC
<          ! Create "diaginfo.dat" and "tracerinfo.dat" files for GAMAP
<          DIAGINFO = 'diaginfo.dat'
<          TRACERINFO = 'tracerinfo.dat'
<          CALL DO_GAMAP( DIAGINFO, TRACERINFO )
<
diff -r code_2-15-2012.v8-03-02/GeosCore/tpcore_bc_mod.f code_2-15-2012.v8-03-02.org/GeosCore/tpcore_bc_mod.f
35,36d34
< !  (19e) BC_CP   (REAL*4  )    : Array containing NA species on coarse grid
< !  (19e) BC_CU_CP(REAL*4  )    : Array containing CU species on coarse grid
138c136
< !  (3 ) Now get HALFPOLAR for GEOS or GCAP grids (bmy, 6/28/05)NC
---
> !  (3 ) Now get HALFPOLAR for GEOS or GCAP grids (bmy, 6/28/05)
191,193c189
<       !fha 3-3-11
<       REAL*4,  ALLOCATABLE :: BC_CP(:,:,:,:)
<       REAL*4,  ALLOCATABLE :: BC_CU_CP(:,:,:,:)
---
>
246d241
< !  (5 ) Now creates CSPEC files too 101, 102 (fha, 3-4-11)
254c249
<       USE FILE_MOD,      ONLY : IU_BC, IU_BC_NA, IU_BC_EU, IU_BC_CH
---
>       USE FILE_MOD,      ONLY : IU_BC, IU_BC_NA, IU_BC_EU, IU_BC_CH
256,257d250
<       !fha 3-3-11
<       USE FILE_MOD,      ONLY : IU_BC_CP, IU_BC_CU_CP
294,297d286
<          ELSEIF ( WINDOW .eq. 101 ) THEN
<             FILENAME = TRIM( TPBC_DIR ) // 'BC.CSPEC.YYYYMMDD'
<          ELSEIF ( WINDOW .eq. 102 ) THEN
<             FILENAME = TRIM( TPBC_DIR_NA ) // 'BC.CSPEC.YYYYMMDD'
315,318d303
<            ELSEIF ( WINDOW .eq. 101 ) THEN
<             IF ( PRESENT( FOR_WRITE ) )
<      &        CALL OPEN_BPCH2_FOR_WRITE( IU_BC_CU_CP,
<      &               FILENAME )
323,326d307
<          ELSEIF ( WINDOW .eq. 102 ) THEN
<             IF ( PRESENT( FOR_WRITE ) )
<      &        CALL OPEN_BPCH2_FOR_WRITE( IU_BC_CP,
<      &               FILENAME )
359,360d339
< !  (5 ) Hardcoded act+inact species count for CSPEC output 116 (fha, 11/9/11)
< !  (6 ) Changed CSPEC species count to NTSPEC(1) (fha, 5/17/12)
371,374d349
<       ! fha 3-3-11: write out CSPEC BC conditions
<       USE COMODE_MOD,  ONLY : CSPEC_FULL
<       USE FILE_MOD,    ONLY : IU_BC_CP, IU_BC_CU_CP
< !      USE COMODE_LOOP_MOD, ONLY : IGAS, NTSPEC
377,378c352
< #     include "comode.h"   ! NTSPEC (from havala: readchem:NACTIVE, NGAS, NTSPEC)
<
---
>
392,395d365
<       ! fha 3-3-11
<       CHARACTER(LEN=40)  :: CATEGOR_S = 'IJ-CHK-$'
<       CHARACTER(LEN=40)  :: UNIT_S    = 'molec/cm3/box'
<
422c392
<             DO N = 1, N_TRACERS
---
>             DO N = 1, N_TRACERS
438,463d407
<                      ! Echo info
<                STAMP = TIMESTAMP_STRING()
<                WRITE( 6, 110 ) STAMP
<  110  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote BC''s at ', a )
<
<             ! fha 3-3-11: write out CSPEC BC conditions
<             CALL OPEN_BC_FILE( FOR_WRITE=.TRUE., WINDOW=101)
<
<             ! Loop over each tracer
<             DO N = 1, NTSPEC(1)
<
<                ! Save concentrations in WINDOW REGION to diski
<                ! fha 11-9-11
<                DO L = 1, 38
<                   BC_CU_CP(1:IM_BC,1:JM_BC,L,N) =
<      &               CSPEC_FULL(I1_BC:I2_BC,J1_BC:J2_BC,L,N)
<                ENDDO
<
<                ! Write boundary conditions to binary punch file
<                CALL BPCH2( IU_BC_CU_CP,  MODELNAME, LONRES,   LATRES,
<      &               HALFPOLAR, CENTER180, CATEGOR_S, N,
<      &               UNIT_S,   TAU,       TAU,      RESERVED,
<      &               IM_BC,  JM_BC,   LLPAR,  I1_BC,
<      &               J1_BC, 1, BC_CU_CP(1:IM_BC, 1:JM_BC,
<      &               1:LLPAR, N) )
465,467d408
<             ENDDO
<
<
470,471c411,412
<                WRITE( 6, 120 ) STAMP
<  120  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote CU CP BC''s at ', a )
---
>                WRITE( 6, 110 ) STAMP
>  110  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote BC''s at ', a )
491c432
<      &               IM_BC_NA,  JM_BC_NA,   38,  I1_BC_NA,
---
>      &               IM_BC_NA,  JM_BC_NA,   LLPAR,  I1_BC_NA,
493c434
<      &               1:38, N) )
---
>      &               1:LLPAR, N) )
495a437
>
500,527d441
<             ! fha 3-3-11: write out CSPEC BC conditions
<             CALL OPEN_BC_FILE( FOR_WRITE=.TRUE., WINDOW=102)
<
<             ! Loop over each tracer
<             DO N = 1, NTSPEC(1)
<
<                ! Save concentrations in WINDOW REGION to disk
<                ! fha 11-9-11
<                DO L = 1, 38
<                   BC_CP(1:IM_BC_NA,1:JM_BC_NA,L,N) =
<      &               CSPEC_FULL(I1_BC_NA:I2_BC_NA,J1_BC_NA:J2_BC_NA,L,N)
<                ENDDO
<
<                ! Write boundary conditions to binary punch file
<                CALL BPCH2( IU_BC_CP,  MODELNAME, LONRES,   LATRES,
<      &               HALFPOLAR, CENTER180, CATEGOR_S, N,
<      &               UNIT_S,   TAU,       TAU,      RESERVED,
<      &               IM_BC_NA,  JM_BC_NA,  38,  I1_BC_NA,
<      &               J1_BC_NA, 1, BC_CP(1:IM_BC_NA, 1:JM_BC_NA,
<      &               1:38, N) )
<
<             ENDDO
<
<             ! Echo info
<             STAMP = TIMESTAMP_STRING()
<             WRITE( 6, 121 ) STAMP
<  121  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote NA CP BC''s at ', a )
581,582d494
<
<
738,740c650
<       USE TIME_MOD,   ONLY : TIMESTAMP_STRING !fha 3-5-11
< !      USE COMODE_MOD, ONLY : IGAS, NTSPEC
<
---
>
743d652
< #     include "comode.h"
747d655
<       CHARACTER(LEN=16)  :: STAMP !fha 3-5-11
767,770c675
<             ! Echo info
<             STAMP = TIMESTAMP_STRING()
<             WRITE( 6, 113 ) STAMP
<  113  FORMAT( '     - BEGIN_CLEAR_TPCORE_BC: at ', a )
---
>
783,800d687
<          ENDDO
< !$OMP END PARALLEL DO
<             ! Echo info
<             STAMP = TIMESTAMP_STRING()
<             WRITE( 6, 114 ) STAMP
<  114  FORMAT( '     - CLEARED_CU_TPCORE_BC: at ', a )
< !$OMP PARALLEL DO
< !$OMP+DEFAULT( SHARED )
< !$OMP+PRIVATE( I, J, L, N )
<          DO N = 1, IGAS
<          DO L = 1, 38
<          DO J = 1, JM_BC
<          DO I = 1, IM_BC
<             !fha 3-4-11
<             BC_CU_CP(I,J,L,N) = 0e0
<          ENDDO
<          ENDDO
<          ENDDO
803,806d689
<             ! Echo info
<             STAMP = TIMESTAMP_STRING()
<             WRITE( 6, 115 ) STAMP
<  115  FORMAT( '     - CLEARED_CU_CP_TPCORE_BC: at ', a )
823,844d705
<             ! Echo info
<             STAMP = TIMESTAMP_STRING()
<             WRITE( 6, 116 ) STAMP
<  116  FORMAT( '     - CLEARED_NA_TPCORE_BC: at ', a )
< !$OMP PARALLEL DO
< !$OMP+DEFAULT( SHARED )
< !$OMP+PRIVATE( I, J, L, N )
<          DO N = 1, IGAS
<          DO L = 1, 38
<          DO J = 1, JM_BC_NA
<          DO I = 1, IM_BC_NA
<                 !fha 3-3-11
<                 BC_CP(I,J,L,N) = 0e0
<          ENDDO
<          ENDDO
<          ENDDO
<          ENDDO
< !$OMP END PARALLEL DO
<             ! Echo info
<             STAMP = TIMESTAMP_STRING()
<             WRITE( 6, 117 ) STAMP
<  117  FORMAT( '     - CLEARED_NA_CP_TPCORE_BC: at ', a )
1349d1209
< #     include "comode.h" !fha 3-4-11
1408c1268
< #if defined(GRID4x5) || defined(NESTED_NA)
---
> #if defined(GRID4x5)
1419,1421d1278
<          ! fha 3-4-11: New boundaries for CMAQ AQMII domain
<          ! actual corners: LL: (18,55) UR: (50, 73)
<          ! pad 5 cells to be sure LL-5 UR+5
1423,1424c1280,1281
<          I1_BC_NA = 13 !minimum longitude
<          J1_BC_NA = 50 !minimum latitude
---
>          I1_BC_NA = 17
>          J1_BC_NA = 51
1427,1437c1284,1285
<          I2_BC_NA = 55 !maximum longitude
<          J2_BC_NA = 78 !maximum latitude
<
<
<          ! Lower-left corner of coarse-grid NA BC WINDOW region
<          !I1_BC_NA = 10
<          !J1_BC_NA = 48
<
<          ! Upper-right corner of coarse-grid NA BC WINDOW region
<          !I2_BC_NA = 61
<          !J2_BC_NA = 83
---
>          I2_BC_NA = 57
>          J2_BC_NA = 81
1524,1528d1371
<          !fha 3-4-11
<          ALLOCATE( BC_CU_CP( IM_BC, JM_BC, 38, IGAS ), STAT=AS )
<          IF ( AS /= 0 ) CALL ALLOC_ERR( 'BC_CU_CP' )
<          BC_CU_CP = 0e0
<
1547,1552d1389
<          !fha 3-3-11
<          ALLOCATE( BC_CP( IM_BC_NA, JM_BC_NA, 38, IGAS )
<      &                   , STAT=AS )
<          IF ( AS /= 0 ) CALL ALLOC_ERR( 'BC_CP' )
<          BC_CP = 0e0
<
1634,1636d1470
<       !fha 3-3-11
<       IF ( ALLOCATED( BC_CP  ) ) DEALLOCATE( BC_CP  )
<         IF ( ALLOCATED( BC_CU_CP  ) ) DEALLOCATE( BC_CU_CP  )
diff -r code_2-15-2012.v8-03-02/GeosCore/tracerid_mod.f code_2-15-2012.v8-03-02.org/GeosCore/tracerid_mod.f
1335,1338d1334
<       ! hotp for fha (3/9/11)
<       print*, 'CSPEC SPECIES NAMES'
<       ! end hotp for fha
<
1340,1344d1335
<
<          ! hotp for fha (3/9/11)
<          print*,I,NAMEGAS(I)
<          ! end hotp for fha
<
diff -r code_2-15-2012.v8-03-02/GeosCore/wetscav_mod.f code_2-15-2012.v8-03-02.org/GeosCore/wetscav_mod.f
378,380d377
<
<             ! 062212 - fha - implemented fix for negative values on 22 July 2010 from wiki
<             ! http://wiki.seas.harvard.edu/geos-chem/index.php/Wet_deposition
384,385d380
<                ! mp hack for -ve wet deposition
<                if ( preacc(i,j) .lt. 1.0E-10 ) frac = 0.67
388,389d382
<                ! mp hack for -ve wet deposition
<                if ( preacc(i,j) .lt. 1.0E-10 ) frac = 0.33
diff -r code_2-15-2012.v8-03-02/GeosUtil/file_mod.f code_2-15-2012.v8-03-02.org/GeosUtil/file_mod.f
43,44d42
<       INTEGER, PUBLIC, PARAMETER :: IU_BC_CP   = 24  ! TPCORE BC files: NA CSPEC grid
<       INTEGER, PUBLIC, PARAMETER :: IU_BC_CU_CP = 25 ! TPCORE BC files: GLOBAL CSPEC grid
98d95
< !  26 Apr 2011 - F. Akhtar   - Added boundary CSPEC array files
390d386
< !  26 Apr 2012 - F. Akhtar   - Added boundary CSPEC files
411,412d406
<       CLOSE( IU_BC_CP   )
<       CLOSE( IU_BC_CU_CP )
diff -r code_2-15-2012.v8-03-02/Headers/define.h code_2-15-2012.v8-03-02.org/Headers/define.h
137,138c137,138
< #define GRID2x25    'GRID2x25'
< !#define GRID4x5     'GRID4x5'
---
> !#define GRID2x25    'GRID2x25'
> #define GRID4x5     'GRID4x5'


# v9-01-01 Differences

diff -r Code_v9_01_01_BC_CP/GeosCore/input_mod.f Code_v9_01_01/GeosCore/input_mod.f
4658a4659,4660
>       ! fha 3-8-11: for better organized BC directories index by DATE
>       USE TIME_MOD,     ONLY : GET_NYMDb, GET_NHMSb, EXPAND_DATE
4664a4667
> !  08 Mar 2011 - F. Akhtar - Organized directories by date
4693a4697,4699
>       ! fha 3-8-11: Expand YYYYMM tokens in the directory name
>       CALL EXPAND_DATE( TPBC_DIR_NA, GET_NYMDb(), GET_NHMSb() )
>
4701a4708,4710
>       ! fha 3-8-11: Expand YYYYMM tokens in the directory name
>       CALL EXPAND_DATE( TPBC_DIR_EU, GET_NYMDb(), GET_NHMSb() )
>
4709a4719,4721
>       ! fha 3-8-11: Expand YYYYMM tokens in the directory name
>       CALL EXPAND_DATE( TPBC_DIR_CH, GET_NYMDb(), GET_NHMSb() )
>
4717a4730,4732
>       ! fha 3-8-11: Expand YYYYMM tokens in the directory name
>       CALL EXPAND_DATE( TPBC_DIR, GET_NYMDb(), GET_NHMSb() )
>
4743,4744c4758,4759
<       WRITE( 6, 110     ) 'Dir w/ archived OH files    : ',
<      &                     TRIM( TPBC_DIR )
---
>       WRITE( 6, 110     ) 'Dir w/ archived BC files    : ',
>      &                     TRIM( TPBC_DIR ) !fha 3-8-11: fixed "OH" -> "BC"
diff -r Code_v9_01_01_BC_CP/GeosCore/main.f Code_v9_01_01/GeosCore/main.f
166a167,168
>       !for better CSPEC tracerinfos (fha, 3/3/11)
>       USE GAMAP_MOD,         ONLY : DO_GAMAP
231a234,235
>       CHARACTER(LEN=255) :: DIAGINFO
>       CHARACTER(LEN=255) :: TRACERINFO
546d549
<
1171a1175,1179
>          ! fha - 3-8-11: Redo this after the chem initialization for CSPEC
>          ! Create "diaginfo.dat" and "tracerinfo.dat" files for GAMAP
>          DIAGINFO = 'diaginfo.dat'
>          TRACERINFO = 'tracerinfo.dat'
>          CALL DO_GAMAP( DIAGINFO, TRACERINFO )
diff -r Code_v9_01_01_BC_CP/GeosCore/tpcore_bc_mod.f Code_v9_01_01/GeosCore/tpcore_bc_mod.f
35a36
> !  (19e) BC_CU_CP(REAL*4  )    : Array containing CU species on coarse grid
137c138
< !  (3 ) Now get HALFPOLAR for GEOS or GCAP grids (bmy, 6/28/05)
---
> !  (3 ) Now get HALFPOLAR for GEOS or GCAP grids (bmy, 6/28/05)NC
191a193
>         REAL*4,  ALLOCATABLE :: BC_CU_CP(:,:,:,:)
243a246
> !  (5 ) Now creates CSPEC files too 101, 102 (fha, 3-4-11)
254c257
<       USE FILE_MOD,      ONLY : IU_BC_CP
---
>       USE FILE_MOD,      ONLY : IU_BC_CP, IU_BC_CU_CP
290a294,295
>          ELSEIF ( WINDOW .eq. 101 ) THEN
>             FILENAME = TRIM( TPBC_DIR ) // 'BC.CSPEC.YYYYMMDD'
309a315,318
>            ELSEIF ( WINDOW .eq. 101 ) THEN
>             IF ( PRESENT( FOR_WRITE ) )
>      &        CALL OPEN_BPCH2_FOR_WRITE( IU_BC_CU_CP,
>      &               FILENAME )
313a323,326
>          ELSEIF ( WINDOW .eq. 102 ) THEN
>             IF ( PRESENT( FOR_WRITE ) )
>      &        CALL OPEN_BPCH2_FOR_WRITE( IU_BC_CP,
>      &               FILENAME )
320c333
<      &        CALL OPEN_BPCH2_FOR_WRITE( IU_BC_CP,
---
>      &        CALL OPEN_BPCH2_FOR_WRITE( IU_BC_CH,
358c371
<       USE FILE_MOD,    ONLY : IU_BC_CP
---
>       USE FILE_MOD,    ONLY : IU_BC_CP, IU_BC_CU_CP
422,423c435
<
<                ! Echo info
---
>                      ! Echo info
426a439,466
>
>             ! fha 3-3-11: write out CSPEC BC conditions
>             CALL OPEN_BC_FILE( FOR_WRITE=.TRUE., WINDOW=101)
>
>             ! Loop over each tracer
>             DO N = 1, IGAS
>
>                ! Save concentrations in WINDOW REGION to disk
>                DO L = 1, LLPAR
>                   BC_CU_CP(1:IM_BC,1:JM_BC,L,N) =
>      &               CSPEC_FULL(I1_BC:I2_BC,J1_BC:J2_BC,L,N)
>                ENDDO
>
>                ! Write boundary conditions to binary punch file
>                CALL BPCH2( IU_BC_CU_CP,  MODELNAME, LONRES,   LATRES,
>      &               HALFPOLAR, CENTER180, CATEGOR_S, N,
>      &               UNIT_S,   TAU,       TAU,      RESERVED,
>      &               IM_BC,  JM_BC,   LLPAR,  I1_BC,
>      &               J1_BC, 1, BC_CU_CP(1:IM_BC, 1:JM_BC,
>      &               1:LLPAR, N) )
>
>             ENDDO
>
>
>                ! Echo info
>                STAMP = TIMESTAMP_STRING()
>                WRITE( 6, 120 ) STAMP
>  120  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote CU CP BC''s at ', a )
451c491,494
<
---
>             ! Echo info
>             STAMP = TIMESTAMP_STRING()
>             WRITE( 6, 111 ) STAMP
>  111  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote NA BC''s at ', a )
456c499
<             DO N = 1, NTSPEC(1)
---
>             DO N = 1, IGAS
476,477c519,520
<             WRITE( 6, 111 ) STAMP
<  111  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote NA BC''s at ', a )
---
>             WRITE( 6, 121 ) STAMP
>  121  FORMAT( '     - SAVE_GLOBAL_TPCORE_BC: Wrote NA CP BC''s at ', a )
688c731,732
<
---
>         USE TIME_MOD,   ONLY : TIMESTAMP_STRING !fha 3-5-11
>
690a735
> #     include "comode.h"
693a739
>       CHARACTER(LEN=16)  :: STAMP !fha 3-5-11
713c759,762
<
---
>             ! Echo info
>             STAMP = TIMESTAMP_STRING()
>             WRITE( 6, 113 ) STAMP
>  113  FORMAT( '     - BEGIN_CLEAR_TPCORE_BC: at ', a )
725a775,792
>          ENDDO
> !$OMP END PARALLEL DO
>             ! Echo info
>             STAMP = TIMESTAMP_STRING()
>             WRITE( 6, 114 ) STAMP
>  114  FORMAT( '     - CLEARED_CU_TPCORE_BC: at ', a )
> !$OMP PARALLEL DO
> !$OMP+DEFAULT( SHARED )
> !$OMP+PRIVATE( I, J, L, N )
>          DO N = 1, IGAS
>          DO L = 1, LLPAR
>          DO J = 1, JM_BC
>          DO I = 1, IM_BC
>                       !fha 3-4-11
>                       BC_CU_CP(I,J,L,N) = 0e0
>                ENDDO
>          ENDDO
>          ENDDO
727a795,798
>             ! Echo info
>             STAMP = TIMESTAMP_STRING()
>             WRITE( 6, 115 ) STAMP
>  115  FORMAT( '     - CLEARED_CU_CP_TPCORE_BC: at ', a )
739,740d809
<           !fha 3-3-11
<           BC_CP(I,J,L,N) = 0e0
745a815,836
>             ! Echo info
>             STAMP = TIMESTAMP_STRING()
>             WRITE( 6, 116 ) STAMP
>  116  FORMAT( '     - CLEARED_NA_TPCORE_BC: at ', a )
> !$OMP PARALLEL DO
> !$OMP+DEFAULT( SHARED )
> !$OMP+PRIVATE( I, J, L, N )
>          DO N = 1, IGAS
>          DO L = 1, LLPAR
>          DO J = 1, JM_BC_NA
>          DO I = 1, IM_BC_NA
>               !fha 3-3-11
>               BC_CP(I,J,L,N) = 0e0
>          ENDDO
>          ENDDO
>          ENDDO
>          ENDDO
> !$OMP END PARALLEL DO
>             ! Echo info
>             STAMP = TIMESTAMP_STRING()
>             WRITE( 6, 117 ) STAMP
>  117  FORMAT( '     - CLEARED_NA_CP_TPCORE_BC: at ', a )
1249a1341
> #     include "comode.h" !fha 3-4-11
1318a1411,1420
>          ! fha 3-4-11: New boundaries for CMAQ AQMII domain
>          ! Lower-left corner of coarse-grid NA BC WINDOW region
>          I1_BC_NA = 18 !minimum longitude
>          J1_BC_NA = 51 !minimum latitude
>
>          ! Upper-right corner of coarse-grid NA BC WINDOW region
>          I2_BC_NA = 62 !maximum longitude
>          J2_BC_NA = 83 !maximum latitude
>
>
1320,1321c1422,1423
<          I1_BC_NA = 17
<          J1_BC_NA = 51
---
>          !I1_BC_NA = 10
>          !J1_BC_NA = 48
1324,1325c1426,1427
<          I2_BC_NA = 57
<          J2_BC_NA = 81
---
>          !I2_BC_NA = 61
>          !J2_BC_NA = 83
1410a1513,1517
>
>                !fha 3-4-11
>                ALLOCATE( BC_CU_CP( IM_BC, JM_BC, LLPAR, IGAS ), STAT=AS )
>          IF ( AS /= 0 ) CALL ALLOC_ERR( 'BC_CU_CP' )
>          BC_CU_CP = 0e0
1431c1538
<          ALLOCATE( BC_CP( IM_BC_NA, JM_BC_NA, LLPAR, N_TRACERS )
---
>          ALLOCATE( BC_CP( IM_BC_NA, JM_BC_NA, LLPAR, IGAS )
1518a1626
>         IF ( ALLOCATED( BC_CU_CP  ) ) DEALLOCATE( BC_CU_CP  )
diff -r Code_v9_01_01_BC_CP/GeosCore/tracerid_mod.f Code_v9_01_01/GeosCore/tracerid_mod.f
1334a1335,1338
>       ! hotp for fha (3/9/11)
>       print*, 'CSPEC SPECIES NAMES'
>       ! end hotp for fha
>
1335a1340,1344
>
>          ! hotp for fha (3/9/11)
>          print*,I,NAMEGAS(I)
>          ! end hotp for fha
>
diff -r Code_v9_01_01_BC_CP/GeosUtil/file_mod.f Code_v9_01_01/GeosUtil/file_mod.f
42c42,43
<       INTEGER, PUBLIC, PARAMETER :: IU_BC_CP   = 24  ! TPCORE BC files: NA CSPEC grid
---
>       INTEGER, PUBLIC, PARAMETER :: IU_BC_CP   = 24  ! TPCORE BC files: NA CSPEC grid
>       INTEGER, PUBLIC, PARAMETER :: IU_BC_CU_CP = 25  ! TPCORE BC files: GLOBAL CSPEC grid
391c392
< !  03 Mar 2011 - F. Akhtar - Now close IU_BC_CP
---
> !  03 Mar 2011 - F. Akhtar - Now close IU_BC_CP, IU_BC_CU_CP
412a414
>       CLOSE( IU_BC_CU_CP )
