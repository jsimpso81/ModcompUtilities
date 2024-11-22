CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      SUBROUTINE POUT(KNT,IVD)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   I,        IBT,      IVD,      KNT,                  V02.01
     :              LDC,      LMT,      LTYPE,                          V02.01
     :              LU1,      LU2,      LU3,      LU4,                  V02.01
     :              LU5,      LWDTH,    MAXX,                           V02.01
     :              MINX,     MIX,      MSK,                            V02.01
     :              NBAD,     NBITS,    NBTM1,    NBYM1,                V02.01
     :              NBYTE,    NCHAR,    NCHM1,    NDX,                  V02.01
     :              NDY,      NPLOT,    NSKP                            V02.01
                                                                        V02.01
      REAL * 4      C,        CMAT,     DEGRAD,                         V02.01
     :              PREF,     RORG,     SAME,     SPX,                  V02.01
     :              SPY,      XYDOTS                                    V02.01
                                                                        V02.01
      INTEGER*2 ZZZ(1)
CJAS  DIMENSION IVD(1)
      DIMENSION IVD(*)
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
                                                                        V02.01
      DATA I      / 0 /                                                 V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
                                                                        V02.01
    1 FORMAT (' ***PLOT',I3,',',I5,' UNPLOTTABLE POINTS***'//)
C-----OUTPUT 'KNT'
CJAS  CALL PPUT(KNT,1)
      ZZZ(1) = KNT
      CALL PPUT(ZZZ,1)
C-----TEST 'KNT' FOR SPECIFIED OPERATION
      IF (KNT) 300,200,100
C-----*OUTPUT ATOMS*
  100 CALL PPUT(IVD,KNT)
      RETURN
C-----*END OF CURRENT PLOT/FRAME*
  200 CONTINUE
CJAS  CALL PPUT(MINX,1)
CJAS  CALL PPUT(MAXX,1)
      ZZZ(1) = MINX
      CALL PPUT(ZZZ,1)
      ZZZ(1) = MAXX
      CALL PPUT(ZZZ,1)
      IF (CMAT(1,3)) 230,210,220
  210 IF (NBAD.LE.0) GO TO 230
C-----LIST PLOT NUMBER AND UNPLOTTABLE POINTS COUNT
  220 WRITE (LU2,1) NPLOT,NBAD
C-----REINITIALIZE FOR NEXT PLOT
  230 NBAD=0
      NPLOT=NPLOT+1
      MINX=32767
      MAXX=-9999
      LTYPE=0
      LWDTH=0
      DO 240 I=1,2
      LDC(I)=-9999
  240 PREF(I)=0.0
      RETURN
C-----*TERMINATE ALL PLOTTING*
  300 CONTINUE
CJAS  CALL PPUT(KNT,-1)
      ZZZ(1) = KNT
      CALL PPUT(ZZZ,-1)
  999 RETURN
      END
