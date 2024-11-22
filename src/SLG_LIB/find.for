C-----FIND.FOR                                                          V01.01
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
C                                                                       V02.02
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
C  V02.02  03/08/88  HEB  Declare variables.                            V02.02
C                         Initialize local variables in DATA.           V02.02
C                                                                       V02.02
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
                                                                        V02.02
      SUBROUTINE FIND(X,Y,LN,IDS)
                                                                        V02.02
      IMPLICIT NONE                                                     V02.02
                                                                        V02.02
      INTEGER * 2   I,        IBT,      IDS,      ITENS,                V02.02
     :              ITG,      J,                                        V02.02
     :              LDC,      LMT,      LN,       LTYPE,                V02.02
     :              LWDTH,    MAXX,     MDF,      MDS,                  V02.02
     :              MDT,      MINX,     MIX,      MSK,                  V02.02
     :              NBAD,     NBITS,    NBTM1,    NBYM1,                V02.02
     :              NBYTE,    NCHAR,    NCHM1,    NDX,                  V02.02
     :              NDY,      NPLOT,    NSKP                            V02.02
                                                                        V02.02
      INTEGER * 2   INTGRZ                                              V02.02
                                                                        V02.02
      REAL * 4      C,        CMAT,     DEGRAD,   REF,                  V02.02
     :              PREF,     RORG,     SAME,     SPX,                  V02.02
     :              SPY,      TC,       TINC,     X,                    V02.02
     :              XYDOTS,   Y                                         V02.02
                                                                        V02.02
      DIMENSION X(*),Y(*)                                               V01.02
      DIMENSION REF(2),TINC(2),MDS(2),MDT(2),MDF(2),ITG(2)
      DIMENSION ITENS(2)
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
      EQUIVALENCE (ITG(1),C(1))
      DATA ITENS /100,10/
                                                                        V02.02
      DATA I      / 0 /,                                                V02.02
     :     J      / 0 /,                                                V02.02
     :     MDF    / 2 * 0 /,                                            V02.02
     :     MDS    / 2 * 0 /,                                            V02.02
     :     MDT    / 2 * 0 /                                             V02.02
                                                                        V02.02
      DATA REF    / 2 * 0.0 /,                                          V02.02
     :     TC     / 0.0 /,                                              V02.02
     :     TINC    / 2 * 0.0 /                                          V02.02
                                                                        V02.02
C-----------------------------------------------------------------------V02.02
                                                                        V02.02
      IF (LN.GT.0) GO TO 50
C-----RESET FOR NEW ARRAYS, DECODE 'IDS'
      REF(1)=X(1)
      REF(2)=Y(1)
      DO 40 I=1,2
      J=MOD(IDS/ITENS(I),10)
C-----DATA IS REAL IF MDF=0, INTEGER IF MDF=1
CCC   MDF(I)=IAND(J,"1)                                                 V01.01
      MDF(I)=IAND(J, 1)                                                 V01.01
C-----DATA IS COORDINATE IF MDT=1, DELTA IF MDT=2, INCREMENTAL IF MDT=3
CCC   MDT(I)=IAND(ISHFT(J,-1),"1)+IAND(ISHFT(J,-2),"2)+1                V01.01
      MDT(I)=IAND(ISHFT(J,-1), 1)+IAND(ISHFT(J,-2), 2)+1                V01.01
C-----DATA IS SCALED IF MDS=0, UNSCALED IF MDS=1
CCC40 MDS(I)=IAND(ISHFT(J,-2),"1)                                       V01.01
   40 MDS(I)=IAND(ISHFT(J,-2), 1)                                       V01.01
      RETURN
C-----ELEMENT EXTRACTION
   50 IF (MDF(2).NE.0) GO TO 60
C-----Y DATA IS REAL
      C(2)=Y(LN)
      GO TO 65
   60 CONTINUE
C-----Y DATA IS INTEGER, ADJUST SUBSCRIPTING TO DATA TYPE
      C(2)=INTGRZ(Y,LN)
   65 IF (MDF(1).NE.0) GO TO 75
C-----X DATA IS REAL
      C(1)=X(LN)
      GO TO 80
   75 CONTINUE
C-----X DATA IS INTEGER, ADJUST SUBSCRIPTING TO DATA TYPE
      C(1)=INTGRZ(X,LN)
   80 CONTINUE
C-----COORDINATE PROCESSING
   90 DO 200 I=1,2
      TC=C(I)
      IF (MDS(I).EQ.0) GO TO 130
C-----UNSCALED (PERFORM SCALING)
  110 J=7+I
      TC=(TC-CMAT(J,1))/CMAT(J,2)
C-----EXAMINE DATA TYPE
  130 J=MDT(I)
      GO TO (200,160,140,150), J
C-----INCREMENTAL (1ST INCREMENT)
  140 TINC(I)=TC
      REF(I)=0.0
      MDT(I)=4
      TC=0.0
      GO TO 200
C-----INCREMENTAL (SUBSEQUENT INCREMENTS)
  150 TC=TINC(I)
C-----DELTA
  160 TC=TC+REF(I)
      REF(I)=TC
C-----COORDINATE
  200 C(I)=TC
      RETURN
      END
