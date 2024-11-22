C-----DRAW.FOR
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      SUBROUTINE DRAW(X,Y,NE,IDS)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   I,        IBT,      IDS,      ITH,                  V02.01
     :              J,        K,        KAF,      L,                    V02.01
     :              LDC,      LINE,     LMT,      LN,                   V02.01
     :              LTYPE,    LWDTH,    M,        MAXX,                 V02.01
     :              MINX,     MIX,      MSK,      MXX,                  V02.01
     :              N,        NBAD,     NBITS,    NBTM1,    NBYM1,      V02.01
     :              NBYTE,    NCHAR,    NCHM1,    NDC,                  V02.01
     :              NDLT,     NDX,      NDY,      NE,                   V02.01
     :              NERR,     NPLOT,    NSKP                            V02.01
                                                                        V02.01
      REAL * 4      ALMT,     AX,       AY,                             V02.01
     :              C,        CMAT,     DEGRAD,   DLT,                  V02.01
     :              PREF,     RORG,     RSC,      SAME,     SPX,        V02.01
     :              SPY,      X,        XYDOTS,   Y                     V02.01
C      
      DIMENSION AX(1), AY(1)
      INTEGER*2  ZZZ(1)
C      
      DIMENSION X(*),Y(*)
      DIMENSION RSC(4),DLT(2),NDC(2),NDLT(2)
      COMMON /PLTCOM/SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
                                                                        V02.01
      DATA I      / 0 /,                                                V02.01
     :     ITH    / 0 /,                                                V02.01
     :     J      / 0 /,                                                V02.01
     :     K      / 0 /,                                                V02.01
     :     KAF    / 0 /,                                                V02.01
     :     L      / 0 /,                                                V02.01
     :     LINE   / 0 /,                                                V02.01
     :     LN     / 0 /,                                                V02.01
     :     M      / 0 /,                                                V02.01
     :     MXX    / 0 /,                                                V02.01
     :     N      / 0 /,                                                V02.01
     :     NDC    / 2 * 0 /,                                            V02.01
     :     NDLT   / 2 * 0 /,                                            V02.01
     :     NERR   / 0 /                                                 V02.01
                                                                        V02.01
      DATA ALMT   / 0.0 /,                                              V02.01
     :     AX     / 0.0 /,                                              V02.01
     :     AY     / 0.0 /,                                              V02.01
     :     DLT    / 2 * 0.0 /,                                          V02.01
     :     RSC    / 4 * 0.0 /                                           V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
                                                                        V02.01
      IF (NE) 10,50,100
C-----PRESENT POSITION REQUEST
   10 X(1)=PREF(1)
      Y(1)=PREF(2)
      RETURN
C-----TERMINATE PLOTTING
   50 CONTINUE
CJAS  CALL POUT(-1,0)
      ZZZ(1) = 0
      CALL POUT(-1,ZZZ)
      RETURN
C-----COORDINATE PROCESSING
C-----EXAMINE LINE TYPE (STRAIGHT LINE=0, ELLIPTICAL CLOCKWISE=1,
C-----ELLIPTICAL COUNTERCLOCKWISE=2, POINT PLOT=3)
  100 ITH=MOD(IDS/1000,10)
      I=IAND(ISHFT(ITH,-1), 3)
      IF (I.EQ.LTYPE) GO TO 120
      LTYPE=I
      I=MSK(6)+LTYPE
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      I = ZZZ(1)
C-----EXAMINE LINE WIDTH AND CONTROL
  120 LINE=-1
      I=MOD(IDS,10)-1
      IF (I.GT.7) GO TO 200
      IF (I.LT.0) GO TO 160
      IF (I.EQ.LWDTH) GO TO 160
      LWDTH=I
      I=IBT(5)+LWDTH
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      I = ZZZ(1)
  160 LINE=0
C-----INITIALIZE *FIND* FOR COORDINATE PROCESSING
  200 CONTINUE
      AX(1)=PREF(1)
      AY(1)=PREF(2)
      CALL FIND(AX,AY,0,IDS)
C-----EXTRACT AND PROCESS INDIVIDUAL COORDINATES
      LN=1
      DO 650 MXX=1,NE
      CALL FIND(X,Y,LN,IDS)
C-----INITIALIZE COORDINATE PROCESSING ARRAYS
      DO 210 I=1,2
      DLT(I)=PREF(I)-C(I)
      RSC(I)=PREF(I)
  210 RSC(I+2)=C(I)
      DO 350 I=1,3,2
      DO 290 N=1,2
      NERR=0
C-----TEST THAT LINE SEGMENT PASSES WITHIN LIMITS
      DO 280 J=1,2
      K=J+I-1
      M=3-I+J
      L=J+1
C-----TEST UPPER LIMITS
      ALMT=CMAT(L,1)
      IF (RSC(K).LE.ALMT) GO TO 230
      IF (RSC(M).GT.ALMT) GO TO 300
      GO TO 250
C-----TEST LOWER LIMITS
  230 ALMT=CMAT(L,2)
      IF (RSC(K).GE.ALMT) GO TO 280
      IF (RSC(M).LT.ALMT) GO TO 300
C-----COORDINATE OUT OF RANGE - ATTEMPT INTERSECTION
  250 NERR=-1
      L=3-J
      RSC(K)=ALMT
      KAF=5-M
      RSC(KAF)=(DLT(L)/DLT(J))*(ALMT-PREF(J))+PREF(L)
  280 CONTINUE
  290 CONTINUE
C-----CHECK FOR INTERSECTION FAILURE
      IF (NERR.EQ.0) GO TO 350
  300 NBAD=NBAD+1
      GO TO 630
  350 CONTINUE
C-----CHECK FOR PRESENT POSITION WITHIN RANGE
      N=3
      L=LINE
      DO 420 I=1,2
      IF (PREF(I).EQ.RSC(I)) GO TO 420
      L=0
      N=1
  420 CONTINUE
      DO 620 I=N,3,2
C-----COMPUTE NEW DOT COORDINATES AND DELTAS
      DO 450 J=1,2
      KAF=I+J-1
      NDC(J)=(RSC(KAF)+RORG(J))*XYDOTS(J)+0.5
  450 NDLT(J)=NDC(J)-LDC(J)
      NDX=NDC(1)
      NDY=NDC(2)
      IF (NDX.GT.MAXX) MAXX=NDX
      IF (NDX.LT.MINX) MINX=NDX
C-----COORDINATE ENCODING PROCEDURES
C-----X-ONLY OR Y-ONLY DELTA
  500 M=1
      DO 530 J=1,2
      K=3-J
      IF (NDLT(J).NE.0) GO TO 530
      IF (IABS(NDLT(K)).GT.2047) GO TO 550
      IF (IOR(NDLT(K),L).EQ.0) GO TO 610
      LDC(1)=IBT(K+13)+IAND(L,IBT(13))+NDLT(K)+2047
      GO TO 600
  530 CONTINUE
C-----X-Y DELTA
      DO 540 J=1,2
      IF (IABS(NDLT(J)).GT.63) GO TO 550
  540 NDLT(J)=NDLT(J)+63
      KAF=IAND(L,IBT(15))+NDLT(1)*128+NDLT(2)
      LDC(1)=IOR(IBT(16),KAF)
      GO TO 600
C-----X-Y COORDINATE
  550 LDC(1)=MSK(1)+NDY
      LDC(2)=IOR(IAND(L,IBT(16)),NDX)
      M=2
C-----OUTPUT
  600 CALL POUT(M,LDC)
C-----UPDATE DOT LOCATION TO NEW POSITION
  610 LDC(1)=NDX
      LDC(2)=NDY
  620 L=LINE
C-----UPDATE PRESENT REFERENCE LOCATION
  630 PREF(1)=C(1)
      PREF(2)=C(2)
      LINE=-1
  650 LN=LN+MIX
C-----EXAMINE SPECIAL FUNCTIONS
C-----TEST FOR END OF PLOT OT FRAME SPECIFICATION
      I=IAND(ITH, 1)
      IF (ITH.GE.8) GO TO 750
C-----TEST FOR RE-ORIGIN SPECIFICATION
      IF (I.EQ.0) GO TO 999
C-----RE-ORIGIN AT PRESENT POSITION
      DO 730 I=1,2
      J=I+1
      ALMT=PREF(I)
      RORG(I)=RORG(I)+ALMT
      CMAT(J,1)=CMAT(J,1)-ALMT
      CMAT(J,2)=CMAT(J,2)-ALMT
  730 PREF(I)=0.0
      RETURN
C-----END OF PLOT OR END OF FRAME
  750 I=MSK(7)+I
CJAS  CALL POUT(1,I)
CJAS  CALL POUT(0,0)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      ZZZ(1) = 0
      CALL POUT(0,ZZZ)
  999 CONTINUE
      RETURN
      END
