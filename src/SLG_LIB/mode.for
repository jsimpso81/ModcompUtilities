C-----MODE.FOR
C
C         ME
C         A1(*)
C         A2(*)
C         A3(*)      
C      
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
C                                                                       V02.02
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
C  V02.02  03/08/88  HEB  Declare variables.                            V02.02
C                         Initialize local variables in DATA.           V02.02
C                                                                       V02.02
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
                                                                        V02.02
      SUBROUTINE MODE(ME,A1,A2,A3)
                                                                        V02.02
      IMPLICIT NONE                                                     V02.02
                                                                        V02.02
      INTEGER * 2   I,        IA2,      IBT,      IISAME,               V02.02
     :              ITEXT,    ITG,      J,        K,                    V02.02
     :              LDC,      LMT,      LNAME,    LTYPE,                V02.02
     :              LU1,      LU2,      LU3,      LU4,                  V02.02
     :              LU5,      LWDTH,    M,        MAXX,                 V02.02
     :              ME,       MINX,     MIX,      MSK,                  V02.02
     :              NBAD,     NBITS,    NBTM1,    NBYM1,                V02.02
     :              NBYTE,    NCHAR,    NCHM1,    NDX,                  V02.02
     :              NDY,      NPLOT,    NSKP                            V02.02
                                                                        V02.02
      INTEGER * 2   INTGRZ,   JFIX,     LG1GRZ                          V02.02
                                                                        V02.02
      REAL * 4      A1,       A2,       A3,       AT,                   V02.02
     :              C,        CMAT,     DEGRAD,   FACT,                 V02.02
     :              PREF,     RORG,     SAME,     SPX,                  V02.02
     :              SPY,      T1,       T2,       T3,                   V02.02
     :              XYDOTS                                              V02.02
C                                                                       V02.02
      INTEGER*2 ZZZ(1)
C      
      DIMENSION ITEXT(2),ITG(2),A1(*),A2(*),A3(*)
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /PLTCOM/SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
      EQUIVALENCE (IISAME,SAME)
      DATA FACT/1.0/,ITEXT/'X ','Y '/
                                                                        V02.02
      DATA I      / 0 /,                                                V02.02
     :     IA2    / 0 /,                                                V02.02
     :     ITG    / 2 * 0 /,                                            V02.02
     :     J      / 0 /,                                                V02.02
     :     K      / 0 /,                                                V02.02
     :     LNAME  / 0 /,                                                V02.02
     :     M      / 0 /                                                 V02.02
                                                                        V02.02
      DATA AT     / 0.0 /,                                              V02.02
     :     T1     / 0.0 /,                                              V02.02
     :     T2     / 0.0 /,                                              V02.02
     :     T3     / 0.0 /                                               V02.02
                                                                        V02.02
C-----------------------------------------------------------------------V02.02
                                                                        V02.02
    1 FORMAT (' LOWER ',A2,'LIMIT OFFSET NOT ALLOWABLE, RESET TO 0.0')
    2 FORMAT (' UPPER ',A2,'LIMIT NOT ALLOWABLE, RESET TO',F7.2)
      M=IABS(ME)
      IF (M.GT.10) GO TO 999
      IF (ME) 20,1100,30
C-----MODE ENTRY VALUE REQUESTS
   20 A1(1)=CMAT(M,1)
      A2(1)=CMAT(M,2)
      A3(1)=CMAT(M,3)
      RETURN
C-----CHECK FOR 'SAME' ENTRIES
   30 CONTINUE
      IF (M.EQ.10) GO TO 70
      IF (A1(1).NE.SAME) CMAT(M,1)=A1(1)
      IF (A2(1).NE.SAME) CMAT(M,2)=A2(1)
   70 IF (A3(1).NE.SAME) CMAT(M,3)=A3(1)
C-----PROCESS NEW SPECIFICATIONS
      GO TO (100,300,300,400,500,600,700,900,900,1000), M
C-----UNITS OF MEASURE AND DRAWING FACTOR CHANGES------ROW 3 ENTRIES***
  100 T1=CMAT(1,2)/CMAT(1,1)
      T2=FACT/T1
      DO 122 I=1,2
      PREF(I)=PREF(I)*T2
      RORG(I)=RORG(I)*T2
      M=I+1
      DO 120 J=1,3
  120 CMAT(M,J)=CMAT(M,J)*T2
  122 XYDOTS(I)=XYDOTS(I)/T2
      FACT=T1
      RETURN
C-----SCALED PLOTTING AREA SPECIFICATION--ROW 2 AND 3 ENTRIES***
  300 I=M-1
      IF (CMAT(M,3).GE.0.0) GO TO 330
      CMAT(M,3)=0.0
      WRITE (LU2,1) ITEXT(I)
  330 T1=CMAT(M,1)
      T2=CMAT(M,3)-CMAT(M,2)
      RORG(I)=T2
      T3=T1-CMAT(M,2)
      T2=FLOAT(LMT(I))/XYDOTS(I)+T2
      IF (T3.LE.0.0) GO TO 350
      IF (T2.GE.T1) GO TO 999
  350 CMAT(M,1)=T2
      WRITE (LU2,2) ITEXT(I),T2
      RETURN
C-----ANNOTATION ANGLE-----------ROW 4 ENTRIES***
  400 IF (A3(1).EQ.SAME) GO TO 470
      M=MSK(3)
      AT=A3(1)
C-----REDUCE ANGULAR MEASUREMENT TO RANGE +0 TO +359
  420 I=JFIX(AT)-360*JFIX(AT/360.0)
  460 I=I+M
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
CCC   I=IAND(ISHFT(M,-9),"3)                                            V01.01
      I=IAND(ISHFT(M,-9), 3)                                            V01.01
  462 GO TO (520,999,470), I
C-----CHARACTER HEIGHT AND WIDTH
  470 IF (A1(1).EQ.SAME.AND.A2(1).EQ.SAME) GO TO 495
      I=CMAT(4,2)*XYDOTS(1)+0.5
      ITG(1)=IBT(13)+I
      ITG(2)=CMAT(4,1)*XYDOTS(2)+0.5
      CALL POUT(2,ITG)
C-----CHAT HORIZONTAL AND VERTICAL SPACING
      CMAT(6,2)=CMAT(4,2)*1.6
      CMAT(6,3)=CMAT(4,1)*1.6
  490 I=CMAT(6,2)*XYDOTS(1)+0.5
      ITG(1)=IBT(12)+I
      ITG(2)=CMAT(6,3)*XYDOTS(2)+0.5
      CALL POUT(2,ITG)
  495 T1=CMAT(4,3)*DEGRAD
      T3=CMAT(6,2)
      SPX=COS(T1)*T3
      SPY=SIN(T1)*T3
      RETURN
C--------------------------------------------ROW 5 ENTRIES***
C-----CHAR SLANT ANGLE
  500 IF (A1(1).EQ.SAME) GO TO 520
      M=IBT(10)
      AT=A1(1)
      GO TO 420
C-----CHARACTER ORIENTATION ANGLE
  520 IF (A2(1).EQ.SAME) GO TO 999
      M=IBT(11)
      AT=A2(1)
      GO TO 420
C-----CHAR LINE WIDTH-----------------------------ROW 6 ENTRIES***
  600 IF (A1(1).EQ.SAME) GO TO 620
      I=A1(1)
CCC   I=IBT(4)+IAND(I-1,"7)                                             V01.01
      I=IBT(4)+IAND(I-1, 7)                                             V01.01
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
C-----(CHARACTER HORIZONTAL AND VERTICAL SPACING)
  620 IF (A2(1).EQ.SAME.AND.A3(1).EQ.SAME) GO TO 999
      GO TO 490
C---------------------------------------------ROW 7 ENTRIES***
C-----SYMBOL/MULTIPLE ARRAY SKIP FACTORS
  700 NSKP=CMAT(7,3)
      MIX=(CMAT(7,3)-FLOAT(NSKP))*10.1
      IF (MIX.LE.0) MIX=1
  720 IF (NSKP.GT.0) GO TO 999
      NSKP=1
      RETURN
C---------------------------------------------ROW 8 AND 9 ENTRIES***
  900 IF (A2(1).NE.0.0) GO TO 999
      CMAT(M,2)=1.0
  999 RETURN
C------------------------------------------------ROW 10 ENTRIES
C-----LINE MASK
 1000 IF (A1(1).EQ.SAME) GO TO 1020
      ITG(1)=IBT(2)
      ITG(2)=A1(1)
      CALL POUT(2,ITG)
C-----TONE PATTERN AND COUNT
 1020 M=CMAT(10,3)
      I=MSK(4)+M
      IA2=INTGRZ(A2,1)
      IF (IA2.NE.IISAME) GO TO 1050
 1030 IF (A3(1).EQ.SAME) GO TO 999
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      RETURN
 1050 I=I+IBT(7)
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      CALL POUT(M,A2(1))
      RETURN
C-----------------------------------------------------ROW 0 ENTRIES ***
C-----SPECIAL SYSTEM INITIALIZATION PROCEDURES
 1100 LNAME=INTGRZ(A1,1)
      CALL POUT(1,IBT(3))
CJAS  CALL POUT(1,LNAME)
      ZZZ(1) = LNAME
      CALL POUT(1,ZZZ)
      DO 1101 I=1,19,2
      K=0
      IF (I.LE.LNAME) K=K+LG1GRZ(A2,I)
      IF (I+1.LE.LNAME) K=K+ISHFT(LG1GRZ(A2,I+1),8)
CJAS  CALL POUT(1,K)
      ZZZ(1) = K
      CALL POUT(1,ZZZ)
 1101 CONTINUE
      RETURN
      END
