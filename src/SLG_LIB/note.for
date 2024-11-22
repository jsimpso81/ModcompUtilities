C-----NOTE.FOR
C
C      X ARRAY
C      Y ARRAY
C      ITEXT ARRAY(2)
C      NC
C      
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
C                                                                       V02.02
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
C  V02.02  03/08/88  HEB  Declare variables.                            V02.02
C                         Initialize local variables in DATA.           V02.02
C                                                                       V02.02
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
                                                                        V02.02
      SUBROUTINE NOTE(X,Y,ITEXT,NC)
                                                                        V02.02
      IMPLICIT NONE                                                     V02.02
                                                                        V02.02
      INTEGER * 2   I,        IBT,      IBUF,     IDASH,                V02.02
     :              IDOTT,    IDS,      II,       III,                  V02.02
     :              IOV1,     IOV2,     IT1,      L,                    V02.02
     :              ITEXT,    ITG,      J,        K,                    V02.02
     :              LDC,      LMT,      LTYPE,                          V02.02
     :              LWDTH,    MAXX,                                     V02.02
     :              MINX,     MIX,      MSK,      NBUF,                 V02.02
     :              NBAD,     NBITS,    NBTM1,    NBYM1,                V02.02
     :              NBYTE,    NC,       NCHAR,    NCHM1,                V02.02
     :              ND,       NDX,      NTXT,                           V02.02
     :              NDY,      NPLOT,    NRQD,     NSKP                  V02.02
                                                                        V02.02
      INTEGER * 2   LG1GRZ                                              V02.02
                                                                        V02.02
      REAL * 4      C,        CMAT,     DEGRAD,                         V02.02
     :              PREF,     RORG,     SAME,     SPX,                  V02.02
     :              SPY,      T1,       T2,       T3,                   V02.02
     :              T4,       X,        X0,       Y0,                   V02.02
     :              XYDOTS,   Y                                         V02.02
                                                                        V02.02
C
      INTEGER*2 ZZZ(1)
C      
      DIMENSION X(*),Y(*)
      DIMENSION NBUF(6),ITG(2),IBUF(13)
CCC   DIMENSION ITEXT(1)                                                V01.01
      DIMENSION ITEXT(2)                                                V01.01
      DIMENSION IT1(2)
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
      EQUIVALENCE (C(1),ITG(1))
      EQUIVALENCE (T1,IT1(1))
CCC   DATA IBUF(1)/"40/                                                 V01.01
      DATA IBUF   / 32, 12 * 0 /                                        V02.02
CCC   DATA IDASH/"55/                                                   V01.01
      DATA IDASH/ 45/                                                   V01.01
CCC   DATA IDOTT/"56/,NTXT/"60/                                         V01.01
      DATA IDOTT/ 46/,NTXT/ 48/                                         V01.01
      DATA IOV1/'##'/,IOV2/'# '/
                                                                        V02.02
      DATA I      / 0 /,                                                V02.02
     :     IDS    / 0 /,                                                V02.02
     :     II     / 0 /,                                                V02.02
     :     III    / 0 /,                                                V02.02
     :     L      / 0 /,                                                V02.02
     :     J      / 0 /,                                                V02.02
     :     K      / 0 /,                                                V02.02
     :     NBUF   / 6 * 0 /,                                            V02.02
     :     ND     / 0 /,                                                V02.02
     :     NRQD   / 0 /                                                 V02.02
                                                                        V02.02
      DATA T1     / 0.0 /,                                              V02.02
     :     T2     / 0.0 /,                                              V02.02
     :     T3     / 0.0 /,                                              V02.02
     :     T4     / 0.0 /,                                              V02.02
     :     X0     / 0.0 /,                                              V02.02
     :     Y0     / 0.0 /                                               V02.02
                                                                        V02.02
C-----------------------------------------------------------------------V02.02
                                                                        V02.02
      X0=X(1)
      Y0=Y(1)
      IF (X0.EQ.SAME) X0=PREF(1)
      IF (Y0.EQ.SAME) Y0=PREF(2)
C-----EXAMINE 'NC' FOR PROCESSING SPECIFICATIONS
      IF (NC) 200,300,50

   50 ND=NC-1000
      IF (ND.GE.0) GO TO 400

C-----**ALPHANUMERIC TEXT**
      ND=NC
  110 CALL DRAW(X0,Y0,1,0)
C-----INSERT 'ALPHA TEXT FOLLOWS' CODE INTO DATA STREAM
      I=IBT(9)+ND
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      IF (ND.NE.NC) GO TO 130
C-----INSERT ALPHA TEXT

C--------IF ND=1 AND ITEXT IS LESS THAN 256 THEN SHIFT TO THE UPPER     JAS
C--------BYTE                                                           JAS
      IF (ND.EQ.1.AND.ITEXT(1).LT.256) ITEXT(1)=ISHFT(ITEXT(1),8)       JAS

      DO 120 I=1,ND,2
      K=LG1GRZ(ITEXT,I)
CCC   IF (I+1.LE.ND) K=K+ISHFT(LG1GRZ(ITEXT,I+1),8)                     JAS
      K = ISHFT(K,8)                                                    JAS
      IF (I+1.LE.ND) K=K+LG1GRZ(ITEXT,I+1)                              JAS
CJAS  CALL POUT(1,K)                                                    JAS
      ZZZ(1) = K
      CALL POUT(1,ZZZ)                                                  JAS
120   CONTINUE                                                          JAS
      GO TO 150
C-----COMPUTE WORD COUNT AND INSERT NUMERIC TEXT
  130 I=(ND+NBYM1)/NBYTE
      CALL POUT(I,NBUF)
C-----UPDATE PEN POSITION TO END OF TEXT LINE
  150 X0=X0+FLOAT(ND)*SPX
      Y0=Y0+FLOAT(ND)*SPY
      CALL DRAW(X0,Y0,1,0)
      RETURN

C-----**SYMBOL PLOTTING**
  200 IDS=CMAT(5,3)
      CALL DRAW(X,Y,1,IDS)
      I=MOD(IDS,10)
      IF (I.NE.0) IDS=IDS-I+9
      J=IAND(ISHFT(ITEXT(1),-8),NCHM1)
      IF (J.EQ.0) J=IAND(ITEXT(1),NCHM1)
      I=IBT(8)+J
      ND=-NC
      K=NSKP
      L=1
      DO 250 J=1,ND
      CALL DRAW(X(L),Y(L),1,IDS)
      K=K+1
      IF (K.LT.NSKP) GO TO 250
CJAS  CALL POUT(1,I)
      ZZZ(1) = I
      CALL POUT(1,ZZZ)
      K=0
  250 L=L+MIX
      RETURN

C-----**INTEGER NUMBER VALUE TEXT GENERATION**
  300 T1=ITEXT(1)
      ND=0
      GO TO 500

C-----**REAL NUMBER VALUE TEXT GENERATION**
C-----(ITEXT IS ACTUALLY FL. PT. NUMBER)
  400 CONTINUE
      IT1(1)=ITEXT(1)
      IT1(2)=ITEXT(2)

C-----*NUMERIC VALUE TO ALPHANUMERIC TEXT*
C-----NRQD IS THE MINIMUM NUMBER OF CHARACTERS REQUIRED
C-----TO GUARANTEE AT LEAST ONE DIGIT TO THE LEFT OF THE REAL OR
C-----IMPLIED DECIMAL POINT.
  500 NRQD=ND+2
C-----K IS THE VALUE I WILL HAVE JUST BEFORE THE FIRST DIGIT TO THE
C-----LEFT OF THE DECIMAL WOULD BE PROCESSED IF IT WERE NOT FOR THE
C-----DECIMAL POINT ITSELF, OR ZERO IF NO DECIMAL POINT IS NEEDED.
      K=0
      IF (ND.NE.0) K=ND+2
      T2=AINT(ABS(T1)*10.0**ND+0.5)
C-----T2 IS NOW A POSITIVE, FLOATING POINT INTEGER, THE DIGITS
C-----OF WHICH REPRESENT THE ORIGINAL NUMBER.
C
C-----IF ALL DIGITS ARE ZERO, PREVENT MINUS SIGN
      IF (T2.EQ.0.0) T1=0.0
C-----ALLOW FOR TEN DIGITS.  NOTE THAT THE LOOP STARTS WITH I .EQ. 2
C-----TO ALLOW FOR THE PAD IN IBUF.
      L=11
C-----ALSO FOR A DECIMAL, IF REQUIRED.
      IF (ND.GT.0) L=L+1
      DO 520 I=2,L
      IF (I.NE.K) GO TO 510
C-----AT THIS POINT, A DECIMAL POINT IS REQUIRED.  FURTHERMORE,
C-----I-2 .EQ. K-2 .EQ. ND DIGITS TO THE RIGHT OF THE DECIMAL HAVE
C-----BEEN PROCESSED, SO THIS IS THE PLACE FOR THE DECIMAL.
      IBUF(I)=IDOTT
      GO TO 520
C-----GENERATE THE NEXT DIGIT (FROM RIGHT TO LEFT).
  510 T3=T2
      T2=AINT(T2/10.0)
      T4=T3-10.0*T2
      IF (T4.LT.0.0.OR.T4.GT.9.0) T4=0.0
      IBUF(I)=IFIX(T4)+NTXT
  520 IF (T2.EQ.0.0.AND.I.GE.NRQD) GO TO 530
C-----AT THIS POINT, TEN DIGITS HAVE BEEN PROCESSED AND
C-----MORE ARE YET REQUIRED.  PLOT ### IN PLACE OF THE NUMBER.
      ND=3
      NBUF(1)=IOV1
      NBUF(2)=IOV2
      GO TO 110
C-----THE DIGITS AND DECIMAL POINT ARE NOW IN IBUF
C-----PUT A MINUS SIGN IN IBUF IF REQUIRED.
  530 IF (T1.GE.0.0) GO TO 540
      I=I+1
      IBUF(I)=IDASH
  540 CONTINUE
C-----THE NUMBER IS NOW COMPLETELY ASSEMBLED IN IBUF IN REVERSE
C-----ORDER.  IT IS NOW REVERSED AND PACKED INTO NBUF, TWO CHARACTERS
C-----PER WORD.
      J=1
      DO 550 III=2,I,2
      II=I+2-III
CCC   NBUF(J)=IOR(IBUF(II),ISHFT(IBUF(II-1),8))                         JAS
      NBUF(J)=IOR(IBUF(II-1),ISHFT(IBUF(II),8))                         JAS
  550 J=J+1
C-----THERE ARE NOW I-1 DESIRABLE CHARACTERS IN NBUF TO BE PLOTTED
      ND=I-1
      GO TO 110
      END
