C-----NUMBER.FOR
C
C     NUMBER  DRAW A NUMBER
C
C     CONVERTS A REAL NUMBER TO ITS FIXED-DECIMAL EQUIVALENT
C     TO BE PLOTTED IN FORTRAN F-TYPE OR I-TYPE FORMAT.
C
C     NUMBER IS A PREPROCESSOR TO THE SYMBOL SUBROUTINE.
C     IT CALLS SYMBOL, WHICH IN TURN CALLS PLOT.
C
C     PARAMETER DESCRIPTIONS:
C
C     XPAGE,  THE X AND Y COORDINATES, IN INCHES, OF THE
C     YPAGE   LOWER-LEFT CORNER OF THE FIRST CHARACTER TO
C             BE PRODUCED.  IF THE CHARACTERS ARE NOT AT
C             ZERO DEGREES, THIS POINT IS ALSO THE CENTER
C             OF ROTATION.
C
C             THE PEN IS UP WHILE MOVING TO THIS POINT.
C
C     HEIGHT  THE HEIGHT, IN INCHES, OF THE NUMBER.
C
C             THE WIDTH OF THE NUMBER, INCLUDING SPACING,
C             WILL BE EQUAL TO THE HEIGHT.
C
C     FPN     THE VARIABLE NAME OF THE REAL NUMBER TO
C             BE CONVERTED AND PLOTTED.
C
C     ANGLE   THE ANGLE AT WHICH THE ANNOTATION IS TO BE
C             PLOTTED, IN DEGREES COUNTERCLOCKWISE FROM
C             THE X AXIS.  IF ANGLE=0, THE NUMBER
C             WILL BE PLOTTED RIGHT SIDE UP, PARALLEL TO
C             THE X-AXIS.
C
C     NDEC    CONTROLS THE PRECISION OF THE REAL-NUMBER
C             CONVERSION, BY SPECIFYING THE NUMBER OF
C             DIGITS TO THE RIGHT OF THE DECIMAL
C             POINT WHICH WILL BE CONVERTED AND PLOTTED,
C             AFTER ROUNDING.
C
C     NOTE:   ANY ATTEMPT TO PLOT MORE THAN 10 DIGITS
C             (EXCLUSIVE OF THE SIGN AND DECIMAL POINT)
C             WILL RESULT IN THE PLOTTING OF THREE
C             NUMBER SIGNS (###).  IF THIS OCCURS,
C             YOUR NUMBER SHOULD BE SCALED DOWN TO 10 OR FEWER DIGITS.
C
C             IF NDEC=0, ONLY THE INTEGER PORTION OF
C             THE NUMBER (FOLLOWED BY A DECIMAL POINT)
C             IS PLOTTED AFTER ROUNDING.
C
C             IF NDEC=1, ONLY THE INTEGER PORTION OF
C             THE NUMBER IS PLOTTED AFTER ROUNDING.
C
C             IF NDEC<-1, -NDEC-1 DIGITS ARE TRUNCATED
C             FROM THE INTEGER PORTION AFTER ROUNDING,
C             AND THE REMAINING DIGITS ARE PLOTTED.
C
C             THE NUMBER IS PLOTTED AFTER ROUNDING.
C
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
C                                                                       V02.02
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
C  V02.02  03/08/88  HEB  Declare variables.                            V02.02
C                         Initialize local variables in DATA.           V02.02
C                                                                       V02.02
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.02
                                                                        V02.02
      SUBROUTINE NUMBER(XPAGE,YPAGE,HEIGHT,FPN,ANGLE,NDEC)
                                                                        V02.02
      IMPLICIT NONE                                                     V02.02
                                                                        V02.02
      INTEGER * 2   I,        ICHAR,    II,       III,                  V02.02
     :              J,        JCHAR,    KDECPT,   KMINUS,               V02.02
     :              KZERO,    L,        M,        N,                    V02.02
     :              NDEC,     NRQD                                      V02.02
                                                                        V02.02
      REAL * 4      ANGLE,    FPN,      FPS,      FPV,                  V02.02
     :              HEIGHT,   TEMP,     XPAGE,    YPAGE                 V02.02
                                                                        V02.02
      DIMENSION ICHAR(13),JCHAR(6)
C     PAD ICHAR WITH A BLANK TO ALLOW FOR PACKING AN ODD NUMBER
C     OF CHARACTERS INTO JCHAR FROM ICHAR.
CCC   DATA ICHAR(1)/"40/                                                V01.01
      DATA ICHAR(1)/ 32/                                                V01.01
CCC   DATA KMINUS,KDECPT,KZERO/"55,"56,"60/                             V01.01
      DATA KMINUS,KDECPT,KZERO/ 45, 46, 48/                             V01.01
                                                                        V02.02
      DATA I      / 0 /,                                                V02.02
     :     II     / 0 /,                                                V02.02
     :     III    / 0 /,                                                V02.02
     :     J      / 0 /,                                                V02.02
     :     JCHAR  / 6 * 0 /,                                            V02.02
     :     L      / 0 /,                                                V02.02
     :     M      / 0 /,                                                V02.02
     :     N      / 0 /,                                                V02.02
     :     NRQD   / 0 /                                                 V02.02
                                                                        V02.02
      DATA FPS    / 0.0 /,                                              V02.02
     :     FPV    / 0.0 /,                                              V02.02
     :     TEMP   / 0.0 /                                               V02.02
                                                                        V02.02
C-----------------------------------------------------------------------V02.02
                                                                        V02.02
C     NRQD IS THE MINIMUM NUMBER OF CHARACTERS REQUIRED TO
C     GUARANTEE AT LEAST ONE DIGIT TO THE LEFT OF THE REAL OR
C     IMPLIED DECIMAL POINT.
      NRQD=NDEC+2
      IF (NRQD.LT.1) NRQD=1
C     N IS THE NUMBER OF PLACES TO THE RIGHT OF THE DECIMAL POINT
C     OR MINUS THE NUMBER OF DIGITS TO BE TRUNCATED.
      N=NDEC
      IF (N.LT.0) N=N+1
C     M IS THE VALUE I WILL HAVE JUST BEFORE THE FIRST DIGIT
C     TO THE LEFT OF THE DECIMAL WOULD BE PROCESSED IF IT WERE NOT
C     FOR THE DECIMAL POINT ITSELF.
      M=NDEC+2
      FPV=AINT(ABS(FPN)*10.0**N+0.5)
C     FPV IS NOW A POSITIVE, FLOATING POINT INTEGER, THE DIGITS
C     OF WHICH REPRESENT THE ORIGINAL NUMBER.
C
C-----IF ALL DIGITS ARE ZERO, PREVENT MINUS SIGN
      FPS=FPN
      IF (FPV.EQ.0.0) FPS=0.0
C     ALLOW FOR TEN DIGITS.  NOTE THAT THE LOOP STARTS WITH
C     I.EQ.2 TO ALLOW FOR THE PAD IN ICHAR.
      L=11
C     ALSO ALLOW FIR A DECIMAL, IF REQUIRED.
      IF (NDEC.GE.0) L=L+1
      DO 2 I=2,L
      IF (I.NE.M) GO TO 1
C     AT THIS POINT, M MUST BE .GE.2, SO NDEC.EQ.M-2 MUST BE .GE.0.
C     A DECIMAL POINT IS THEREFORE REQUIRED.  FURTHERMORE,
C     I-2.EQ.M-2.EQ.NDEC DIGITS TO THE RIGHT OF THE DECIMAL HAVE
C     BEEN PROCESSED, SO THIS IS THE PLACE FOR THE DECIMAL
      ICHAR(I)=KDECPT
      GO TO 2
C     GENERATE THE NEXT DIGIT (FROM RIGHT TO LEFT).
    1 TEMP=FPV
      FPV=AINT(FPV/10.0)
      ICHAR(I)=IFIX(TEMP-10.0*FPV)+KZERO
    2 IF (FPV.EQ.0.0.AND.I.GE.NRQD) GO TO 3
C     AT THIS POINT, TEN DIGITS HAVE BEEN PROCESSED AND MORE ARE
C     YET REQUIRED.  PLOT ### IN PLACE OF THE NUMBER.
CJAS  CALL SYMBOL(XPAGE,YPAGE,HEIGHT,'###',ANGLE,3)
      CALL SYMBOL(XPAGE,YPAGE,HEIGHT,4H### ,ANGLE,3)
      RETURN
C     THE DIGITS AND DECIMAL POINT ARE NOW IN ICHAR.  PUT A MINUS
C     SIGN IN ICHAR IF REQUIRED.
    3 IF (FPS.GE.0.0) GO TO 4
      I=I+1
      ICHAR(I)=KMINUS
    4 CONTINUE
C     THE NUMBER IS NOW COMPLETELY ASSEMBLED IN ICHAR IN REVERSE
C     ORDER.  IT IS NOW REVERSED AND PACKED INTO JCHAR, TWO
C     CHARACTERS PER WORD.
      J=1
      DO 5 III=2,I,2
      II=I+2-III
CCC   JCHAR(J)=IOR(ICHAR(II),ISHFT(ICHAR(II-1),8))
      JCHAR(J)=IOR(ICHAR(II-1),ISHFT(ICHAR(II),8))                      JAS
    5 J=J+1
C     THERE ARE NOW I-1 DESIRABLE CHARACTERS IN JCHAR TO BE PLOTTED
      CALL SYMBOL(XPAGE,YPAGE,HEIGHT,JCHAR,ANGLE,I-1)
      RETURN
      END
