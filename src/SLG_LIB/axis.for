C-----AXIS.FOR
C
C     AXIS    DRAW AXIS LINE
C
C     DRAWS AN AXIS LINE WITH THE APPROPRIATE SCALE ANNOTATION
C     AND TITLE.
C
C     IT CALLS SYMBOL AND NUMBER AS WELL AS PLOT
C
C     AXIS DRAWS ANY LENGTH LINE AT ANY ANGLE, DIVIDES IT INTO ONE
C     INCH INTERVALS, ANNOTATES THE DIVISIONS WITH
C     APPROPRIATE SCALE VALUES AND LABELS THE AXIS WITH A CENTERED
C     TITLE.  WHEN BOTH X AND Y AXES ARE NEEDED, AXIS IS CALLED
C     SEPERATELY FOR EACH ONE.
C
C
C     PARAMETER DESCRIPTIONS:
C
C     XPAGE, ARE THE COORDINATES OF THE STARTING POINT OF THE AXIS
C     YPAGE  LINE.  THE ENTIRE LINE AND TERMINAL ENDS SHOULD BE
C            AT LEAST ONE-HALF INCH FROM ANY SIDE TO ALLOW SPACE
C            FOR THE SCALE ANNOTATION AND AXIS TITLE.  USUALLY,
C            BOTH X AND Y AXES ARE JOINED AT THE ORIGIN OF THE
C            GRAPH, WHERE XPAGE AND YPAGE EQUAL ZERO; BUT OTHER
C            STARTING POINTS CAN BE USED IF DESIRED.  WHEN USING
C            THE LINE SUBROUTINE, HOWEVER, AT LEAST ONE OF THE
C            COORDINATES MUST BE ZERO; I.E., FOR AN X-AXIS, XPAGE=0
C            AND FOR A Y-AXIS, YPAGE=0.
C
C     IBCD   IS THE TITLE, WHICH IS CENTERED AND PLACED PARALLEL
C            TO THE AXIS LINE.  THIS PARAMETER MAY BE AN ALPHAMERIC
C            ARRAY, OR IT MAY BE A HOLLERITH LITERAL IF THE
C            FORTRAN COMPILER BEING USED PERMITS IT.  THE CHARACTERS
C            HAVE A FIXED HEIGHT OF 0.14 IN (ABOUT 7 CHARACTERS/INCH).
C
C     NCHAR  SPECIFIES THE NUMBER OF CHARACTERS IN THE AXIS TITLE,
C            AND DETERMINES, BY ITS SIGN, WHICH SIDE OF THE LINE THE
C            SCALE (TICK) MARKS AND LABELING INFORMATION SHALL
C            BE PLACED.  SINCE THE AXIS LINE MAY BE DRAWN AT ANY ANGLE
C            THE LINE ITSELF IS USED AS A REFERENCE.  IF NCHAR IS
C            POSITIVE, THE COUNTERCLOCKWISE SIDE OF THE AXES
C            (RELATIVE TO XPAGE,YPAGE) IS ANNOTATED, AND CONVERSELY.
C
C     AXLEN  IS THE LENGTH OF THE AXIS LINE, IN INCHES.  IT IS
C            RECOMMENDED THAT THE FRACTIONAL PORTION OF THIS
C            ARGUMENT BE EQUAL TO ZERO.
C
C     ANGLE  IS THE ANGLE, IN POSITIVE OR NEGATIVE DEGREES, AT WHICH
C            AXIS IS TO BE DRAWN.
C            NORMALLY, THIS VALUE IS ZERO FOR THE X-AXIS AND 90.0
C            FOR THE Y-AXIS.
C
C     FIRSTV IS THE STARTING VALUE (EITHER MINIMUM OR MAXIMUM) WHICH
C            WILL APPEAR AT THE FIRST TICK MARK ON THE AXIS.  THIS MAY
C            BE EITHER THE VALUE COMPUTED BY THE SCALE SUBROUTINE,
C            STORED AT SUBSCRIPTED LOCATION ARRAY(NPTS*INC+1), OR
C            A VALUE DETERMINED BY THE USER, STORED ANYWHERE.
C
C            THIS NUMBER AND EACH SCALE VALUE ALONG THE AXIS IS ALWAYS
C            DRAWN WITH TWO DECIMAL PLACES.  SINCE THE DIGIT SIZE IS
C            0.105 INCH (ABOUT 10 CHARACTERS/INCH), AND SINCE A
C            SCALE VALUE APPEARS EVERY INCH, NO MORE THAN SIX DIGITS
C            AND A SIGN SHOULD APPEAR TO THE LEFT OF THE DECIMAL PT.
C
C     DELTAV REPRESENTS THE RATIO OF ROWS 2 DATA POINTS PER INCH OF
C            AXIS. THIS INCREMENT (OR DECREMENT) IS ADDED TO FIRSTV
C            FOR EACH SUCCEEDING ONE-INCH DIVISION ALONG THE
C            AXIS.  THIS MAY BE THE VALUE COMPUTED BY SCALE, STORED
C            BEYOND FIRSTV AT ARRAY(NPTS*INC+INC+1), OR A VALUE
C            DETERMINED BY THE USER, STORED ANYWHERE.
C
C            IN ORDER TO USE A STANDARD FORMAT OF TWO DECIMAL PLACES,
C            THE SIZE OF DELTAV IS ADJUSTED SO IT IS LESS THAN 100, BUT
C            NOT LESS THAN 0.01.  AS A RESULT, THE DECIMAL POINT MAY BE
C            SHIFTED LEFT OR RIGHT IN THE SCALE VALUES AS DRAWN.
C            AND THE AXIS TITLE IS THEN FOLLOWED BY "*10**N", WHERE N
C            IS THE POWER-OF-TEN ADJUSTMENT FACTOR.
C
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
      SUBROUTINE AXIS (XPAGE,YPAGE,IBCD,NCHAR,AXLEN,ANGLE,FIRSTV,DELTAV)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   I,                                                  V02.01
     :              IBCD,                                               V02.01
     :              IEX,                                                V02.01
     :              KEXP,                                               V02.01
     :              KN,                                                 V02.01
     :              LOGADX,                                             V02.01
     :              NCHAR,                                              V02.01
     :              NTIC                                                V02.01
                                                                        V02.01
      INTEGER * 2   JFIX                                                V02.01
                                                                        V02.01
      REAL * 4      A,                                                  V02.01
     :              ADX,                                                V02.01
     :              ANGLE,                                              V02.01
     :              AXLEN,                                              V02.01
     :              CTH,                                                V02.01
     :              DELTAV,                                             V02.01
     :              DXA,                                                V02.01
     :              DXB,                                                V02.01
     :              DYA,                                                V02.01
     :              DYB,                                                V02.01
     :              FIRSTV,                                             V02.01
     :              STH,                                                V02.01
     :              XN,                                                 V02.01
     :              XPAGE,                                              V02.01
     :              XT,                                                 V02.01
     :              XVAL,                                               V02.01
     :              YN,                                                 V02.01
     :              YPAGE,                                              V02.01
     :              YT,                                                 V02.01
     :              Z                                                   V02.01
                                                                        V02.01
      REAL * 4      XROT,                                               V02.01
     :              YROT,                                               V02.01
     :              X,                                                  V02.01
     :              Y                                                   V02.01
                                                                        V02.01
      DIMENSION IBCD(1)
                                                                        V02.01
      DATA   I      / 0 /,                                              V02.01
     :       IEX    / 0 /,                                              V02.01
     :       KEXP   / 0 /,                                              V02.01
     :       KN     / 0 /,                                              V02.01
     :       LOGADX / 0 /,                                              V02.01
     :       NTIC   / 0 /                                               V02.01
                                                                        V02.01
      DATA   A      / 0.0 /,                                            V02.01
     :       ADX    / 0.0 /,                                            V02.01
     :       CTH    / 0.0 /,                                            V02.01
     :       DXA    / 0.0 /,                                            V02.01
     :       DXB    / 0.0 /,                                            V02.01
     :       DYA    / 0.0 /,                                            V02.01
     :       DYB    / 0.0 /,                                            V02.01
     :       STH    / 0.0 /,                                            V02.01
     :       XN     / 0.0 /,                                            V02.01
     :       XT     / 0.0 /,                                            V02.01
     :       XVAL   / 0.0 /,                                            V02.01
     :       YN     / 0.0 /,                                            V02.01
     :       YT     / 0.0 /,                                            V02.01
     :       Z      / 0.0 /                                             V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
C     ARITHMETIC STATEMENT FUNCTIONS TO ROTATE A VECTOR (X,Y) AN
C     ANGLE THETA, DEFINED BY CTH=COS(THETA) AND STH=SIN(THETA)
      XROT (X,Y)=X*CTH-Y*STH
      YROT (X,Y)=Y*CTH+X*STH
C     SEPARATE SIGN AND MAGNITUDE OF NCHAR
      KN=IABS(NCHAR)
      A=ISIGN(1,NCHAR)
C
C     ADJUST FOR FORMAT OF 2 DECIMAL PLACES
C
C     EX IS EXPONENT, DELTAV (ADX) IS ADJUSTED FOR <100
C
C     THE AXIS TITLE IS FOLLOWED BY "10**EX" WHERE EX IS
C     THE POWER-OF-TEN ADJUSTMENT FACTOR
C
      IEX=0
      ADX=ABS(DELTAV)
C     IF ADX.EQ.0, LEAVE IT ALONE
      IF (ADX.EQ.0.0) GO TO 7
      LOGADX=JFIX(ALOG10(ADX))
C     IF ADX.GE.100, REDUCE IT
      IF (LOGADX.LT.2) GO TO 5
      IEX=LOGADX-2
      GO TO 6
C     IF ADX.LT.0.01, INCREASE IT
    5 IF (LOGADX.GE.-2) GO TO 7
      IEX=LOGADX+2
    6 ADX=ADX/10.0**IEX
    7 ADX=SIGN(ADX,DELTAV)
      XVAL=FIRSTV*ADX/DELTAV
      STH=ANGLE*0.017453293
      CTH=COS(STH)
      STH=SIN(STH)
C     START SCALE ONE DIGIT TO THE LEFT OF THE TICK MARK
      DXB=-0.105
C     ALLOW 0.1 INCH OF CLEARANCE
      DYB=0.1525*A-0.0525
C     ROTATE THE DISPLACEMENT VECTOR (DXB,DYB) BY ANGLE THETA
C     AND TRANSLATE IT BY (XPAGE,YPAGE)
      XN=XPAGE+XROT(DXB,DYB)
      YN=YPAGE+YROT(DXB,DYB)
C
C     NUMBER OF TICKS (INCHES)
C
      NTIC=JFIX(AXLEN)+1
C
C     PUT SCALE ON TICK MARKS
C
      DO 10 I=1,NTIC
      CALL NUMBER(XN,YN,0.105,XVAL,ANGLE,2)
C     CALCULATE NEXT SCALE VALUE
      XVAL=XVAL+ADX
C     NEXT SCALE VALUE IS ONE INCH FURTHER ALONG AXIS
C     CALCULATE THE LOCATION
      XN=XN+CTH
   10 YN=YN+STH
C     CALCULATE LENGTH OF ANNOTATION
      KEXP=0
      IF (IEX.EQ.0) GO TO 13
C     ALLOW ROOM FOR POWER OR TEN SCALE FACTOR
      KEXP=KEXP+5
C     ALLOW ROOM FOR SECOND DIGIT OF EXPONENT IF NECESSARY
      IF (IABS(IEX).GE.10) KEXP=KEXP+1
C     ALLOW ROOM FOR MINUS SIGN IF EXPONENT IS NEGATIVE
      IF (IEX.LT.0) KEXP=KEXP+1
   13 Z=0.14*FLOAT(KN+KEXP)
C     CENTER ANNOTATION ALONG AXIS
      DXB=0.5*(AXLEN-Z)
C     ALLOW 0.25 INCH CLEARANCE
      DYB=0.32*A-0.07
C     ROTATE AND TRANSLATE ANNOTATION DISPLACEMENT
C
C     COORDINATES OF LOWER LEFTHAND CORNER OF FIRST CHAR TO
C     BE PRODUCED
C
      XT=XPAGE+XROT(DXB,DYB)
      YT=YPAGE+YROT(DXB,DYB)
C
C     PRODUCE AXIS ANNOTATION, CENTERED
C
      CALL SYMBOL(XT,YT,0.14,IBCD(1),ANGLE,KN)
C
C     IF THE DATA UNITS/INCH WERE SCALED, PRODUCE SCALE FACTOR "*10**EX"
C
      IF (IEX.EQ.0) GO TO 20
      Z=0.14*FLOAT(KN)
      XT=XT+XROT(Z,0.0)
      YT=YT+YROT(Z,0.0)
C
C     PLOT "*10"
C
CJAS  CALL SYMBOL (XT,YT,0.14,'*10',ANGLE,4)
      CALL SYMBOL (XT,YT,0.14,4H*10 ,ANGLE,4)
C
C     PLOT EXPONENT
C
      XT=XT+XROT(0.56,0.07)
      YT=YT+YROT(0.56,0.07)
      CALL NUMBER (XT,YT,0.14,FLOAT(IEX),ANGLE,-1)
   20 CONTINUE
C
C     MOVE PEN TO BEGINNING OF AXIS LINE (PEN IS UP)
C
      CALL PLOT (XPAGE+XROT(AXLEN,0.0),YPAGE+YROT(AXLEN,0.0),3)
      DXA=XROT(-1.0,0.0)
      DYA=YROT(-1.0,0.0)
CCCCCCDXB=XROT(0.0,0.07)
CCCCCCDYB=YROT(0.0,0.07)
      DXB=XROT(0.0,0.07) * A
      DYB=YROT(0.0,0.07) * A
      A=NTIC-1
      XN=XPAGE+XROT(A,0.0)
      YN=YPAGE+YROT(A,0.0)
C     DRAW AXIS LINE WITH TICK MARKS
C
      DO 30 I=1,NTIC
C
C     DRAW AXIS LINE TO TICK MARK (MAY BE VACUOUS)
C
      CALL PLOT(XN,YN,2)
C
C     DRAW TICK MARK
C
      CALL PLOT(XN+DXB,YN+DYB,2)
C
C     RETURN TO AXIS LINE
C
      CALL PLOT (XN,YN,3)
      XN=XN+DXA
      YN=YN+DYA
   30 CONTINUE
      RETURN
      END
