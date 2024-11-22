C-----LINE.FOR
C
C     LINE    DRAWS DATA LINE
C
C     PLOTS A SERIES OF SCALED DATA POINTS DEFINED BY TWO
C     ARRAYS (X&Y), CONNECTING THE POINTS WITH STRAIGHT
C     LINES IF DESIRED.  IT MAY CALL SYMBOL AS WELL AS PLOT.
C
C     THE SCALING PARAMETERS CORRESPONDING TO FIRSTV & DELTAV
C     MUST IMMEDIATELY FOLLOW EACH ARRAY.
C     IF THESE PARAMETERS HAVE NOT BEEN COMPUTED BY THE
C     SCALE SUBROUTINE, THEY MUST BE SUPPLIED BY THE USER.
C
C     PARAMETER DESCRIPTIONS
C
C     XARRAY IS THE NAME OF THE ARRAY CONTAINING THE ABCISSA (X)
C            VALUES AND THE SCALING PARAMETERS FOR THE X-ARRAY
C
C     YARRAY IS THE NAME OF THE ARRAY CONTAINING THE ORDINATE (Y)
C            VALUES AND THE SCALING PARAMETERS FOR THE Y-ARRAY.
C
C     NPTS   IS THE NUMBER OF DATA POINTS IN ONE OF THE TWO ARRAYS
C            PREVIOUSLY MENTIONED.  THE COUNT DOES NOT INCLUDE THE
C            EXTRA TWO LOCATIONS FOR THE SCALING PARAMETERS.
C            THE NUMBER OF POINTS IN EACH ARRAY MUST BE THE SAME.
C
C     INC    IS THE INCREMENT THAT THE LINE SUBROUTINE IS TO USE
C            IN GATHERING DATA FROM THE TWO ARRAYS, AS DESCRIBED
C            IN THE SCALE ROUTINE.
C
C     LINTYP IS A CONTROL PARAMETER WHICH DESCRIBES THE TYPE OF LINE
C            TO BE DRAWN THROUGH THE DATA POINTS.  THE MAGNITUDE
C            OF LINTYP DETERMINES THE FREQUENCY OF PLOTTED SYMBOLS;
C            E.G. IF LINTYP=4, A SPECIAL SYMBOL (DENOTED BY
C            INTEQ) IS PLOTTED AT EVERY 4TH DATA POINT.
C
C            IF LINTYP IS ZERO, NO SYMBOLS ARE PLOTTED.
C
C            IF LINTYP IS POSITIVE, A STRAIGHT LINE CONNECTS
C            EVERY DATA POINT DEFINED IN THE ARRAY.  (THE PEN IS
C            UP WHEN MOVING FROM ITS CURRENT POSITION TO THE FIRST POINT).
C
C            IF LINTYP IS NEGATIVE, NO CONNECTING LINES ARE DRAWN;
C            ONLY THE SYMBOLS ARE PLOTTED.
C
C     INTEQ  IS THE INTEGER EQUIVALENT OF THE SPECIAL PLOTTING SYMBOL
C            CENTERED AT EACH DATA POINT.  THIS VALUE NORMALLY CAN
C            BE 0 THROUGH 13, AND HAS MEANING ONLY WHEN LINTYP IS
C            NOT ZERO.  SOME OF THESE SYMBOLS ARE: BOX, OCTAGON, TRIANGLE,
C            PLUS, X, DIAMOND, AND ASTERISK.
C
C     NOTE:  THE LINE SUBROUTINE EXPECTS TO FIND FIRSTX AT
C            XARRAY (NPTS*INC+1) AND DELTAX AT XARRAY(NPTS*INC+INC+1)
C            AND FIRSTY AND DELTAY AT EQUIVALENT LOCATIONS IN XARRAY.
C
      SUBROUTINE LINE (XARRAY,YARRAY, NPTS, INC, LINTYP, INTEQ)
      INTEGER*2 ZZZ(1)
      DIMENSION XARRAY(*),YARRAY(*)
C
C     LMIN IS FIRSTV POSITION IN THE ARRAYS
C
      LMIN=NPTS*INC+1
C
C     LDX IS THE DELTAV POSITION IN THE ARRAYS
C
      LDX=LMIN+INC
C
C     NL IS END OF ARRAY TO PLOT
C
      NL=LMIN-INC
      FIRSTX=XARRAY(LMIN)
      DELTAX=XARRAY(LDX)
      FIRSTY=YARRAY(LMIN)
      DELTAY=YARRAY(LDX)
      IPEN=3
      NT=IABS(LINTYP)
      NF=1
C     SET POINT COUNTER TO PLOT A SYMBOL AT THE FIRST POINT
C     UNLESS NO SYMBOLS ARE TO BE DRAWN
      NA=NT
      IF (LINTYP.EQ.0) NA=1
C     SET PEN UP UNLESS LINE IS TO BE DRAWN
      IPENA=3
      IF (LINTYP.GE.0) IPENA=2
      DO 30 I=1, NPTS
      XN=(XARRAY(NF)-FIRSTX)/DELTAX
      YN=(YARRAY(NF)-FIRSTY)/DELTAY
      IF (NA.NE.NT) GO TO 22
CJAS  CALL SYMBOL (XN,YN,0.08,INTEQ,0.0,IPEN-4)
      ZZZ(1) = INTEQ
      CALL SYMBOL (XN,YN,0.08,ZZZ,0.0,IPEN-4)
      NA=1
      GO TO 25
   22 IF (LINTYP.GE.0) CALL PLOT (XN,YN,IPEN)
   23 NA=NA+1
   25 NF=NF+INC
   30 IPEN=IPENA
      RETURN
      END