C-----WHERE.FOR
C
C     WHERE RETURNS THE CURRENT PEN POSITION COORDINATES AND
C     THE CURRENT SIZE FACTOR TO THE THREE VARIABLES DESIGNED
C     IN THE CALLING SEQUENCE
C
C     PARAMETER DESCRIPTIONS:
C
C     KNOW    THE VARIABLE TO BE SET TO THE PEN'S CURRENT
C             X-COORDINATE, THE COORDINATE RESULTING FROM
C             THE PREVIOUS CALL TO PLOT.
C
C     YNOW    THE VARIABLE TO BE SET TO THE PEN'S CURRENT
C             Y-COORDINATE, THE COORDINATE RESULTING FROM
C             THE PREVIOUS CALL TO PLOT.
C
C     DFACT   THE VARIABLE TO BE SET TO THE CURRENT SIZE FACTOR
C             RESULTING FROM A PREVIOUS CALL TO FACTOR OR
C             1.0 IF FACTOR HAS NOT BEEN CALLED
C
      SUBROUTINE WHERE(XNOW,YNOW,DFACT)
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
C-----RETURN PRESENT REFERENCE POSITION AND DRAWING FACTOR
      XNOW=PREF(1)
      YNOW=PREF(2)
      DFACT=CMAT(1,2)
      RETURN
      END
