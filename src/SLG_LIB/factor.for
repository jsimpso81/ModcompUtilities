C-----FACTOR.FOR
C
C     FACTOR ENABLES THE USER TO REDUCE OR ENLARGE PLOT OUTPUT BY
C     SPECIFYING THE RATIO BETWEEN DESIRED PLOT SIZE AND THE NORMAL
C     PLOT SIZE.  NORMAL PLOT SIZE IS 1.0.  IF THE RATIO SPECIFIED
C     IS 0.5, ALL SUBSEQUENT PEN MOVEMENTS ARE REDUCED BY 1/2.
C     IF THE RATIO SPECIFIED IS 2.0, ALL SUBSEQUENT PEN MOVEMENTS
C     ARE ENLARGED TO TWICE THEIR NORMAL SIZE.
C
C     PARAMETER DESCRIPTIONS:
C
C     FACT   THE RATIO BETWEEN DESIRED PLOT SIZE AND NORMAL PLOT
C             SIZE (I.E., THE EFFECTIVE VALUE OF THE SIZE OF THE
C             PLOT IS THE ABSOLUTE VALUE OF THE ARGUMENT)
C
      SUBROUTINE FACTOR(FACT)
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
C-----MODIFY COMMON FOR NEW DRAWING FACTOR
      T2=CMAT(1,2)/FACT
      DO 12 I=1,2
      PREF(I)=PREF(I)*T2
      RORG(I)=RORG(I)*T2
      M=I+1
      DO 11 J=1,3
   11 CMAT(M,J)=CMAT(M,J)*T2
   12 XYDOTS(I)=XYDOTS(I)/T2
      CMAT(1,2)=FACT
      RETURN
      END
