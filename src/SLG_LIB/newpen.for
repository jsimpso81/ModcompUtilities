C-----NEWPEN.FOR
C
C     NEWPEN SPECIFIES THE USE OF ANY OF SEVERAL PENS.  THE CURRENT
C     PEN IS RAISED AND THE SPECIFIED PEN TAKES THE OLD PEN POSITION.
C
C     PARAMETER DESCRIPTIONS:
C
C     INP  AN INDEX SPECIFYING THE PEN TO BE USED.
C
      SUBROUTINE NEWPEN(INP)
      INTEGER*2 ZZZ(1)
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
C-----MASK INP TO 0-7 CODE, TEST FOR CHANGE
      I=IAND(INP-1, 7)
      IF (LWDTH-I) 10,99,10
C-----CHANGE LINE WIDTH AS SPECIFIED
 10   LWDTH=I
      I=IBT(5)+LWDTH
CJAS  CALL POUT(1,I)                                                    JAS
      ZZZ(1) = I
      CALL POUT(1,ZZZ)                                                  JAS
C--------CHANGE WIDTH FOR CHARACTERS TOO!                               JAS
      I=IBT(4)+LWDTH                                                    JAS
CJAS  CALL POUT(1,I)                                                    JAS
      ZZZ(1) = I
      CALL POUT(1,ZZZ)                                                  JAS
 99   RETURN
      END
