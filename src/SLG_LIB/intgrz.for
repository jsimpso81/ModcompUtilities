C-----INTGRZ.FOR
C
C      GET AN INTEGER*2 VALUE OUT OF A REAL*4 ARRAY
C     IXY - REAL*4 ARRAY
C     LN - INDEX INTO EQUIVALENT I*2 ARRAY      
C       
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      FUNCTION INTGRZ(IXY,LN)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   INTGRZ,   IXY,      LN                              V02.01
C
      INTEGER*2 TMPI2(2)
      REAL*4    TMPR4(1)
C                                                                        V02.01
      DIMENSION IXY(*)
C
      EQUIVALENCE (TMPI2, TMPR4 )
C                                                                       V02.01
C-----------------------------------------------------------------------V02.01
      INTGRZ=IXY(LN)
      RETURN
      END
