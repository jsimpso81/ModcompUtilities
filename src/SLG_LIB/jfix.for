C-----JFIX.FOR
C     THIS ROUTINE PROVIDES THE BRACKET FUNCTION.  JFIX COULD BE
C     IFIX EXCEPT THAT MOST FORTRAN IMPLEMENTATIONS DO NOT
C     PROPERLY CALCULATE THE BRACKET FUNCTION, BUT RATHER
C     MAKE IFIX EQUIVALENT TO INT.  THE DISTINCTION IS FOR
C     NON-INTEGER NEGATIVE ARGUMENTS.  FOR THESE ARGUMENTS,
C     JFIX RETURNS THE ALGEBRAICALLY NEXT SMALLER INTEGER,
C     WHILE INT RETURNS THE ALGEBRAICALLY NEXT LARGER INTEGER.
C-----
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      FUNCTION JFIX(X)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   JFIX                                                V02.01
                                                                        V02.01
      REAL * 4   X                                                      V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
      JFIX=INT(X)
      IF (FLOAT(JFIX).EQ.X) RETURN
      IF (X.LT.0.0) JFIX=JFIX-1
      RETURN
      END
