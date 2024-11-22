      SUBROUTINE INIT
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
      IMPLICIT INTEGER(A-Z)
      CHARACTER*20 FNAME                                                V01.01
      COMMON /XY/ X,Y
      WRITE (7,100)                                                     V01.01
  100 FORMAT (1X,'OUTPUT FILE NAME -- ')                                V01.01
      READ (5,200) FNAME                                                V01.01
  200 FORMAT (A20)                                                      V01.01
      OPEN (6,FILE=FNAME)                                               V01.01
      X=0
      Y=0
      REWIND 2
      RETURN
      END
