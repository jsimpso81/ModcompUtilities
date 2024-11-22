C-----PPUT.FOR
C
C      IA - ARRAY OF DATA TO WRITE TO OUTPUT FILE
C      N  - NUMBER OF ITEMS IN IA
C
C
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      SUBROUTINE PPUT(IA,N)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   I,        IA,       IBUFF,    KOUNT,                V02.01
     :              LU1,      LU2,      LU3,      LU4,                  V02.01
     :              LU5,      N,        JBLK                            V02.01
                                                                        V02.01
      PARAMETER ( JBLK=126 )
      DIMENSION IA(*)
      DIMENSION IBUFF(JBLK)
      LOGICAL OPEN
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
C      
      DATA OPEN/.FALSE./
      DATA KOUNT/1/
      DATA IBUFF/JBLK*0/
                                                                        V02.01
      DATA I / 0 /                                                      V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
      IF (OPEN) GO TO 5
      OPEN=.TRUE.
      REWIND LU4
C      
    5 CONTINUE
      IF (N.LE.0) GO TO 40
C
   10 CONTINUE
      DO 20 I=1,N
      IBUFF(KOUNT)=IA(I)
      KOUNT=KOUNT+1
      IF (KOUNT.LE.JBLK) GO TO 20
      WRITE (LU4) IBUFF
      KOUNT=1
C      
   20 CONTINUE
      GO TO 99
C      
   40 CONTINUE
      IF (KOUNT.GT.1) WRITE (LU4) IBUFF
      ENDFILE LU4
      OPEN=.FALSE.
      KOUNT=1
C      
   99 CONTINUE
      RETURN
      END
