C-----SYM1.FOR
C
C     SYM1 IS USED BY SYMBOL TO SAVE AND RESTORE THE CHARACTER
C     VARIABLES AROUND CALLS TO NOTE.
C
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      SUBROUTINE SYM1(XINCH,YINCH,HEIGHT,NASCII,ANGLE,ICHAR)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   ICHAR,    NASCII                                    V02.01
                                                                        V02.01
      REAL * 4      ANGLE,    HEIGHT,   OAA,      OHT,                  V02.01
     :              OWD,      XINCH,    YINCH                           V02.01
                                                                        V02.01
CJAS  DIMENSION NASCII(1)
      DIMENSION NASCII(*)
                                                                        V02.01
      DATA   OAA / 0.0 /,                                               V02.01
     :       OHT / 0.0 /,                                               V02.01
     :       OWD / 0.0 /                                                V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
                                                                        V02.01
C-----SAVE CHARACTER VARIABLES
      CALL MODE(-4,OHT,OWD,OAA)
C-----SET CHARACTER VARIABLES
      CALL MODE(4,HEIGHT,0.625*HEIGHT,ANGLE)
C-----READY TO CALL NOTE
      CALL NOTE(XINCH,YINCH,NASCII,ICHAR)
C-----RESET CHARACTER VARIABLES
      CALL MODE(4,OHT,OWD,OAA)
C-----DONE
      RETURN
      END
