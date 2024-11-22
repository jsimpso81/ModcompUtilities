C-----LG1GRZ.FOR
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
CCC   V01.01  03/28/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
CCC   V01.02  04/23/85  JAS  CHANGED TO GET OUT THE BYTES...
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      FUNCTION LG1GRZ(TEXT,LN)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
CCC   LOGICAL*1 TEXT(1)                                                 V01.01
      INTEGER*2 TEXT(*)                                                 V01.01
      INTEGER*2 WORD,BYTE,STUFF,LN                                      V02.01
      INTEGER*2 LG1GRZ                                                  V02.01
                                                                        V02.01
      DATA WORD   / 0 /,                                                V02.01
     :     BYTE   / 0 /,                                                V02.01
     :     STUFF  / 0 /                                                 V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
                                                                        V02.01
      BYTE = MOD(LN,2)                                                  V01.02
      WORD = (LN+1)/2                                                   V01.02
      IF ( BYTE .EQ. 1 ) THEN                                           V01.02
         STUFF =  ISHFT( TEXT(WORD), -8 )                               V01.02
      ELSE IF ( BYTE .EQ. 0 ) THEN                                      V01.02
         STUFF = IAND( TEXT(WORD), 255 )                                V01.02
      ELSE                                                              V01.02
         PAUSE 'LG1GRZ'                                                 V01.02
      ENDIF                                                             V01.02
      LG1GRZ=STUFF                                                      V01.02
      RETURN
      END
