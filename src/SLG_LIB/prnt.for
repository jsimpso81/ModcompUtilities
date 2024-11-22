C-----PRNT.FOR                                                          V01.01
C  V01.01  03/26/85  JAS  CHANGES FOR MODCOMP SYSTEM                    V01.01
      SUBROUTINE PRNT(NW)
C*****************************
C     THIS SUBPROGRAM CONTROLS THE PRINTRONIX 300
C*****************************
      DIMENSION IO(48),I6B(8),KREV(64),LOCX(64)
      LOGICAL OPNFLG
      LOGICAL NEWPLT,NEWNAM
      CHARACTER*20 FNAME                                                V01.01
      CHARACTER*20 FBLANK                                               V01.01

      SAVE

      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /PLTCOM/
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16),
     &LDX,LDY,IXT,IYT,NDLTX,NDLTY,MOVY,NCLOCX,
     &LMSK(16),ICH,ICW,IVS,IHS,
     &ISCAN,NSCAN,NWORD,IM(30720)
C*****NOTE: INTEGER*2 IM(NWORD) IS TRUE DIMENSION
C           (AS SPECIFIED IN BLOCK COMMON AND MAIN)
      COMMON /IOCNTL/NEWPLT,NEWNAM,LNAME,NAME(10)
CCC   DATA KPLOT/"5/,ICC/' '/,KONE/'1'/,KBLNK/' '/                      V01.01
CC    DATA KPLOT/ 5/,ICC/' '/,KONE/'1'/,KBLNK/' '/                      V01.02
      DATA KPLOT/ Z'0500'/,ICC/' '/,KONE/'1'/,KBLNK/' '/                V01.02
      DATA FNAME / '                    ' /                             V01.01
      DATA FBLANK / '                    ' /                            V01.01
      DATA OPNFLG/.FALSE./
C-----6-BIT REVERSAL TABLE
C
C     THE RANGE OF THE FOLLOWING NUMBERS IS 32-95
C     IF VALUE OF LOWER SIX BITS IS < 32 THEN 64 WAS ADDED.
C
      DATA KREV/64,96,80,48,72,40,88,56,
     &          68,36,84,52,76,44,92,60,
     &          66,34,82,50,74,42,90,58,
     &          70,38,86,54,78,46,94,62,
     &          65,33,81,49,73,41,89,57,
     &          69,37,85,53,77,45,93,61,
     &          67,35,83,51,75,43,91,59,
     &          71,39,87,55,79,47,95,63/

C-----TEST EJECT REQUEST
      IF (NW.GT.0) GO TO 22
C-----SET EJECT FLAG
CCC   IF (NW.LT.0) ICC=KONE
      IF (NW.LE.0) ICC=KONE
      NEWPLT=.TRUE.
                                                                        V01.01
      OPNFLG=.FALSE.
      GO TO 99
C-----PRINT THE BAND
   22 CONTINUE
      IF (.NOT.NEWPLT) GO TO 60
      NEWPLT=.FALSE.
      IF(.NOT.NEWNAM) GO TO 60
      NEWNAM=.FALSE.
      IF (LNAME.GE.0) GO TO 50
      WRITE (LU2,100)
CC100 FORMAT ('+','OUTPUT FILE NAME--',$)                               V01.01
  100 FORMAT (' ','OUTPUT FILE NAME--'  )                               V01.01
CCC   READ (LU1,200) LNAME,NAME                                         V01.01
      READ (LU1,200) FNAME                                              V01.01
      LNAME = 0                                                         V01.01
      IF ( FNAME .NE. FBLANK ) LNAME = 10                               V01.01
CC200 FORMAT (Q,10A2)                                                   V01.01
  200 FORMAT (A20)                                                      V01.01
CCC50 IF (OPNFLG) CALL CLOSE(LU3)                                       V01.01
   50 IF (OPNFLG) CLOSE(LU3)                                            V01.01
      OPNFLG=.TRUE.
      IF (LNAME.GT.0) GO TO 51
CCC   CALL ASSIGN(LU3,'LP:',3,'NEW','CC',1)                             V01.01
      OPEN (LU3,FILE='LP')                                              V01.01
      GO TO 60
CCC51 CALL ASSIGN(LU3,NAME,LNAME,'NEW','CC',1)                          V01.01
   51 OPEN (LU3,FILE=FNAME)                                             V01.01
   60 CONTINUE
      IK=NW/ISCAN
      KK=0
      DO 27 ILINE=1,IK
      DO 222 I=1,ISCAN
      KK=KK+1
      IO(I)=IM(KK)                                                      ENDDO
222   CONTINUE                                                          ENDDO
      I1=1
      I2=1
   23 CONTINUE
C-----FETCH 3 WORDS (=3*16=48 BITS)
      LOC1=IO(I1  )
      LOC2=IO(I1+1)
      LOC3=IO(I1+2)
C-----CONVERT TO 8 6-BIT CODES (48=8*6 BITS)
CCC   I6B(1)=    IAND(ISHFT(LOC1,-10),"77)                              V01.01
      I6B(1)=    IAND(ISHFT(LOC1,-10), 63)                              V01.01
CCC   I6B(2)=    IAND(ISHFT(LOC1,-04),"77)                              V01.01
      I6B(2)=    IAND(ISHFT(LOC1,-04), 63)                              V01.01
CCC   I6B(3)=IOR(IAND(ISHFT(LOC1,+02),"74),IAND(ISHFT(LOC2,-14),"03))   V01.01
      I6B(3)=IOR(IAND(ISHFT(LOC1,+02), 60),IAND(ISHFT(LOC2,-14), 03))   V01.01
CCC   I6B(4)=    IAND(ISHFT(LOC2,-08),"77)                              V01.01
      I6B(4)=    IAND(ISHFT(LOC2,-08), 63)                              V01.01
CCC   I6B(5)=    IAND(ISHFT(LOC2,-02),"77)                              V01.01
      I6B(5)=    IAND(ISHFT(LOC2,-02), 63)                              V01.01
CCC   I6B(6)=IOR(IAND(ISHFT(LOC2,+04),"60),IAND(ISHFT(LOC3,-12),"17))   V01.01
      I6B(6)=IOR(IAND(ISHFT(LOC2,+04), 48),IAND(ISHFT(LOC3,-12), 15))   V01.01
CCC   I6B(7)=    IAND(ISHFT(LOC3,-06),"77)                              V01.01
      I6B(7)=    IAND(ISHFT(LOC3,-06), 63)                              V01.01
CCC   I6B(8)=    IAND(      LOC3     ,"77)                              V01.01
      I6B(8)=    IAND(      LOC3     , 63)                              V01.01
      DO 24 I=1,7,2
      IF (I2.GT.NCLOCX) GO TO 25
CC    LOCX(I2)=IOR(KREV(I6B(I)+1),ISHFT(KREV(I6B(I+1)+1),8))            JAS
      LOCX(I2)=IOR(KREV(I6B(I+1)+1),ISHFT(KREV(I6B(I)+1),8))            JAS
      I2=I2+1
   24 CONTINUE
      I1=I1+3
      IF (I2.LE.NCLOCX) GO TO 23
   25 CONTINUE
      J=NCLOCX
      DO 26 JJ=1,NCLOCX
CCC   IF (LOCX(J).NE."40100) GO TO 260                                  V01.01
      IF (LOCX(J).NE. 16448) GO TO 260                                  V01.01
      J=J-1                                                             ENDDO
26    CONTINUE                                                          ENDDO
      J=1
260   CONTINUE                                                          JAS
      IF ( ICC .EQ. KONE ) WRITE(LU3,2604) KONE                         JAS
      ICC = KBLNK                                                       JAS
      WRITE (LU3,2603) ICC,KPLOT,(LOCX(I),I=1,J)                        JAS
CCC   ICC=KBLNK                                                         JAS
   27 CONTINUE
 2603 FORMAT (2A1,64A2)                                                 JAS
2604  FORMAT(A1)                                                        JAS
C-----
   99 CONTINUE
      RETURN
      END
