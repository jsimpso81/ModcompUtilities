C-----BLKD1.FOR --BLOCK DATA-- PASS ONE
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
      BLOCK DATA BLKD1
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /PLTCOM/
     &SAME,PREF(2),LDC(2),C(2),RORG(2),CMAT(10,3),LMT(2),
     &XYDOTS(2),SPX,SPY,MIX,NSKP,NBAD,
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16)
C
C-----LOGICAL UNIT DEFINITION
C
C     DEFINE LUS AS FOLLOWS:
C
C     LU1 - CONSOLE TERMINAL (IN)
C     LU2 - CONSOLE TERMINAL (OUT)
C     LU3 - OUTPUT FILE CONTAINING PLOT
C     LU4 - BINARY FILE PASSED FROM PASS 1 TO PASS 2
C     LU5 - BINARY FILE USED WITHIN PASS 2
C     LU6+ - USER FILES
C
      DATA LU1,LU2,LU3,LU4,LU5/5,7,3,2,4/
      DATA SAME/9999./,PREF/0.,0./,LDC/-9999,-9999/,RORG/.75,.75/
C-----    1     2     3     4     5     6     7     8     9    10
      DATA CMAT /
     &  1.0, 9.25,12.25, .100,   0.,1.000,  7.0,  0.0,  0.0,  -1.,
     &  1.0, -.75, -.75, .067,   0., .100,  9.0,  1.0,  1.0,9999.,
     &  0.0, 0.00, 0.00, .000, 440., .166,  1.1,  0.0,  0.0,   1./
      DATA LMT/32767,792/
      DATA XYDOTS/72.,60./
      DATA SPX/.1/,SPY/.0/,MIX/1/,NSKP/1/,NBAD/0/
      DATA NPLOT/1/,MINX/32767/,MAXX/-9999/
      DATA LTYPE/0/,LWDTH/0/,DEGRAD/.01745329/
      DATA NBITS/16/,NBTM1/15/,NBYTE/2/,NBYM1/1/,NCHAR/128/,NCHM1/127/
      DATA MSK / O'60000',O'14000',O'3000',O'600',O'140',O'30',O'6' /
      DATA IBT / O'1',O'2',O'4',O'10',O'20',O'40',O'100',O'200',
     & O'400',O'1000',O'2000',O'4000',O'10000',O'20000',O'40000',
     & O'100000' /
      END
