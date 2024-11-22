C-----PASS2.FOR
C-----PASS2 (MAIN SUBPROGRAM)
      PROGRAM PASS2                                                     V01.01
CCC   V01.01  4/15/85  RTB  CHANGES FOR MODCOMP SYSTEM                  V01.01
      INTEGER*4 LRTN
      DIMENSION NDW(2),NBD(2),SLOPE(2),Y(2)
      DIMENSION NX1(15),NY1(15),NX2(15),NY2(15),MSKD(7),ITP(16)
      DIMENSION XYDOTS(2)
      DIMENSION COSTAB(91),ITRIG(4,4),ATRIG(4,2)
CCC   LOGICAL*1 NODE,NVDB(2)                                            V01.01
      INTEGER*2 NODE                                                    V01.01
      LOGICAL NEWPLT,NEWNAM
      COMMON /PLTCOM/
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16),
     &LDX,LDY,IXT,IYT,NDLTX,NDLTY,MOVY,NCLOCX,
     &LMSK(16),ICH,ICW,IVS,IHS,
     &ISCAN,NSCAN,NWORD,IM(30720)
C*****NOTE: INTEGER*2 IM(NWORD) IS TRUE DIMENSION
C           (AS SPECIFIED IN BLOCK COMMON AND MAIN)
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /CHRCOM/ KHAR(129),NODE(950)
      COMMON /IOCNTL/ NEWPLT,NEWNAM,LNAME,NAME(10)
CCC   EQUIVALENCE (NVDB,NVD)                                            V01.01
CCC   DATA MSKD /"17777,"3777,"777,"177,"37,"7,"1/                      V01.01
      DATA MSKD /  8191, 2047, 511, 127, 31, 7, 1/                      V01.01
      DATA XYDOTS/72.,60./
      DATA ATRIG /+1.0,-1.0,-1.0,+1.0,  +1.0,+1.0,-1.0,-1.0/
      DATA ITRIG /   0, 180,-180, 360,    90, -90, 270,-270,
     &              +1,  -1,  +1,  -1,    -1,  +1,  -1,  +1/
      DATA COS1  /0.9998476951/
      DATA SIN1  /0.0174524064/
      ASSIGN 300 TO LRTN
C-----GENERATE COSINE-TABLE
      CAA=1.0
      SAA=0.0
      DO 102 I=1,90
      COSTAB(I)=CAA
      T=CAA
      CAA=COS1*T-SIN1*SAA
      SAA=SIN1*T+COS1*SAA                                               ENDDO
102   CONTINUE                                                          ENDDO
      COSTAB(91)=0.0
C-----INITIALIZE FILE FROM PASS 1
      K=NXTW(-1)
      CALL PRNT(0)
  100 CONTINUE
D     WRITE(LU2,*) ' PASS2-START OF FRAME/PLOT'
      CALL PUTW(-1,0)
C-----GENERATE WORK FILE
  110 K=NXTW(0)
      IF (K) 9999,150,120
  120 DO 130 I=1,K
      CALL PUTW(0,NXTW(0))                                              ENDDO
130   CONTINUE                                                          ENDDO
      GO TO 110
C-----INITIALIZE PLOT PROCESSING PROCEDURES
  150 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' PASS2-WORK FILE GEN COMPLETE'
      MINX=NXTW(0)                                                      XXXX
      MAXX=NXTW(0)
      CALL PUTW(1,0)
C-----SET FOR FIRST BAND
      IX0=0
C-----INITIALIZE BAND
  200 LTYPE=NXTA(-1)
      LDX=-9999
      LDY=-9999
      IPP=-1
      LWDTH=0
      LWTHC=0
      NTP=1
      ITP(1)=-1
      LMASK=-1
      DO 210 I=1,NBITS
      LMSK(I)=-1                                                        ENDDO
210   CONTINUE                                                          ENDDO
C-----CLEAR THE BAND
      DO 220 I=1,NWORD
      IM(I)=0                                                           ENDDO
220   CONTINUE                                                          ENDDO
C-----SET CHAR CONTROLS TO DEFAULT VALUES
      CAA=1.0
      SAA=0.0
      COA=1.0
      SOA=0.0
      CSA=1.0
      SSA=0.0
      LCH=ICH
      LCW=ICW
      LVS=IVS
      LHS=IHS
      FLCH=FLOAT(LCH)/XYDOTS(2)
      FLCW=FLOAT(LCW)/XYDOTS(1)
      FLVS=FLOAT(LVS)/XYDOTS(2)
      FLHS=FLOAT(LHS)/XYDOTS(1)
      GO TO 3500
C-----FETCH ATOM
  300 CONTINUE                                                          ZZZZ
CC    WRITE(LU2,*) LMSK                                                 ZZZZ
      NVD=NXTA(+1)
      IF (IAND(NVD,IBT(16)).NE.0) GO TO 800
C-----TRY TO DECODE ATOM
      J=13
      DO 340 I=1,7
      IF (IAND(NVD,MSK(I)).GT.0) GO TO 350
      J=J-2
340   CONTINUE                                                          ENDDO
C-----UNABLE TO DECODE ATOM--IGNORE
      WRITE(LU2,12348) NVD                                              XXXX
12348 FORMAT(' PASS2-UNIDENTIFIED ATOM',I7)                             XXXX
      GO TO  300
C-----ATOM IDENTIFIED
CC350 K=IAND(ISHFT(NVD,-J),"3)-2
  350 CONTINUE
      K=IAND(ISHFT(NVD,-J), 3)-2
C     WRITE(LU2,2143) 'I,K,J,NVD=',I,K,J,NVD
C2143 FORMAT(1X,A10,3I6,2X,Z4.4)
      NVD=IAND(NVD,MSKD(I))                                             XXXX
      GO TO  (410,420,430,440,450,460,470), I                           XXXX
C              1    2    3
  410 IF (K) 1000,1300,1500
  420 IF (K) 2000,2500,0300
  430 CONTINUE
      IQUAD=NVD/90+1
      IDEGC=ITRIG(IQUAD,1)+ITRIG(IQUAD,3)*NVD+1
      IDEGS=ITRIG(IQUAD,2)+ITRIG(IQUAD,4)*NVD+1
      COST=COSTAB(IDEGC)*ATRIG(IQUAD,1)
      SINT=COSTAB(IDEGS)*ATRIG(IQUAD,2)
      IF (K) 3000,3100,3200
  440 IF (K) 4000,4100,4700
  450 IF (K.LT.0) GO TO  5000
      GO TO 300
  460 IF (K) 6000,6400,6700
  470 IF (K) 7000,0300,7500
C-----DELX,DELY-----------------------------------0 ATOM(0) =1
CC800 NDY=LDY+IAND(NVD,"177)-63                                         V01.01
  800 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' DELTA X, DELTA Y'
      NDY=LDY+IAND(NVD, 127)-63                                         XXXX01
CCC   NDX=LDX+IAND(ISHFT(NVD,-7),"177)-63                               V01.01
      NDX=LDX+IAND(ISHFT(NVD,-7), 127)-63                               V01.01
      LINE =IAND(NVD,IBT(15))
      GO TO 1700
C-----DELX---------------------------------------1 ATOM(1-2)=01
 1000 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' DELTA X'
      LINE=IAND(NVD,IBT(13))                                            XXXX
      NDX=LDX+NVD-LINE-2047
      GO TO 1700
C-----DELY----------------------------------------1 ATOM(1-2)=10
 1300 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' DELTA Y'
      LINE=IAND(NVD,IBT(13))                                            XXXX
      NDY=LDY+NVD-LINE-2047
      GO TO 1700
C-----X,Y----------------------------------------1 ATOM(1-2)=11
 1500 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' X,Y'
      NDY=NVD                                                           XXXX
      NVD=NXTA(+1)
CCC   NDX=IAND(NVD,"77777)-IX0                                          V01.01
      NDX=IAND(NVD, 32767)-IX0                                          V01.01
      LINE=IAND(ISHFT(NVD,-1),IBT(15))
      GO TO 1700
C-----POINT OR LINE?
 1700 IF (IPP.NE.0) GO TO 1720
C-----PLOT POINT
      LDX=NDX
      LDY=NDY
      GO TO 1730
C-----PLOT LINE
 1720 IF (LINE.EQ.0) GO TO 1750
 1730 CALL PLIN
 1750 LDX=NDX
      LDY=NDY
      GO TO LRTN,(300,4410)
C-----CHAR H AND V SPACING ------------------------2 ATOM(3-4)=01
 2000 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' CHARACTER SPACING'
      LHS=NVD                                                           XXXX
      LVS=NXTA(+1)
      FLHS=FLOAT(LHS)/XYDOTS(1)
      FLVS=FLOAT(LVS)/XYDOTS(2)
      GO TO 3700
C-----CHAR H AND W ----------------------------------2 ATOM(3-4)=10
 2500 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' CHARACTER SIZE'
      LCW=NVD                                                           XXXX
      LCH=NXTA(+1)
      FLCW=FLOAT(LCW)/XYDOTS(1)
      FLCH=FLOAT(LCH)/XYDOTS(2)
      GO TO 3600
C-----SLANT ANGLE ------------------------------3 ATOM(5-6)=01
 3000 CONTINUE
D     WRITE(LU2,*) ' CHARACTER SLANT'
      CSA=COST
      SSA=SINT
      GO TO 3500
C-----ORIENTATION ANGLE --------------------3 ATOM(5-6)=10
 3100 CONTINUE
D     WRITE(LU2,*) ' CHARACTER ORIENTATION'
      COA=COST
      SOA=SINT
      GO TO 3500
C-----ANOTATION ANGLE ------------------------3 ATOM(5-6)=11
 3200 CONTINUE
D     WRITE(LU2,*) ' CHARACTER ANNOTATION'
      CAA=COST
      SAA=SINT
      GO TO 3500
C-----CHAR ROTATIONS
 3500 CONTINUE
      C1=CAA*COA-SAA*SOA
      S1=SAA*COA+CAA*SOA
      C2=C1*CSA-S1*SSA
      S2=S1*CSA-C1*SSA
C-----CHAR VIRTUAL NODES
 3600 CONTINUE
      X1=FLCW*C1*XYDOTS(1)
      Y1=FLCW*S1*XYDOTS(2)
      X2=-FLCH*S2*XYDOTS(1)
      Y2=FLCH*C2*XYDOTS(2)
      RNDX1=0.0
      IF (X1.NE.0.0) RNDX1=SIGN(0.5,X1)
      RNDY1=0.0
      IF (Y1.NE.0.0) RNDY1=SIGN(0.5,Y1)
      RNDX2=0.0
      IF (X2.NE.0.0) RNDX2=SIGN(0.5,X2)
      RNDY2=0.0
      IF (Y2.NE.0.0) RNDY2=SIGN(0.5,Y2)
      T=0.0
      DO 3650 I=1,15
      NX1(I)=T*X1+RNDX1
      NY1(I)=T*Y1+RNDY1
      NX2(I)=T*X2+RNDX2
      NY2(I)=T*Y2+RNDY2
      T=T+0.083333333                                                   ENDDO
3650  CONTINUE                                                          ENDDO
C-----CHAR SPACING
 3700 T1=ABS(COA)*FLHS+ABS(SOA)*FLVS
      T2=ABS(SOA)*FLHS+ABS(COA)*FLVS
      NSPX=T1*CAA*XYDOTS(1)+0.5
      NSPY=T1*SAA*XYDOTS(2)+0.5
      SPT=ABS(CAA)*T1+ABS(SAA)*T2
      NSPT=SPT*XYDOTS(1)+0.5
      GO TO 300
C-----SYMBOL --------------------------------------4 ATOM(7-8)=01
 4000 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' SYMBOL'
      NX0=LDX-(NX1(9)+NX2(9))                                           XXXX
      NY0=LDY-(NY1(9)+NY2(9))
      K=0
      KK=1
      NCHAR=NVD
      GO TO 4200
C-----TEXT ----------------------------------------4 ATOM(7-8)=10
 4100 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' TEXT'
      NX0=LDX-(NX1(3)+NX2(3))                                           XXXX
      NY0=LDY-(NY1(3)+NY2(3))
      K=NVD
      KK=K
      GO TO 4200
C-----APPLY LIMITS FOR SYMBOLS/TEXT
 4200 IXT=NX0
      IYT=NX0+NSPX*KK
      IF (IXT.LE.IYT) GO TO 4220
      I=IXT
      IXT=IYT
      IYT=I
 4220 IXT=IXT-NSPT
      IYT=IYT+NSPT
      IF (IXT.LT.NSCAN.AND.IYT.GE.0) GO TO 4280
      IF (K.LE.0) GO TO  300
      K=(K-1)/NBYTE+1
      DO 4270 I=1,K
      J=NXTA(+1)                                                        ENDDO
4270  CONTINUE                                                          ENDDO
      GO TO 300
C-----PREPARE FOR CHAR/SYMBOL GENERATION
 4280 LXS=LDX
      LYS=LDY
      LWTHS=LWDTH
      LWDTH=LWTHC
      LTYPS=LTYPE
      DO 4290 I=1,NBITS
      LMSK(I)=-1                                                        ENDDO
4290  CONTINUE                                                          ENDDO
      ASSIGN 4410 TO LRTN
      IF (K.LE.0) GO TO 4330
C-----SET WORD EMPTY
      NB=NBYTE+1
C-----MORE BYTES IN THIS WORD?
 4300 IF (NB.LE.NBYTE) GO TO 4320
C-----NO, FETCH NEXT WORD
 4310 NVD=NXTA(+1)
      NB=1
C-----GET NEXT BYTE
                                                                        V01.01
C--------REPLACE THE BYTE INDEXING HERE (LOGICAL*1) WITH CODE TO        V01.01
C--------DUPLICATE ITS EFFECT.MAYBE BACKWARDS!!                         V01.01
C4320 NCHAR=NVDB(NB)                                                    V01.01
4320  CONTINUE                                                          V01.01
      IF ( NB .EQ. 1 ) THEN
         NCHAR = ISHFT( NVD, -8 )                                       V01.01
      ELSE IF ( NB .EQ. 2 ) THEN
         NCHAR = IAND( NVD, 255 )                                       V01.01
      ELSE                                                              V01.01
         WRITE(LU2,12345) NB                                            V01.01
      ENDIF                                                             V01.01
12345 FORMAT(' BAD BYTE INDEX = ',I5)                                   V01.01

      NB=NB+1
C-----NODE COUNT AND INITIAL INDEX
 4330 LINE=0
      LTYPE=0
      IXXXXX=IAND(NCHAR,NCHM1)
      I=KHAR(IXXXXX+1)
      J=KHAR(IXXXXX+2)-I
C-----FETCH NODE
 4360 NW=NODE(I)
CCC   NH=IAND(NW,"17)                                                   V01.01
      NH=IAND(NW, 15)                                                   V01.01
CCC   NW=IAND(ISHFT(NW,-4),"17)                                         V01.01
      NW=IAND(ISHFT(NW,-4), 15)                                         V01.01
C-----LINE CONTROL?
      IF (NW.GT.0) GO TO  4400
C-----LINE CONTROL
      IF (NH.GT.0) GO TO 4390
      LINE=0
      GO TO  4430
 4390 LTYPE=NH-2
      GO TO  4430
C-----MOVE TO NODE
 4400 NDX=NX0+NX1(NW)+NX2(NH)
      NDY=NY0+NY1(NW)+NY2(NH)
      GO TO 1720
 4410 LINE =-1
C-----ADVANCE TO NEXT NODE
 4430 I=I+1
C-----DECREMENT NODE COUNT AND TEST
      J=J-1
      IF (J.GT.0) GO TO 4360
C-----DECREMENT CHAR COUNT AND TEST
      K=K-1
      IF (K.LE.0) GO TO  4500
C-----MOVE TO LOCATION OF NEXT SYMBOL
      NX0=NX0+NSPX
      NY0=NY0+NSPY
C-----GET NEXT SYMBOL
      GO TO  4300
C-----RESTORE AFTER CHAR/TEXT OPERATIONS
 4500 NDX=LXS
      NDY=LYS
      LWDTH=LWTHS
      LTYPE=LTYPS
      ASSIGN 300 TO LRTN
      GO TO 7010
C-----FETCH COUNT AND TONE PATTERN------------------4 ATOM(7-8)=11
C4700 NTP=IAND(NVD,"77)                                                 V01.01
 4700 CONTINUE                                                          XXXX
D     WRITE(LU2,*) ' TONE PATTERN'
      NTP=IAND(NVD, 63)                                                 XXXX01
      IF (NVD.EQ.NTP) GO TO 300
      DO 4750 I=1,NTP
      ITP(I)=NXTA(+1)                                                   ENDDO
4750  CONTINUE                                                          ENDDO
      GO TO 300
C-----DETERMINE TONAL ZONE ------------------5 ATOM(9-10)=01
 5000 CONTINUE
D     WRITE(LU2,*) ' TONAL ZONE'
      DO 5020 I=1,2
      L=NXTA(+1)-MSK(1)
      J=NXTA(+1)
      M=NXTA(+1)-MSK(1)
      K=NXTA(+1)
      SLOPE(I)=0.
      IF (K.NE.J) SLOPE(I)=FLOAT(M-L)/FLOAT(K-J)
      Y(I)=FLOAT(L)-SLOPE(I)*FLOAT(J)+0.5                               ENDDO
5020  CONTINUE                                                          ENDDO
C-----DELIMIT AREA SECTION WITHIN THE BAND
      IF (J.GE.IX0) GO TO 5050
      IF (K.LT.IX0) GO TO 300
      J=IX0
 5050 L=IX0+NSCAN-1
      IF (J.GT.L) GO TO 300
      IF (K.GT.L) K=L
C-----CHECK EACH SCAN LINE
      DO 5180 IXT=J,K
      DO 5120 I=1,2
      IYT=SLOPE(I)*FLOAT(IXT)+Y(I)
      NDW(I)=(IXT-IX0)*ISCAN+IYT/NBITS+1
      NBD(I)=NBITS-MOD(IYT,NBITS)                                       ENDDO
5120  CONTINUE                                                          ENDDO
C-----INITIALIZE FOR EACH SCAN LINE
      IDUMMY=MOD(IXT,NTP)+1
      L=ITP(IDUMMY)
      IDUMMY=NBD(1)
      M=IBT(IDUMMY)-1
      NWD=NDW(1)
      NB=NDW(2)
C-----WITHIN EACH SCAN LINE, EXAMINE AREA WORDS
      DO 5180 I=NWD,NB
      IF (NB.GT.I) GO TO 5150
C-----HERE FOR LAST AREA WORD
      IDUMMY=NBD(2)
      M=M-(IBT(IDUMMY)-1)
C-----TONE OR CLEAR AREA?
 5150 IF (NVD.EQ.0) GO TO 5170
C-----HERE IF TONE
 5160 IM(I)=IOR(IM(I),IAND(L,M))
      GO TO 5178
C-----HERE IF CLEAR ZONE
 5170 IM(I)=IAND(IM(I),-1-M)
5178  CONTINUE                                                          ENDDO
      M=-1                                                              ENDDO
5180  CONTINUE                                                          ENDDO
      GO TO 300
C-----LINE WIDTH FOR CHAR -------------------------6 ATOM(11-12)=01
 6000 CONTINUE
D     WRITE(LU2,*) ' WIDTH FOR CHARACTERS'
      LWTHC=NVD
      GO TO 300
C-----LINE WIDTH FOR PLOTTING -------------------6 ATOM (11-12)=10
 6400 CONTINUE
D     WRITE(LU2,*) ' WIDTH FOR PLOTTING'
      LWDTH=NVD
      GO TO 300
C-----LINE TYPE FOR PLOTTING ---------------------6 ATOM (11-12)=11
C6700 LTYPE=IAND(NVD,"1)-IAND(ISHFT(NVD,-1),"1)                         V01.01
 6700 CONTINUE
D     WRITE(LU2,*) ' LINE TYPE'
      LTYPE=IAND(NVD, 1)-IAND(ISHFT(NVD,-1), 1)                         V01.01
      IPP=NVD-3
      GO TO 300
C-----LINE MASK ---------------------------------7 ATOM(13-14)=01
 7000 CONTINUE
D     WRITE(LU2,*) ' LINE MASK'
      LMASK=NXTA(+1)
C-----PREPARE LINE MASKS
 7010 DO 7050 I=1,NBITS
      LMSK(I)=0
CCC   IF (IAND(ISHFT(LMASK,1-I),"1).NE.0) LMSK(I)=-1
      IF (IAND(ISHFT(LMASK,1-I), 1).NE.0) LMSK(I)=-1
 7050 CONTINUE
      GO TO 1750
C-----OUTPUT FILE NAME----------------------------7 ATOM(13-14)=10
 7100 CONTINUE
D     WRITE(LU2,*) ' OUTPUT FILE NAME'
      LNAME=NXTA(+1)
      DO 7110 I=1,10
      NAME(I)=NXTA(I)                                                   ENDDO
7110  CONTINUE                                                          ENDDO
      NEWNAM=.TRUE.
      GO TO 300
C-----BAND IS COMPLETE ---------------------------7 ATOM(13-14)=11
 7500 CONTINUE
D     WRITE(LU2,*) ' END PLOT/FRAME'
      IF (IX0+NSCAN.GT.MAXX) GO TO 7520
C-----INTERMEDIATE BAND IS FULL
      CALL PRNT(NWORD)
      IX0=IX0+NSCAN
      GO TO 200
C-----FINAL BAND (PARTIAL)
 7520 CALL PRNT((MAXX-IX0+1)*ISCAN)
      IF (NVD.NE.0) CALL PRNT (0)
      GO TO 100
C-----END OF PROCESSING
 9999 CONTINUE
      ENDFILE LU3
      CALL EXIT
      END
