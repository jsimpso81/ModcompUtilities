C-----NXTA.FOR
      INTEGER FUNCTION NXTA(ITRK)
      PARAMETER ( JBLK = 126 )
      DIMENSION JBUF(JBLK)
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /PLTCOM/
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16),
     &LDX,LDY,IXT,IYT,NDLTX,NDLTY,MOVY,NCLOCX,
     &LMSK(16),ICH,ICW,IVS,IHS,
     &ISCAN,NSCAN,NWORD,IM(1)
C*****NOTE: INTEGER*2 IM(NWORD) IS TRUE DIMENSION
C           (AS SPECIFIED IN BLOCK COMMON AND MAIN)
      IF (ITRK) 100,300,200
  100 REWIND LU5
      INDEX=JBLK
      NXTA=0
      GO TO 999
C-----RETURN NEXT ATOM
  200 INDEX=INDEX+1
      IF (INDEX.LE.JBLK) GO TO 300
      READ (LU5) JBUF
      INDEX=1
C-----RETURN WITH CURRENT ATOM
  300 NXTA=JBUF(INDEX)
  999 RETURN
      END
