C ---- SOLID.FOR
C
C               PROGRAM:        SOLID.FOR
C               PURPOSE:        TO PLOT SURFACES OF Z=F(X,Y)
C               AUTHOR:         JIM SIMPSON
C               DATE:           DEC 1980
C               EDIT:           V02
C
C
C       GRAPHICAL OUTPUT OF SURFACES
C
C       CALL SOLID(Z,XNUM,YNUM,XLEN,YLEN)
C
C       Z ----- IS A 2 DIMENSIONAL ARRAY CONTAINING THE Z - COORDINATE
C               FOR THE RELATIVE POINT OF ITS SUBSCRIPTS
C
C       XNUM -  THE X DIMENSION OF THE ARRAY
C
C       YNUM -  THE Y DIMENSION OF THE ARRAY
C
C       XLEN
C       YLEN -  A RELATIVE LENGTH FOR EACH AXIS. THE VALUE OF THESE
C               VARIABLES CAN BE 0.0 - 10.0 .  VALUES OF 10. FOR
C               EACH VARIABLE SPECIFY THAT THE LARGEST POSSIBLE
C               GRAPH IS TO BE PRODUCED AND THAT THE X AND Y AXES ARE
C               OF THE SAME LENGTH.
C
        SUBROUTINE SOLID(Z,XNUM,YNUM,XLEN,YLEN)
        INTEGER XNUM,YNUM
        DIMENSION Z(XNUM,YNUM),NPTARA(1),XAREA(5),YAREA(5)
C -- INIT SOME CONSTANTS
        XSTART=5.0
        YSTART=3.6602543
        XCOS= 0.96592583
        YSIN= 0.25881905
        SIDE= 5.17638
        NPTARA(1)=4
        XDIST=XLEN/10.0*SIDE/FLOAT(XNUM)
        YDIST=YLEN/10.0*SIDE/FLOAT(YNUM)
        RMAX=0.0
C -- SET THE PLOT SIZES
        CALL MODE(7,11.,11.,9999.)
        CALL MODE(2,11.,-.25,9999.)
C -- SCALE THE Z COORDINATE
        DO 10 J=1,YNUM
        DO 10 I=1,XNUM
        TRY=ABS(Z(I,J))
        IF ( TRY .GT. RMAX ) RMAX=TRY
10      CONTINUE
        ZDIST=YSTART/RMAX
        IF (ZDIST.GT.1.0) ZDIST=1.0
C -- PLOT THE STUFF
        DO 50 J=YNUM-1,1,-1
        ZY0 = YDIST*FLOAT(J)
        ZY1 = ZY0 - YDIST
        DO 50 I=XNUM-1,1,-1
        ZX0 = XDIST*FLOAT(I)
        ZX1 = ZX0 - XDIST
        XAREA(1)=(ZX0-ZY0)*XCOS+XSTART
        XAREA(2)=(ZX1-ZY0)*XCOS+XSTART
        XAREA(3)=(ZX1-ZY1)*XCOS+XSTART
        XAREA(4)=(ZX0-ZY1)*XCOS+XSTART
        XAREA(5)=XAREA(1)
        YAREA(1)=(ZX0+ZY0)*YSIN+YSTART+ZDIST*Z(I+1,J+1)
        YAREA(2)=(ZX1+ZY0)*YSIN+YSTART+ZDIST*Z(I,J+1)
        YAREA(3)=(ZX1+ZY1)*YSIN+YSTART+ZDIST*Z(I,J)
        YAREA(4)=(ZX0+ZY1)*YSIN+YSTART+ZDIST*Z(I+1,J)
        YAREA(5)=YAREA(1)
        CALL TONE(XAREA,YAREA,NPTARA,-1)
        CALL DRAW(XAREA,YAREA,5,0001)
50      CONTINUE
        RETURN
        END