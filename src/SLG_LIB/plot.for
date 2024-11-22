C-----PLOT.FOR
C
C     PLOT MOVES THE PEN IN A STRAIGHT LINE TO A NEW POSITION,
C     WITH THE PEN EITHER RAISED (UP) OR LOWERED (DOWN), BY
C     CONVERTING THE ARGUMENTS TO AN APPROPRIATE SEQUENCE OF
C     PLOTTER COMMANDS.
C
C     PARAMETER DESCRIPTIONS:
C
C     X       THE X-COORDINATE OF THE FINAL PEN POSITION, IN
C             INCHES FROM THE CURRENT ORIGIN.
C
C     Y       THE Y-COORDINATE OF THE FINAL PEN POSITION, IN
C             INCHES FROM THE CURRENT ORIGIN.
C
C     IPEN    A SIGNED INTEGER CONTROLLING PEN UP/DOWN STATUS AND
C             ORIGIN DEFINITION.
C
C             A NEGATIVE SIGN (-IPEN) ALWAYS DEFINES A NEW ORIGIN.
C
C             THE MAGNITUDE OF THIS ARGUMENT CONTROLS PEN UP/DOWN.
C
C             IF IABS(IPEN) .EQ. 2, THE PEN IS DOWN DURING MOVEMENT
C             TO ITS NEW POSITION.
C
C             IF IABS(IPEN) .EQ. 3, THE PEN IS UP DUTRING MOVEMENT
C             TO ITS NEW POSITION.
C
C             IF IPEN .EQ. 999, THE CURRENT PLOT IS TERMINATED.
C             THE PEN IS UP.
C
C             IF IPEN .EQ. 9999, ALL PLOTTING IS TERMINATED.
C             THE PEN IS UP.
C
C             ANY OTHER VALUE RAISES THE PEN.
C
CCCCCCCCCCCCCCCCCCCCC  Modification History  CCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
C                                                                       V02.01
C  V02.01  03/08/88  HEB  Declare variables.                            V02.01
C                         Initialize local variables in DATA.           V02.01
C                                                                       V02.01
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCV02.01
                                                                        V02.01
      SUBROUTINE PLOT(X,Y,IPEN)
                                                                        V02.01
      IMPLICIT NONE                                                     V02.01
                                                                        V02.01
      INTEGER * 2   IPEN,     KORG,     LINE                            V02.01
                                                                        V02.01
      REAL * 4      X,        Y                                         V02.01
                                                                        V02.01
      DATA   KORG   / 0 /,                                              V02.01
     :       LINE   / 0 /                                               V02.01
                                                                        V02.01
C-----------------------------------------------------------------------V02.01
      IF (IPEN.NE.9999) GO TO 100
C-----TERMINATE ALL PLOTTING
      CALL DRAW(0,0,0,9999)
      GO TO 9999
C-----SET LINE TO 9 IF LINE IS TO BE DRAWN, 0 OTHERWISE
  100 LINE=0
      IF (IABS(IPEN).EQ.2) LINE=9
C-----SET KORG TO 1000 IF RE-ORIGIN REQUIRED, 0 OTHERWISE
      KORG=0
      IF (IPEN.LT.0) KORG=1000
C-----SET KORG TO 9000 IF END OF PLOT
      IF (IPEN.EQ.999) KORG=9000
C-----DRAW THE LINE
      CALL DRAW(X,Y,1,KORG+LINE)
 9999 RETURN
      END
