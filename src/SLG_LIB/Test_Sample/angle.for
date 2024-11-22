C-----ANGLE.FTN
      DIMENSION X(151),Y(151)
      DATA SAME/9999.0/
      DATA PI/3.14159265/
      DATA NE/151/
      CALL MODE(3,7.25,SAME,SAME)
      CALL MODE(7,SAME,7.,SAME)
      DO 21 J=1,NE
        W=J
       AIMAG=(2.5E6 * W)
       REAL =(-1E4*W**2 + 1E10)
       ANGLE=ATAN(AIMAG/REAL)
       X(J)= W
   21   Y(J)= ANGLE*180.0/PI
      CALL SCAN (X,Y,-NE,440)
      CALL DRAW (X,Y,NE,441)
      CALL AXES (9.1,'Frequency',10.2,'Angle (Dg)')
      CALL MODE (6,3.,SAME,SAME)
      CALL MODE (4,.18,.14,SAME)
      CALL DRAW (0.,0.,1,9000)
      CALL DRAW(0.0,0.0,0,9999)
      CALL EXIT
      END
