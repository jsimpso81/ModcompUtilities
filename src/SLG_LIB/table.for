C-----TABLE.FOR                                                         V01.01
CCC   V01.01  03/29/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
      SUBROUTINE TABLE
      DIMENSION KDIG(3)
CCC   DATA ICNT/15/,MINOCT/"40/,MAXCHR/78/                              V01.01
      DATA ICNT/15/,MINOCT/ 32/,MAXCHR/78/                              V01.01
      CALL PLOT(0.0,11.0,3)
      CALL PLOT(8.5,11.0,2)
      CALL PLOT(8.5,0.0,2)
      CALL PLOT(0.0,0.0,2)
      CALL PLOT(0.0,11.0,2)
      CALL SYMBOL(0.75,10.5,.14,
     & 38HCHARACTERS AVAILABLE IN SYMBOL ROUTINE ,0.0,38)
      CALL SYMBOL(1.0,10.3,0.10,18HINTEGER EQUIVALENT ,0.0,18)
      CALL SYMBOL(1.2,10.1,0.10,14HLEFT OF SYMBOL ,0.0,14)
      CALL SYMBOL(5.15,10.3,0.10,19HINTERNAL EQUIVALENT ,0.0,19)
      CALL SYMBOL(5.35,10.1,0.10,15HRIGHT OF SYMBOL ,0.0,15)
      CALL PLOT(8.25,10.,3)
      CALL PLOT(0.25,10.,2)
      CALL PLOT(0.25,0.25,2)
      CALL PLOT(8.25,0.25,2)
      CALL PLOT(8.25,10.0,2)
      X=0.5
      K=0
      DO 200 I=1,5
      Y=9.4
      DO 100 J=1,19
      CALL NUMBER(X,Y+0.1,.14,FLOAT(K),0.0,-1)
      IF (K.LT.ICNT) CALL SYMBOL(X+0.55,Y+0.24,0.28,K,0.0,-1)
      IF (K.GE.ICNT) CALL SYMBOL(X+0.45,Y,0.35,K,0.0,-1)
      IF (K.LT.ICNT) CALL SYMBOL(X+1.0,Y+0.1,.14,'-',0.0,1)
      IF (K.LT.ICNT) GO TO 10
      KINT=MINOCT+K-ICNT
      DO 5 L=1,3
      LINT=KINT/8
      KDIG(L)=KINT-8*LINT
    5 KINT=LINT
      KNUM=100*KDIG(3)+10*KDIG(2)+KDIG(1)
      CALL NUMBER(X+0.90,Y+0.1,0.14,FLOAT(KNUM),0.0,-1)
   10 CONTINUE
      K=K+1
      Y=Y-0.5
      IF (K.GT.MAXCHR) GO TO 300
  100 CONTINUE
      CALL PLOT(X+1.35,0.25,3)
      CALL PLOT(X+1.35,10.,2)
  200 X=X+1.6
C 300 CALL PLOT(10.0,0.0,-3)
300   CONTINUE                                                          JAS
      CALL PLOT(10.0,0.0,999)                                           JAS
      CALL PLOT(0.0,0.0,9999)                                           JAS
      RETURN
      END
