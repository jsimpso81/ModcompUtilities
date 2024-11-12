C  SAVE/RESTORE PROCESSOR
C
C WRITTEN BY	BOB SUPNIK
C		DISK ENGINEERING
C		25-AUG-78
C
C  CURRENT LIMITS:
C	750 TRAVEL OPTIONS (TRAVEL, TRVSIZ).
C	300 VOCABULARY WORDS (KTAB, ATAB, TABSIZ).
C	150 LOCATIONS (LTEXT, STEXT, KEY, COND, ABB, ATLOC, LOCSIZ).
C	100 OBJECTS (PLAC, PLACE, FIXD, FIXED, LINK (TWICE), PTEXT, PROP).
C	 35 "ACTION" VERBS (ACTSPK, VRBSIZ).
C	205 RANDOM MESSAGES (RTEXT, RTXSIZ).
C	 12 DIFFERENT PLAYER CLASSIFICATIONS (CTEXT, CVAL, CLSMAX).
C	 20 HINTS, LESS 3 (HINTLC, HINTED, HINTS, HNTSIZ).
C  THERE ARE ALSO LIMITS WHICH CANNOT BE EXCEEDED DUE TO THE STRUCTURE OF
C  THE DATABASE.  (E.G., THE VOCABULARY USES N/1000 TO DETERMINE WORD TYPE,
C  SO THERE CAN'T BE MORE THAN 1000 WORDS.)  THESE UPPER LIMITS ARE:
C	1000 NON-SYNONYMOUS VOCABULARY WORDS
C	300 LOCATIONS
C	100 OBJECTS
C
	SUBROUTINE RSTRGM(F1,F2)
	LOGICAL F1
	INTEGER F2
C	IMPLICIT INTEGER (A-Z)
	LOGICAL LMWARN,CLOSNG,PANIC,HINTED,
     1		CLOSED,GAVEUP,SCORNG,DSEEN
C
	COMMON /VERSN/ VMAJ, VMIN, VEDIT
	INTEGER VMAJ, VMIN, VEDIT
	COMMON /FILES/ INDXNM, TEXTNM, SAVENM
	INTEGER INDXNM(3), TEXTNM(3), SAVENM(3)
	COMMON /TXTCOM/ RTEXT,LINES,ASCVAR,TXTLOC,DATA
	COMMON /VOCCOM/ KTAB,ATAB,TABSIZ
	COMMON /PLACOM/ ATLOC,LINK,PLACE,FIXED,HOLDNG
	COMMON /PTXCOM/ PTEXT
	COMMON /ABBCOM/ ABB
	COMMON /MISCOM/ LINUSE,TRVS,CLSSES,OLDLOC,LOC,CVAL,TK,NEWLOC,
     1	KEY,PLAC,FIXD,ACTSPK,COND,HINTS,HNTMAX,PROP,TALLY,TALLY2,
     2	HINTLC,CHLOC,CHLOC2,DSEEN,DFLAG,DLOC,DALTLC,KEYS,LAMP,GRATE
	COMMON /MISCOM/
     3	CAGE,ROD,ROD2,STEPS,BIRD,DOOR,PILLOW,SNAKE,FISSUR,TABLET,
     4	CLAM,OYSTER,MAGZIN,DWARF,KNIFE,FOOD,BOTTLE,WATER,OIL,PLANT,
     5	PLANT2,AXE,MIRROR,DRAGON,CHASM,TROLL,TROLL2,BEAR,MESSAG,VEND,
     6	BATTER,NUGGET,COINS,CHEST,EGGS,TRIDNT,VASE,EMRALD,PYRAM
	COMMON /MISCOM/
     7	PEARL,RUG,CHAIN,BACK,LOOK,CAVE,NULL,ENTRNC,DPRSSN,SAY,LOCK,
     8	THROW,FIND,INVENT,TURNS,LMWARN,KNFLOC,DETAIL,ABBNUM,
     9	NUMDIE,MAXDIE,DKILL,FOOBAR,BONUS,CLOCK1,CLOCK2,
     1	CLOSNG,PANIC,CLOSED,GAVEUP,SCORNG,ODLOC,STREAM,SPICES
	COMMON /MISC2/ I,RTXSIZ,CLSMAX,LOCSIZ,CTEXT,STEXT,LTEXT,
     1	SECT,TRAVEL,TRVCON,TRVLOC,TRVSIZ,TABNDX,OBJ,J,K,VERB,HNTSIZ,
     2	MAXTRS,HINTED,HNTLOC,KK
	COMMON /MISC3/ATTACK,DTOTAL,OLDLC2,LIMIT,MXSCOR,SCORE,
     1  STICK,WZDARK
C
	INTEGER LINES(12),DATA(78)
C	The TRAVEL, TRVCON, and TRVLOC arrays are
C	Packed with words 0,1,2 holding the data. Saves lots
C	of wasted space at the expense of some complexity.
	INTEGER TRAVEL(250), TRVCON(250), TRVLOC(250), TRVSIZ
	INTEGER KTAB(300),ATAB(300),TABSIZ
	INTEGER LTEXT(150),STEXT(150),KEY(150),COND(150),ABB(150),
     1	ATLOC(150)
	INTEGER PLAC(100),PLACE(100),FIXD(100),FIXED(100),LINK(200),
     1	PTEXT(100),PROP(100),HOLDNG
	INTEGER ACTSPK(35)
	INTEGER RTEXT(205)
	INTEGER CTEXT(12),CVAL(12)
	INTEGER HINTLC(20),HINTS(20,4)
	DIMENSION HINTED(20)
	INTEGER TK(20),DLOC(6),ODLOC(6)
	DIMENSION DSEEN(6)
	INTEGER ASCVAR, TXTLOC, TRVS, CLSSES, OLDLOC
	INTEGER HNTSIZ, HNTMAX, TALLY, TALLY2, CHLOC, CHLOC2, DFLAG
	INTEGER DALTLC,GRATE,CAGE,ROD,ROD2,STEPS,BIRD,DOOR,PILLOW,SNAKE
	INTEGER FISSUR,TABLET,CLAM,OYSTER,MAGZIN,DWARF,KNIFE,FOOD,BOTTLE
	INTEGER WATER,OIL,PLANT,PLANT2,AXE,MIRROR,DRAGON,CHASM,EMRALD
	INTEGER BEAR,MESSAG,VEND,BATTER,COINS,CHEST,EGGS,TRIDNT,VASE
	INTEGER PYRAM,PEARL,RUG,CHAIN,BACK,LOOK,CAVE,NULL,ENTRNC,DPRSSN
	INTEGER LOCK,THROW,FIND,INVENT,TURNS,KNFLOC,DETAIL,ABBNUM,SAY
	INTEGER NUMDIE,MAXDIE,DKILL,FOOBAR,BONUS,CLOCK1,CLOCK2
	INTEGER	TROLL,TROLL2,STREAM,SPICES
	INTEGER RTXSIZ,CLSMAX,LOCSIZ,SECT,TABNDX,OBJ
	INTEGER VERB,HNTLOC,KK
	INTEGER ATTACK,DTOTAL,OLDLC2,LIMIT,MXSCOR,SCORE,STICK,WZDARK
C
C USR call - FORTRAN unit, filename, operation, error flag
C Operation is 2, open input; 3 open output; 4 close output.
C Calling "CLOSE" on an input file causes it to be deleted!
C
	IF (.NOT. F1) GOTO 10
C
C Attempt to restore saved database
C
	CALL USR(7, SAVENM, 2, IERR)
	IF (IERR.NE.0) GOTO 60
	GOTO 20
C
C Attempt to restore initial database
C
10	CALL USR(7, INDXNM, 2, IERR)
	IF (IERR.EQ.0) GOTO 20
	F2=-1
	RETURN
C
20	READ(7) I1,I2,I3
	IF((I1.NE.VMAJ).OR.(I2.NE.VMIN)) GO TO 50
	READ(7) RTEXT,KTAB,ATAB
	READ(7) ATLOC,LINK,PLACE,FIXED,HOLDNG
	READ(7) PTEXT,ABB,LINUSE,TRVS,CLSSES
	READ(7) OLDLOC,LOC,CVAL,NEWLOC,KEY
	READ(7) PLAC,FIXD,ACTSPK,COND,HINTS
	READ(7) HNTMAX,PROP,TALLY,TALLY2,HINTLC
	READ(7) CHLOC,CHLOC2,DSEEN,DFLAG,DLOC,DALTLC
	READ(7) TURNS,LMWARN,KNFLOC,DETAIL,ABBNUM
	READ(7) NUMDIE,MAXDIE,DKILL,FOOBAR,BONUS
	READ(7) CLOCK1,CLOCK2,CLOSNG,PANIC,CLOSED
	READ(7) GAVEUP,SCORNG,ODLOC,CTEXT,STEXT,LTEXT
	READ(7) TRAVEL,TRVCON,TRVLOC,MAXTRS,HINTED,HNTLOC
	READ(7) ATTACK,DTOTAL,OLDLC2,LIMIT,MXSCOR,SCORE,STICK,WZDARK
C
C	CLOSNG = 0
	IF(F1) CALL SIXOUT('R]ESTORED.@',5,0)
	F2=0
	RETURN
50	IF(F1) CALL SIXOUT('F]ILE IS OBSOLETE, [RESTORE] FAILS.',18,0)
	F2=-1
	RETURN
60	CALL SIXOUT('C]AN''T OPEN SAVE FILE, [RESTORE] FAILS.', 22, 0)
	F2=-1
	RETURN
	END