C--------I/O
C--------5 INPUT    
C--------6 OUTPUT
C                                                                           1330
C BLOCK DATA - INITIALIZE GLOBAL VARIABLES                                  1340
C                                                                           1350
       BLOCK DATA                                                           1360
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),       1370
     * EXTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK      1380
       INTEGER EXTDIG                                                       1390
       INTEGER INTDIG                                                       1400
       INTEGER EXTLET                                                       1410
       INTEGER INTLET                                                       1420
       INTEGER EXTBIG                                                       1430
       INTEGER INTBIG                                                       1440
       INTEGER EXTCHR                                                       1450
       INTEGER INTCHR                                                       1460
       INTEGER EXTBLK                                                       1470
       INTEGER INTBLK                                                       1480
       COMMON /CDEFIO/ BP, BUF(300)                                         1490
       INTEGER BP                                                           1500
       INTEGER BUF                                                          1510
       COMMON /CFOR/ FORDEP, FORSTK(200)                                    1520
       INTEGER FORDEP                                                       1530
       INTEGER FORSTK                                                       1540
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE    1550
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V    1560
     *UNTIL                                                                 1570
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)     1580
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                 1590
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)     1600
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                 1610
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                           1620
       INTEGER LEVEL                                                        1630
       INTEGER LINECT                                                       1640
       INTEGER INFILE                                                       1650
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)                1660
       INTEGER LASTP                                                        1670
       INTEGER LASTT                                                        1680
       INTEGER NAMPTR                                                       1690
       INTEGER TABLE                                                        1700
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                     1710
       INTEGER OUTP                                                         1720
       INTEGER OUTBUF                                                       1730
       DATA OUTP /0/                                                        1740
       DATA LEVEL /1/                                                       1750
       DATA LINECT(1) /1/                                                   1760
       DATA INFILE(1) /5/                                                   1770
       DATA BP /0/                                                          1780
       DATA FORDEP /0/                                                      1790
       DATA LASTP /0/                                                       1800
       DATA LASTT /0/                                                       1810
       DATA SDO(1), SDO(2), SDO(3) /100, 111, 10002/                        1820
       DATA VDO(1), VDO(2) /10266, 10002/                                   1830
       DATA SIF(1), SIF(2), SIF(3) /105, 102, 10002/                        1840
       DATA VIF(1), VIF(2) /10261, 10002/                                   1850
       DATA SELSE(1), SELSE(2), SELSE(3), SELSE(4), SELSE(5) /101,  108,    1860
     * 115, 101, 10002/                                                     1870
       DATA VELSE(1), VELSE(2) /10262, 10002/                               1880
       DATA SWHILE(1), SWHILE(2), SWHILE(3), SWHILE(4), SWHILE(5), SWHIL    1890
     *E(6) /119, 104, 105, 108, 101, 10002/                                 1900
       DATA VWHILE(1), VWHILE(2) /10263, 10002/                             1910
       DATA SBREAK(1), SBREAK(2), SBREAK(3), SBREAK(4), SBREAK(5), SBREA    1920
     *K(6) /98, 114, 101, 97, 107, 10002/                                   1930
       DATA VBREAK(1), VBREAK(2) /10264, 10002/                             1940
       DATA SNEXT(1), SNEXT(2), SNEXT(3), SNEXT(4), SNEXT(5) /110,  101,    1950
     * 120, 116, 10002/                                                     1960
       DATA VNEXT(1), VNEXT(2) /10265, 10002/                               1970
       DATA SFOR(1), SFOR(2), SFOR(3), SFOR(4) /102,  111, 114, 10002/      1980
       DATA VFOR(1), VFOR(2) /10268, 10002/                                 1990
       DATA SREPT(1), SREPT(2), SREPT(3), SREPT(4), SREPT(5), SREPT(6),     2000
     * SREPT(7) /114, 101, 112, 101, 97, 116, 10002/                        2010
       DATA VREPT(1), VREPT(2) /10269, 10002/                               2020
       DATA SUNTIL(1), SUNTIL(2), SUNTIL(3), SUNTIL(4), SUNTIL(5), SUNTI    2030
     *L(6) /117, 110, 116, 105, 108, 10002/                                 2040
       DATA VUNTIL(1), VUNTIL(2) /10270, 10002/                             2050
       DATA EXTBLK /1H /, INTBLK /32/                                       2060
       DATA EXTDIG(1) /1H0/, INTDIG(1) /48/                                 2070
       DATA EXTDIG(2) /1H1/, INTDIG(2) /49/                                 2080
       DATA EXTDIG(3) /1H2/, INTDIG(3) /50/                                 2090
       DATA EXTDIG(4) /1H3/, INTDIG(4) /51/                                 2100
       DATA EXTDIG(5) /1H4/, INTDIG(5) /52/                                 2110
       DATA EXTDIG(6) /1H5/, INTDIG(6) /53/                                 2120
       DATA EXTDIG(7) /1H6/, INTDIG(7) /54/                                 2130
       DATA EXTDIG(8) /1H7/, INTDIG(8) /55/                                 2140
       DATA EXTDIG(9) /1H8/, INTDIG(9) /56/                                 2150
       DATA EXTDIG(10) /1H9/, INTDIG(10) /57/                               2160
       DATA EXTLET(1) /1HA/, INTLET(1) /97/                                 2170
       DATA EXTLET(2) /1HB/, INTLET(2) /98/                                 2180
       DATA EXTLET(3) /1HC/, INTLET(3) /99/                                 2190
       DATA EXTLET(4) /1HD/, INTLET(4) /100/                                2200
       DATA EXTLET(5) /1HE/, INTLET(5) /101/                                2210
       DATA EXTLET(6) /1HF/, INTLET(6) /102/                                2220
       DATA EXTLET(7) /1HG/, INTLET(7) /103/                                2230
       DATA EXTLET(8) /1HH/, INTLET(8) /104/                                2240
       DATA EXTLET(9) /1HI/, INTLET(9) /105/                                2250
       DATA EXTLET(10) /1HJ/, INTLET(10) /106/                              2260
       DATA EXTLET(11) /1HK/, INTLET(11) /107/                              2270
       DATA EXTLET(12) /1HL/, INTLET(12) /108/                              2280
       DATA EXTLET(13) /1HM/, INTLET(13) /109/                              2290
       DATA EXTLET(14) /1HN/, INTLET(14) /110/                              2300
       DATA EXTLET(15) /1HO/, INTLET(15) /111/                              2310
       DATA EXTLET(16) /1HP/, INTLET(16) /112/                              2320
       DATA EXTLET(17) /1HQ/, INTLET(17) /113/                              2330
       DATA EXTLET(18) /1HR/, INTLET(18) /114/                              2340
       DATA EXTLET(19) /1HS/, INTLET(19) /115/                              2350
       DATA EXTLET(20) /1HT/, INTLET(20) /116/                              2360
       DATA EXTLET(21) /1HU/, INTLET(21) /117/                              2370
       DATA EXTLET(22) /1HV/, INTLET(22) /118/                              2380
       DATA EXTLET(23) /1HW/, INTLET(23) /119/                              2390
       DATA EXTLET(24) /1HX/, INTLET(24) /120/                              2400
       DATA EXTLET(25) /1HY/, INTLET(25) /121/                              2410
       DATA EXTLET(26) /1HZ/, INTLET(26) /122/                              2420
       DATA EXTBIG(1) /1HA/, INTBIG(1) /65/                                 2430
       DATA EXTBIG(2) /1HB/, INTBIG(2) /66/                                 2440
       DATA EXTBIG(3) /1HC/, INTBIG(3) /67/                                 2450
       DATA EXTBIG(4) /1HD/, INTBIG(4) /68/                                 2460
       DATA EXTBIG(5) /1HE/, INTBIG(5) /69/                                 2470
       DATA EXTBIG(6) /1HF/, INTBIG(6) /70/                                 2480
       DATA EXTBIG(7) /1HG/, INTBIG(7) /71/                                 2490
       DATA EXTBIG(8) /1HH/, INTBIG(8) /72/                                 2500
       DATA EXTBIG(9) /1HI/, INTBIG(9) /73/                                 2510
       DATA EXTBIG(10) /1HJ/, INTBIG(10) /74/                               2520
       DATA EXTBIG(11) /1HK/, INTBIG(11) /75/                               2530
       DATA EXTBIG(12) /1HL/, INTBIG(12) /76/                               2540
       DATA EXTBIG(13) /1HM/, INTBIG(13) /77/                               2550
       DATA EXTBIG(14) /1HN/, INTBIG(14) /78/                               2560
       DATA EXTBIG(15) /1HO/, INTBIG(15) /79/                               2570
       DATA EXTBIG(16) /1HP/, INTBIG(16) /80/                               2580
       DATA EXTBIG(17) /1HQ/, INTBIG(17) /81/                               2590
       DATA EXTBIG(18) /1HR/, INTBIG(18) /82/                               2600
       DATA EXTBIG(19) /1HS/, INTBIG(19) /83/                               2610
       DATA EXTBIG(20) /1HT/, INTBIG(20) /84/                               2620
       DATA EXTBIG(21) /1HU/, INTBIG(21) /85/                               2630
       DATA EXTBIG(22) /1HV/, INTBIG(22) /86/                               2640
       DATA EXTBIG(23) /1HW/, INTBIG(23) /87/                               2650
       DATA EXTBIG(24) /1HX/, INTBIG(24) /88/                               2660
       DATA EXTBIG(25) /1HY/, INTBIG(25) /89/                               2670
       DATA EXTBIG(26) /1HZ/, INTBIG(26) /90/                               2680
       DATA EXTCHR(1) /1H!/, INTCHR(1) /33/                                 2690
       DATA EXTCHR(2) /1H"/, INTCHR(2) /34/                                 2700
       DATA EXTCHR(3) /1H#/, INTCHR(3) /35/                                 2710
       DATA EXTCHR(4) /1H$/, INTCHR(4) /36/                                 2720
       DATA EXTCHR(5) /1H%/, INTCHR(5) /37/                                 2730
       DATA EXTCHR(6) /1H&/, INTCHR(6) /38/                                 2740
       DATA EXTCHR(7) /1H'/, INTCHR(7) /39/                                 2750
       DATA EXTCHR(8) /1H(/, INTCHR(8) /40/                                 2760
       DATA EXTCHR(9) /1H)/, INTCHR(9) /41/                                 2770
       DATA EXTCHR(10) /1H*/, INTCHR(10) /42/                               2780
       DATA EXTCHR(11) /1H+/, INTCHR(11) /43/                               2790
       DATA EXTCHR(12) /1H,/, INTCHR(12) /44/                               2800
       DATA EXTCHR(13) /1H-/, INTCHR(13) /45/                               2810
       DATA EXTCHR(14) /1H./, INTCHR(14) /46/                               2820
       DATA EXTCHR(15) /1H//, INTCHR(15) /47/                               2830
       DATA EXTCHR(16) /1H:/, INTCHR(16) /58/                               2840
       DATA EXTCHR(17) /1H;/, INTCHR(17) /59/                               2850
       DATA EXTCHR(18) /1H</, INTCHR(18) /60/                               2860
       DATA EXTCHR(19) /1H=/, INTCHR(19) /61/                               2870
       DATA EXTCHR(20) /1H>/, INTCHR(20) /62/                               2880
       DATA EXTCHR(21) /1H?/, INTCHR(21) /63/                               2890
       DATA EXTCHR(22) /1H@/, INTCHR(22) /64/                               2900
       DATA EXTCHR(23) /1H[/, INTCHR(23) /91/                               2910
       DATA EXTCHR(24) /1H\/, INTCHR(24) /92/                               2920
       DATA EXTCHR(25) /1H]/, INTCHR(25) /93/                               2930
       DATA EXTCHR(26) /1H_/, INTCHR(26) /95/                               2940
       DATA EXTCHR(27) /1H{/, INTCHR(27) /123/                              2950
       DATA EXTCHR(28) /1H|/, INTCHR(28) /124/                              2960
       DATA EXTCHR(29) /1H}/, INTCHR(29) /125/                              2970
       DATA EXTCHR(30) /1H/, INTCHR(30) /8/                                2980
       DATA EXTCHR(31) /1H	/, INTCHR(31) /9/                                2990
       DATA EXTCHR(32) /1H^/, INTCHR(32) /33/                               3000
       DATA EXTCHR(33) /1H~/, INTCHR(33) /33/                               3010
       END                                                                  3020
C                                                                           3030
C RATFOR - MAIN PROGRAM FOR RATFOR                                          3040
C                                                                           3050
       CALL PARSE                                                           3060
       STOP                                                                 3070
       END                                                                  3080
C                                                                           3090
C ALLDIG - RETURN YES IF STR IS ALL DIGITS                                  3100
C                                                                           3110
       INTEGER FUNCTION ALLDIG(STR)                                         3120
       INTEGER TYPE                                                         3130
       INTEGER STR(100)                                                     3140
       INTEGER I                                                            3150
       ALLDIG = 0                                                           3160
       IF(.NOT.(STR(1) .EQ. 10002))  GOTO 23000                             3170
       RETURN                                                               3180
23000  CONTINUE                                                             3190
       CONTINUE                                                             3200
       I = 1                                                                3210
23002  IF(.NOT.( STR(I) .NE. 10002)) GOTO 23004                             3220
       IF(.NOT.(TYPE(STR(I)) .NE. 2))   GOTO 23005                          3230
       RETURN                                                               3240
23005  CONTINUE                                                             3250
23003   I = I + 1                                                           3260
       GOTO 23002                                                           3270
23004  CONTINUE                                                             3280
       ALLDIG = 1                                                           3290
       RETURN                                                               3300
       END                                                                  3310
C                                                                           3320
C BALPAR - COPY BALANCED PAREN STRING                                       3330
C                                                                           3340
       SUBROUTINE BALPAR                                                    3350
       INTEGER GETTOK                                                       3360
       INTEGER T, TOKEN(200)                                                3370
       INTEGER NLPAR                                                        3380
       IF(.NOT.(GETTOK(TOKEN, 200) .NE. 40))  GOTO 23007                    3390
       CALL SYNERR(19HMISSING LEFT PAREN.)                                  3400
       RETURN                                                               3410
23007  CONTINUE                                                             3420
       CALL OUTSTR(TOKEN)                                                   3430
       NLPAR = 1                                                            3440
       CONTINUE                                                             3450
23009  CONTINUE                                                             3460
       T = GETTOK(TOKEN, 200)                                               3470
       IF(.NOT.(T.EQ.59 .OR. T.EQ.123 .OR. T.EQ.125 .OR. T.EQ.10003)) GO    3480
     *TO 23012                                                              3490
       CALL PBSTR(TOKEN)                                                    3500
       GOTO 23011                                                           3510
23012  CONTINUE                                                             3520
       IF(.NOT.(T .EQ. 10)) GOTO 23014                                      3530
       TOKEN(1) = 10002                                                     3540
       GOTO 23015                                                           3550
23014  CONTINUE                                                             3560
       IF(.NOT.(T .EQ. 40)) GOTO 23016                                      3570
       NLPAR = NLPAR + 1                                                    3580
       GOTO 23017                                                           3590
23016  CONTINUE                                                             3600
       IF(.NOT.(T .EQ. 41)) GOTO 23018                                      3610
       NLPAR = NLPAR - 1                                                    3620
23018  CONTINUE                                                             3630
23017  CONTINUE                                                             3640
23015  CONTINUE                                                             3650
       CALL OUTSTR(TOKEN)                                                   3660
23010  IF(.NOT.(NLPAR .LE. 0)) GOTO 23009                                   3670
23011  CONTINUE                                                             3680
       IF(.NOT.(NLPAR .NE. 0)) GOTO 23020                                   3690
       CALL SYNERR(33HMISSING PARENTHESIS IN CONDITION.)                    3700
23020  CONTINUE                                                             3710
       RETURN                                                               3720
       END                                                                  3730
C                                                                           3740
C BRKNXT - GENERATE CODE FOR BREAK AND NEXT                                 3750
C                                                                           3760
       SUBROUTINE BRKNXT(SP, LEXTYP, LABVAL, TOKEN)                         3770
       INTEGER I, LABVAL(100), LEXTYP(100), SP, TOKEN                       3780
       CONTINUE                                                             3790
       I = SP                                                               3800
23022  IF(.NOT.( I .GT. 0)) GOTO 23024                                      3810
       IF(.NOT.(LEXTYP(I) .EQ. 10263 .OR. LEXTYP(I) .EQ. 10266       .OR    3820
     *. LEXTYP(I) .EQ. 10268 .OR. LEXTYP(I) .EQ. 10269))  GOTO 23025        3830
       IF(.NOT.(TOKEN .EQ. 10264))   GOTO 23027                             3840
       CALL OUTGO(LABVAL(I)+1)                                              3850
       GOTO 23028                                                           3860
23027  CONTINUE                                                             3870
       CALL OUTGO(LABVAL(I))                                                3880
23028  CONTINUE                                                             3890
       RETURN                                                               3900
23025  CONTINUE                                                             3910
23023   I = I - 1                                                           3920
       GOTO 23022                                                           3930
23024  CONTINUE                                                             3940
       IF(.NOT.(TOKEN .EQ. 10264))   GOTO 23029                             3950
       CALL SYNERR(14HILLEGAL BREAK.)                                       3960
       GOTO 23030                                                           3970
23029  CONTINUE                                                             3980
       CALL SYNERR(13HILLEGAL NEXT.)                                        3990
23030  CONTINUE                                                             4000
       RETURN                                                               4010
       END                                                                  4020
C                                                                           4030
C CLOSE - EXCEEDINGLY TEMPORARY VERSION FOR GETTOK                          4040
C                                                                           4050
       SUBROUTINE ZCLOSE(FD)                                                 4060
       INTEGER FD                                                           4070
       REWIND FD                                                            4080
       RETURN                                                               4090
       END                                                                  4100
C                                                                           4110
C CTOI - CONVERT STRING AT IN(I) TO INTEGER, INCREMENT I                    4120
C                                                                           4130
       INTEGER FUNCTION CTOI(IN, I)                                         4140
CJAS   INTEGER IN(100)                                                      4150
       INTEGER IN(*)
       INTEGER ZINDEX                                                        4160
       INTEGER D, I                                                         4170
       INTEGER DIGITS(11)                                                   4180
       DATA DIGITS(1) /48/                                                  4190
       DATA DIGITS(2) /49/                                                  4200
       DATA DIGITS(3) /50/                                                  4210
       DATA DIGITS(4) /51/                                                  4220
       DATA DIGITS(5) /52/                                                  4230
       DATA DIGITS(6) /53/                                                  4240
       DATA DIGITS(7) /54/                                                  4250
       DATA DIGITS(8) /55/                                                  4260
       DATA DIGITS(9) /56/                                                  4270
       DATA DIGITS(10) /57/                                                 4280
       DATA DIGITS(11) /10002/                                              4290
       CONTINUE                                                             4300
23031  IF(.NOT.(IN(I) .EQ. 32 .OR. IN(I) .EQ. 9))   GOTO 23032              4310
       I = I + 1                                                            4320
       GOTO 23031                                                           4330
23032  CONTINUE                                                             4340
       CONTINUE                                                             4350
       CTOI = 0                                                             4360
23033  IF(.NOT.( IN(I) .NE. 10002))  GOTO 23035                             4370
       D = ZINDEX(DIGITS, IN(I))                                             4380
       IF(.NOT.(D .EQ. 0))  GOTO 23036                                      4390
       GOTO 23035                                                           4400
23036  CONTINUE                                                             4410
       CTOI = 10 * CTOI + D - 1                                             4420
23034   I = I + 1                                                           4430
       GOTO 23033                                                           4440
23035  CONTINUE                                                             4450
       RETURN                                                               4460
       END                                                                  4470
C                                                                           4480
C DEFTOK - GET TOKEN; PROCESS MACRO CALLS AND INVOCATIONS                   4490
C                                                                           4500
       INTEGER FUNCTION DEFTOK(TOKEN, TOKSIZ, FD)                           4510
       INTEGER GTOK                                                         4520
       INTEGER FD, TOKSIZ                                                   4530
       INTEGER DEFN(200), T, TOKEN(TOKSIZ)                                  4540
       INTEGER LOOKUP                                                       4550
       CONTINUE                                                             4560
       T=GTOK(TOKEN, TOKSIZ, FD)                                            4570
23038  IF(.NOT.( T.NE.10003))  GOTO 23040                                   4580
       IF(.NOT.(T .NE. 10100)) GOTO 23041                                   4590
       GOTO 23040                                                           4600
23041  CONTINUE                                                             4610
       IF(.NOT.(LOOKUP(TOKEN, DEFN) .EQ. 0))  GOTO 23043                    4620
       GOTO 23040                                                           4630
23043  CONTINUE                                                             4640
       IF(.NOT.(DEFN(1) .EQ. 10010)) GOTO 23045                             4650
       CALL GETDEF(TOKEN, TOKSIZ, DEFN, 200, FD)                            4660
       CALL INSTAL(TOKEN, DEFN)                                             4670
       GOTO 23046                                                           4680
23045  CONTINUE                                                             4690
       CALL PBSTR(DEFN)                                                     4700
23046  CONTINUE                                                             4710
23039   T=GTOK(TOKEN, TOKSIZ, FD)                                           4720
       GOTO 23038                                                           4730
23040  CONTINUE                                                             4740
       DEFTOK = T                                                           4750
       IF(.NOT.(DEFTOK .EQ. 10100))  GOTO 23047                             4760
       CALL FOLD(TOKEN)                                                     4770
23047  CONTINUE                                                             4780
       RETURN                                                               4790
       END                                                                  4800
C                                                                           4810
C FOLD - CONVERT ALPHABETIC TOKEN TO SINGLE CASE                            4820
C                                                                           4830
       SUBROUTINE FOLD(TOKEN)                                               4840
       INTEGER TOKEN(100)                                                   4850
       INTEGER I                                                            4860
       CONTINUE                                                             4870
       I = 1                                                                4880
23049  IF(.NOT.( TOKEN(I) .NE. 10002))  GOTO 23051                          4890
       IF(.NOT.(TOKEN(I) .GE. 65 .AND. TOKEN(I) .LE. 90)) GOTO 23052        4900
       TOKEN(I) = TOKEN(I) - 65 + 97                                        4910
23052  CONTINUE                                                             4920
23050   I = I + 1                                                           4930
       GOTO 23049                                                           4940
23051  CONTINUE                                                             4950
       RETURN                                                               4960
       END                                                                  4970
C                                                                           4980
C DOCODE - GENERATE CODE FOR BEGINNING OF DO                                4990
C                                                                           5000
       SUBROUTINE DOCODE(LAB)                                               5010
       INTEGER LABGEN                                                       5020
       INTEGER LAB                                                          5030
       INTEGER DOSTR(4)                                                     5040
       DATA DOSTR(1), DOSTR(2), DOSTR(3), DOSTR(4)/100, 111, 32, 10002/     5050
       CALL OUTTAB                                                          5060
       CALL OUTSTR(DOSTR)                                                   5070
       LAB = LABGEN(2)                                                      5080
       CALL OUTNUM(LAB)                                                     5090
       CALL EATUP                                                           5100
       CALL OUTDON                                                          5110
       RETURN                                                               5120
       END                                                                  5130
C                                                                           5140
C DOSTAT - GENERATE CODE FOR END OF DO STATEMENT                            5150
C                                                                           5160
       SUBROUTINE DOSTAT(LAB)                                               5170
       INTEGER LAB                                                          5180
       CALL OUTCON(LAB)                                                     5190
       CALL OUTCON(LAB+1)                                                   5200
       RETURN                                                               5210
       END                                                                  5220
C                                                                           5230
C EATUP - PROCESS REST OF STATEMENT; INTERPRET CONTINUATIONS                5240
C                                                                           5250
       SUBROUTINE EATUP                                                     5260
       INTEGER GETTOK                                                       5270
       INTEGER PTOKEN(200), T, TOKEN(200)                                   5280
       INTEGER NLPAR                                                        5290
       NLPAR = 0                                                            5300
       CONTINUE                                                             5310
23054  CONTINUE                                                             5320
       T = GETTOK(TOKEN, 200)                                               5330
       IF(.NOT.(T .EQ. 59 .OR. T .EQ. 10)) GOTO 23057                       5340
       GOTO 23056                                                           5350
23057  CONTINUE                                                             5360
       IF(.NOT.(T .EQ. 125))   GOTO 23059                                   5370
       CALL PBSTR(TOKEN)                                                    5380
       GOTO 23056                                                           5390
23059  CONTINUE                                                             5400
       IF(.NOT.(T .EQ. 123 .OR. T .EQ. 10003))   GOTO 23061                 5410
       CALL SYNERR(24HUNEXPECTED BRACE OR EOF.)                             5420
       CALL PBSTR(TOKEN)                                                    5430
       GOTO 23056                                                           5440
23061  CONTINUE                                                             5450
       IF(.NOT.(T .EQ. 44 .OR. T .EQ. 95)) GOTO 23063                       5460
       IF(.NOT.(GETTOK(PTOKEN, 200) .NE. 10)) GOTO 23065                    5470
       CALL PBSTR(PTOKEN)                                                   5480
23065  CONTINUE                                                             5490
       IF(.NOT.(T .EQ. 95)) GOTO 23067                                      5500
       TOKEN(1) = 10002                                                     5510
23067  CONTINUE                                                             5520
       GOTO 23064                                                           5530
23063  CONTINUE                                                             5540
       IF(.NOT.(T .EQ. 40)) GOTO 23069                                      5550
       NLPAR = NLPAR + 1                                                    5560
       GOTO 23070                                                           5570
23069  CONTINUE                                                             5580
       IF(.NOT.(T .EQ. 41)) GOTO 23071                                      5590
       NLPAR = NLPAR - 1                                                    5600
23071  CONTINUE                                                             5610
23070  CONTINUE                                                             5620
23064  CONTINUE                                                             5630
       CALL OUTSTR(TOKEN)                                                   5640
23055  IF(.NOT.(NLPAR .LT. 0)) GOTO 23054                                   5650
23056  CONTINUE                                                             5660
       IF(.NOT.(NLPAR .NE. 0)) GOTO 23073                                   5670
       CALL SYNERR(23HUNBALANCED PARENTHESES.)                              5680
23073  CONTINUE                                                             5690
       RETURN                                                               5700
       END                                                                  5710
C                                                                           5720
C ELSEIF - GENERATE CODE FOR END OF IF BEFORE ELSE                          5730
C                                                                           5740
       SUBROUTINE ELSEIF(LAB)                                               5750
       INTEGER LAB                                                          5760
       CALL OUTGO(LAB+1)                                                    5770
       CALL OUTCON(LAB)                                                     5780
       RETURN                                                               5790
       END                                                                  5800
C                                                                           5810
C EQUAL - COMPARE STR1 TO STR2; RETURN YES IF EQUAL, NO IF NOT              5820
C                                                                           5830
       INTEGER FUNCTION EQUAL(STR1, STR2)                                   5840
CJAS   INTEGER STR1(100), STR2(100)                                         5850
       INTEGER STR1(*), STR2(*)                                             5850
       INTEGER I                                                            5860
       CONTINUE                                                             5870
       I = 1                                                                5880
23075  IF(.NOT.( STR1(I) .EQ. STR2(I))) GOTO 23077                          5890
       IF(.NOT.(STR1(I) .EQ. 10002)) GOTO 23078                             5900
       EQUAL = 1                                                            5910
       RETURN                                                               5920
23078  CONTINUE                                                             5930
23076   I = I + 1                                                           5940
       GOTO 23075                                                           5950
23077  CONTINUE                                                             5960
       EQUAL = 0                                                            5970
       RETURN                                                               5980
       END                                                                  5990
C                                                                           6000
C ERROR - PRINT FATAL ERROR MESSAGE, THEN DIE                               6010
C                                                                           6020
       SUBROUTINE ERROR(BUF)                                                6030
       INTEGER BUF(100)                                                     6040
       CALL REMARK(BUF)                                                     6050
       STOP                                                                 6060
       END                                                                  6070
C                                                                           6080
C FORCOD - BEGINNING OF FOR STATEMENT                                       6090
C                                                                           6100
       SUBROUTINE FORCOD(LAB)                                               6110
       INTEGER GETTOK                                                       6120
       INTEGER T, TOKEN(200)                                                6130
       INTEGER LENGTH, LABGEN                                               6140
       INTEGER I, J, LAB, NLPAR                                             6150
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E    6160
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK        6170
       INTEGER EXTDIG                                                       6180
       INTEGER INTDIG                                                       6190
       INTEGER EXTLET                                                       6200
       INTEGER INTLET                                                       6210
       INTEGER EXTBIG                                                       6220
       INTEGER INTBIG                                                       6230
       INTEGER EXTCHR                                                       6240
       INTEGER INTCHR                                                       6250
       INTEGER EXTBLK                                                       6260
       INTEGER INTBLK                                                       6270
       COMMON /CDEFIO/ BP, BUF(300)                                         6280
       INTEGER BP                                                           6290
       INTEGER BUF                                                          6300
       COMMON /CFOR/ FORDEP, FORSTK(200)                                    6310
       INTEGER FORDEP                                                       6320
       INTEGER FORSTK                                                       6330
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE    6340
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V    6350
     *UNTIL                                                                 6360
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)     6370
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                 6380
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)     6390
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                 6400
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                           6410
       INTEGER LEVEL                                                        6420
       INTEGER LINECT                                                       6430
       INTEGER INFILE                                                       6440
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)                6450
       INTEGER LASTP                                                        6460
       INTEGER LASTT                                                        6470
       INTEGER NAMPTR                                                       6480
       INTEGER TABLE                                                        6490
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                     6500
       INTEGER OUTP                                                         6510
       INTEGER OUTBUF                                                       6520
       INTEGER IFNOT(9)                                                     6530
       DATA IFNOT(1) /105/                                                  6540
       DATA IFNOT(2) /102/                                                  6550
       DATA IFNOT(3) /40/                                                   6560
       DATA IFNOT(4) /46/                                                   6570
       DATA IFNOT(5) /110/                                                  6580
       DATA IFNOT(6) /111/                                                  6590
       DATA IFNOT(7) /116/                                                  6600
       DATA IFNOT(8) /46/                                                   6610
       DATA IFNOT(9) /10002/                                                6620
       LAB = LABGEN(3)                                                      6630
       CALL OUTCON(0)                                                       6640
       IF(.NOT.(GETTOK(TOKEN, 200) .NE. 40))  GOTO 23080                    6650
       CALL SYNERR(19HMISSING LEFT PAREN.)                                  6660
       RETURN                                                               6670
23080  CONTINUE                                                             6680
       IF(.NOT.(GETTOK(TOKEN, 200) .NE. 59))  GOTO 23082                    6690
       CALL PBSTR(TOKEN)                                                    6700
       CALL OUTTAB                                                          6710
       CALL EATUP                                                           6720
       CALL OUTDON                                                          6730
23082  CONTINUE                                                             6740
       IF(.NOT.(GETTOK(TOKEN, 200) .EQ. 59))  GOTO 23084                    6750
       CALL OUTCON(LAB)                                                     6760
       GOTO 23085                                                           6770
23084  CONTINUE                                                             6780
       CALL PBSTR(TOKEN)                                                    6790
       CALL OUTNUM(LAB)                                                     6800
       CALL OUTTAB                                                          6810
       CALL OUTSTR(IFNOT)                                                   6820
       CALL OUTCH(40)                                                       6830
       NLPAR = 0                                                            6840
       CONTINUE                                                             6850
23086  IF(.NOT.(NLPAR .GE. 0)) GOTO 23087                                   6860
       T = GETTOK(TOKEN, 200)                                               6870
       IF(.NOT.(T .EQ. 59)) GOTO 23088                                      6880
       GOTO 23087                                                           6890
23088  CONTINUE                                                             6900
       IF(.NOT.(T .EQ. 40)) GOTO 23090                                      6910
       NLPAR = NLPAR + 1                                                    6920
       GOTO 23091                                                           6930
23090  CONTINUE                                                             6940
       IF(.NOT.(T .EQ. 41)) GOTO 23092                                      6950
       NLPAR = NLPAR - 1                                                    6960
23092  CONTINUE                                                             6970
23091  CONTINUE                                                             6980
       IF(.NOT.(T .NE. 10 .AND. T .NE. 95))   GOTO 23094                    6990
       CALL OUTSTR(TOKEN)                                                   7000
23094  CONTINUE                                                             7010
       GOTO 23086                                                           7020
23087  CONTINUE                                                             7030
       CALL OUTCH(41)                                                       7040
       CALL OUTCH(41)                                                       7050
       CALL OUTGO(LAB+2)                                                    7060
       IF(.NOT.(NLPAR .LT. 0)) GOTO 23096                                   7070
       CALL SYNERR(19HINVALID FOR CLAUSE.)                                  7080
23096  CONTINUE                                                             7090
23085  CONTINUE                                                             7100
       FORDEP = FORDEP + 1                                                  7110
       J = 1                                                                7120
       CONTINUE                                                             7130
       I = 1                                                                7140
23098  IF(.NOT.( I .LT. FORDEP))  GOTO 23100                                7150
       J = J + LENGTH(FORSTK(J)) + 1                                        7160
23099   I = I + 1                                                           7170
       GOTO 23098                                                           7180
23100  CONTINUE                                                             7190
       FORSTK(J) = 10002                                                    7200
       NLPAR = 0                                                            7210
       CONTINUE                                                             7220
23101  IF(.NOT.(NLPAR .GE. 0)) GOTO 23102                                   7230
       T = GETTOK(TOKEN, 200)                                               7240
       IF(.NOT.(T .EQ. 40)) GOTO 23103                                      7250
       NLPAR = NLPAR + 1                                                    7260
       GOTO 23104                                                           7270
23103  CONTINUE                                                             7280
       IF(.NOT.(T .EQ. 41)) GOTO 23105                                      7290
       NLPAR = NLPAR - 1                                                    7300
23105  CONTINUE                                                             7310
23104  CONTINUE                                                             7320
       IF(.NOT.(NLPAR .GE. 0 .AND. T .NE. 10 .AND. T .NE. 95))  GOTO 231    7330
     *07                                                                    7340
       CALL SCOPY(TOKEN, 1, FORSTK, J)                                      7350
       J = J + LENGTH(TOKEN)                                                7360
23107  CONTINUE                                                             7370
       GOTO 23101                                                           7380
23102  CONTINUE                                                             7390
       LAB = LAB + 1                                                        7400
       RETURN                                                               7410
       END                                                                  7420
C                                                                           7430
C FORS - PROCESS END OF FOR STATEMENT                                       7440
C                                                                           7450
       SUBROUTINE FORS(LAB)                                                 7460
       INTEGER LENGTH                                                       7470
       INTEGER I, J, LAB                                                    7480
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E    7490
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK        7500
       INTEGER EXTDIG                                                       7510
       INTEGER INTDIG                                                       7520
       INTEGER EXTLET                                                       7530
       INTEGER INTLET                                                       7540
       INTEGER EXTBIG                                                       7550
       INTEGER INTBIG                                                       7560
       INTEGER EXTCHR                                                       7570
       INTEGER INTCHR                                                       7580
       INTEGER EXTBLK                                                       7590
       INTEGER INTBLK                                                       7600
       COMMON /CDEFIO/ BP, BUF(300)                                         7610
       INTEGER BP                                                           7620
       INTEGER BUF                                                          7630
       COMMON /CFOR/ FORDEP, FORSTK(200)                                    7640
       INTEGER FORDEP                                                       7650
       INTEGER FORSTK                                                       7660
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE    7670
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V    7680
     *UNTIL                                                                 7690
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)     7700
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                 7710
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)     7720
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                 7730
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                           7740
       INTEGER LEVEL                                                        7750
       INTEGER LINECT                                                       7760
       INTEGER INFILE                                                       7770
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)                7780
       INTEGER LASTP                                                        7790
       INTEGER LASTT                                                        7800
       INTEGER NAMPTR                                                       7810
       INTEGER TABLE                                                        7820
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                     7830
       INTEGER OUTP                                                         7840
       INTEGER OUTBUF                                                       7850
       CALL OUTNUM(LAB)                                                     7860
       J = 1                                                                7870
       CONTINUE                                                             7880
       I = 1                                                                7890
23109  IF(.NOT.( I .LT. FORDEP))  GOTO 23111                                7900
       J = J + LENGTH(FORSTK(J)) + 1                                        7910
23110   I = I + 1                                                           7920
       GOTO 23109                                                           7930
23111  CONTINUE                                                             7940
       IF(.NOT.(LENGTH(FORSTK(J)) .GT. 0)) GOTO 23112                       7950
       CALL OUTTAB                                                          7960
       CALL OUTSTR(FORSTK(J))                                               7970
       CALL OUTDON                                                          7980
23112  CONTINUE                                                             7990
       CALL OUTGO(LAB-1)                                                    8000
       CALL OUTCON(LAB+1)                                                   8010
       FORDEP = FORDEP - 1                                                  8020
       RETURN                                                               8030
       END                                                                  8040
C                                                                           8050
C GETCH - GET CHARACTERS FROM FILE                                          8060
C                                                                           8070
       INTEGER FUNCTION GETCH(C, F)                                         8080
       INTEGER INMAP                                                        8090
       INTEGER BUF(81), C                                                   8100
       INTEGER F, I, LASTC                                                  8110
       DATA LASTC /81/, BUF(81) /10/                                        8120
       IF(.NOT.(BUF(LASTC) .EQ. 10 .OR. LASTC .GE. 81))   GOTO 23114        8130
       READ(F, 1, END=10) (BUF(I), I = 1, 80)                               8140
1         FORMAT(80 A1)                                                     8150
       CONTINUE                                                             8160
       I = 1                                                                8170
23116  IF(.NOT.( I .LE. 80))   GOTO 23118                                   8180
       BUF(I) = INMAP(BUF(I))                                               8190
23117   I = I + 1                                                           8200
       GOTO 23116                                                           8210
23118  CONTINUE                                                             8220
       CONTINUE                                                             8230
       I = 80                                                               8240
23119  IF(.NOT.( I .GT. 0)) GOTO 23121                                      8250
       IF(.NOT.(BUF(I) .NE. 32))  GOTO 23122                                8260
       GOTO 23121                                                           8270
23122  CONTINUE                                                             8280
23120   I = I - 1                                                           8290
       GOTO 23119                                                           8300
23121  CONTINUE                                                             8310
       BUF(I+1) = 10                                                        8320
       LASTC = 0                                                            8330
23114  CONTINUE                                                             8340
       LASTC = LASTC + 1                                                    8350
       C = BUF(LASTC)                                                       8360
       GETCH = C                                                            8370
       RETURN                                                               8380
10        C = 10003                                                         8390
       GETCH = 10003                                                        8400
       RETURN                                                               8410
       END                                                                  8420
C                                                                           8430
C GETDEF (FOR NO ARGUMENTS) - GET NAME AND DEFINITION                       8440
C                                                                           8450
       SUBROUTINE GETDEF(TOKEN, TOKSIZ, DEFN, DEFSIZ, FD)                   8460
       INTEGER GTOK, NGETCH                                                 8470
       INTEGER DEFSIZ, FD, I, NLPAR, TOKSIZ                                 8480
       INTEGER C, DEFN(DEFSIZ), TOKEN(TOKSIZ)                               8490
       IF(.NOT.(NGETCH(C, FD) .NE. 40)) GOTO 23124                          8500
       CALL REMARK(19HMISSING LEFT PAREN.)                                  8510
23124  CONTINUE                                                             8520
       IF(.NOT.(GTOK(TOKEN, TOKSIZ, FD) .NE. 10100))   GOTO 23126           8530
       CALL REMARK(22HNON-ALPHANUMERIC NAME.)                               8540
       GOTO 23127                                                           8550
23126  CONTINUE                                                             8560
       IF(.NOT.(NGETCH(C, FD) .NE. 44)) GOTO 23128                          8570
       CALL REMARK(24HMISSING COMMA IN DEFINE.)                             8580
23128  CONTINUE                                                             8590
23127  CONTINUE                                                             8600
       NLPAR = 0                                                            8610
       CONTINUE                                                             8620
       I = 1                                                                8630
23130  IF(.NOT.( NLPAR .GE. 0))   GOTO 23132                                8640
       IF(.NOT.(I .GT. DEFSIZ))   GOTO 23133                                8650
       CALL ERROR(20HDEFINITION TOO LONG.)                                  8660
       GOTO 23134                                                           8670
23133  CONTINUE                                                             8680
       IF(.NOT.(NGETCH(DEFN(I), FD) .EQ. 10003)) GOTO 23135                 8690
       CALL ERROR(20HMISSING RIGHT PAREN.)                                  8700
       GOTO 23136                                                           8710
23135  CONTINUE                                                             8720
       IF(.NOT.(DEFN(I) .EQ. 40)) GOTO 23137                                8730
       NLPAR = NLPAR + 1                                                    8740
       GOTO 23138                                                           8750
23137  CONTINUE                                                             8760
       IF(.NOT.(DEFN(I) .EQ. 41)) GOTO 23139                                8770
       NLPAR = NLPAR - 1                                                    8780
23139  CONTINUE                                                             8790
23138  CONTINUE                                                             8800
23136  CONTINUE                                                             8810
23134  CONTINUE                                                             8820
23131   I = I + 1                                                           8830
       GOTO 23130                                                           8840
23132  CONTINUE                                                             8850
       DEFN(I-1) = 10002                                                    8860
       RETURN                                                               8870
       END                                                                  8880
C                                                                           8890
C GETTOK - GET TOKEN. HANDLES FILE INCLUSION AND LINE NUMBERS               8900
C                                                                           8910
       INTEGER FUNCTION GETTOK(TOKEN, TOKSIZ)                               8920
       INTEGER EQUAL, ZOPEN                                                  8930
       INTEGER JUNK, TOKSIZ                                                 8940
       INTEGER DEFTOK                                                       8950
       INTEGER NAME(30), TOKEN(TOKSIZ)                                      8960
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E    8970
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK        8980
       INTEGER EXTDIG                                                       8990
       INTEGER INTDIG                                                       9000
       INTEGER EXTLET                                                       9010
       INTEGER INTLET                                                       9020
       INTEGER EXTBIG                                                       9030
       INTEGER INTBIG                                                       9040
       INTEGER EXTCHR                                                       9050
       INTEGER INTCHR                                                       9060
       INTEGER EXTBLK                                                       9070
       INTEGER INTBLK                                                       9080
       COMMON /CDEFIO/ BP, BUF(300)                                         9090
       INTEGER BP                                                           9100
       INTEGER BUF                                                          9110
       COMMON /CFOR/ FORDEP, FORSTK(200)                                    9120
       INTEGER FORDEP                                                       9130
       INTEGER FORSTK                                                       9140
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE    9150
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V    9160
     *UNTIL                                                                 9170
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)     9180
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                 9190
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)     9200
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                 9210
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                           9220
       INTEGER LEVEL                                                        9230
       INTEGER LINECT                                                       9240
       INTEGER INFILE                                                       9250
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)                9260
       INTEGER LASTP                                                        9270
       INTEGER LASTT                                                        9280
       INTEGER NAMPTR                                                       9290
       INTEGER TABLE                                                        9300
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                     9310
       INTEGER OUTP                                                         9320
       INTEGER OUTBUF                                                       9330
       INTEGER INCL(8)                                                      9340
       DATA INCL(1) /105/                                                   9350
       DATA INCL(2) /110/                                                   9360
       DATA INCL(3) /99/                                                    9370
       DATA INCL(4) /108/                                                   9380
       DATA INCL(5) /117/                                                   9390
       DATA INCL(6) /100/                                                   9400
       DATA INCL(7) /101/                                                   9410
       DATA INCL(8) /10002/                                                 9420
       CONTINUE                                                             9430
23141  IF(.NOT.( LEVEL .GT. 0))   GOTO 23143                                9440
       CONTINUE                                                             9450
       GETTOK = DEFTOK(TOKEN, TOKSIZ, INFILE(LEVEL))                        9460
23144  IF(.NOT.( GETTOK .NE. 10003)) GOTO 23146                             9470
       IF(.NOT.(EQUAL(TOKEN, INCL) .EQ. 0))   GOTO 23147                    9480
       RETURN                                                               9490
23147  CONTINUE                                                             9500
       JUNK = DEFTOK(NAME, 30, INFILE(LEVEL))                               9510
       IF(.NOT.(LEVEL .GE. 5)) GOTO 23149                                   9520
       CALL SYNERR(27HINCLUDES NESTED TOO DEEPLY.)                          9530
       GOTO 23150                                                           9540
23149  CONTINUE                                                             9550
       INFILE(LEVEL+1) = ZOPEN(NAME, 0)                                      9560
       LINECT(LEVEL+1) = 1                                                  9570
       IF(.NOT.(INFILE(LEVEL+1) .EQ. 10001))  GOTO 23151                    9580
       CALL SYNERR(19HCAN'T OPEN INCLUDE.)                                  9590
       GOTO 23152                                                           9600
23151  CONTINUE                                                             9610
       LEVEL = LEVEL + 1                                                    9620
23152  CONTINUE                                                             9630
23150  CONTINUE                                                             9640
23145           GETTOK = DEFTOK(TOKEN, TOKSIZ, INFILE(LEVEL))               9650
       GOTO 23144                                                           9660
23146  CONTINUE                                                             9670
       IF(.NOT.(LEVEL .GT. 1)) GOTO 23153                                   9680
       CALL ZCLOSE(INFILE(LEVEL))                                            9690
23153  CONTINUE                                                             9700
23142   LEVEL = LEVEL - 1                                                   9710
       GOTO 23141                                                           9720
23143  CONTINUE                                                             9730
       GETTOK = 10003                                                       9740
       RETURN                                                               9750
       END                                                                  9760
C                                                                           9770
C GTOK - GET TOKEN FOR RATFOR                                               9780
C                                                                           9790
       INTEGER FUNCTION GTOK(LEXSTR, TOKSIZ, FD)                            9800
       INTEGER NGETCH, TYPE                                                 9810
       INTEGER FD, I, TOKSIZ                                                9820
       INTEGER C, LEXSTR(TOKSIZ)                                            9830
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E    9840
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK        9850
       INTEGER EXTDIG                                                       9860
       INTEGER INTDIG                                                       9870
       INTEGER EXTLET                                                       9880
       INTEGER INTLET                                                       9890
       INTEGER EXTBIG                                                       9900
       INTEGER INTBIG                                                       9910
       INTEGER EXTCHR                                                       9920
       INTEGER INTCHR                                                       9930
       INTEGER EXTBLK                                                       9940
       INTEGER INTBLK                                                       9950
       COMMON /CDEFIO/ BP, BUF(300)                                         9960
       INTEGER BP                                                           9970
       INTEGER BUF                                                          9980
       COMMON /CFOR/ FORDEP, FORSTK(200)                                    9990
       INTEGER FORDEP                                                      10000
       INTEGER FORSTK                                                      10010
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   10020
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   10030
     *UNTIL                                                                10040
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    10050
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                10060
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    10070
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                10080
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          10090
       INTEGER LEVEL                                                       10100
       INTEGER LINECT                                                      10110
       INTEGER INFILE                                                      10120
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               10130
       INTEGER LASTP                                                       10140
       INTEGER LASTT                                                       10150
       INTEGER NAMPTR                                                      10160
       INTEGER TABLE                                                       10170
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    10180
       INTEGER OUTP                                                        10190
       INTEGER OUTBUF                                                      10200
       CONTINUE                                                            10210
23155  IF(.NOT.(NGETCH(C, FD) .NE. 10003)) GOTO 23156                      10220
       IF(.NOT.(C .NE. 32 .AND. C .NE. 9)) GOTO 23157                      10230
       GOTO 23156                                                          10240
23157  CONTINUE                                                            10250
       GOTO 23155                                                          10260
23156  CONTINUE                                                            10270
       CALL PUTBAK(C)                                                      10280
       CONTINUE                                                            10290
       I = 1                                                               10300
23159  IF(.NOT.( I .LT. TOKSIZ-1))   GOTO 23161                            10310
       GTOK = TYPE(NGETCH(LEXSTR(I), FD))                                  10320
       IF(.NOT.(GTOK .NE. 1 .AND. GTOK .NE. 2))  GOTO 23162                10330
       GOTO 23161                                                          10340
23162  CONTINUE                                                            10350
23160   I = I + 1                                                          10360
       GOTO 23159                                                          10370
23161  CONTINUE                                                            10380
       IF(.NOT.(I .GE. TOKSIZ-1)) GOTO 23164                               10390
       CALL SYNERR(15HTOKEN TOO LONG.)                                     10400
23164  CONTINUE                                                            10410
       IF(.NOT.(I .GT. 1))  GOTO 23166                                     10420
       CALL PUTBAK(LEXSTR(I))                                              10430
       LEXSTR(I) = 10002                                                   10440
       GTOK = 10100                                                        10450
       GOTO 23167                                                          10460
23166  CONTINUE                                                            10470
       IF(.NOT.(LEXSTR(1) .EQ. 36))  GOTO 23168                            10480
       IF(.NOT.(NGETCH(LEXSTR(2), FD) .EQ. 40))  GOTO 23170                10490
       LEXSTR(1) = 123                                                     10500
       GTOK = 123                                                          10510
       GOTO 23171                                                          10520
23170  CONTINUE                                                            10530
       IF(.NOT.(LEXSTR(2) .EQ. 41))  GOTO 23172                            10540
       LEXSTR(1) = 125                                                     10550
       GTOK = 125                                                          10560
       GOTO 23173                                                          10570
23172  CONTINUE                                                            10580
       CALL PUTBAK(LEXSTR(2))                                              10590
23173  CONTINUE                                                            10600
23171  CONTINUE                                                            10610
       GOTO 23169                                                          10620
23168  CONTINUE                                                            10630
       IF(.NOT.(LEXSTR(1) .EQ. 39 .OR. LEXSTR(1) .EQ. 34))   GOTO 23174    10640
       CONTINUE                                                            10650
       I = 2                                                               10660
23176  IF(.NOT.( NGETCH(LEXSTR(I), FD) .NE. LEXSTR(1)))   GOTO 23178       10670
       IF(.NOT.(LEXSTR(I) .EQ. 10 .OR. I .GE. TOKSIZ-1))  GOTO 23179       10680
       CALL SYNERR(14HMISSING QUOTE.)                                      10690
       LEXSTR(I) = LEXSTR(1)                                               10700
       CALL PUTBAK(10)                                                     10710
       GOTO 23178                                                          10720
23179  CONTINUE                                                            10730
23177   I = I + 1                                                          10740
       GOTO 23176                                                          10750
23178  CONTINUE                                                            10760
       GOTO 23175                                                          10770
23174  CONTINUE                                                            10780
       IF(.NOT.(LEXSTR(1) .EQ. 35))  GOTO 23181                            10790
       CONTINUE                                                            10800
23183  IF(.NOT.(NGETCH(LEXSTR(1), FD) .NE. 10))  GOTO 23184                10810
       GOTO 23183                                                          10820
23184  CONTINUE                                                            10830
       GTOK = 10                                                           10840
       GOTO 23182                                                          10850
23181  CONTINUE                                                            10860
       IF(.NOT.(LEXSTR(1) .EQ. 62 .OR. LEXSTR(1) .EQ. 60 .OR. LEXSTR(1)    10870
     *.EQ. 33      .OR. LEXSTR(1) .EQ. 61 .OR. LEXSTR(1) .EQ. 38 .OR. LE   10880
     *XSTR(1) .EQ. 124)) GOTO 23185                                        10890
       CALL RELATE(LEXSTR, I, FD)                                          10900
23185  CONTINUE                                                            10910
23182  CONTINUE                                                            10920
23175  CONTINUE                                                            10930
23169  CONTINUE                                                            10940
23167  CONTINUE                                                            10950
       LEXSTR(I+1) = 10002                                                 10960
       IF(.NOT.(LEXSTR(1) .EQ. 10))  GOTO 23187                            10970
       LINECT(LEVEL) = LINECT(LEVEL) + 1                                   10980
23187  CONTINUE                                                            10990
       RETURN                                                              11000
       END                                                                 11010
C                                                                          11020
C IFCODE - GENERATE INITIAL CODE FOR IF                                    11030
C                                                                          11040
       SUBROUTINE IFCODE(LAB)                                              11050
       INTEGER LABGEN                                                      11060
       INTEGER LAB                                                         11070
       LAB = LABGEN(2)                                                     11080
       CALL IFGO(LAB)                                                      11090
       RETURN                                                              11100
       END                                                                 11110
C                                                                          11120
C IFGO - GENERATE "IF(.NOT.(...))GOTO LAB"                                 11130
C                                                                          11140
       SUBROUTINE IFGO(LAB)                                                11150
       INTEGER LAB                                                         11160
       INTEGER IFNOT(9)                                                    11170
       DATA IFNOT(1) /105/                                                 11180
       DATA IFNOT(2) /102/                                                 11190
       DATA IFNOT(3) /40/                                                  11200
       DATA IFNOT(4) /46/                                                  11210
       DATA IFNOT(5) /110/                                                 11220
       DATA IFNOT(6) /111/                                                 11230
       DATA IFNOT(7) /116/                                                 11240
       DATA IFNOT(8) /46/                                                  11250
       DATA IFNOT(9) /10002/                                               11260
       CALL OUTTAB                                                         11270
       CALL OUTSTR(IFNOT)                                                  11280
       CALL BALPAR                                                         11290
       CALL OUTCH(41)                                                      11300
       CALL OUTGO(LAB)                                                     11310
       RETURN                                                              11320
       END                                                                 11330
C                                                                          11340
C INDEX - FIND CHARACTER  C  IN STRING  STR                                11350
C                                                                          11360
       INTEGER FUNCTION ZINDEX(STR, C)                                      11370
CJAS   INTEGER C, STR(100)                                                 11380
       INTEGER C, STR(*)                                                   11380
       CONTINUE                                                            11390
       ZINDEX = 1                                                           11400
23189  IF(.NOT.( STR(ZINDEX) .NE. 10002))   GOTO 23191                      11410
       IF(.NOT.(STR(ZINDEX) .EQ. C))  GOTO 23192                            11420
       RETURN                                                              11430
23192  CONTINUE                                                            11440
23190  ZINDEX = ZINDEX + 1                                                  11450
       GOTO 23189                                                          11460
23191  CONTINUE                                                            11470
       ZINDEX = 0                                                           11480
       RETURN                                                              11490
       END                                                                 11500
C                                                                          11510
C INITKW - INSTALL KEYWORD "DEFINE" IN TABLE                               11520
C                                                                          11530
       SUBROUTINE INITKW                                                   11540
       INTEGER DEFNAM(7), DEFTYP(2)                                        11550
       DATA DEFNAM(1) /100/, DEFNAM(2) /101/, DEFNAM(3) /102/              11560
       DATA DEFNAM(4) /105/, DEFNAM(5) /110/, DEFNAM(6) /101/              11570
       DATA DEFNAM(7) /10002/                                              11580
       DATA DEFTYP(1), DEFTYP(2) /10010, 10002/                            11590
       CALL INSTAL(DEFNAM, DEFTYP)                                         11600
       RETURN                                                              11610
       END                                                                 11620
C                                                                          11630
C INMAP - CONVERT LEFT ADJUSTED EXTERNAL REP TO RIGHT ADJ ASCII            11640
C                                                                          11650
       INTEGER FUNCTION INMAP(INCHAR)                                      11660
       INTEGER I, INCHAR                                                   11670
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   11680
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       11690
       INTEGER EXTDIG                                                      11700
       INTEGER INTDIG                                                      11710
       INTEGER EXTLET                                                      11720
       INTEGER INTLET                                                      11730
       INTEGER EXTBIG                                                      11740
       INTEGER INTBIG                                                      11750
       INTEGER EXTCHR                                                      11760
       INTEGER INTCHR                                                      11770
       INTEGER EXTBLK                                                      11780
       INTEGER INTBLK                                                      11790
       COMMON /CDEFIO/ BP, BUF(300)                                        11800
       INTEGER BP                                                          11810
       INTEGER BUF                                                         11820
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   11830
       INTEGER FORDEP                                                      11840
       INTEGER FORSTK                                                      11850
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   11860
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   11870
     *UNTIL                                                                11880
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    11890
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                11900
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    11910
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                11920
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          11930
       INTEGER LEVEL                                                       11940
       INTEGER LINECT                                                      11950
       INTEGER INFILE                                                      11960
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               11970
       INTEGER LASTP                                                       11980
       INTEGER LASTT                                                       11990
       INTEGER NAMPTR                                                      12000
       INTEGER TABLE                                                       12010
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    12020
       INTEGER OUTP                                                        12030
       INTEGER OUTBUF                                                      12040
       IF(.NOT.(INCHAR .EQ. EXTBLK)) GOTO 23194                            12050
       INMAP = INTBLK                                                      12060
       RETURN                                                              12070
23194  CONTINUE                                                            12080
       DO23196I = 1, 10                                                    12090
       IF(.NOT.(INCHAR .EQ. EXTDIG(I))) GOTO 23198                         12100
       INMAP = INTDIG(I)                                                   12110
       RETURN                                                              12120
23198  CONTINUE                                                            12130
23196  CONTINUE                                                            12140
23197  CONTINUE                                                            12150
       DO23200I = 1, 26                                                    12160
       IF(.NOT.(INCHAR .EQ. EXTLET(I))) GOTO 23202                         12170
       INMAP = INTLET(I)                                                   12180
       RETURN                                                              12190
23202  CONTINUE                                                            12200
23200  CONTINUE                                                            12210
23201  CONTINUE                                                            12220
       DO23204I = 1, 26                                                    12230
       IF(.NOT.(INCHAR .EQ. EXTBIG(I))) GOTO 23206                         12240
       INMAP = INTBIG(I)                                                   12250
       RETURN                                                              12260
23206  CONTINUE                                                            12270
23204  CONTINUE                                                            12280
23205  CONTINUE                                                            12290
       DO23208I = 1, 33                                                    12300
       IF(.NOT.(INCHAR .EQ. EXTCHR(I))) GOTO 23210                         12310
       INMAP = INTCHR(I)                                                   12320
       RETURN                                                              12330
23210  CONTINUE                                                            12340
23208  CONTINUE                                                            12350
23209  CONTINUE                                                            12360
       INMAP = INCHAR                                                      12370
       RETURN                                                              12380
       END                                                                 12390
C                                                                          12400
C INSTAL - ADD NAME AND DEFINITION TO TABLE                                12410
C                                                                          12420
       SUBROUTINE INSTAL(NAME, DEFN)                                       12430
CJAS   INTEGER DEFN(200), NAME(200)                                        12440
       INTEGER DEFN(*), NAME(*)                                            12440
       INTEGER LENGTH                                                      12450
       INTEGER DLEN, NLEN                                                  12460
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   12470
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       12480
       INTEGER EXTDIG                                                      12490
       INTEGER INTDIG                                                      12500
       INTEGER EXTLET                                                      12510
       INTEGER INTLET                                                      12520
       INTEGER EXTBIG                                                      12530
       INTEGER INTBIG                                                      12540
       INTEGER EXTCHR                                                      12550
       INTEGER INTCHR                                                      12560
       INTEGER EXTBLK                                                      12570
       INTEGER INTBLK                                                      12580
       COMMON /CDEFIO/ BP, BUF(300)                                        12590
       INTEGER BP                                                          12600
       INTEGER BUF                                                         12610
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   12620
       INTEGER FORDEP                                                      12630
       INTEGER FORSTK                                                      12640
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   12650
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   12660
     *UNTIL                                                                12670
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    12680
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                12690
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    12700
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                12710
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          12720
       INTEGER LEVEL                                                       12730
       INTEGER LINECT                                                      12740
       INTEGER INFILE                                                      12750
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               12760
       INTEGER LASTP                                                       12770
       INTEGER LASTT                                                       12780
       INTEGER NAMPTR                                                      12790
       INTEGER TABLE                                                       12800
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    12810
       INTEGER OUTP                                                        12820
       INTEGER OUTBUF                                                      12830
       NLEN = LENGTH(NAME) + 1                                             12840
       DLEN = LENGTH(DEFN) + 1                                             12850
       IF(.NOT.(LASTT + NLEN + DLEN .GT. 1500 .OR. LASTP .GE. 200))   GO   12860
     *TO 23212                                                             12870
       CALL PUTLIN(NAME, 6)                                                12880
       CALL REMARK(23H: TOO MANY DEFINITIONS.)                             12890
23212  CONTINUE                                                            12900
       LASTP = LASTP + 1                                                   12910
       NAMPTR(LASTP) = LASTT + 1                                           12920
       CALL SCOPY(NAME, 1, TABLE, LASTT + 1)                               12930
       CALL SCOPY(DEFN, 1, TABLE, LASTT + NLEN + 1)                        12940
       LASTT = LASTT + NLEN + DLEN                                         12950
       RETURN                                                              12960
       END                                                                 12970
C                                                                          12980
C ITOC - CONVERT INTEGER  INT  TO CHAR STRING IN  STR                      12990
C                                                                          13000
       INTEGER FUNCTION ITOC(INT, STR, SIZE)                               13010
       INTEGER IABS, MOD                                                   13020
       INTEGER D, I, INT, INTVAL, J, K, SIZE                               13030
       INTEGER STR(SIZE)                                                   13040
       INTEGER DIGITS(11)                                                  13050
       DATA DIGITS(1) /48/                                                 13060
       DATA DIGITS(2) /49/                                                 13070
       DATA DIGITS(3) /50/                                                 13080
       DATA DIGITS(4) /51/                                                 13090
       DATA DIGITS(5) /52/                                                 13100
       DATA DIGITS(6) /53/                                                 13110
       DATA DIGITS(7) /54/                                                 13120
       DATA DIGITS(8) /55/                                                 13130
       DATA DIGITS(9) /56/                                                 13140
       DATA DIGITS(10) /57/                                                13150
       DATA DIGITS(11) /10002/                                             13160
       INTVAL = IABS(INT)                                                  13170
       STR(1) = 10002                                                      13180
       I = 1                                                               13190
       CONTINUE                                                            13200
23214  CONTINUE                                                            13210
       I = I + 1                                                           13220
       D = MOD(INTVAL, 10)                                                 13230
       STR(I) = DIGITS(D+1)                                                13240
       INTVAL = INTVAL / 10                                                13250
23215  IF(.NOT.(INTVAL .EQ. 0 .OR. I .GE. SIZE)) GOTO 23214                13260
23216  CONTINUE                                                            13270
       IF(.NOT.(INT .LT. 0 .AND. I .LT. SIZE))   GOTO 23217                13280
       I = I + 1                                                           13290
       STR(I) = 45                                                         13300
23217  CONTINUE                                                            13310
       ITOC = I - 1                                                        13320
       CONTINUE                                                            13330
       J = 1                                                               13340
23219  IF(.NOT.( J .LT. I)) GOTO 23221                                     13350
       K = STR(I)                                                          13360
       STR(I) = STR(J)                                                     13370
       STR(J) = K                                                          13380
       I = I - 1                                                           13390
23220   J = J + 1                                                          13400
       GOTO 23219                                                          13410
23221  CONTINUE                                                            13420
       RETURN                                                              13430
       END                                                                 13440
C                                                                          13450
C LABELC - OUTPUT STATEMENT NUMBER                                         13460
C                                                                          13470
       SUBROUTINE LABELC(LEXSTR)                                           13480
       INTEGER LEXSTR(100)                                                 13490
       INTEGER LENGTH                                                      13500
       IF(.NOT.(LENGTH(LEXSTR) .EQ. 5)) GOTO 23222                         13510
       IF(.NOT.(LEXSTR(1) .EQ. 50 .AND. LEXSTR(2) .EQ. 51))  GOTO 23224    13520
       CALL SYNERR(33HWARNING: POSSIBLE LABEL CONFLICT.)                   13530
23224  CONTINUE                                                            13540
23222  CONTINUE                                                            13550
       CALL OUTSTR(LEXSTR)                                                 13560
       CALL OUTTAB                                                         13570
       RETURN                                                              13580
       END                                                                 13590
C                                                                          13600
C LABGEN - GENERATE  N  CONSECUTIVE LABELS, RETURN FIRST ONE               13610
C                                                                          13620
       INTEGER FUNCTION LABGEN(N)                                          13630
       INTEGER LABEL, N                                                    13640
       DATA LABEL /23000/                                                  13650
       LABGEN = LABEL                                                      13660
       LABEL = LABEL + N                                                   13670
       RETURN                                                              13680
       END                                                                 13690
C                                                                          13700
C LENGTH - COMPUTE LENGTH OF STRING                                        13710
C                                                                          13720
       INTEGER FUNCTION LENGTH(STR)                                        13730
       INTEGER STR(100)                                                    13740
       CONTINUE                                                            13750
       LENGTH = 0                                                          13760
23226  IF(.NOT.( STR(LENGTH+1) .NE. 10002))   GOTO 23228                   13770
23227   LENGTH = LENGTH + 1                                                13780
       GOTO 23226                                                          13790
23228  CONTINUE                                                            13800
       RETURN                                                              13810
       END                                                                 13820
C                                                                          13830
C LEX - RETURN LEXICAL TYPE OF TOKEN                                       13840
C                                                                          13850
       INTEGER FUNCTION LEX(LEXSTR)                                        13860
       INTEGER GETTOK                                                      13870
       INTEGER LEXSTR(200)                                                 13880
       INTEGER ALLDIG, EQUAL                                               13890
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   13900
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       13910
       INTEGER EXTDIG                                                      13920
       INTEGER INTDIG                                                      13930
       INTEGER EXTLET                                                      13940
       INTEGER INTLET                                                      13950
       INTEGER EXTBIG                                                      13960
       INTEGER INTBIG                                                      13970
       INTEGER EXTCHR                                                      13980
       INTEGER INTCHR                                                      13990
       INTEGER EXTBLK                                                      14000
       INTEGER INTBLK                                                      14010
       COMMON /CDEFIO/ BP, BUF(300)                                        14020
       INTEGER BP                                                          14030
       INTEGER BUF                                                         14040
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   14050
       INTEGER FORDEP                                                      14060
       INTEGER FORSTK                                                      14070
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   14080
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   14090
     *UNTIL                                                                14100
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    14110
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                14120
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    14130
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                14140
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          14150
       INTEGER LEVEL                                                       14160
       INTEGER LINECT                                                      14170
       INTEGER INFILE                                                      14180
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               14190
       INTEGER LASTP                                                       14200
       INTEGER LASTT                                                       14210
       INTEGER NAMPTR                                                      14220
       INTEGER TABLE                                                       14230
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    14240
       INTEGER OUTP                                                        14250
       INTEGER OUTBUF                                                      14260
       CONTINUE                                                            14270
23229  IF(.NOT.(GETTOK(LEXSTR, 200) .EQ. 10)) GOTO 23230                   14280
       GOTO 23229                                                          14290
23230  CONTINUE                                                            14300
       LEX = LEXSTR(1)                                                     14310
       IF(.NOT.(LEX.EQ.10003 .OR. LEX.EQ.59 .OR. LEX.EQ.123 .OR. LEX.EQ.   14320
     *125))  GOTO 23231                                                    14330
       RETURN                                                              14340
23231  CONTINUE                                                            14350
       IF(.NOT.(ALLDIG(LEXSTR) .EQ. 1)) GOTO 23233                         14360
       LEX = 10260                                                         14370
       GOTO 23234                                                          14380
23233  CONTINUE                                                            14390
       IF(.NOT.(EQUAL(LEXSTR, SIF) .EQ. 1))   GOTO 23235                   14400
       LEX = VIF(1)                                                        14410
       GOTO 23236                                                          14420
23235  CONTINUE                                                            14430
       IF(.NOT.(EQUAL(LEXSTR, SELSE) .EQ. 1)) GOTO 23237                   14440
       LEX = VELSE(1)                                                      14450
       GOTO 23238                                                          14460
23237  CONTINUE                                                            14470
       IF(.NOT.(EQUAL(LEXSTR, SWHILE) .EQ. 1))   GOTO 23239                14480
       LEX = VWHILE(1)                                                     14490
       GOTO 23240                                                          14500
23239  CONTINUE                                                            14510
       IF(.NOT.(EQUAL(LEXSTR, SDO) .EQ. 1))   GOTO 23241                   14520
       LEX = VDO(1)                                                        14530
       GOTO 23242                                                          14540
23241  CONTINUE                                                            14550
       IF(.NOT.(EQUAL(LEXSTR, SBREAK) .EQ. 1))   GOTO 23243                14560
       LEX = VBREAK(1)                                                     14570
       GOTO 23244                                                          14580
23243  CONTINUE                                                            14590
       IF(.NOT.(EQUAL(LEXSTR, SNEXT) .EQ. 1)) GOTO 23245                   14600
       LEX = VNEXT(1)                                                      14610
       GOTO 23246                                                          14620
23245  CONTINUE                                                            14630
       IF(.NOT.(EQUAL(LEXSTR, SFOR) .EQ. 1))  GOTO 23247                   14640
       LEX = VFOR(1)                                                       14650
       GOTO 23248                                                          14660
23247  CONTINUE                                                            14670
       IF(.NOT.(EQUAL(LEXSTR, SREPT) .EQ. 1)) GOTO 23249                   14680
       LEX = VREPT(1)                                                      14690
       GOTO 23250                                                          14700
23249  CONTINUE                                                            14710
       IF(.NOT.(EQUAL(LEXSTR, SUNTIL) .EQ. 1))   GOTO 23251                14720
       LEX = VUNTIL(1)                                                     14730
       GOTO 23252                                                          14740
23251  CONTINUE                                                            14750
       LEX = 10267                                                         14760
23252  CONTINUE                                                            14770
23250  CONTINUE                                                            14780
23248  CONTINUE                                                            14790
23246  CONTINUE                                                            14800
23244  CONTINUE                                                            14810
23242  CONTINUE                                                            14820
23240  CONTINUE                                                            14830
23238  CONTINUE                                                            14840
23236  CONTINUE                                                            14850
23234  CONTINUE                                                            14860
       RETURN                                                              14870
       END                                                                 14880
C                                                                          14890
C LOOKUP - LOCATE NAME, EXTRACT DEFINITION FROM TABLE                      14900
C                                                                          14910
       INTEGER FUNCTION LOOKUP(NAME, DEFN)                                 14920
       INTEGER DEFN(200), NAME(200)                                        14930
       INTEGER I, J, K                                                     14940
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   14950
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       14960
       INTEGER EXTDIG                                                      14970
       INTEGER INTDIG                                                      14980
       INTEGER EXTLET                                                      14990
       INTEGER INTLET                                                      15000
       INTEGER EXTBIG                                                      15010
       INTEGER INTBIG                                                      15020
       INTEGER EXTCHR                                                      15030
       INTEGER INTCHR                                                      15040
       INTEGER EXTBLK                                                      15050
       INTEGER INTBLK                                                      15060
       COMMON /CDEFIO/ BP, BUF(300)                                        15070
       INTEGER BP                                                          15080
       INTEGER BUF                                                         15090
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   15100
       INTEGER FORDEP                                                      15110
       INTEGER FORSTK                                                      15120
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   15130
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   15140
     *UNTIL                                                                15150
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    15160
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                15170
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    15180
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                15190
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          15200
       INTEGER LEVEL                                                       15210
       INTEGER LINECT                                                      15220
       INTEGER INFILE                                                      15230
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               15240
       INTEGER LASTP                                                       15250
       INTEGER LASTT                                                       15260
       INTEGER NAMPTR                                                      15270
       INTEGER TABLE                                                       15280
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    15290
       INTEGER OUTP                                                        15300
       INTEGER OUTBUF                                                      15310
       CONTINUE                                                            15320
       I = LASTP                                                           15330
23253  IF(.NOT.( I .GT. 0)) GOTO 23255                                     15340
       J = NAMPTR(I)                                                       15350
       CONTINUE                                                            15360
       K = 1                                                               15370
23256  IF(.NOT.( NAME(K) .EQ. TABLE(J) .AND. NAME(K) .NE. 10002))  GOTO    15380
     *23258                                                                15390
       J = J + 1                                                           15400
23257   K = K + 1                                                          15410
       GOTO 23256                                                          15420
23258  CONTINUE                                                            15430
       IF(.NOT.(NAME(K) .EQ. TABLE(J))) GOTO 23259                         15440
       CALL SCOPY(TABLE, J+1, DEFN, 1)                                     15450
       LOOKUP = 1                                                          15460
       RETURN                                                              15470
23259  CONTINUE                                                            15480
23254   I = I - 1                                                          15490
       GOTO 23253                                                          15500
23255  CONTINUE                                                            15510
       LOOKUP = 0                                                          15520
       RETURN                                                              15530
       END                                                                 15540
C                                                                          15550
C NGETCH - GET A (POSSIBLY PUSHED BACK) CHARACTER                          15560
C                                                                          15570
       INTEGER FUNCTION NGETCH(C, FD)                                      15580
       INTEGER GETCH                                                       15590
       INTEGER C                                                           15600
       INTEGER FD                                                          15610
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   15620
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       15630
       INTEGER EXTDIG                                                      15640
       INTEGER INTDIG                                                      15650
       INTEGER EXTLET                                                      15660
       INTEGER INTLET                                                      15670
       INTEGER EXTBIG                                                      15680
       INTEGER INTBIG                                                      15690
       INTEGER EXTCHR                                                      15700
       INTEGER INTCHR                                                      15710
       INTEGER EXTBLK                                                      15720
       INTEGER INTBLK                                                      15730
       COMMON /CDEFIO/ BP, BUF(300)                                        15740
       INTEGER BP                                                          15750
       INTEGER BUF                                                         15760
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   15770
       INTEGER FORDEP                                                      15780
       INTEGER FORSTK                                                      15790
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   15800
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   15810
     *UNTIL                                                                15820
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    15830
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                15840
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    15850
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                15860
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          15870
       INTEGER LEVEL                                                       15880
       INTEGER LINECT                                                      15890
       INTEGER INFILE                                                      15900
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               15910
       INTEGER LASTP                                                       15920
       INTEGER LASTT                                                       15930
       INTEGER NAMPTR                                                      15940
       INTEGER TABLE                                                       15950
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    15960
       INTEGER OUTP                                                        15970
       INTEGER OUTBUF                                                      15980
       IF(.NOT.(BP .GT. 0)) GOTO 23261                                     15990
       C = BUF(BP)                                                         16000
       GOTO 23262                                                          16010
23261  CONTINUE                                                            16020
       BP = 1                                                              16030
       BUF(BP) = GETCH(C, FD)                                              16040
23262  CONTINUE                                                            16050
       BP = BP - 1                                                         16060
       NGETCH = C                                                          16070
       RETURN                                                              16080
       END                                                                 16090
C                                                                          16100
C ZOPEN - EXCEEDINGLY TEMPORARY VERSION FOR GETTOK                          16110
C                                                                          16120
       INTEGER FUNCTION ZOPEN(NAME, MODE)                                   16130
       INTEGER NAME(30)                                                    16140
       INTEGER CTOI                                                        16150
       INTEGER I, MODE                                                     16160
       I = 1                                                               16170
       ZOPEN = CTOI(NAME, I)                                                16180
       RETURN                                                              16190
       END                                                                 16200
C                                                                          16210
C OTHERC - OUTPUT ORDINARY FORTRAN STATEMENT                               16220
C                                                                          16230
       SUBROUTINE OTHERC(LEXSTR)                                           16240
       INTEGER LEXSTR(100)                                                 16250
       CALL OUTTAB                                                         16260
       CALL OUTSTR(LEXSTR)                                                 16270
       CALL EATUP                                                          16280
       CALL OUTDON                                                         16290
       RETURN                                                              16300
       END                                                                 16310
C                                                                          16320
C OUTCH - PUT ONE CHARACTER INTO OUTPUT BUFFER                             16330
C                                                                          16340
       SUBROUTINE OUTCH(C)                                                 16350
       INTEGER C                                                           16360
       INTEGER I                                                           16370
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   16380
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       16390
       INTEGER EXTDIG                                                      16400
       INTEGER INTDIG                                                      16410
       INTEGER EXTLET                                                      16420
       INTEGER INTLET                                                      16430
       INTEGER EXTBIG                                                      16440
       INTEGER INTBIG                                                      16450
       INTEGER EXTCHR                                                      16460
       INTEGER INTCHR                                                      16470
       INTEGER EXTBLK                                                      16480
       INTEGER INTBLK                                                      16490
       COMMON /CDEFIO/ BP, BUF(300)                                        16500
       INTEGER BP                                                          16510
       INTEGER BUF                                                         16520
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   16530
       INTEGER FORDEP                                                      16540
       INTEGER FORSTK                                                      16550
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   16560
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   16570
     *UNTIL                                                                16580
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    16590
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                16600
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    16610
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                16620
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          16630
       INTEGER LEVEL                                                       16640
       INTEGER LINECT                                                      16650
       INTEGER INFILE                                                      16660
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               16670
       INTEGER LASTP                                                       16680
       INTEGER LASTT                                                       16690
       INTEGER NAMPTR                                                      16700
       INTEGER TABLE                                                       16710
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    16720
       INTEGER OUTP                                                        16730
       INTEGER OUTBUF                                                      16740
       IF(.NOT.(OUTP .GE. 72)) GOTO 23263                                  16750
       CALL OUTDON                                                         16760
       CONTINUE                                                            16770
       I = 1                                                               16780
23265  IF(.NOT.( I .LT. 6)) GOTO 23267                                     16790
       OUTBUF(I) = 32                                                      16800
23266   I = I + 1                                                          16810
       GOTO 23265                                                          16820
23267  CONTINUE                                                            16830
       OUTBUF(6) = 42                                                      16840
       OUTP = 6                                                            16850
23263  CONTINUE                                                            16860
       OUTP = OUTP + 1                                                     16870
       OUTBUF(OUTP) = C                                                    16880
       RETURN                                                              16890
       END                                                                 16900
C                                                                          16910
C OUTCON - OUTPUT "N   CONTINUE"                                           16920
C                                                                          16930
       SUBROUTINE OUTCON(N)                                                16940
       INTEGER N                                                           16950
       INTEGER CONTIN(9)                                                   16960
       DATA CONTIN(1) /99/                                                 16970
       DATA CONTIN(2) /111/                                                16980
       DATA CONTIN(3) /110/                                                16990
       DATA CONTIN(4) /116/                                                17000
       DATA CONTIN(5) /105/                                                17010
       DATA CONTIN(6) /110/                                                17020
       DATA CONTIN(7) /117/                                                17030
       DATA CONTIN(8) /101/                                                17040
       DATA CONTIN(9) /10002/                                              17050
       IF(.NOT.(N .GT. 0))  GOTO 23268                                     17060
       CALL OUTNUM(N)                                                      17070
23268  CONTINUE                                                            17080
       CALL OUTTAB                                                         17090
       CALL OUTSTR(CONTIN)                                                 17100
       CALL OUTDON                                                         17110
       RETURN                                                              17120
       END                                                                 17130
C                                                                          17140
C OUTDON - FINISH OFF AN OUTPUT LINE                                       17150
C                                                                          17160
       SUBROUTINE OUTDON                                                   17170
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   17180
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       17190
       INTEGER EXTDIG                                                      17200
       INTEGER INTDIG                                                      17210
       INTEGER EXTLET                                                      17220
       INTEGER INTLET                                                      17230
       INTEGER EXTBIG                                                      17240
       INTEGER INTBIG                                                      17250
       INTEGER EXTCHR                                                      17260
       INTEGER INTCHR                                                      17270
       INTEGER EXTBLK                                                      17280
       INTEGER INTBLK                                                      17290
       COMMON /CDEFIO/ BP, BUF(300)                                        17300
       INTEGER BP                                                          17310
       INTEGER BUF                                                         17320
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   17330
       INTEGER FORDEP                                                      17340
       INTEGER FORSTK                                                      17350
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   17360
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   17370
     *UNTIL                                                                17380
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    17390
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                17400
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    17410
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                17420
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          17430
       INTEGER LEVEL                                                       17440
       INTEGER LINECT                                                      17450
       INTEGER INFILE                                                      17460
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               17470
       INTEGER LASTP                                                       17480
       INTEGER LASTT                                                       17490
       INTEGER NAMPTR                                                      17500
       INTEGER TABLE                                                       17510
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    17520
       INTEGER OUTP                                                        17530
       INTEGER OUTBUF                                                      17540
       OUTBUF(OUTP+1) = 10                                                 17550
       OUTBUF(OUTP+2) = 10002                                              17560
       CALL PUTLIN(OUTBUF, 6)                                              17570
       OUTP = 0                                                            17580
       RETURN                                                              17590
       END                                                                 17600
C                                                                          17610
C OUTGO - OUTPUT "GOTO  N"                                                 17620
C                                                                          17630
       SUBROUTINE OUTGO(N)                                                 17640
       INTEGER N                                                           17650
       INTEGER GOTO(6)                                                     17660
       DATA GOTO(1) /103/                                                  17670
       DATA GOTO(2) /111/                                                  17680
       DATA GOTO(3) /116/                                                  17690
       DATA GOTO(4) /111/                                                  17700
       DATA GOTO(5) /32/                                                   17710
       DATA GOTO(6) /10002/                                                17720
       CALL OUTTAB                                                         17730
       CALL OUTSTR(GOTO)                                                   17740
       CALL OUTNUM(N)                                                      17750
       CALL OUTDON                                                         17760
       RETURN                                                              17770
       END                                                                 17780
C                                                                          17790
C OUTMAP - CONVERT RIGHT ADJ ASCII TO LEFT ADJUSTED EXTERNAL REP           17800
C                                                                          17810
       INTEGER FUNCTION OUTMAP(INCHAR)                                     17820
       INTEGER I, INCHAR                                                   17830
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   17840
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       17850
       INTEGER EXTDIG                                                      17860
       INTEGER INTDIG                                                      17870
       INTEGER EXTLET                                                      17880
       INTEGER INTLET                                                      17890
       INTEGER EXTBIG                                                      17900
       INTEGER INTBIG                                                      17910
       INTEGER EXTCHR                                                      17920
       INTEGER INTCHR                                                      17930
       INTEGER EXTBLK                                                      17940
       INTEGER INTBLK                                                      17950
       COMMON /CDEFIO/ BP, BUF(300)                                        17960
       INTEGER BP                                                          17970
       INTEGER BUF                                                         17980
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   17990
       INTEGER FORDEP                                                      18000
       INTEGER FORSTK                                                      18010
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   18020
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   18030
     *UNTIL                                                                18040
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    18050
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                18060
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    18070
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                18080
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          18090
       INTEGER LEVEL                                                       18100
       INTEGER LINECT                                                      18110
       INTEGER INFILE                                                      18120
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               18130
       INTEGER LASTP                                                       18140
       INTEGER LASTT                                                       18150
       INTEGER NAMPTR                                                      18160
       INTEGER TABLE                                                       18170
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    18180
       INTEGER OUTP                                                        18190
       INTEGER OUTBUF                                                      18200
       IF(.NOT.(INCHAR .EQ. INTBLK)) GOTO 23270                            18210
       OUTMAP = EXTBLK                                                     18220
       RETURN                                                              18230
23270  CONTINUE                                                            18240
       DO23272I = 1, 10                                                    18250
       IF(.NOT.(INCHAR .EQ. INTDIG(I))) GOTO 23274                         18260
       OUTMAP = EXTDIG(I)                                                  18270
       RETURN                                                              18280
23274  CONTINUE                                                            18290
23272  CONTINUE                                                            18300
23273  CONTINUE                                                            18310
       DO23276I = 1, 26                                                    18320
       IF(.NOT.(INCHAR .EQ. INTLET(I))) GOTO 23278                         18330
       OUTMAP = EXTLET(I)                                                  18340
       RETURN                                                              18350
23278  CONTINUE                                                            18360
23276  CONTINUE                                                            18370
23277  CONTINUE                                                            18380
       DO23280I = 1, 26                                                    18390
       IF(.NOT.(INCHAR .EQ. INTBIG(I))) GOTO 23282                         18400
       OUTMAP = EXTBIG(I)                                                  18410
       RETURN                                                              18420
23282  CONTINUE                                                            18430
23280  CONTINUE                                                            18440
23281  CONTINUE                                                            18450
       DO23284I = 1, 33                                                    18460
       IF(.NOT.(INCHAR .EQ. INTCHR(I))) GOTO 23286                         18470
       OUTMAP = EXTCHR(I)                                                  18480
       RETURN                                                              18490
23286  CONTINUE                                                            18500
23284  CONTINUE                                                            18510
23285  CONTINUE                                                            18520
       OUTMAP = INCHAR                                                     18530
       RETURN                                                              18540
       END                                                                 18550
C                                                                          18560
C OUTNUM - OUTPUT DECIMAL NUMBER                                           18570
C                                                                          18580
       SUBROUTINE OUTNUM(N)                                                18590
       INTEGER CHARS(10)                                                   18600
       INTEGER ITOC                                                        18610
       INTEGER I, LEN, N                                                   18620
       LEN = ITOC(N, CHARS, 10)                                            18630
       CONTINUE                                                            18640
       I = 1                                                               18650
23288  IF(.NOT.( I .LE. LEN))  GOTO 23290                                  18660
       CALL OUTCH(CHARS(I))                                                18670
23289   I = I + 1                                                          18680
       GOTO 23288                                                          18690
23290  CONTINUE                                                            18700
       RETURN                                                              18710
       END                                                                 18720
C                                                                          18730
C OUTSTR - OUTPUT STRING                                                   18740
C                                                                          18750
       SUBROUTINE OUTSTR(STR)                                              18760
CJAS   INTEGER C, STR(100)                                                 18770
       INTEGER C, STR(*)                                                   18770
       INTEGER I, J                                                        18780
       CONTINUE                                                            18790
       I = 1                                                               18800
23291  IF(.NOT.( STR(I) .NE. 10002)) GOTO 23293                            18810
       C = STR(I)                                                          18820
       IF(.NOT.(C .NE. 39 .AND. C .NE. 34))   GOTO 23294                   18830
       CALL OUTCH(C)                                                       18840
       GOTO 23295                                                          18850
23294  CONTINUE                                                            18860
       I = I + 1                                                           18870
       CONTINUE                                                            18880
       J = I                                                               18890
23296  IF(.NOT.( STR(J) .NE. C))  GOTO 23298                               18900
23297   J = J + 1                                                          18910
       GOTO 23296                                                          18920
23298  CONTINUE                                                            18930
       CALL OUTNUM(J-I)                                                    18940
       CALL OUTCH(104)                                                     18950
       CONTINUE                                                            18960
23299  IF(.NOT.( I .LT. J)) GOTO 23301                                     18970
       CALL OUTCH(STR(I))                                                  18980
23300   I = I + 1                                                          18990
       GOTO 23299                                                          19000
23301  CONTINUE                                                            19010
23295  CONTINUE                                                            19020
23292   I = I + 1                                                          19030
       GOTO 23291                                                          19040
23293  CONTINUE                                                            19050
       RETURN                                                              19060
       END                                                                 19070
C                                                                          19080
C OUTTAB - GET PAST COLUMN 6                                               19090
C                                                                          19100
       SUBROUTINE OUTTAB                                                   19110
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   19120
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       19130
       INTEGER EXTDIG                                                      19140
       INTEGER INTDIG                                                      19150
       INTEGER EXTLET                                                      19160
       INTEGER INTLET                                                      19170
       INTEGER EXTBIG                                                      19180
       INTEGER INTBIG                                                      19190
       INTEGER EXTCHR                                                      19200
       INTEGER INTCHR                                                      19210
       INTEGER EXTBLK                                                      19220
       INTEGER INTBLK                                                      19230
       COMMON /CDEFIO/ BP, BUF(300)                                        19240
       INTEGER BP                                                          19250
       INTEGER BUF                                                         19260
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   19270
       INTEGER FORDEP                                                      19280
       INTEGER FORSTK                                                      19290
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   19300
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   19310
     *UNTIL                                                                19320
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    19330
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                19340
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    19350
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                19360
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          19370
       INTEGER LEVEL                                                       19380
       INTEGER LINECT                                                      19390
       INTEGER INFILE                                                      19400
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               19410
       INTEGER LASTP                                                       19420
       INTEGER LASTT                                                       19430
       INTEGER NAMPTR                                                      19440
       INTEGER TABLE                                                       19450
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    19460
       INTEGER OUTP                                                        19470
       INTEGER OUTBUF                                                      19480
       CONTINUE                                                            19490
23302  IF(.NOT.(OUTP .LT. 6))  GOTO 23303                                  19500
       CALL OUTCH(32)                                                      19510
       GOTO 23302                                                          19520
23303  CONTINUE                                                            19530
       RETURN                                                              19540
       END                                                                 19550
C                                                                          19560
C PARSE - PARSE RATFOR SOURCE PROGRAM                                      19570
C                                                                          19580
       SUBROUTINE PARSE                                                    19590
       INTEGER LEXSTR(200)                                                 19600
       INTEGER LEX                                                         19610
       INTEGER LAB, LABVAL(100), LEXTYP(100), SP, TOKEN                    19620
       CALL INITKW                                                         19630
       SP = 1                                                              19640
       LEXTYP(1) = 10003                                                   19650
       CONTINUE                                                            19660
       TOKEN = LEX(LEXSTR)                                                 19670
23304  IF(.NOT.( TOKEN .NE. 10003))  GOTO 23306                            19680
       IF(.NOT.(TOKEN .EQ. 10261))   GOTO 23307                            19690
       CALL IFCODE(LAB)                                                    19700
       GOTO 23308                                                          19710
23307  CONTINUE                                                            19720
       IF(.NOT.(TOKEN .EQ. 10266))   GOTO 23309                            19730
       CALL DOCODE(LAB)                                                    19740
       GOTO 23310                                                          19750
23309  CONTINUE                                                            19760
       IF(.NOT.(TOKEN .EQ. 10263))   GOTO 23311                            19770
       CALL WHILEC(LAB)                                                    19780
       GOTO 23312                                                          19790
23311  CONTINUE                                                            19800
       IF(.NOT.(TOKEN .EQ. 10268))   GOTO 23313                            19810
       CALL FORCOD(LAB)                                                    19820
       GOTO 23314                                                          19830
23313  CONTINUE                                                            19840
       IF(.NOT.(TOKEN .EQ. 10269))   GOTO 23315                            19850
       CALL REPCOD(LAB)                                                    19860
       GOTO 23316                                                          19870
23315  CONTINUE                                                            19880
       IF(.NOT.(TOKEN .EQ. 10260))   GOTO 23317                            19890
       CALL LABELC(LEXSTR)                                                 19900
       GOTO 23318                                                          19910
23317  CONTINUE                                                            19920
       IF(.NOT.(TOKEN .EQ. 10262))   GOTO 23319                            19930
       IF(.NOT.(LEXTYP(SP) .EQ. 10261)) GOTO 23321                         19940
       CALL ELSEIF(LABVAL(SP))                                             19950
       GOTO 23322                                                          19960
23321  CONTINUE                                                            19970
       CALL SYNERR(13HILLEGAL ELSE.)                                       19980
23322  CONTINUE                                                            19990
23319  CONTINUE                                                            20000
23318  CONTINUE                                                            20010
23316  CONTINUE                                                            20020
23314  CONTINUE                                                            20030
23312  CONTINUE                                                            20040
23310  CONTINUE                                                            20050
23308  CONTINUE                                                            20060
       IF(.NOT.(TOKEN.EQ.10261 .OR. TOKEN.EQ.10262 .OR. TOKEN.EQ.10263     20070
     *      .OR. TOKEN.EQ.10268 .OR. TOKEN.EQ.10269         .OR. TOKEN.E   20080
     *Q.10266 .OR. TOKEN.EQ.10260 .OR. TOKEN.EQ.123))  GOTO 23323          20090
       SP = SP + 1                                                         20100
       IF(.NOT.(SP .GT. 100))  GOTO 23325                                  20110
       CALL ERROR(25HSTACK OVERFLOW IN PARSER.)                            20120
23325  CONTINUE                                                            20130
       LEXTYP(SP) = TOKEN                                                  20140
       LABVAL(SP) = LAB                                                    20150
       GOTO 23324                                                          20160
23323  CONTINUE                                                            20170
       IF(.NOT.(TOKEN .EQ. 125))  GOTO 23327                               20180
       IF(.NOT.(LEXTYP(SP) .EQ. 123))   GOTO 23329                         20190
       SP = SP - 1                                                         20200
       GOTO 23330                                                          20210
23329  CONTINUE                                                            20220
       CALL SYNERR(20HILLEGAL RIGHT BRACE.)                                20230
23330  CONTINUE                                                            20240
       GOTO 23328                                                          20250
23327  CONTINUE                                                            20260
       IF(.NOT.(TOKEN .EQ. 10267))   GOTO 23331                            20270
       CALL OTHERC(LEXSTR)                                                 20280
       GOTO 23332                                                          20290
23331  CONTINUE                                                            20300
       IF(.NOT.(TOKEN .EQ. 10264 .OR. TOKEN .EQ. 10265))  GOTO 23333       20310
       CALL BRKNXT(SP, LEXTYP, LABVAL, TOKEN)                              20320
23333  CONTINUE                                                            20330
23332  CONTINUE                                                            20340
23328  CONTINUE                                                            20350
       TOKEN = LEX(LEXSTR)                                                 20360
       CALL PBSTR(LEXSTR)                                                  20370
       CALL UNSTAK(SP, LEXTYP, LABVAL, TOKEN)                              20380
23324  CONTINUE                                                            20390
23305   TOKEN = LEX(LEXSTR)                                                20400
       GOTO 23304                                                          20410
23306  CONTINUE                                                            20420
       IF(.NOT.(SP .NE. 1)) GOTO 23335                                     20430
       CALL SYNERR(15HUNEXPECTED EOF.)                                     20440
23335  CONTINUE                                                            20450
       RETURN                                                              20460
       END                                                                 20470
C                                                                          20480
C PBSTR - PUSH STRING BACK ONTO INPUT                                      20490
C                                                                          20500
       SUBROUTINE PBSTR(IN)                                                20510
       INTEGER IN(100)                                                     20520
       INTEGER LENGTH                                                      20530
       INTEGER I                                                           20540
       CONTINUE                                                            20550
       I = LENGTH(IN)                                                      20560
23337  IF(.NOT.( I .GT. 0)) GOTO 23339                                     20570
       CALL PUTBAK(IN(I))                                                  20580
23338   I = I - 1                                                          20590
       GOTO 23337                                                          20600
23339  CONTINUE                                                            20610
       RETURN                                                              20620
       END                                                                 20630
C                                                                          20640
C PUTBAK - PUSH CHARACTER BACK ONTO INPUT                                  20650
C                                                                          20660
       SUBROUTINE PUTBAK(C)                                                20670
       INTEGER C                                                           20680
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   20690
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       20700
       INTEGER EXTDIG                                                      20710
       INTEGER INTDIG                                                      20720
       INTEGER EXTLET                                                      20730
       INTEGER INTLET                                                      20740
       INTEGER EXTBIG                                                      20750
       INTEGER INTBIG                                                      20760
       INTEGER EXTCHR                                                      20770
       INTEGER INTCHR                                                      20780
       INTEGER EXTBLK                                                      20790
       INTEGER INTBLK                                                      20800
       COMMON /CDEFIO/ BP, BUF(300)                                        20810
       INTEGER BP                                                          20820
       INTEGER BUF                                                         20830
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   20840
       INTEGER FORDEP                                                      20850
       INTEGER FORSTK                                                      20860
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   20870
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   20880
     *UNTIL                                                                20890
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    20900
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                20910
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    20920
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                20930
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          20940
       INTEGER LEVEL                                                       20950
       INTEGER LINECT                                                      20960
       INTEGER INFILE                                                      20970
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               20980
       INTEGER LASTP                                                       20990
       INTEGER LASTT                                                       21000
       INTEGER NAMPTR                                                      21010
       INTEGER TABLE                                                       21020
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    21030
       INTEGER OUTP                                                        21040
       INTEGER OUTBUF                                                      21050
       BP = BP + 1                                                         21060
       IF(.NOT.(BP .GT. 300))  GOTO 23340                                  21070
       CALL ERROR(32HTOO MANY CHARACTERS PUSHED BACK.)                     21080
23340  CONTINUE                                                            21090
       BUF(BP) = C                                                         21100
       RETURN                                                              21110
       END                                                                 21120
C                                                                          21130
C PUTCH (INTERIM VERSION)  PUT CHARACTERS                                  21140
C                                                                          21150
       SUBROUTINE PUTCH(C, F)                                              21160
       INTEGER BUF(81), C                                                  21170
       INTEGER OUTMAP                                                      21180
       INTEGER F, I, LASTC                                                 21190
       DATA LASTC /0/                                                      21200
       IF(.NOT.(LASTC .GE. 81 .OR. C .EQ. 10))   GOTO 23342                21210
       IF(.NOT.( LASTC .LE. 0 ))  GOTO 23344                               21220
       WRITE(F,2)                                                          21230
2         FORMAT(/)                                                        21240
       GOTO 23345                                                          21250
23344  CONTINUE                                                            21260
       WRITE(F, 1) (BUF(I), I = 1, LASTC)                                  21270
1         FORMAT(80 A1)                                                    21280
23345  CONTINUE                                                            21290
       LASTC = 0                                                           21300
23342  CONTINUE                                                            21310
       IF(.NOT.(C .NE. 10)) GOTO 23346                                     21320
       LASTC = LASTC + 1                                                   21330
       BUF(LASTC) = OUTMAP(C)                                              21340
23346  CONTINUE                                                            21350
       RETURN                                                              21360
       END                                                                 21370
C                                                                          21380
C PUTLIN - PUT OUT LINE BY REPEATED CALLS TO PUTCH                         21390
C                                                                          21400
       SUBROUTINE PUTLIN(B, F)                                             21410
CJAS   INTEGER B(100)                                                      21420
       INTEGER B(*)
       INTEGER F, I                                                        21430
       CONTINUE                                                            21440
       I = 1                                                               21450
23348  IF(.NOT.( B(I) .NE. 10002))   GOTO 23350                            21460
       CALL PUTCH(B(I), F)                                                 21470
23349   I = I + 1                                                          21480
       GOTO 23348                                                          21490
23350  CONTINUE                                                            21500
       RETURN                                                              21510
       END                                                                 21520
C                                                                          21530
C RELATE - CONVERT RELATIONAL SHORTHANDS INTO LONG FORM                    21540
C                                                                          21550
       SUBROUTINE RELATE(TOKEN, LAST, FD)                                  21560
       INTEGER NGETCH                                                      21570
       INTEGER TOKEN(100)                                                  21580
       INTEGER LENGTH                                                      21590
       INTEGER FD, LAST                                                    21600
       INTEGER DOTGE(5), DOTGT(5), DOTLT(5), DOTLE(5)                      21610
       INTEGER DOTNE(5), DOTNOT(6), DOTEQ(5), DOTAND(6), DOTOR(5)          21620
       DATA DOTGE(1), DOTGE(2), DOTGE(3), DOTGE(4), DOTGE(5)/ 46, 103, 1   21630
     *01, 46, 10002/                                                       21640
       DATA DOTGT(1), DOTGT(2), DOTGT(3), DOTGT(4), DOTGT(5)/ 46, 103, 1   21650
     *16, 46, 10002/                                                       21660
       DATA DOTLE(1), DOTLE(2), DOTLE(3), DOTLE(4), DOTLE(5)/ 46, 108, 1   21670
     *01, 46, 10002/                                                       21680
       DATA DOTLT(1), DOTLT(2), DOTLT(3), DOTLT(4), DOTLT(5)/ 46, 108, 1   21690
     *16, 46, 10002/                                                       21700
       DATA DOTNE(1), DOTNE(2), DOTNE(3), DOTNE(4), DOTNE(5)/ 46, 110, 1   21710
     *01, 46, 10002/                                                       21720
       DATA DOTEQ(1), DOTEQ(2), DOTEQ(3), DOTEQ(4), DOTEQ(5)/ 46, 101, 1   21730
     *13, 46, 10002/                                                       21740
       DATA DOTOR(1), DOTOR(2), DOTOR(3), DOTOR(4), DOTOR(5)/ 46, 111, 1   21750
     *14, 46, 10002/                                                       21760
       DATA DOTAND(1), DOTAND(2), DOTAND(3), DOTAND(4), DOTAND(5), DOTAN   21770
     *D(6) /46, 97, 110, 100, 46, 10002/                                   21780
       DATA DOTNOT(1), DOTNOT(2), DOTNOT(3), DOTNOT(4), DOTNOT(5), DOTNO   21790
     *T(6) /46, 110, 111, 116, 46, 10002/                                  21800
       IF(.NOT.(NGETCH(TOKEN(2), FD) .NE. 61))   GOTO 23351                21810
       CALL PUTBAK(TOKEN(2))                                               21820
23351  CONTINUE                                                            21830
       IF(.NOT.(TOKEN(1) .EQ. 62))   GOTO 23353                            21840
       IF(.NOT.(TOKEN(2) .EQ. 61))   GOTO 23355                            21850
       CALL SCOPY(DOTGE, 1, TOKEN, 1)                                      21860
       GOTO 23356                                                          21870
23355  CONTINUE                                                            21880
       CALL SCOPY(DOTGT, 1, TOKEN, 1)                                      21890
23356  CONTINUE                                                            21900
       GOTO 23354                                                          21910
23353  CONTINUE                                                            21920
       IF(.NOT.(TOKEN(1) .EQ. 60))   GOTO 23357                            21930
       IF(.NOT.(TOKEN(2) .EQ. 61))   GOTO 23359                            21940
       CALL SCOPY(DOTLE, 1, TOKEN, 1)                                      21950
       GOTO 23360                                                          21960
23359  CONTINUE                                                            21970
       CALL SCOPY(DOTLT, 1, TOKEN, 1)                                      21980
23360  CONTINUE                                                            21990
       GOTO 23358                                                          22000
23357  CONTINUE                                                            22010
       IF(.NOT.(TOKEN(1) .EQ. 33))   GOTO 23361                            22020
       IF(.NOT.(TOKEN(2) .EQ. 61))   GOTO 23363                            22030
       CALL SCOPY(DOTNE, 1, TOKEN, 1)                                      22040
       GOTO 23364                                                          22050
23363  CONTINUE                                                            22060
       CALL SCOPY(DOTNOT, 1, TOKEN, 1)                                     22070
23364  CONTINUE                                                            22080
       GOTO 23362                                                          22090
23361  CONTINUE                                                            22100
       IF(.NOT.(TOKEN(1) .EQ. 61))   GOTO 23365                            22110
       IF(.NOT.(TOKEN(2) .EQ. 61))   GOTO 23367                            22120
       CALL SCOPY(DOTEQ, 1, TOKEN, 1)                                      22130
       GOTO 23368                                                          22140
23367  CONTINUE                                                            22150
       TOKEN(2) = 10002                                                    22160
23368  CONTINUE                                                            22170
       GOTO 23366                                                          22180
23365  CONTINUE                                                            22190
       IF(.NOT.(TOKEN(1) .EQ. 38))   GOTO 23369                            22200
       CALL SCOPY(DOTAND, 1, TOKEN, 1)                                     22210
       GOTO 23370                                                          22220
23369  CONTINUE                                                            22230
       IF(.NOT.(TOKEN(1) .EQ. 124))  GOTO 23371                            22240
       CALL SCOPY(DOTOR, 1, TOKEN, 1)                                      22250
       GOTO 23372                                                          22260
23371  CONTINUE                                                            22270
       TOKEN(2) = 10002                                                    22280
23372  CONTINUE                                                            22290
23370  CONTINUE                                                            22300
23366  CONTINUE                                                            22310
23362  CONTINUE                                                            22320
23358  CONTINUE                                                            22330
23354  CONTINUE                                                            22340
       LAST = LENGTH(TOKEN)                                                22350
       RETURN                                                              22360
       END                                                                 22370
C                                                                          22380
C REMARK - PRINT WARNING MESSAGE                                           22390
C                                                                          22400
       SUBROUTINE REMARK(BUF)                                              22410
CJAS   INTEGER BUF(100), I                                                 22420
       INTEGER BUF(*), I
       WRITE(6, 10) (BUF(I), I = 1, 5)                                     22430
10        FORMAT(5A4)                                                      22440
       RETURN                                                              22450
       END                                                                 22460
C                                                                          22470
C REPCOD - GENERATE CODE FOR BEGINNING OF REPEAT                           22480
C                                                                          22490
       SUBROUTINE REPCOD(LAB)                                              22500
       INTEGER LABGEN                                                      22510
       INTEGER LAB                                                         22520
       CALL OUTCON(0)                                                      22530
       LAB = LABGEN(3)                                                     22540
       CALL OUTCON(LAB)                                                    22550
       LAB = LAB + 1                                                       22560
       RETURN                                                              22570
       END                                                                 22580
C                                                                          22590
C SCOPY - COPY STRING AT FROM(I) TO TO(J)                                  22600
C                                                                          22610
       SUBROUTINE SCOPY(FROM, I, TO, J)                                    22620
CJAS       INTEGER FROM(100), TO(100)                                          22630
       INTEGER FROM(*), TO(*)
       INTEGER I, J, K1, K2                                                22640
       K2 = J                                                              22650
       CONTINUE                                                            22660
       K1 = I                                                              22670
23373  IF(.NOT.( FROM(K1) .NE. 10002))  GOTO 23375                         22680
       TO(K2) = FROM(K1)                                                   22690
       K2 = K2 + 1                                                         22700
23374   K1 = K1 + 1                                                        22710
       GOTO 23373                                                          22720
23375  CONTINUE                                                            22730
       TO(K2) = 10002                                                      22740
       RETURN                                                              22750
       END                                                                 22760
C                                                                          22770
C SYNERR - REPORT RATFOR SYNTAX ERROR                                      22780
C                                                                          22790
       SUBROUTINE SYNERR(MSG)                                              22800
       INTEGER LC(81), MSG(81)                                             22810
       INTEGER ITOC                                                        22820
       INTEGER I, JUNK                                                     22830
       COMMON /CCHAR/ EXTDIG(10), INTDIG(10), EXTLET(26), INTLET(26),  E   22840
     *XTBIG(26), INTBIG(26), EXTCHR(33), INTCHR(33),  EXTBLK, INTBLK       22850
       INTEGER EXTDIG                                                      22860
       INTEGER INTDIG                                                      22870
       INTEGER EXTLET                                                      22880
       INTEGER INTLET                                                      22890
       INTEGER EXTBIG                                                      22900
       INTEGER INTBIG                                                      22910
       INTEGER EXTCHR                                                      22920
       INTEGER INTCHR                                                      22930
       INTEGER EXTBLK                                                      22940
       INTEGER INTBLK                                                      22950
       COMMON /CDEFIO/ BP, BUF(300)                                        22960
       INTEGER BP                                                          22970
       INTEGER BUF                                                         22980
       COMMON /CFOR/ FORDEP, FORSTK(200)                                   22990
       INTEGER FORDEP                                                      23000
       INTEGER FORSTK                                                      23010
       COMMON /CKEYWD/ SDO, SIF, SELSE, SWHILE, SBREAK, SNEXT, SFOR, SRE   23020
     *PT, SUNTIL, VDO, VIF, VELSE, VWHILE, VBREAK, VNEXT, VFOR, VREPT, V   23030
     *UNTIL                                                                23040
       INTEGER SDO(3), SIF(3), SELSE(5), SWHILE(6), SBREAK(6), SNEXT(5)    23050
       INTEGER SFOR(4), SREPT(7), SUNTIL(6)                                23060
       INTEGER VDO(2), VIF(2), VELSE(2), VWHILE(2), VBREAK(2), VNEXT(2)    23070
       INTEGER VFOR(2), VREPT(2), VUNTIL(2)                                23080
       COMMON /CLINE/ LEVEL, LINECT(5), INFILE(5)                          23090
       INTEGER LEVEL                                                       23100
       INTEGER LINECT                                                      23110
       INTEGER INFILE                                                      23120
       COMMON /CLOOK/ LASTP, LASTT, NAMPTR(200), TABLE(1500)               23130
       INTEGER LASTP                                                       23140
       INTEGER LASTT                                                       23150
       INTEGER NAMPTR                                                      23160
       INTEGER TABLE                                                       23170
       COMMON /COUTLN/ OUTP, OUTBUF(81)                                    23180
       INTEGER OUTP                                                        23190
       INTEGER OUTBUF                                                      23200
       CALL REMARK(14HERROR AT LINE.)                                      23210
       CONTINUE                                                            23220
       I = 1                                                               23230
23376  IF(.NOT.( I .LE. LEVEL))   GOTO 23378                               23240
       CALL PUTCH(32, 6)                                                   23250
       JUNK = ITOC(LINECT(I), LC, 81)                                      23260
       CALL PUTLIN(LC, 6)                                                  23270
23377   I = I + 1                                                          23280
       GOTO 23376                                                          23290
23378  CONTINUE                                                            23300
       CALL PUTCH(58, 6)                                                   23310
       CALL PUTCH(10, 6)                                                   23320
       CALL REMARK(MSG)                                                    23330
       RETURN                                                              23340
       END                                                                 23350
C                                                                          23360
C TYPE - RETURN LETTER, DIGIT OR CHARACTER                                 23370
C                                                                          23380
       INTEGER FUNCTION TYPE(C)                                            23390
       INTEGER C                                                           23400
       IF(.NOT.( C .GE. 48 .AND. C .LE. 57 )) GOTO 23379                   23410
       TYPE = 2                                                            23420
       GOTO 23380                                                          23430
23379  CONTINUE                                                            23440
       IF(.NOT.( C .GE. 97 .AND. C .LE. 122 ))   GOTO 23381                23450
       TYPE = 1                                                            23460
       GOTO 23382                                                          23470
23381  CONTINUE                                                            23480
       IF(.NOT.( C .GE. 65 .AND. C .LE. 90 )) GOTO 23383                   23490
       TYPE = 1                                                            23500
       GOTO 23384                                                          23510
23383  CONTINUE                                                            23520
       TYPE = C                                                            23530
23384  CONTINUE                                                            23540
23382  CONTINUE                                                            23550
23380  CONTINUE                                                            23560
       RETURN                                                              23570
       END                                                                 23580
C                                                                          23590
C UNSTAK - UNSTACK AT END OF STATEMENT                                     23600
C                                                                          23610
       SUBROUTINE UNSTAK(SP, LEXTYP, LABVAL, TOKEN)                        23620
       INTEGER LABVAL(100), LEXTYP(100), SP, TOKEN                         23630
       CONTINUE                                                            23640
23385  IF(.NOT.( SP .GT. 1))   GOTO 23387                                  23650
       IF(.NOT.(LEXTYP(SP) .EQ. 123))   GOTO 23388                         23660
       GOTO 23387                                                          23670
23388  CONTINUE                                                            23680
       IF(.NOT.(LEXTYP(SP) .EQ. 10261 .AND. TOKEN .EQ. 10262))  GOTO 233   23690
     *90                                                                   23700
       GOTO 23387                                                          23710
23390  CONTINUE                                                            23720
       IF(.NOT.(LEXTYP(SP) .EQ. 10261)) GOTO 23392                         23730
       CALL OUTCON(LABVAL(SP))                                             23740
       GOTO 23393                                                          23750
23392  CONTINUE                                                            23760
       IF(.NOT.(LEXTYP(SP) .EQ. 10262)) GOTO 23394                         23770
       IF(.NOT.(SP .GT. 2)) GOTO 23396                                     23780
       SP = SP - 1                                                         23790
23396  CONTINUE                                                            23800
       CALL OUTCON(LABVAL(SP)+1)                                           23810
       GOTO 23395                                                          23820
23394  CONTINUE                                                            23830
       IF(.NOT.(LEXTYP(SP) .EQ. 10266)) GOTO 23398                         23840
       CALL DOSTAT(LABVAL(SP))                                             23850
       GOTO 23399                                                          23860
23398  CONTINUE                                                            23870
       IF(.NOT.(LEXTYP(SP) .EQ. 10263)) GOTO 23400                         23880
       CALL WHILES(LABVAL(SP))                                             23890
       GOTO 23401                                                          23900
23400  CONTINUE                                                            23910
       IF(.NOT.(LEXTYP(SP) .EQ. 10268)) GOTO 23402                         23920
       CALL FORS(LABVAL(SP))                                               23930
       GOTO 23403                                                          23940
23402  CONTINUE                                                            23950
       IF(.NOT.(LEXTYP(SP) .EQ. 10269)) GOTO 23404                         23960
       CALL UNTILS(LABVAL(SP), TOKEN)                                      23970
23404  CONTINUE                                                            23980
23403  CONTINUE                                                            23990
23401  CONTINUE                                                            24000
23399  CONTINUE                                                            24010
23395  CONTINUE                                                            24020
23393  CONTINUE                                                            24030
23386   SP = SP - 1                                                        24040
       GOTO 23385                                                          24050
23387  CONTINUE                                                            24060
       RETURN                                                              24070
       END                                                                 24080
C                                                                          24090
C UNTILS - GENERATE CODE FOR UNTIL OR END OF REPEAT                        24100
C                                                                          24110
       SUBROUTINE UNTILS(LAB, TOKEN)                                       24120
       INTEGER PTOKEN(200)                                                 24130
       INTEGER LEX                                                         24140
       INTEGER JUNK, LAB, TOKEN                                            24150
       CALL OUTNUM(LAB)                                                    24160
       IF(.NOT.(TOKEN .EQ. 10270))   GOTO 23406                            24170
       JUNK = LEX(PTOKEN)                                                  24180
       CALL IFGO(LAB-1)                                                    24190
       GOTO 23407                                                          24200
23406  CONTINUE                                                            24210
       CALL OUTGO(LAB-1)                                                   24220
23407  CONTINUE                                                            24230
       CALL OUTCON(LAB+1)                                                  24240
       RETURN                                                              24250
       END                                                                 24260
C                                                                          24270
C WHILEC - GENERATE CODE FOR BEGINNING OF WHILE                            24280
C                                                                          24290
       SUBROUTINE WHILEC(LAB)                                              24300
       INTEGER LABGEN                                                      24310
       INTEGER LAB                                                         24320
       CALL OUTCON(0)                                                      24330
       LAB = LABGEN(2)                                                     24340
       CALL OUTNUM(LAB)                                                    24350
       CALL IFGO(LAB+1)                                                    24360
       RETURN                                                              24370
       END                                                                 24380
C                                                                          24390
C WHILES - GENERATE CODE FOR END OF WHILE                                  24400
C                                                                          24410
       SUBROUTINE WHILES(LAB)                                              24420
       INTEGER LAB                                                         24430
       CALL OUTGO(LAB)                                                     24440
       CALL OUTCON(LAB+1)                                                  24450
       RETURN                                                              24460
       END                                                                 24470
