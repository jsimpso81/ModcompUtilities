C-----BLKD2.FOR
CCC   V01.01  03/26/85  RTB  CHANGES FOR MODCOMP SYSTEM                 V01.01
CCC   BLOCK DATA                                                        V01.01
      BLOCK DATA BLKD2                                                  V01.01
CCC   LOGICAL*1 NODE                                                    V01.01
      INTEGER*2 NODE, NODE1, NODE2                                      V01.01
      LOGICAL NEWPLT,NEWNAM
      DIMENSION NODE(950)                                               V01.01
      COMMON /IOUNIT/LU1,LU2,LU3,LU4,LU5
      COMMON /PLTCOM/
     &NPLOT,MINX,MAXX,NDX,NDY,LTYPE,LWDTH,DEGRAD,
     &NBITS,NBTM1,NBYTE,NBYM1,NCHAR,NCHM1,MSK(7),IBT(16),
     &LDX,LDY,IXT,IYT,NDLTX,NDLTY,MOVY,NCLOCX,
     &LMSK(16),ICH,ICW,IVS,IHS,
     &ISCAN,NSCAN,NWORD,IM(30720)
      EQUIVALENCE (NODE1,NODE)                                          V01.01
C-----          DIMENSION OF IM (=NWORD)
C-----ORIG       1536
C-----CURR      12288
C*****NOTE: INTEGER*2 IM(NWORD) IS TRUE DIMENSION
C           (AS SPECIFIED IN BLOCK COMMON AND MAIN)
C     ISCAN = (WORDS/SCAN) =     48
C     NSCAN = (SCANS/BAND) =     32
C     NWORD = (WORDS/BAND) =   1536
C     NWORD = ISCAN*NSCAN = 48*32 = 1536
C     NCLOCX = ISCAN*NBITS/NBEXT = 48*16/6 = 128
C     NBEXT IS THE NUMBER OF BITS PER BYTE TRANSFERRED
C     TO THE EXTERNAL DEVICE
CCC   COMMON /CHRCOM/ KHAR(129),NODE(950)                               V01.01
      COMMON /CHRCOM/ KHAR(129),NODE1(475),NODE2(475)                   V01.01
      COMMON /IOCNTL/ NEWPLT,NEWNAM,LNAME,NAME(10)
      DATA NEWPLT/.FALSE./,NEWNAM/.TRUE./,LNAME/-1/
      DATA LU1,LU2,LU3,LU4,LU5/5,7,3,2,4/
      DATA NPLOT/1/,MINX/32767/,MAXX/0/,LDX/-9999/,LDY/-9999/
      DATA DEGRAD/0.01745329/
      DATA NBITS/16/,NBTM1/15/,NBYTE/2/,NBYM1/1/,NCHAR/128/,NCHM1/127/
CCC   DATA MSK /"60000,"14000,"3000,"600,"140,"30,"6/                   V01.01
      DATA MSK / 24576,  6144, 1536, 384,  96, 24, 6/                   V01.01
CCC   DATA IBT /"1,"2,"4,"10,"20,"40,"100,"200,                         V01.01
CCC  &"400,"1000,"2000,"4000,"10000,"20000,"40000,"100000/              V01.01
      DATA IBT / 1, 2, 4,  8, 16, 32,  64, 128,                         V01.01
     & 256,  512, 1024, 2048,  4096,  8192, 16384,Z'8000'/              V01.01
CMOD & 256,  512, 1024, 2048,  4096,  8192, 16384,4Z8000/               V01.01
      DATA NCLOCX/64/
      DATA ICH/6/,ICW/5/,IVS/10/,IHS/8/
      DATA ISCAN/48/,NSCAN/640/,NWORD/30720/
C-----          NSCAN     NWORD
C-----ORIG         32      1536
C-----CURR        256     12288
C*****NOTE: NWORD = TRUE DIMENSION OF IM(...)
C           (SEE COMMENT ABOVE)
      DATA KHAR /
     &  1,  8, 20, 25, 30, 35, 41, 46, 50, 57, 63, 79, 90, 95, 97,107,
     &115,127,131,142,152,160,168,169,170,171,172,173,174,175,176,177,
     &178,179,192,207,218,231,248,259,266,270,274,285,290,298,300,306,
     &308,318,323,334,346,352,361,372,376,386,397,410,425,428,433,436,
     &450,463,476,490,499,507,514,520,531,537,543,549,556,559,564,568,
     &584,592,612,622,631,635,642,645,650,656,661,668,672,674,678,684,
     &690,697,706,715,721,730,737,744,755,763,776,788,796,802,817,825,
     &831,840,849,856,865,872,880,883,893,898,910,914,922,924,932,939,
     &950
     &/
CCC   DATA NODE/                                                        V01.01
CCC  &"231,"235," 75," 65,"365,"375,"235,"231,"235,"155,                V01.01
CCC  &" 73," 67,"145,"145,"305,"367,"373,"315,"235,"231,                V01.01
CCC  &"235," 65,"365,"235,"235,"225,"  0," 71,"371," 75,                V01.01
CCC  &"365,"  0," 65,"375,"231,"235," 71,"225,"371,"235,                V01.01
CCC  &"225,"235," 71,"371,"235," 65,"375," 75,"365," 75,                V01.01
CCC  &"375," 65,"365,"  0,"151,"311," 75,"231,"225,"  0,                V01.01
CCC  &"231,"375,"231,"313,"153,"147,"307,"313,"375,"  0,                V01.01
CCC  &" 75,"153,"  0," 65,"147,"  0,"365,"307," 75,"365,                V01.01
CCC  &"  0," 65,"375,"  0," 71,"371,"  0,"235,"225," 75,                V01.01
CCC  &"375," 65,"365," 75,"235,"225,"231,"235," 67,"367,                V01.01
CCC  &"235,"  0," 73,"225,"373," 73,"231,"235," 73," 67,                V01.01
CCC  &"225,"367,"373,"235,"345,"  3,"224,"110,"234,"353,                V01.01
CCC  &"  0,"257,"254,"  0,"244,"241," 66,"235,"366," 66,                V01.01
CCC  &"225,"  1,"371,"235," 71,"225,"  2,"235,"  0," 71,                V01.01
CCC  &"371,"365," 65," 75,"375,"365,"225,"235,"  0,"371,                V01.01
CCC  &" 71,"225," 71,"235,"371,"225,"235," 71,"371," 66,                V01.01
CCC  &"366,"374," 74," 66,"  0,"231,"234,"  0,"  0,"  0,                V01.01
CCC  &"  0,"  0,"  0,"  0,"  0,"  0,"  0,"  0,"223,"  3,                V01.01
CCC  &"204,"225,"244,"223,"  0,"276,"227,"176,"  3,"237,                V01.01
CCC  &"276,"113,"  1,"176,"157,"136,"  3,"113,"  0,"253,                V01.01
CCC  &"  1,"336,"317,"276,"  3,"253,"164,"176,"  0,"276,                V01.01
CCC  &"264,"  0,"367," 67,"  0," 73,"373,"105,"225,"  1,                V01.01
CCC  &"347,"231,"  3,"113,"235,"  2,"355,"  0,"237,"223,                V01.01
CCC  &"304,"  1,"325,"306,"265,"304,"  0,"104,"  2,"356,                V01.01
CCC  &"  0,"136,"  3,"155,"134,"115,"136,"363,"172,"  3,                V01.01
CCC  &"154,"237,"314,"211,"  1," 66,"223,"366,"173,"  1,                V01.01
CCC  &"256,"237,"216,"  3,"173,"263,"  3,"171,"277,"163,                V01.01
CCC  &"  1,"271,"177,"225,"235,"  0,"354,"106,"  0," 71,                V01.01
CCC  &"371,"  0,"346,"114,"225,"235,"  0,"111,"351,"161,                V01.01
CCC  &"  1,"244,"225,"204,"223,"  3,"161,"111,"351,"223,                V01.01
CCC  &"  3,"204,"225,"244,"223," 63,"377,"223,"  3," 71,                V01.01
CCC  &"237,"371,"223,"  0," 65,"  2,"375,"143,"303,"223,                V01.01
CCC  &"237,"155,"363," 63," 64,"  3,"170,"  2,"230,"  1,                V01.01
CCC  &"374,"217," 75," 65,"  1,"223,"366,"251,"  2,"171,                V01.01
CCC  &"251,"  1,"374,"237," 75,"303,"317,"  0," 77," 67,                V01.01
CCC  &"367," 65,"  1,"223,"366,"232,"  2," 72," 77,"377,                V01.01
CCC  &" 66,"  3,"232,"367,"223," 66,"  2," 74,"  3,"237,                V01.01
CCC  &"375,"143,"377," 77," 75,"231,"  3,"366,"223," 66,                V01.01
CCC  &"231," 74,"237,"374,"231," 65,"  1,"223,"366,"  2,                V01.01
CCC  &"374,"  1,"237," 73,"230,"374,"223,"  3,"204,"225,                V01.01
CCC  &"244,"223,"  0,"230,"  3,"211,"232,"251,"230,"161,                V01.01
CCC  &"  1,"244,"225,"204,"223,"  3,"161,"  0,"230,"  1,                V01.01
CCC  &"251,"232,"211,"230,"344,"111,"356,"107,"347,"  0,                V01.01
CCC  &"113,"353,"104,"351,"116,"222,"  3,"203,"224,"243,                V01.01
CCC  &"222,"  0," 74,"237,"374,"272,"  1,"230,"226,"365,                V01.01
CCC  &"  3,"224," 51,"235,"371,"327,"270,"271,"227,"151,                V01.01
CCC  &"233,"271," 63," 73,"  3,"177,"  2,"277,"  3,"373,                V01.01
CCC  &"  2,"367," 67,"367,"363," 63," 77,"237,"  3,"354,                V01.01
CCC  &"231,"  2," 71,"231,"  3,"366,"263,"  2," 63,"366,                V01.01
CCC  &"  3,"223," 67,"  2," 73,"  3,"237,"374," 63," 77,                V01.01
CCC  &"177,"  3,"371,"163,"  2," 63,"363," 63," 71,"311,                V01.01
CCC  &" 71," 77,"377," 63," 71,"311," 71," 77,"377,"270,                V01.01
CCC  &"370,"366,"  3,"223," 67,"  2," 73,"  3,"237,"374,                V01.01
CCC  &" 63," 77," 71,"371,"377,"363,"143,"303,"223,"237,                V01.01
CCC  &"157,"317," 67,"  1,"223,"367,"  2,"377," 63," 77,                V01.01
CCC  &" 71,"173,"363,"173,"377,"363," 63," 77," 63," 77,                V01.01
CCC  &"231,"377,"363," 63," 77,"363,"377,"263,"163,"  3,                V01.01
CCC  &" 67,"  2," 73,"  3,"177,"  2,"277,"  3,"373,"  2,                V01.01
CCC  &"367,"  3,"263," 63," 77,"237,"  3,"374,"231,"  2,                V01.01
CCC  &" 71,"243,"203,"  3," 67,"  2," 73,"  3,"217,"  2,                V01.01
CCC  &"257,"  3,"373,"  2,"367,"  3,"243,"  0,"227,"  2,                V01.01
CCC  &"363," 63," 77,"237,"  3,"374,"231,"  2," 71,"171,                V01.01
CCC  &"363," 65,"  1,"223,"366,"231,"  3," 74,"237,"375,                V01.01
CCC  &"223,"237," 77,"377," 77," 67,"  1,"223,"367,"  2,                V01.01
CCC  &"377," 77,"223,"377," 77," 63,"231,"363,"377," 63,                V01.01
CCC  &"377,"  0," 77,"  2,"363," 77,"231,"223,"231,"377,                V01.01
CCC  &"363," 63,"377," 77,"  0,"311,"131,"277,"177,"163,                V01.01
CCC  &"263," 77,"363,"177,"277,"263,"163,"223,"237,"  0,                V01.01
CCC  &"154,"237,"314," 71,"371,"  0,"154," 71,"146,"273,                V01.01
CCC  &"  3,"216,"237,"256,"  1,"273,"373,"363,"  0,"367,                V01.01
CCC  &"  1,"233," 67,"223,"367," 77," 63,"  0," 67,"  1,                V01.01
CCC  &"223,"367,"233," 67,"371,"  1,"233," 67,"223,"365,                V01.01
CCC  &"377,"363,"  0,"367,"  1,"233," 67,"223,"367," 67,                V01.01
CCC  &"367,"  1,"233," 67,"223,"364,"151,"311,"  0,"223,                V01.01
CCC  &"233,"  3,"277,"374,"364,"  3,"141,"  0,"370,"  1,                V01.01
CCC  &"234," 70,"224,"370," 77," 63,"  0,"363,"370,"  1,                V01.01
CCC  &"233," 70,"152,"232,"223,"  0,"143,"303,"  0,"237,                V01.01
CCC  &"  1,"216,"235,"256,"237,"272,"263,"  3,"161,"122,                V01.01
CCC  &"  0,"277,"  1,"256,"275,"316,"277," 77," 63,"  0,                V01.01
CCC  &"333," 66,"  0,"170,"303,"177,"257,"243,"  0,"163,                V01.01
CCC  &"323," 73," 63,"  0,"230,"223,"  0,"370,"363,"  0,                V01.01
CCC  &" 70,"  3,"153,"230,"313,"370," 73," 63,"  0,"363,                V01.01
CCC  &"370,"  1,"233," 67,"233,"  1," 67,"223,"367,"233,                V01.01
CCC  &" 74," 61,"  0," 70,"  1,"224,"370,"234," 70,"374,                V01.01
CCC  &"361,"  0,"370,"  1,"234," 70,"224,"370," 73," 63,                V01.01
CCC  &"  0," 67,"  3,"233,"372,"372,"  1,"233," 71,"227,                V01.01
CCC  &"  3,"365,"223," 64,"173,"333,"  0,"257,"245,"  1,                V01.01
CCC  &"303,"373,"363,"  0," 73," 66,"  1,"223,"366," 73,                V01.01
CCC  &"223,"373," 73," 66,"  0,"373,"366,"  3,"303,"226,                V01.01
CCC  &"143," 66," 73,"363,"  0," 63,"373," 74," 67,"  1,                V01.01
CCC  &"224,"367,"  0,"141,"221,"  1,"364,"  2,"374," 73,                V01.01
CCC  &"373," 63,"363,"277,"  1,"234,"  3,"171,"226,"  1,                V01.01
CCC   &"263,"257,"243,"177,"  3,"234,"  1,"271,"226,"  3,               V01.01
CCC   &"163,"131,"  3,"172,"231,"  1,"270,"331," 77,"377,               V01.01
CCC   &"  0," 73,"373,"  0," 67,"367,"  0," 63,"363,"  0/               V01.01
      DATA NODE1/                                                       V01.01
     & 153, 157,  61,  53, 245, 253, 157, 153, 157, 109,                V01.01
     &  59,  55, 101, 101, 197, 247, 251, 205, 157, 153,                V01.01
     & 157,  53, 245, 157, 157, 149,   0,  57, 249,  61,                V01.01
     & 245,   0,  53, 253, 153, 157,  57, 149, 249, 157,                V01.01
     & 149, 157,  57, 249, 157,  53, 253,  61, 245,  61,                V01.01
     & 253,  53, 245,   0, 105, 201,  61, 153, 149,   0,                V01.01
     & 153, 253, 153, 203, 107, 103, 199, 203, 253,   0,                V01.01
     &  61, 107,   0,  53, 103,   0, 245, 199,  61, 245,                V01.01
     &   0,  53, 253,   0,  57, 249,   0, 157, 149,  61,                V01.01
     & 253,  53, 245,  61, 157, 149, 153, 157,  55, 247,                V01.01
     & 157,   0,  59, 149, 251,  59, 153, 157,  59,  55,                V01.01
     & 149, 247, 251, 157, 229,   3, 148,  72, 156, 235,                V01.01
     &   0, 175, 172,   0, 164, 161,  54, 157, 246,  54,                V01.01
     & 149,   1, 249, 157,  57, 149,   2, 157,   0,  57,                V01.01
     & 249, 245,  53,  61, 253, 245, 149, 157,   0, 249,                V01.01
     &  57, 149,  57, 157, 249, 149, 157,  57, 249,  54,                V01.01
     & 246, 252,  60,  54,   0, 153, 156,   0,   0,   0,                V01.01
     &   0,   0,   0,   0,   0,   0,   0,   0, 147,   3,                V01.01
     & 132, 149, 164, 147,   0, 190, 151, 126,   3, 159,                V01.01
     & 190,  75,   1, 126, 111,  94,   3,  75,   0, 171,                V01.01
     &   1, 222, 207, 190,   3, 171, 116, 126,   0, 190,                V01.01
     & 180,   0, 247,  55,   0,  59, 251,  69, 149,   1,                V01.01
     & 231, 153,   3,  75, 157,   2, 237,   0, 159, 147,                V01.01
     & 196,   1, 213, 198, 181, 196,   0,  68,   2, 238,                V01.01
     &   0,  94,   3, 109,  92,  77,  94, 243, 122,   3,                V01.01
     & 108, 159, 204, 137,   1,  54, 147, 246, 123,   1,                V01.01
     & 174, 159, 142,   3, 123, 179,   3, 121, 191, 115,                V01.01
     &   1, 185, 127, 149, 157,   0, 236,  70,   0,  57,                V01.01
     & 249,   0, 230,  76, 149, 157,   0,  73, 233, 113,                V01.01
     &   1, 164, 149, 132, 147,   3, 113,  73, 233, 147,                V01.01
     &   3, 132, 149, 164, 147,  51, 255, 147,   3,  57,                V01.01
     & 159, 249, 147,   0,  53,   2, 253,  99, 195, 147,                V01.01
     & 159, 109, 243,  51,  52,   3, 120,   2, 152,   1,                V01.01
     & 252, 143,  61,  53,   1, 147, 246, 169,   2, 121,                V01.01
     & 169,   1, 252, 159,  61, 195, 207,   0,  63,  55,                V01.01
     & 247,  53,   1, 147, 246, 154,   2,  58,  63, 255,                V01.01
     &  54,   3, 154, 247, 147,  54,   2,  60,   3, 159,                V01.01
     & 253,  99, 255,  63,  61, 153,   3, 246, 147,  54,                V01.01
     & 153,  60, 159, 252, 153,  53,   1, 147, 246,   2,                V01.01
     & 252,   1, 159,  59, 152, 252, 147,   3, 132, 149,                V01.01
     & 164, 147,   0, 152,   3, 137, 154, 169, 152, 113,                V01.01
     &   1, 164, 149, 132, 147,   3, 113,   0, 152,   1,                V01.01
     & 169, 154, 137, 152, 228,  73, 238,  71, 231,   0,                V01.01
     &  75, 235,  68, 233,  78, 146,   3, 131, 148, 163,                V01.01
     & 146,   0,  60, 159, 252, 186,   1, 152, 150, 245,                V01.01
     &   3, 148,  41, 157, 249, 215, 184, 185, 151, 105,                V01.01
     & 155, 185,  51,  59,   3, 127,   2, 191,   3, 251,                V01.01
     &   2, 247,  55, 247, 243/                                         V01.01
      DATA NODE2/                                                       V01.01
     &                           51,  63, 159,   3, 236,                V01.01
     & 153,   2,  57, 153,   3, 246, 179,   2,  51, 246,                V01.01
     &   3, 147,  55,   2,  59,   3, 159, 252,  51,  63,                V01.01
     & 127,   3, 249, 115,   2,  51, 243,  51,  57, 201,                V01.01
     &  57,  63, 255,  51,  57, 201,  57,  63, 255, 184,                V01.01
     & 248, 246,   3, 147,  55,   2,  59,   3, 159, 252,                V01.01
     &  51,  63,  57, 249, 255, 243,  99, 195, 147, 159,                V01.01
     & 111, 207,  55,   1, 147, 247,   2, 255,  51,  63,                V01.01
     &  57, 123, 243, 123, 255, 243,  51,  63,  51,  63,                V01.01
     & 153, 255, 243,  51,  63, 243, 255, 179, 115,   3,                V01.01
     &  55,   2,  59,   3, 127,   2, 191,   3, 251,   2,                V01.01
     & 247,   3, 179,  51,  63, 159,   3, 252, 153,   2,                V01.01
     &  57, 163, 131,   3,  55,   2,  59,   3, 143,   2,                V01.01
     & 175,   3, 251,   2, 247,   3, 163,   0, 151,   2,                V01.01
     & 243,  51,  63, 159,   3, 252, 153,   2,  57, 121,                V01.01
     & 243,  53,   1, 147, 246, 153,   3,  60, 159, 253,                V01.01
     & 147, 159,  63, 255,  63,  55,   1, 147, 247,   2,                V01.01
     & 255,  63, 147, 255,  63,  51, 153, 243, 255,  51,                V01.01
     & 255,   0,  63,   2, 243,  63, 153, 147, 153, 255,                V01.01
     & 243,  51, 255,  63,   0, 201,  89, 191, 127, 115,                V01.01
     & 179,  63, 243, 127, 191, 179, 115, 147, 159,   0,                V01.01
     & 108, 159, 204,  57, 249,   0, 108,  57, 102, 187,                V01.01
     &   3, 142, 159, 174,   1, 187, 251, 243,   0, 247,                V01.01
     &   1, 155,  55, 147, 247,  63,  51,   0,  55,   1,                V01.01
     & 147, 247, 155,  55, 249,   1, 155,  55, 147, 245,                V01.01
     & 255, 243,   0, 247,   1, 155,  55, 147, 247,  55,                V01.01
     & 247,   1, 155,  55, 147, 244, 105, 201,   0, 147,                V01.01
     & 155,   3, 191, 252, 244,   3,  97,   0, 248,   1,                V01.01
     & 156,  56, 148, 248,  63,  51,   0, 243, 248,   1,                V01.01
     & 155,  56, 106, 154, 147,   0,  99, 195,   0, 159,                V01.01
     &   1, 142, 157, 174, 159, 186, 179,   3, 113,  82,                V01.01
     &   0, 191,   1, 174, 189, 206, 191,  63,  51,   0,                V01.01
     & 219,  54,   0, 120 ,195, 127, 175, 163,   0, 115,                V01.01
     & 211,  59,  51,   0, 152, 147,   0, 248, 243,   0,                V01.01
     &  56,   3, 107, 152, 203, 248,  59,  51,   0, 243,                V01.01
     & 248,   1, 155,  55, 155,   1,  55, 147, 247, 155,                V01.01
     &  60,  49,   0,  56,   1, 148, 248, 156,  56, 252,                V01.01
     & 241,   0, 248,   1, 156,  56, 148, 248,  59,  51,                V01.01
     &   0,  55,   3, 155, 250, 250,   1, 155,  57, 151,                V01.01
     &   3, 245, 147,  52, 123, 219,   0, 175, 165,   1,                V01.01
     & 195, 251, 243,   0,  59,  54,   1, 147, 246,  59,                V01.01
     & 147, 251,  59,  54,   0, 251, 246,   3, 195, 150,                V01.01
     &  99,  54,  59, 243,   0,  51, 251,  60,  55,   1,                V01.01
     & 148, 247,   0,  97, 145,   1, 244,   2, 252,  59,                V01.01
     & 251,  51, 243, 191,   1, 156,   3, 121, 150,   1,                V01.01
     & 179, 175, 163, 127,   3, 156,   1, 185, 150,   3,                V01.01
     & 115,  89,   3, 122, 153,   1, 184, 217,  63, 255,                V01.01
     &   0,  59, 251,   0,  55, 247,   0,  51, 243,   0/                V01.01
      END
