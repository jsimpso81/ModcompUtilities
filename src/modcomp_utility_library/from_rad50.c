// C-- - RAD50.FOR   CHANGE RAD50 DATA INTO ASCII
// SUBROUTINE RAD50(IN, OUT1, OUT2, OUT3)
// IMPLICIT INTEGER * 2 (A - Z)
// INTEGER * 4 YYY, FRTY, MOD, XXX, IOR, K256
// DIMENSION INX(40)
// DATA FRTY / 40 /
// DATA K256 / 256 /
// DATA INX / '  ', 'A ', 'B ', 'C ', 'D ', 'E ', 'F ', 'G ',
//     :           'H ', 'I ', 'J ', 'K ', 'L ', 'M ', 'N ', 'O ',
//     : 'P ', 'Q ', 'R ', 'S ', 'T ', 'U ', 'V ', 'W ',
//     : 'X ', 'Y ', 'Z ', '? ', '? ', '? ', '0 ', '1 ',
//     : '2 ', '3 ', '4 ', '5 ', '6 ', '7 ', '8 ', '9 ' /
//     CALL INTLEN(.FALSE.)
//     XX = IN
//     YY = IN
//     XX = IAND(XX, 255)
//     YY = ISHFT(YY, -8)
//     XXX = XX
//     XXX = XXX * K256
//     YYY = YY
//     YYY = IOR(XXX, YYY)
//     XXX = MOD(YYY, FRTY)
//     XX = XXX
//     IF(XX.LT. 0.OR.XX.GT. 39) XX = 0
//     OUT3 = INX(XX + 1)
//     YYY = YYY / FRTY
//     XXX = MOD(YYY, FRTY)
//     XX = XXX
//     IF(XX.LT. 0.OR.XX.GT. 39) XX = 0
//     OUT2 = INX(XX + 1)
//     XXX = YYY / FRTY
//     XX = XXX
//     IF(XX.LT. 0.OR.XX.GT. 39) XX = 0
//     OUT1 = INX(XX + 1)
//     RETURN
//     END
