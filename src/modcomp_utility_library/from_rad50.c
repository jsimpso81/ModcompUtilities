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


#include <stdio.h>
#include <string.h>

#include "modcomp_utility_library.h"

// --------option TRUE for file name, FALSE for ascii strings and such.   Affects how
// --------the three values between letters and numbers are intepretted.
unsigned int from_rad50(unsigned int rad50_value, unsigned char* result_string, bool option) {

	const unsigned char* rad50_file =  " ABCDEFGHIJKLMNOPQRSTUVWXYZ$%*0123456789";
	const unsigned char* rad50_ascii = " ABCDEFGHIJKLMNOPQRSTUVWXYZ$.%0123456789";

	unsigned int c1 = 0;
	unsigned int c2 = 0;
	unsigned int c3 = 0;
	unsigned int tmp = 0;

	tmp = rad50_value;
	c3 = tmp % 40;
	tmp /= 40;
	c2 = tmp % 40;
	tmp /= 40;
	c1 = tmp % 40;

	if (option) {
		result_string[0] = rad50_file[c1];
		result_string[1] = rad50_file[c2];
		result_string[2] = rad50_file[c3];
	}
	else {
		result_string[0] = rad50_ascii[c1];
		result_string[1] = rad50_ascii[c2];
		result_string[2] = rad50_ascii[c3];
	}
	result_string[3] = 0;

	return 1;

}