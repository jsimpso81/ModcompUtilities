#include <stdio.h>
#include <string.h>

#include "modcomp_utility_library.h"

unsigned char* from_can_code(unsigned int can_value) {

	const unsigned char* can = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:.$";
	unsigned char result1[2] = { 0, 0 };
	unsigned char result2[2] = { 0, 0 };
	unsigned char result3[2] = { 0, 0 };

	unsigned char finalresult[4] = { 0, 0, 0, 0 };


	unsigned int c1 = 0;
	unsigned int c2 = 0;
	unsigned int c3 = 0;
	unsigned int tmp = 0;



	tmp = can_value;
	c3 = tmp % 40;
	tmp /= 40;
	c2 = tmp % 40;
	tmp /= 40;
	c1 = tmp % 40;

	result1[0] = can[c1];
	result2[0] = can[c2];
	result3[0] = can[c3];

	strncat_s(finalresult, 4, result1, 1);
	strncat_s(finalresult, 4, result2, 1);
	strncat_s(finalresult, 4, result3, 1);


	return finalresult;

}