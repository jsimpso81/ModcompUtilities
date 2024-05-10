#include <stdio.h>
#include <string.h>

#include "modcomp_utility_library.h"

unsigned char* from_can_code(unsigned int can_value, unsigned char* result_string) {

	const unsigned char* can = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:.$";

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

	result_string[0] = can[c1];
	result_string[1] = can[c2];
	result_string[2] = can[c3];
	result_string[3] = 0;

	return result_string;

}