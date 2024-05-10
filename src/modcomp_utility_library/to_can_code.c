 
#include "modcomp_utility_library.h"

unsigned int to_can_code(unsigned char* ascii_string) {

	unsigned int result = 0;

	unsigned int code1 = 0;
	unsigned int code2 = 0;
	unsigned int code3 = 0;

	code1 = get_can_index( (unsigned int)ascii_string[0] );
	if (code1 < 40)
		result += code1;
	result *= 40;
	code2 = get_can_index((unsigned int)ascii_string[1] );
	if (code2 < 40)
		result += code2;
	result *= 40;
	code3 = get_can_index((unsigned int)ascii_string[2] );
	if (code3 < 40)
		result += code3;

	return result;

}