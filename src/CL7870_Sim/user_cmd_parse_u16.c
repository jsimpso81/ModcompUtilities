#include "simj_base.h"

// ------- returns true if value is good.
bool user_cmd_parse_u16(char* in_str, SIMJ_U16* out_val, SIMJ_U16 min_val, SIMJ_U16 max_val ) {

	SIMJ_U16 parm_parse = 0;
	bool retval = false;

	if (sscanf_s(in_str, "%hi", &parm_parse) == 1) {
		// -------- see if too low
		if (parm_parse < min_val) {
			printf(" *** ERROR *** Parameter value must be less than or equal to %d, value is %d\n", min_val, parm_parse);
		}
		// -------- see if too high
		else if (parm_parse > max_val) {
			printf(" *** ERROR *** Parameter value must be greater than or equal to %d, value is %d\n", max_val, parm_parse);
		}
		// -------- just right
		else {
			retval = true;
		}
	}

	// -------- not a number
	else {
		printf(" *** ERROR *** Parameter value must be numeric. Value is: %s\n", in_str);
	}
	
	*out_val = parm_parse;
	return retval;

}