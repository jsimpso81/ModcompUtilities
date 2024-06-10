
#include "simj_base.h"

#include <sys/stat.h>


bool util_is_same_stream(FILE* one, FILE* two) {

	bool retval = false;

	retval = (_fileno(one) == _fileno(two));

	// printf(" -- compare fileno  %s\n", (retval ? "Same" : "Diff"));

	return retval;

}