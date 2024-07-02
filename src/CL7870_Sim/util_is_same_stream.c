// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		XXXXXX
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================


#include "simj_base.h"

#include <sys/stat.h>


bool util_is_same_stream(FILE* one, FILE* two) {

	bool retval = false;

	retval = (_fileno(one) == _fileno(two));

	// printf(" -- compare fileno  %s\n", (retval ? "Same" : "Diff"));

	return retval;

}