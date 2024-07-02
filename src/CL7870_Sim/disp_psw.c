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

#include <stdio.h>
#include <stdbool.h>


void disp_psw(FILE* io_unit, PSW loc_psw ) {

	fprintf( io_unit , " Proessor status word 0x%04x, GRP: %d, IM: %d, OM: %d, PRV: %d, OH: %d, N: %d, Z: %d, O: %d, C: %d\n",
		loc_psw.all, loc_psw.sep.grb, loc_psw.sep.im, loc_psw.sep.om, loc_psw.sep.prv,
		loc_psw.sep.oh, loc_psw.sep.cc_n, loc_psw.sep.cc_z, loc_psw.sep.cc_o, loc_psw.sep.cc_c);

}