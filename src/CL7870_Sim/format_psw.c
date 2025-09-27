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


void format_psw(char* disp_string, size_t string_len, PSW loc_psw) {

	sprintf_s(disp_string, string_len, " 0x%04x, GRP:%d, IM:%2d, OM:%2d, PRV:%d, OH:%d, N:%d, Z:%d, O:%d, C:%d",
		loc_psw.all, loc_psw.sep.grb, loc_psw.sep.im, loc_psw.sep.om, loc_psw.sep.prv,
		loc_psw.sep.oh, loc_psw.sep.cc_n, loc_psw.sep.cc_z, loc_psw.sep.cc_o, loc_psw.sep.cc_c);
	return;
}