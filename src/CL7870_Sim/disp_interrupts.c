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

#define WORD_TO_BINARY( val)  \
  (( val ) & 0x8000 ? '1' : '0'), \
  (( val ) & 0x4000 ? '1' : '0'), \
  (( val ) & 0x2000 ? '1' : '0'), \
  (( val ) & 0x1000 ? '1' : '0'), \
  (( val ) & 0x0800 ? '1' : '0'), \
  (( val ) & 0x0400 ? '1' : '0'), \
  (( val ) & 0x0200 ? '1' : '0'), \
  (( val ) & 0x0100 ? '1' : '0'), \
  (( val ) & 0x0080 ? '1' : '0'), \
  (( val ) & 0x0040 ? '1' : '0'), \
  (( val ) & 0x0020 ? '1' : '0'), \
  (( val ) & 0x0010 ? '1' : '0'), \
  (( val ) & 0x0008 ? '1' : '0'), \
  (( val ) & 0x0004 ? '1' : '0'), \
  (( val ) & 0x0002 ? '1' : '0'), \
  (( val ) & 0x0001 ? '1' : '0')

void disp_interrupts(FILE* io_unit) {


	SIMJ_U16 act = 0;
	SIMJ_U16 req = 0;
	SIMJ_U16 ena = 0;
	SIMJ_U32 di_req = 0;
	SIMJ_U32 di_prc = 0;
	SIMJ_U32 si_req = 0;
	SIMJ_U32 si_prc = 0;

	// ------- get data on interrupts
	cpu_get_interrupt( &act, &req, &ena, &di_req, &di_prc, &si_req, &si_prc);

	fprintf(io_unit, "\n Interrupts\n");
	fprintf(io_unit, "                 0 1 2 3 4 5 6 7 8 9 A B C D E F  \n");
	fprintf(io_unit, "     Active      %c %c %c %c %c %c %c %c %c %c %c %c %c %c %c %c  \n", WORD_TO_BINARY(act));
	fprintf(io_unit, "     Request     %c %c %c %c %c %c %c %c %c %c %c %c %c %c %c %c\n", WORD_TO_BINARY(req));
	fprintf(io_unit, "     Enabled     %c %c %c %c %c %c %c %c %c %c %c %c %c %c %c %c\n", WORD_TO_BINARY(ena));
	fprintf(io_unit, "\n");
	fprintf(io_unit, "       DI interrupts  Requests %10d  Processed %10d\n", di_req, di_prc);
	fprintf(io_unit, "       SI interrupts  Requests %10d  Processed %10d\n", si_req, si_prc);
	fprintf(io_unit, "\n");

	return;
}