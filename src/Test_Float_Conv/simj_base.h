// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			simj_base.h
//
//	Description:	Main definitions for simulator environment.  Defines base data types based on
//					host operating system and architecture.  Defines all other data types.
//					Defines callable procedures.  Defines global memory. 
//
//	Externally accessible routines:
// 
// Notes:
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

// =====================================================================================================================
// 
//  simj_base.h
//
//		Header file for Jim Simpson based emulators and software.
//		This contains mostly command and basic definitions.  It also contains
//		All platform specific items.
//
//		Copyright 2023, 2024  James A. Simpson.  
//
//
// =====================================================================================================================

// #pragma once

// =====================================================================================================================
// --------define operating system and host architecture
#define SIMJ_PLATFORM MSWINDOWS
#define SIMJ_ARCH X86

// =====================================================================================================================
// --------define platform and architecture specific includes and data types.
#if SIMJ_PLATFORM == MSWINDOWS

#include <windows.h>
#include <synchapi.h>

// ---------- simulator data types.
// -- 8 bit unsigned integer
#define SIMJ_U8	unsigned __int8
// -- 8 bit signed integer
#define SIMJ_S8	signed __int8
// -- 16 bit unsigned integer
#define SIMJ_U16 unsigned __int16
// -- 16 bit signed integer
#define SIMJ_S16 signed __int16
// -- 32 bit unsigned integer
#define SIMJ_U32 unsigned __int32
// -- 32 bit signed integer
#define SIMJ_S32 signed __int32
// -- 64 bit unsigned integer
#define SIMJ_U64 unsigned __int64
// -- 64 bit signed integer
#define SIMJ_S64 signed __int64
// -- 8 bit unsigned character
#define SIMJ_UC unsigned char
// -- 8 bit signed character
#define SIMJ_SC signed char
// -- 32 bit IEEE float
#define SIMJ_F32 float 
// -- 64 bit IEEE float
#define SIMJ_F64 double
// -- 32 bit Modcomp float storage
#define SIMJ_M32 SIMJ_U32
// -- 48 bit Modcomp float storage
#define SIMJ_M48 SIMJ_U64	// NEED TO USE 64 bits to store....
// -- 64 bit Modcomp float storage
#define SIMJ_M64 SIMJ_U64

#else
#error SIMJ  simj_base.h - Runtime platform not defined.   Compile aborted.
#endif

// =====================================================================================================================
// -------- includes always used.
#include <stdbool.h>
#include <stdio.h>

// =====================================================================================================================
// -------- simulator data types.
typedef union {
	SIMJ_U16 uval;
	SIMJ_S16 sval;
} VAL16;


typedef union {
	SIMJ_U32 uval;
	SIMJ_S32 sval;
	SIMJ_U16 zval[2];
} VAL32;

typedef union {
	SIMJ_U64 uval;
	SIMJ_S64 sval;
	SIMJ_U16 zval[4];
} VAL64;


// -------- util floating point conversions
// -------- convert 32 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S32_IEEE64(SIMJ_S32 s32_in, SIMJ_F64* f64_out);
// -------- convert 64 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S64_IEEE64(SIMJ_S64 s64_in, SIMJ_F64* f64_out);
// -------- convert 32 bit signed integer to Modcomp 32 bit float
SIMJ_U32 util_cvt_S32_MCS32(SIMJ_S32 s32_in, SIMJ_M32* m32_out);
// -------- convert 32 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S32_MCS64(SIMJ_S32 s32_in, SIMJ_M64* m64_out);
// -------- convert 64 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S64_MCS64(SIMJ_S64 s64_in, SIMJ_M64* m64_out);
// -------- convert IEEE 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_IEEE64_S32(SIMJ_F64 f64_in, SIMJ_S32* s32_out);
// -------- convert IEEE 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_IEEE64_S64(SIMJ_F32 f64_in, SIMJ_S64* s64_out);
// -------- convert Modcomp 32 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS32_S32(SIMJ_M32 m32_in, SIMJ_S32* s32_out);
// -------- convert Modcomp 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS64_S32(SIMJ_M64 m64_in, SIMJ_S32* s32_out);
// -------- convert Modcomp 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_MCS64_S64(SIMJ_M64 m64_in, SIMJ_S64* s64_out);
// -------- convert Modcomp 32 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS32_IEEE64(SIMJ_M32 m32_in, SIMJ_F64* f64_out);
// -------- convert Modcomp 64 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS64_IEEE64(SIMJ_M64 m64_in, SIMJ_F64* f64_out);
// -------- convert IEEE 64 bit float to Modcomp 32 bit float
SIMJ_U32 util_cvt_IEEE64_MCS32(SIMJ_F64 f64_in, SIMJ_M32* m32_out);
// -------- convert IEEE 64 bit float to Modcomp 64 bit float
SIMJ_U32 util_cvt_IEEE64_MCS64(SIMJ_F64 f64_in, SIMJ_M64* m64_out);



// =====================================================================================================================
