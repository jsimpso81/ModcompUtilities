// =====================================================================================================================
// 
//  simj_base.h
//
//		Header file for Jim Simpson based emulators and softer.
//		This contains mostly command and basic definitions.  It also contains
//		All platform specific items.
//
//		Copyright 2023, 2024  James A. Simpson.  
//
//
// =====================================================================================================================

#pragma once

#define SIMJ_PLATFORM MSWINDOWS

#ifdef SIMJ_PLATFORM

#include <windows.h>
#include <synchapi.h>

#define SIMJ_U8	unsigned __int8
#define SIMJ_S8	signed __int8
#define SIMJ_U16 unsigned __int16
#define SIMJ_S16 signed __int16
#define SIMJ_U32 unsigned __int32
#define SIMJ_S32 signed __int32
#define SIMJ_U64 unsigned __int64
#define SIMJ_S64 signed __int64
#define SIMJ_UC unsigned char
#define SIMJ_SC signed char

#else
#error SIMJ  simj_base.h - Runtime platform not defined.   Compile aborted.
#endif

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"

