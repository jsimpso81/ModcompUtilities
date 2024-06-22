// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			cpu_macro_register_access.h
//
//	Description:	Contains macros used by the CPU module for accessing registers.
//
//	Externally accessible routines:
// 
// Notes:			
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

#pragma once

#define GET_REGISTER_VALUE( REG ) ( cpu_register.reg16[REG])
#define SET_REGISTER_VALUE( REG,VAL) {\
					cpu_register.reg16[REG] = VAL; \
					}

// TODO: Make sure endian things don't mess this up! -- THEY DO... NEED TO FIX....
#define GET_REGISTER_VALUE_DOUBLE( DREG ) (SIMJ_U32)( ( cpu_register.reg32[DREG] >> 16 ) | ( cpu_register.reg32[DREG] << 16 ))
#define SET_REGISTER_VALUE_DOUBLE( DREG,DVAL) {\
					cpu_register.reg32[DREG] = (SIMJ_U32)( ( (SIMJ_U32)DVAL >> 16 ) | ( (SIMJ_U32)DVAL << 16 ) ); \
					}
// TODO: Make sure endian things don't mess this up! -- THEY DO... NEED TO FIX....
#define GET_REGISTER_VALUE_QUAD( QREG )  (SIMJ_U64)( \
							( ( cpu_register.reg64[QREG] >> 48 ) & (SIMJ_U64)0x000000000000ffff ) | \
							( ( cpu_register.reg64[QREG] >> 16 ) & (SIMJ_U64)0x00000000ffff0000 ) | \
							( ( cpu_register.reg64[QREG] << 16 ) & (SIMJ_U64)0x0000ffff00000000 ) | \
							( ( cpu_register.reg64[QREG] << 48 ) & (SIMJ_U64)0xffff000000000000 ))


#define SET_REGISTER_VALUE_QUAD( QREG,QVAL) {\
						cpu_register.reg64[QREG] = (SIMJ_U64)( \
							( ( (SIMJ_U64)QVAL >> 48 ) & (SIMJ_U64)0x000000000000ffff ) | \
							( ( (SIMJ_U64)QVAL >> 16 ) & (SIMJ_U64)0x00000000ffff0000 ) | \
							( ( (SIMJ_U64)QVAL << 16 ) & (SIMJ_U64)0x0000ffff00000000 ) | \
							( ( (SIMJ_U64)QVAL << 48 ) & (SIMJ_U64)0xffff000000000000 )); \
					}

#define ISREGNUM_DOUBLE( REG )  ( ( REG & 0x1) == 0 )
#define ISREGNUM_QUAD( REG )  ( ( REG & 0x3) == 0 )

// ================================================================================================
