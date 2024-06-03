#pragma once



#define GET_REGISTER_VALUE( REG ) ( cpu_register[cpu_register_block].reg16[REG])
#define SET_REGISTER_VALUE( REG,VAL) {\
					cpu_register[cpu_register_block].reg16[REG] = VAL; \
					}

// TODO: Make sure endian things don't mess this up! -- THEY DO... NEED TO FIX....
#define GET_REGISTER_VALUE_DOUBLE( DREG ) (unsigned __int32)( ( cpu_register[cpu_register_block].reg32[DREG] >> 16 ) | ( cpu_register[cpu_register_block].reg32[DREG] << 16 ))
#define SET_REGISTER_VALUE_DOUBLE( DREG,DVAL) {\
					cpu_register[cpu_register_block].reg32[DREG] = (unsigned __int32)( ( (unsigned __int32)DVAL >> 16 ) | ( (unsigned __int32)DVAL << 16 ) ); \
					}
// TODO: Make sure endian things don't mess this up!
#define GET_REGISTER_VALUE_QUAD( QREG ) ( cpu_register[cpu_register_block].reg64[QREG])
#define SET_REGISTER_VALUE_QUAD( QREG,QVAL) {\
					cpu_register[cpu_register_block].reg64[QREG] = QVAL; \
					}
