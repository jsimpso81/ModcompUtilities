#pragma once

// ==========================================================================================================
// CPU condition code macros

#define	SET_CC_N(COND_N) {\
				cpu_cond_code_n = (COND_N); \
				}

#define	SET_CC_Z(COND_Z) {\
				cpu_cond_code_z = (COND_Z); \
				}

#define	SET_CC_O(COND_O) {\
				cpu_overflow_hist = cpu_overflow_hist || cpu_cond_code_o;\
				cpu_cond_code_o = (COND_O); \
				}

#define	SET_CC_C(COND_C) {\
				cpu_cond_code_c = (COND_C); \
				}

// TODO: finish SET_CC_CHAR macro  -- lower case alpha?
#define SET_CC_CHAR( VAL ) {\
				if ( (( VAL ) & 0x00ff ) == 0x28 ) { \
					SET_CC_N( true ); \
					SET_CC_Z( false ); \
					SET_CC_O( false ); \
					SET_CC_C( false ); \
				} \
				else if (((VAL) & 0x00ff ) == 0x29 ) { \
					SET_CC_N( true ); \
					SET_CC_Z( false ); \
					SET_CC_O( false ); \
					SET_CC_C( true ); \
				} \
				else if (((VAL) & 0x00ff ) == 0x2a ) { \
					SET_CC_N( true ); \
					SET_CC_Z( false ); \
					SET_CC_O( true ); \
					SET_CC_C( false ); \
				} \
				else if (((VAL) & 0x00ff ) == 0x2b ) {\
					SET_CC_N( true );\
					SET_CC_Z( false );\
					SET_CC_O( true );\
					SET_CC_C( true );\
				}\
				else if (((VAL) & 0x00ff ) == 0x2c ) {\
					SET_CC_N( true );\
					SET_CC_Z( true );\
					SET_CC_O( false );\
					SET_CC_C( false );\
				}\
				else if (((VAL) & 0x00ff ) == 0x2d ) {\
					SET_CC_N( true );\
					SET_CC_Z( true );\
					SET_CC_O( false );\
					SET_CC_C( true );\
				}\
				else if (((VAL) & 0x00ff ) == 0x2e ) {\
					SET_CC_N( true );\
					SET_CC_Z( true );\
					SET_CC_O( true );\
					SET_CC_C( false );\
				}\
				else if (((VAL) & 0x00ff) == 0x2f) {\
					SET_CC_N(true); \
					SET_CC_Z(true); \
					SET_CC_O(true); \
					SET_CC_C(true); \
				}\
				else if ((((VAL) & 0x00ff) >= 0x41) && (((VAL) & 0x00ff) <= 0x5a)) {\
					SET_CC_N(false); \
					SET_CC_Z(false); \
					SET_CC_O(false); \
					SET_CC_C(false); \
				}\
				else if ((((VAL) & 0x00ff) >= 0x61) && (((VAL) & 0x00ff) <= 0x7a)) {\
					SET_CC_N(false); \
					SET_CC_Z(false); \
					SET_CC_O(false); \
					SET_CC_C(false); \
				}\
				else if ( (((VAL) & 0x00ff ) >= 0x30 ) && (((VAL) & 0x00ff) <= 0x39 ) ) {\
					SET_CC_N(false); \
					SET_CC_Z(true); \
					SET_CC_O(false); \
					SET_CC_C(true); \
				}\
				else if (((VAL) & 0x00ff ) == 0x20) { \
					SET_CC_N(false); \
					SET_CC_Z(false); \
					SET_CC_O(true); \
					SET_CC_C(true); \
				}\
				else {\
					SET_CC_N(false); \
					SET_CC_Z(false); \
					SET_CC_O(false); \
					SET_CC_C(true); \
				}\
				}


#define SET_CC_O_ADD( VAL1, VAL2, RES ) {\
		SET_CC_O( ((VAL1.uval & 0x8000) == (VAL2.uval & 0x8000)) && ((VAL1.uval & 0x8000) != (RES.uval & 0x8000)));\
		}

#define SET_CC_O_ADD_DOUBLE( VAL1, VAL2, RES ) {\
		SET_CC_O( ((VAL1.uval & 0x80000000) == (VAL2.uval & 0x80000000)) && ((VAL1.uval & 0x80000000) != (RES.uval & 0x80000000)));\
		}

#define SET_CC_O_ADD_QUAD( VAL1, VAL2, RES ) {\
		SET_CC_O( ((VAL1.uval & 0x8000000000000000) == (VAL2.uval & 0x8000000000000000)) && ((VAL1.uval & 0x8000000000000000) != (RES.uval & 0x8000000000000000)));\
		}



#define SET_CC_O_SUB( VAL1, SUBTRAHEND, RES ) {\
		SET_CC_O( ( (VAL1.uval & 0x8000) != (SUBTRAHEND.uval & 0x8000)) && ((SUBTRAHEND.uval & 0x8000) == (RES.uval & 0x8000)));\
		}

#define SET_CC_O_SUB_DOUBLE( VAL1, SUBTRAHEND, RES ) {\
		SET_CC_O( ( (VAL1.uval & 0x80000000) != (SUBTRAHEND.uval & 0x80000000)) && ((SUBTRAHEND.uval & 0x80000000) == (RES.uval & 0x80000000)));\
		}

// TODO: Not sure this is correct.
#define SET_CC_C_ADD( VAL1, VAL2, RES ) {\
		SET_CC_C( ( ( ISVAL16_NEG(VAL1) || ISVAL16_NEG(VAL2) ) && !ISVAL16_NEG(RES) ) || ( ISVAL16_NEG(VAL1) && ISVAL16_NEG(VAL2) ) );\
		}
// was SET_CC_C(!ISVAL16_NEG(VAL1) && !ISVAL16_NEG(VAL2) && ISVAL16_NEG(RES)); \


// TODO: Not sure this is correct.
#define SET_CC_C_ADD_DOUBLE( VAL1, VAL2, RES ) {\
		SET_CC_C( ( ( ISVAL32_NEG(VAL1) || ISVAL32_NEG(VAL2) ) && !ISVAL32_NEG(RES) ) || ( ISVAL32_NEG(VAL1) && ISVAL32_NEG(VAL2) ) );\
		}

// TODO: Not sure this is correct.
#define SET_CC_C_ADD_QUAD( VAL1, VAL2, RES ) {\
		SET_CC_C( ( ( ISVAL64_NEG(VAL1) || ISVAL64_NEG(VAL2) ) && !ISVAL64_NEG(RES) ) || ( ISVAL64_NEG(VAL1) && ISVAL64_NEG(VAL2) ) );\
		}

// TODO: Not sure this is correct.
#define SET_CC_C_SUB( VAL1, VAL2, RES ) {\
		SET_CC_C( ( ( ISVAL16_NEG(VAL1) || !ISVAL16_NEG(VAL2) ) && !ISVAL16_NEG(RES) ) || ( ISVAL16_NEG(VAL1) && !ISVAL16_NEG(VAL2) ) );\
		}
// was SET_CC_C(!ISVAL16_NEG(VAL1) && ISVAL16_NEG(VAL2) && ISVAL16_NEG(RES)); \

// TODO: Not sure this is correct.
#define SET_CC_C_SUB_DOUBLE( VAL1, VAL2, RES ) {\
		SET_CC_C( ( ( ISVAL32_NEG(VAL1) || !ISVAL32_NEG(VAL2) ) && !ISVAL32_NEG(RES) ) || ( ISVAL32_NEG(VAL1) && !ISVAL32_NEG(VAL2) ) );\
		}


#define TEST_CC_N ( cpu_cond_code_n )
#define TEST_CC_NOT_N ( !cpu_cond_code_n )

#define TEST_CC_Z ( cpu_cond_code_z )
#define TEST_CC_NOT_Z ( !cpu_cond_code_z )

#define TEST_CC_O ( cpu_cond_code_o )
#define TEST_CC_NOT_O ( !cpu_cond_code_o )

#define TEST_CC_C ( cpu_cond_code_c )
#define TEST_CC_NOT_C ( !cpu_cond_code_c )


// -------- less than or equal to
#define TEST_CC_LE (   cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) )
// -------- greater than
#define TEST_CC_GT ( !(cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) ))

// -------- greater than or equal to
#define TEST_CC_GE (cpu_cond_code_z || ( (!cpu_cond_code_z) && ( (!cpu_cond_code_n) != cpu_cond_code_o ) ) )  // CCZ or ( !CCZ AND ( !CCN XOR CCO ))
// -------- less than
#define TEST_CC_LT (cpu_cond_code_n != cpu_cond_code_o) 

// -------- Not higher
#define TEST_CC_NH	( !cpu_cond_code_c || cpu_cond_code_z )	

// -------- Higher than
#define TEST_CC_HI	( cpu_cond_code_c && (!cpu_cond_code_z) )

// ------- test 16 bit values for neg and zero conditions

// --------16 bit value is negative
#define ISVAL16_NEG( VAL_16 )  ((VAL_16.uval & 0x8000) != 0 )

// --------16 bit value is zero
#define ISVAL16_ZERO( VAL_16 )  ( VAL_16.uval == 0 )

// --------64 bit value is max negative
#define ISVAL16_MAXNEG( VAL_16 )  ( VAL_16.uval == 0x8000 )

// ------- test 32 bit values for neg and zero conditions

// --------32 bit value is negative
#define ISVAL32_NEG( VAL_32 )  ((VAL_32.uval & 0x80000000) != 0 )

// --------32 bit value is zero
#define ISVAL32_ZERO( VAL_32 )  ( VAL_32.uval == 0 )

// --------64 bit value is max negative
#define ISVAL32_MAXNEG( VAL_32 )  ( VAL_32.uval == 0x80000000 )

// ------- test 64 bit values for neg and zero conditions

// --------64 bit value is negative
#define ISVAL64_NEG( VAL_64 )  ((VAL_64.uval & 0x8000000000000000) != 0 )

// --------64 bit value is zero
#define ISVAL64_ZERO( VAL_64 )  ( VAL_64.uval == 0 )

// --------64 bit value is max negative
#define ISVAL64_MAXNEG( VAL_64 )  ( VAL_64.uval == 0x8000000000000000 )
