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
				cpu_cond_code_o = (COND_O); \
				cpu_overflow_hist = cpu_overflow_hist || cpu_cond_code_o;\
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
					SET_CC_C(false); \
				}\
				}

#define TEST_CC_N ( cpu_cond_code_n )
#define TEST_CC_NOT_N ( !cpu_cond_code_n )

#define TEST_CC_Z ( cpu_cond_code_z )
#define TEST_CC_NOT_Z ( !cpu_cond_code_z )

#define TEST_CC_O ( cpu_cond_code_o )
#define TEST_CC_NOT_O ( !cpu_cond_code_o )

#define TEST_CC_C ( cpu_cond_code_c )
#define TEST_CC_NOT_C ( !cpu_cond_code_c )


//	TODO: Verify these CC test macros !!!
// -------- less than or equal to
#define TEST_CC_LE (   cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) )
// -------- greater than
#define TEST_CC_GT ( !(cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) ))

// -------- greater than or equal to
#define TEST_CC_GE (cpu_cond_code_z || ( !cpu_cond_code_z && ( cpu_cond_code_n != cpu_cond_code_c ) ) )
// -------- less than
#define TEST_CC_LT (cpu_cond_code_n != cpu_cond_code_o) 

// -------- Not higher
#define TEST_CC_NH	( !cpu_cond_code_c || cpu_cond_code_z )
// -------- Higher than
#define TEST_CC_HI	( !(!cpu_cond_code_c || cpu_cond_code_z) )

