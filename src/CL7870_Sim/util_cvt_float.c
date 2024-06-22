// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			util_cvt_float.c
//
//	Description:	Routines to convert between integer, modcomp floating point format and
//					IEEE floating point format.
//
//	Externally accessible routines:
//					util_cvt_S32_IEEE64
//					util_cvt_S64_IEEE64
//					util_cvt_S32_MCS32
//					util_cvt_S32_MCS64
//					util_cvt_S64_MCS64
// 
//					util_cvt_IEEE64_S32
//					util_cvt_IEEE64_S64
//					util_cvt_MCS32_S32
//					util_cvt_MCS48_S32
//					util_cvt_MCS64_S32
//					util_cvt_MCS64_S64
//
//					util_cvt_MCS32_IEEE64
//					util_cvt_MCS48_IEEE64
//					util_cvt_MCS64_IEEE64
//
//					util_cvt_IEEE64_MCS32
//					util_cvt_IEEE64_MCS48
//					util_cvt_IEEE64_MCS64
//
//					util_cvt_MCS64_MCS32
//					util_cvt_MCS64_MCS48
//					util_cvt_MCS48_MCS64
//
// Notes:
// TODO: Need to finish status checking
// TODO: Need to finish modcomp number normalization before convert to IEEE
// TODO: Need to classify IEEE values before attempting to convert to modcomp
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Initial version
// ================================================================================================

#include "simj_base.h"

#include <math.h>

// ================================================================================================
// 
// Modcomp float32 format
//			0 - sign bit
//					
//			1 - 9 - exponent
//					unsigned biased binary number 0-511 (-256 to 255 )
//			10 - 31 - fraction (no hidden bit)
// 
//				value interpretation
//						exp = 0, frac = 0		0  
// TODO: finish modcomp format explanation.
//						?? exp = 255, frac != 0	NaN
//						?? exp = 255, frac = 0		+/- INF depending on sign
//						?? exp = 0, frac != 0		denormalized value =  (-1)^S x 2^(-126) x 0.fract
//						?? others					value =  (-1)^S x 2^(E-127) x 1.fract
//
//
//	
// IEEE float32 format
//			0 - sign bit
//					only applies to the fraction
//			1 - 8 - exponent
//					unsigned biased binary number 0-255 (-128 to 127? ), bias is 127
//			9 - 31 - fraction ( hidden bit)
// 
//				value interpretation
//						exp = 255, frac != 0	NaN
//						exp = 255, frac = 0		+/- INF depending on sign
//						exp = 0, frac = 0		0  (+/- zero depending on sign)
//						exp = 0, frac != 0		denormalized value =  (-1)^S x 2^(-126) x 0.fract
//						others					value =  (-1)^S x 2^(E-127) x 1.fract
// 
// IEEE float64 format
//			0 - sign bit
//					only applies to the fraction
//			1 - 11 - exponent
//					unsigned biased binary number 0-2047 (-1024 to 1023? ), bias is 1023
//			12 - 63 - fraction (with hidden bit)
// 
//				value interpretation
//						exp = 2047, frac != 0	NaN
//						exp = 2047, frac = 0	+/- INF depending on sign
//						exp = 0, frac = 0		0  (+/- zero depending on sign)
//						exp = 0, frac != 0		denormalized value =  (-1)^S x 2^(-1022) x 0.fract 
//						others					value =  (-1)^S x 2^(E-1023) x 1.fract
//
// 
// ================================================================================================

// -------- BIT FIELDS IN C ARE **NOT** PORTABLE OR GUARANTEED TO BE WHERE YOU THINK..  DONT USE.
typedef union {
	// struct {
	// 	SIMJ_U32 fract : 22;
	// 	SIMJ_U32 expnt : 9;
	// 	bool sign : 1;
	// } parts;
	SIMJ_M32	native;
	SIMJ_U32	all;
} MOD_FLOAT32;

#define MOD_FLT32_SIGN_MASK 0x80000000
#define MOD_FLT32_SIGN_SHIFT 31
#define MOD_FLT32_EXP_MASK  0x7fc00000
#define MOD_FLT32_EXP_SHIFT 22
#define MOD_FLT32_EXP_BIAS  256
#define MOD_FLT32_FRAC_MASK 0x003fffff
#define MOD_FLT32_FRAC_SHIFT 0

// -------- NOT REALLY SUPPORTING 48 bit modcomp floating.  Convert between M64.

typedef union {
	// struct {
	// 	SIMJ_U64 unued_fract : 16;
	// 	SIMJ_U64 fract : 38;
	// 	SIMJ_U32 expnt : 9;
	// 	bool sign : 1;
	// } parts;
	SIMJ_M48	native;		// really 64 bits in length
	SIMJ_U64	all;
} MOD_FLOAT48;


typedef union {
	// struct {
	// 	SIMJ_U64 fract : 54;
	// 	SIMJ_U32 expnt : 9;
	// 	bool sign : 1;
	// } parts;
	SIMJ_M64	native;
	SIMJ_U64	all;
} MOD_FLOAT64;

#define MOD_FLT64_SIGN_MASK 0x8000000000000000
#define MOD_FLT64_SIGN_SHIFT 63
#define MOD_FLT64_EXP_MASK  0x7fc0000000000000
#define MOD_FLT64_EXP_SHIFT 54
#define MOD_FLT64_EXP_BIAS  256		// includes hidden bit
#define MOD_FLT64_FRAC_MASK 0x003fffffffffffff
#define MOD_FLT64_FRAC_SHIFT 0

typedef union {
	// struct {
	// 	SIMJ_U32 fract : 20;
	// 	SIMJ_U32 expnt : 11;
	// 	bool sign : 1;
	// } parts;
	SIMJ_F32	native;
	SIMJ_U32	all;
} IEEE_FLOAT32;


typedef union {
	// struct {
	// 	SIMJ_U64 fract : 52;
	// 	SIMJ_U32 expnt : 11;
	// 	bool sign : 1;
	// } parts;
	SIMJ_F64	native;
	SIMJ_U64	all;
} IEEE_FLOAT64;

#define IEEE_FLT64_SIGN_MASK   0x8000000000000000
#define IEEE_FLT64_SIGN_SHIFT  63
#define IEEE_FLT64_EXP_MASK    0x7ff0000000000000
#define IEEE_FLT64_EXP_SHIFT   52
#define IEEE_FLT64_EXP_BIAS	   1023
#define IEEE_FLT64_FRAC_HIDDEN 0x0010000000000000
#define IEEE_FLT64_FRAC_MASK   0x000fffffffffffff
#define IEEE_FLT64_FRAC_SHIFT  0

#define CLASSIFY_IEEE64( DVAL ) {\
	numb_type = fpclassify(DVAL);\
	switch (numb_type) {\
		case FP_NORMAL:\
		case FP_ZERO:\
			status = SIMJ_FLTCVT_GOOD;\
			break;\
		case FP_NAN:\
		case FP_INFINITE:\
			status = SIMJ_FLTCVT_OVERFLOW;\
			break;\
		case FP_SUBNORMAL:\
			status = SIMJ_FLTCVT_OTHER_ERR;\
		break;\
	}\
}


// ================================================================================================
// -------- convert 32 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S32_IEEE64(SIMJ_S32 s32_in, SIMJ_F64* f64_out) {

	SIMJ_F64 temp;
	int numb_type;
	SIMJ_U32 status = SIMJ_FLTCVT_OTHER_ERR;

	temp = s32_in;	// -------- Let C do the conversion...

	CLASSIFY_IEEE64( temp );

	*f64_out = temp;

	return status;
}

// ================================================================================================
// -------- convert 64 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S64_IEEE64(SIMJ_S64 s64_in, SIMJ_F64* f64_out) {

	SIMJ_F64 temp;
	int numb_type;
	SIMJ_U32 status = SIMJ_FLTCVT_OTHER_ERR;

	// TODO: check for loss of accuracy...

	temp = s64_in;	// -------- Let C do the conversion...   There could be loss of value !!!

	CLASSIFY_IEEE64(temp);

	*f64_out = temp;

	return status;
}

// ================================================================================================
// -------- convert 32 bit signed integer to Modcomp 32 bit float
SIMJ_U32 util_cvt_S32_MCS32(SIMJ_S32 s32_in, SIMJ_M32* m32_out) {

	SIMJ_F64 temp;
	int numb_type;
	SIMJ_U32 status = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_M32 temp_m32;

	temp = s32_in;	// -------- Let C do the conversion to IEEE64...

	CLASSIFY_IEEE64(temp);

	status1 = util_cvt_IEEE64_MCS32(temp, &temp_m32);

	*m32_out = temp_m32;

	return (status > status1 ? status : status1);
}

// ================================================================================================
// -------- convert 32 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S32_MCS64(SIMJ_S32 s32_in, SIMJ_M64* m64_out) {

	SIMJ_F64 temp;
	int numb_type;
	SIMJ_U32 status = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_M64 temp_m64;

	temp = s32_in;	// -------- Let C do the conversion...
	CLASSIFY_IEEE64(temp);

	status1 = util_cvt_IEEE64_MCS64(temp, &temp_m64);

	*m64_out = temp_m64;

	return (status > status1 ? status : status1);
}

// ================================================================================================
// -------- convert 64 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S64_MCS64(SIMJ_S64 s64_in, SIMJ_M64* m64_out) {

	SIMJ_F64 temp;
	int numb_type;
	SIMJ_U32 status = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_M64 temp_m64;

	temp = s64_in;	// -------- Let C do the conversion...  Could be loss of value!!
	CLASSIFY_IEEE64(temp);

	status1 = util_cvt_IEEE64_MCS64(temp, &temp_m64);

	*m64_out = temp_m64;

	return (status > status1 ? status : status1);
}

// ================================================================================================
// -------- convert IEEE 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_IEEE64_S32(SIMJ_F64 f64_in, SIMJ_S32* s32_out) {

	SIMJ_S32 temp_s32;

	// TODO: check for loss of value

	temp_s32 = f64_in;		// Let c do the conversion.    Could be loss of value!

	*s32_out = temp_s32;

	return SIMJ_FLTCVT_GOOD;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_IEEE64_S64(SIMJ_F64 f64_in, SIMJ_S64* s64_out) {

	SIMJ_S64 temp_s64;

	// TODO: check for overflow.

	temp_s64 = f64_in;		// Let c do the conversion.    Could be loss of value!

	*s64_out = temp_s64;

	return SIMJ_FLTCVT_GOOD;
}

// ================================================================================================
// -------- convert Modcomp 32 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS32_S32(SIMJ_M32 m32_in, SIMJ_S32* s32_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status2 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_F64 temp_f64 = 0.0;
	SIMJ_S32 temp_s32 = 0;

	status1 = util_cvt_MCS32_IEEE64(m32_in, &temp_f64);
	status2 = util_cvt_IEEE64_S32(temp_f64, &temp_s32);

	*s32_out = temp_s32;

	return (status1 > status2 ? status1 : status2);
}

// ================================================================================================
// -------- convert Modcomp 48 bit float (double float) to signed 32 bit integer
SIMJ_U32 util_cvt_MCS48_S32(SIMJ_M48 m48_in, SIMJ_S32* s32_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status2 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_F64 temp_f64 = 0.0;
	SIMJ_S32 temp_s32 = 0;
	SIMJ_M64 temp_in64 = 0;

	util_cvt_MCS48_MCS64(m48_in, &temp_in64);	// status is always good.
	status1 = util_cvt_MCS64_IEEE64(temp_in64, &temp_f64);
	status2 = util_cvt_IEEE64_S32(temp_f64, &temp_s32);

	*s32_out = temp_s32;

	return (status1 > status2 ? status1 : status2);
}

// ================================================================================================
// -------- convert Modcomp 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS64_S32(SIMJ_M64 m64_in, SIMJ_S32* s32_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status2 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_F64 temp_f64 = 0.0;
	SIMJ_S32 temp_s32 = 0;

	status1 = util_cvt_MCS64_IEEE64(m64_in, &temp_f64);
	status2 = util_cvt_IEEE64_S32(temp_f64, &temp_s32);

	*s32_out = temp_s32;

	return (status1 > status2 ? status1 : status2);
}

// ================================================================================================
// -------- convert Modcomp 64 bit float to Modcomp 32 bit float
SIMJ_U32 util_cvt_MCS64_MCS32(SIMJ_M64 m64_in, SIMJ_M32* m32_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status2 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_F64 temp_f64 = 0.0;
	SIMJ_M32 temp_m32 = 0;

	// TODO: There are easier ways to do this convert!!!
	status1 = util_cvt_MCS64_IEEE64(m64_in, &temp_f64);
	status2 = util_cvt_IEEE64_MCS32(temp_f64, &temp_m32);

	*m32_out = temp_m32;

	return (status1 > status2 ? status1 : status2);
}


// ================================================================================================
// -------- convert Modcomp 64 bit float to Modcomp 48 bit float
SIMJ_U32 util_cvt_MCS64_MCS48(SIMJ_M64 m64_in, SIMJ_M48* m48_out) {

	SIMJ_M48 temp_m48 = 0;
	// SIMJ_U64 temp_remainder = 0;

	// --------only the fraction is different.
	temp_m48 = m64_in;
	temp_m48 &= 0xffffffffffff0000;		// remove low bits.   Should to round up!!!
	// TODO: Round up if remainder is half or more.

	*m48_out = temp_m48;

	return (SIMJ_FLTCVT_GOOD);
}

// ================================================================================================
// -------- convert Modcomp 48 bit float to Modcomp 64 bit float
SIMJ_U32 util_cvt_MCS48_MCS64(SIMJ_M48 m48_in, SIMJ_M64* m64_out) {

	SIMJ_M64 temp_in64 = 0;

	// --------extend fraction.
	// --------make into a 64 bit float value
	if ((m48_in & 0x8000000000000000) != 0) {
		temp_in64 = m48_in | 0x000000000000ffff;
	}
	else {
		temp_in64 = m48_in & 0xffffffffffff0000;
	}

	*m64_out = temp_in64;

	return (SIMJ_FLTCVT_GOOD);
}





// ================================================================================================
// -------- convert Modcomp 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_MCS64_S64(SIMJ_M64 m64_in, SIMJ_S64* s64_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32 status2 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_F64 temp_f64 = 0.0;
	SIMJ_S64 temp_s64 = 0;

	status1 = util_cvt_MCS64_IEEE64(m64_in, &temp_f64);
	status2 = util_cvt_IEEE64_S64(temp_f64, &temp_s64);

	*s64_out = temp_s64;

	return (status1 > status2 ? status1 : status2);
}

// ================================================================================================
// -------- convert Modcomp 32 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS32_IEEE64(SIMJ_M32 m32_in, SIMJ_F64* f64_out) {

	MOD_FLOAT32 inval = { .all = 0 };
	IEEE_FLOAT64 outval = { .all = 0 };

	bool IEEE_sign = false;
	bool MOD_sign = false;
	SIMJ_U64 IEEE_exp = 0;
	SIMJ_U64 MOD_exp = 0;
	SIMJ_U64 IEEE_frac = 0;
	SIMJ_U64 MOD_frac = 0;

	inval.native = m32_in;

	// printf(" inval all 0x%016lx\n", inval.all);

	if (inval.all == 0) {
		outval.all = 0;
	}
	else {

		// --------if negative, convert
		MOD_sign = (inval.all & MOD_FLT32_SIGN_MASK);
		if (MOD_sign) {
			inval.all--;
			inval.all = ~inval.all;
		}

		// TODO: Need to normalize fraction !!!
		// --------check to see if fraction is normalized....  If not, then normalize the value.
		MOD_exp = (inval.all & MOD_FLT32_EXP_MASK) >> MOD_FLT32_EXP_SHIFT;
		MOD_frac = inval.all & MOD_FLT32_FRAC_MASK;

		IEEE_sign = MOD_sign;
		IEEE_exp = MOD_exp - MOD_FLT32_EXP_BIAS + IEEE_FLT64_EXP_BIAS - 1;

		IEEE_frac = (MOD_frac << (IEEE_FLT64_EXP_SHIFT + 1 - MOD_FLT32_EXP_SHIFT)) & IEEE_FLT64_FRAC_MASK;

		outval.all = (IEEE_sign ? IEEE_FLT64_SIGN_MASK : 0) | ((IEEE_exp << IEEE_FLT64_EXP_SHIFT) & IEEE_FLT64_EXP_MASK) | IEEE_frac;

		// printf(" in  MOD  32  sign: %s, exp: %04llx  fract: %016llx \n", (MOD_sign ? "neg" : "pos"), MOD_exp, MOD_frac);
		// printf(" out IEEE 64  sign: %s, exp: %04llx  fract: %016llx \n", (IEEE_sign ? "neg" : "pos"), IEEE_exp, IEEE_frac);
	}

	*f64_out = outval.native;

	return SIMJ_FLTCVT_GOOD;
}

// ================================================================================================
// -------- convert Modcomp 48 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS48_IEEE64(SIMJ_M64 m64_in, SIMJ_F64* f64_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_M64 temp_m64 = 0;
	SIMJ_F64 temp_f64 = 0.0;

	util_cvt_MCS48_MCS64(m64_in, &temp_m64);
	status1 = util_cvt_MCS64_IEEE64(temp_m64, &temp_f64);

	*f64_out = temp_f64;

	return status1;
}


// ================================================================================================
// -------- convert Modcomp 64 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS64_IEEE64(SIMJ_M64 m64_in, SIMJ_F64* f64_out) {

	MOD_FLOAT64 inval = { .all = 0 };
	IEEE_FLOAT64 outval = { .all = 0 };

	bool IEEE_sign = false;
	bool MOD_sign = false;
	SIMJ_U64 IEEE_exp = 0;
	SIMJ_U64 MOD_exp = 0;
	SIMJ_U64 IEEE_frac = 0;
	SIMJ_U64 MOD_frac = 0;

	inval.native = m64_in;

	// printf(" inval all 0x%016llx\n", inval.all);

	if (inval.all == 0) {
		outval.all = 0;
	}
	else {

		// --------if negative, convert
		MOD_sign = (inval.all & MOD_FLT64_SIGN_MASK);
		if (MOD_sign) {
			inval.all--;
			inval.all = ~inval.all;
		}

		// TODO: Need to normalize fraction !!!
		// --------check to see if fraction is normalized....  If not, then normalize the value.
		MOD_exp = (inval.all & MOD_FLT64_EXP_MASK) >> MOD_FLT64_EXP_SHIFT;
		MOD_frac = inval.all & MOD_FLT64_FRAC_MASK;

		IEEE_sign = MOD_sign;
		IEEE_exp = MOD_exp - MOD_FLT64_EXP_BIAS + IEEE_FLT64_EXP_BIAS - 1;
		// 52 + 1 - 54
		// IEEE_frac = (MOD_frac << (IEEE_FLT64_EXP_SHIFT + 1 - MOD_FLT64_EXP_SHIFT)) & IEEE_FLT64_FRAC_MASK;
		IEEE_frac = (MOD_frac >> 1) & IEEE_FLT64_FRAC_MASK;
		if (MOD_frac & 0x0000000000000001) {
			IEEE_frac++;
		}

		outval.all = (IEEE_sign ? IEEE_FLT64_SIGN_MASK : 0) | ((IEEE_exp << IEEE_FLT64_EXP_SHIFT) & IEEE_FLT64_EXP_MASK) | IEEE_frac;

		// printf(" in  MOD  64  sign: %s, exp: %04llx  fract: %016llx \n", (MOD_sign ? "neg" : "pos"), MOD_exp, MOD_frac);
		// printf(" out IEEE 64  sign: %s, exp: %04llx  fract: %016llx \n", (IEEE_sign ? "neg" : "pos"), IEEE_exp, IEEE_frac);
	}

	*f64_out = outval.native;

	return SIMJ_FLTCVT_GOOD;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to Modcomp 32 bit float
SIMJ_U32 util_cvt_IEEE64_MCS32(SIMJ_F64 f64_in, SIMJ_M32* m32_out) {

	IEEE_FLOAT64 inval = { .all = 0 };
	MOD_FLOAT32 outval = { .all = 0 };

	bool IEEE_sign = false;
	bool MOD_sign = false;
	SIMJ_U64 IEEE_exp = 0;
	SIMJ_U64 MOD_exp = 0;
	SIMJ_U64 IEEE_frac = 0;
	SIMJ_U64 MOD_frac = 0;
	SIMJ_U64 MOD_lost_frac = 0;

	inval.native = f64_in;

	// printf(" inval all 0x%016llx\n", inval.all);

	// --------check for other special IEEE values.
	if (inval.all == 0) {
		outval.all = 0;
	}
	else {
		IEEE_sign = (inval.all & IEEE_FLT64_SIGN_MASK);
		IEEE_exp = (inval.all & IEEE_FLT64_EXP_MASK) >> IEEE_FLT64_EXP_SHIFT;
		IEEE_frac = inval.all & IEEE_FLT64_FRAC_MASK;

		MOD_sign = IEEE_sign;
		MOD_exp = IEEE_exp - IEEE_FLT64_EXP_BIAS + MOD_FLT32_EXP_BIAS + 1;	// need to fix this value
		// 52 + 1 - 22
		MOD_frac = ((IEEE_FLT64_FRAC_HIDDEN | IEEE_frac) >> (IEEE_FLT64_EXP_SHIFT + 1 - MOD_FLT32_EXP_SHIFT)) & MOD_FLT32_FRAC_MASK;
		// for lost fraction, use this to round fraction up if needed.
		MOD_lost_frac = IEEE_frac & 0x000000007fffffff;
		if (MOD_lost_frac > 0x0000000040000000) {
			// printf(" >>>>>>>> did round up <<<<<<<<\n");
			MOD_frac++;
		}

		outval.all = (SIMJ_U32)((((MOD_exp << MOD_FLT32_EXP_SHIFT) & MOD_FLT32_EXP_MASK) | MOD_frac) & 0x00000000ffffffff);
		if (MOD_sign) {
			outval.all = ~outval.all;
			outval.all++;
		}

		// printf(" in  IEEE 64  sign: %s, exp: %04llx  fract: %016llx \n", (IEEE_sign ? "neg" : "pos"), IEEE_exp, IEEE_frac);
		// printf(" out MOD  32  sign: %s, exp: %04llx  fract: %016llx \n", (MOD_sign ? "neg" : "pos"), MOD_exp, MOD_frac);
		// printf("     IEEE 64                  lost  fract  %016llx \n", MOD_lost_frac);

	}

	*m32_out = outval.native;

	return SIMJ_FLTCVT_GOOD;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to Modcomp 48 bit float
SIMJ_U32 util_cvt_IEEE64_MCS48(SIMJ_F64 f64_in, SIMJ_M48* m48_out) {

	SIMJ_U32 status1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_M64 temp_m64;
	SIMJ_M48 temp_m48;

	status1 = util_cvt_IEEE64_MCS64(f64_in, &temp_m64);
	util_cvt_MCS64_MCS48(temp_m64, &temp_m48);

	*m48_out = temp_m48;

	return status1;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to Modcomp 64 bit float
SIMJ_U32 util_cvt_IEEE64_MCS64(SIMJ_F64 f64_in, SIMJ_M64* m64_out) {

	IEEE_FLOAT64 inval = { .all = 0 };
	MOD_FLOAT64 outval = { .all = 0 };

	bool IEEE_sign = false;
	bool MOD_sign = false;
	SIMJ_U64 IEEE_exp = 0;
	SIMJ_U64 MOD_exp = 0;
	SIMJ_U64 IEEE_frac = 0;
	SIMJ_U64 MOD_frac = 0;

	inval.native = f64_in;

	// printf(" inval all 0x%016llx\n", inval.all);

	// --------check for other special IEEE values.
	if (inval.all == 0) {
		outval.all = 0;
	}
	else {
		IEEE_sign = (inval.all & IEEE_FLT64_SIGN_MASK);
		IEEE_exp = (inval.all & IEEE_FLT64_EXP_MASK) >> IEEE_FLT64_EXP_SHIFT;
		IEEE_frac = inval.all & IEEE_FLT64_FRAC_MASK;

		MOD_sign = IEEE_sign;
		MOD_exp = IEEE_exp - IEEE_FLT64_EXP_BIAS + MOD_FLT64_EXP_BIAS + 1;	// need to fix this value
		// 52 + 1 - 54
		// MOD_frac = ((IEEE_FLT64_FRAC_HIDDEN | IEEE_frac) >> (IEEE_FLT64_EXP_SHIFT + 1 - MOD_FLT64_EXP_SHIFT)) & MOD_FLT64_FRAC_MASK;
		MOD_frac = ((IEEE_FLT64_FRAC_HIDDEN | IEEE_frac) << 1) & MOD_FLT64_FRAC_MASK;
		// no lost fraction bits....

		outval.all = (SIMJ_U64)(((MOD_exp << MOD_FLT64_EXP_SHIFT) & MOD_FLT64_EXP_MASK) | MOD_frac);
		if (MOD_sign) {
			outval.all = ~outval.all;
			outval.all++;
		}

		// printf(" in  IEEE 64  sign: %s, exp: %04llx  fract: %016llx \n", (IEEE_sign ? "neg" : "pos"), IEEE_exp, IEEE_frac);
		// printf(" out MOD  64  sign: %s, exp: %04llx  fract: %016llx \n", (MOD_sign ? "neg" : "pos"), MOD_exp, MOD_frac);

	}

	*m64_out = outval.native;

	return SIMJ_FLTCVT_GOOD;
}
