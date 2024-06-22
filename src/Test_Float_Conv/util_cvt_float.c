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
//					util_cvt_MCS64_S32
//					util_cvt_MCS64_S64
//
//					util_cvt_MCS32_IEEE64
//					util_cvt_MCS64_IEEE64
//
//					util_cvt_IEEE64_MCS32
//					util_cvt_IEEE64_MCS64
//
// Notes:
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Initial version
// ================================================================================================

#include "simj_base.h"


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

typedef union {
	SIMJ_M32	native;
	struct {
		SIMJ_U32 fract : 22;
		SIMJ_U16 exp : 9;
		bool sign : 1;
	} parts;
} MOD_FLOAT32;

typedef union {
	SIMJ_M48	native;		// really 64 bits in length
	struct {
		SIMJ_U64 unued_fract : 16;
		SIMJ_U64 fract : 38;
		SIMJ_U16 exp : 9;
		bool sign : 1;
	} parts;
} MOD_FLOAT48;

typedef union {
	SIMJ_M64	native;
	struct {
		SIMJ_U64 fract : 54;
		SIMJ_U16 exp : 9;
		bool sign : 1;
	} parts;
} MOD_FLOAT64;

typedef union {
	SIMJ_F32	native;
	struct {
		SIMJ_U64 fract : 52;
		SIMJ_U16 exp : 11;
		bool sign : 1;
	} parts;
} IEEE_FLOAT32;


typedef union {
	SIMJ_F64	native;
	struct {
		SIMJ_U64 fract : 52;
		SIMJ_U16 exp : 11;
		bool sign : 1;
	} parts;
} IEEE_FLOAT64;

// ================================================================================================
// -------- convert 32 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S32_IEEE64(SIMJ_S32 s32_in, SIMJ_F64* f64_out) {
	printf("\n *** ERROR *** util_cvt_S32_IEEE64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert 64 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S64_IEEE64(SIMJ_S64 s64_in, SIMJ_F64* f64_out) {
	printf("\n *** ERROR *** util_cvt_S64_IEEE64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert 32 bit signed integer to Modcomp 32 bit float
SIMJ_U32 util_cvt_S32_MCS32(SIMJ_S32 s32_in, SIMJ_M32* m32_out) {
	printf("\n *** ERROR *** util_cvt_S32_MCS32 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert 32 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S32_MCS64(SIMJ_S32 s32_in, SIMJ_M64* m64_out) {
	printf("\n *** ERROR *** util_cvt_S32_MCS64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert 64 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S64_MCS64(SIMJ_S64 s64_in, SIMJ_M64* m64_out) {
	printf("\n *** ERROR *** util_cvt_S64_MCS64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_IEEE64_S32(SIMJ_F64 f64_in, SIMJ_S32* s32_out) {
	printf("\n *** ERROR *** util_cvt_IEEE64_S32 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_IEEE64_S64(SIMJ_F32 f64_in, SIMJ_S64* s64_out) {
	printf("\n *** ERROR *** util_cvt_IEEE64_S64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert Modcomp 32 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS32_S32(SIMJ_M32 m32_in, SIMJ_S32* s32_out) {
	printf("\n *** ERROR *** util_cvt_MCS32_S32 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert Modcomp 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS64_S32(SIMJ_M64 m64_in, SIMJ_S32* s32_out) {
	printf("\n *** ERROR *** util_cvt_MCS64_S32 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert Modcomp 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_MCS64_S64(SIMJ_M64 m64_in, SIMJ_S64* s64_out) {
	printf("\n *** ERROR *** util_cvt_MCS64_S64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert Modcomp 32 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS32_IEEE64(SIMJ_M32 m32_in, SIMJ_F64* f64_out) {
	printf("\n *** ERROR *** util_cvt_MCS32_IEEE64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert Modcomp 64 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS64_IEEE64(SIMJ_M64 m64_in, SIMJ_F64* f64_out) {
	printf("\n *** ERROR *** util_cvt_MCS64_IEEE64 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to Modcomp 32 bit float
SIMJ_U32 util_cvt_IEEE64_MCS32(SIMJ_F64 f64_in, SIMJ_M32* m32_out) {
	printf("\n *** ERROR *** util_cvt_IEEE64_MCS32 ---- NOT DONE \n");
	return 0xffffffff;
}

// ================================================================================================
// -------- convert IEEE 64 bit float to Modcomp 64 bit float
SIMJ_U32 util_cvt_IEEE64_MCS64(SIMJ_F64 f64_in, SIMJ_M64* m64_out) {
	printf("\n *** ERROR *** util_cvt_IEEE64_MCS64 ---- NOT DONE \n");
	return 0xffffffff;
}




/* ------------------------------------------------------------------------- */
/*--------convert 4 byte vax floating point number into modcomp floating point */
/*--------NOT REENTRANT */
unsigned char* mkmodcompfloat(float vaxfloat)
{

	union {
		float flt;
		unsigned char chr[4];
	} junk = { .flt = 0 };

	unsigned char retjunk[4] = { 0 };

	int mantsign;
	int mantissa;
	int exponent;

	unsigned long int signfix;

	/*--------copy value to internal variable */
	junk.flt = vaxfloat;


	/*=============================================================================================*/
	/*--------get the parts of the vax floating point number */

	exponent = 0;
	mantsign = 0;
	mantissa = 0;

	/*-------- excess 128 exponent 0-255 really equals -128-127 */
	exponent = ((junk.chr[1] & 0x7f) << 1) |
		((junk.chr[0] & 0x80) >> 7);

	/*-------- mantissa sign */
	if (junk.chr[1] & 0x80)
		mantsign = 1;
	else
		mantsign = 0;

	/*-------- mantissa */
	mantissa = ((junk.chr[0] & 0x7f) << 16) |
		(junk.chr[3] << 8) | (junk.chr[2]);

	/*printf("\n-------------------------------------------------");
	/*printf("\n vax float: %10f",junk.flt);
	/*printf("\n vax float: %2.2x %2.2x %2.2x %2.2x",
	/*			junk.chr[1],junk.chr[0],
	/*			junk.chr[3],junk.chr[2]);*/


	/*=============================================================================================*/
	/*--------check for special cases */

	/*--------check for a zero value */
	if (mantsign == 0 && exponent == 0) {
		/*printf("\n zero value");*/
		retjunk[0] = 0;
		retjunk[1] = 0;
		retjunk[2] = 0;
		retjunk[3] = 0;
		/*printf("\n mod float: %2.2x %2.2x %2.2x %2.2x",
		/*	retjunk[0],retjunk[1],retjunk[2],retjunk[3]); */
		return(retjunk);
	}

	/*--------check for a bad value -- reserved operand fault */
	if (mantsign == 1 && exponent == 0) {
		/*printf("\n bad float value ");*/
		retjunk[0] = 0;
		retjunk[1] = 0;
		retjunk[2] = 0;
		retjunk[3] = 0;
		/*printf("\n mod float: %2.2x %2.2x %2.2x %2.2x",
		/*	retjunk[0],retjunk[1],retjunk[2],retjunk[3]);*/
		return(retjunk);
	}


	/*=============================================================================================*/
	/*--------massage the numbers a little -- get real numbers from float d hidden things */

	/*--------make the exponent the true number */
	exponent = exponent - 128;

	/*--------add the hidden mantissa bit */
	mantissa = mantissa | 0x00800000;

	/*printf("\n exponent : %10d %8.8x",exponent,exponent);
	/*printf("\n mantsign : %10d %8.8x",mantsign,mantsign);
	/*printf("\n mantissa : %10d %8.8x",mantissa,mantissa);*/

	/*=============================================================================================*/
	/*--------massage the numbers a little -- initial massage for modcomp */

	/*--------make the exponent a 0-511 number */
	exponent = exponent + 256;

	/*=============================================================================================*/
	/*--------make modcomp value */


		/* --------make modcomp floating point number */
	retjunk[0] = ((mantsign << 7) & 0x80) | ((exponent >> 2) & 0x7f);
	retjunk[1] = ((exponent << 6) & 0xc0) | ((mantissa >> 18) & 0x3f);
	retjunk[2] = ((mantissa >> 10) & 0xff);
	retjunk[3] = ((mantissa >> 2) & 0xff);

	/*printf("\n mod float: %2.2x %2.2x %2.2x %2.2x",
	/*	retjunk[0],retjunk[1],retjunk[2],retjunk[3]); */

	/*--------if the sign bit is negative then the entire number is two's comp !!! */
	if (mantsign) {
		signfix = retjunk[0] << 24 |
			retjunk[1] << 16 |
			retjunk[2] << 8 |
			retjunk[3];
		/*printf("\n sign fix %x",signfix);*/
		signfix = ~signfix;
		/*printf("\n sign fix %x",signfix);*/
		signfix++;
		/*printf("\n sign fix %x",signfix);*/
		retjunk[0] = (0xff & (signfix >> 24)) | 0x80;
		retjunk[1] = 0xff & (signfix >> 16);
		retjunk[2] = 0xff & (signfix >> 8);
		retjunk[3] = 0xff & signfix;

		/*printf("\n mod fixed: %2.2x %2.2x %2.2x %2.2x",
		/*	retjunk[0],retjunk[1],retjunk[2],retjunk[3]);*/
	}

	return(retjunk);
}

