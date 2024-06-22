// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			device_null_salprep.h
//
//	Description:	contains code for the SALPREP (400 boot block) loader.
//
//	Externally accessible routines:
// 
// Notes:			This isn't currently functional.
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================


// -------- 400 boot block is 2 sectors ( 256 words )


SIMJ_U8 salprep_loc_size[] = {

0x04, 0x91, 0xFF, 0x6E };

SIMJ_U8 salprep_data[] = {

0xED, 0x20, 0x04, 0x91, 0xEE, 0x20, 0x04, 0x02, 0x65, 0x73, 0xFD, 0x47, 0xFE, 0x77, 0xFD, 0x37,
0xFE, 0x47, 0x7D, 0x33, 0x04, 0x0C, 0xF7, 0x03, 0x70, 0x73, 0x04, 0x05, 0x6C, 0x11, 0xEE, 0x10,
0x00, 0x00, 0x6D, 0xE7, 0x6C, 0xDD, 0x6C, 0xCC, 0x65, 0x6E, 0x6C, 0xBB, 0xE7, 0x50, 0x04, 0x67,
0x0C, 0xBA, 0x08, 0x8A, 0x6C, 0x99, 0x28, 0x84, 0x7D, 0x88, 0x04, 0x27, 0xE7, 0x50, 0x04, 0x67,
0xFE, 0xAE, 0x68, 0xBB, 0x60, 0xEF, 0x71, 0x93, 0x04, 0x1E, 0xE7, 0x00, 0x04, 0x15, 0x61, 0x8F,
0x71, 0x8E, 0x04, 0x32, 0xE7, 0x50, 0x04, 0x67, 0xE7, 0x50, 0x04, 0x67, 0xE7, 0x50, 0x04, 0x67,
0xE7, 0x00, 0x04, 0x15, 0x71, 0x8E, 0x04, 0x50, 0xE7, 0x50, 0x04, 0x67, 0x86, 0x00, 0x04, 0x40,
0x04, 0x40, 0x69, 0xA7, 0x69, 0x7A, 0x6D, 0xE7, 0x82, 0x00, 0x04, 0x10, 0xE7, 0x00, 0x04, 0x15,
0xEE, 0xA0, 0x00, 0x00, 0x76, 0x93, 0x04, 0x47, 0x6D, 0xEA, 0xE7, 0x00, 0x04, 0x15, 0xE7, 0x50,
0x04, 0x67, 0xC7, 0xE0, 0x04, 0x41, 0x04, 0x15, 0x04, 0x4D, 0xFE, 0xAE, 0x70, 0xEF, 0x04, 0x49,
0x71, 0x8E, 0x04, 0x5B, 0x6D, 0xA7, 0x76, 0x90, 0x04, 0x56, 0xF7, 0x03, 0xE7, 0x50, 0x04, 0x67,
0x7D, 0xCC, 0x04, 0x8F, 0xFF, 0x0A, 0x71, 0x8F, 0x04, 0x5E, 0xF7, 0x08, 0x61, 0x8C, 0x70, 0x8F,
0x04, 0x8F, 0xE7, 0x50, 0x04, 0x67, 0xE7, 0x50, 0x04, 0x67, 0xE7, 0x00, 0x04, 0x15, 0x71, 0x6E,
0x04, 0x86, 0x7D, 0xCC, 0x04, 0x8F, 0xE5, 0xF0, 0x04, 0x03, 0xFD, 0xAF, 0x6D, 0xCA, 0x60, 0xFF,
0xFD, 0x6F, 0x6D, 0x46, 0x29, 0x41, 0xC0, 0x40, 0x04, 0x03, 0x68, 0xC6, 0x60, 0xFF, 0xF8, 0xCF,
0x08, 0x3A, 0x61, 0x3E, 0x71, 0x3F, 0x04, 0x7D, 0xF7, 0x03, 0x71, 0x3D, 0x04, 0x8F, 0xE9, 0x60,
0x00, 0x06, 0x0C, 0x3A, 0x79, 0x3D, 0x04, 0x8F, 0x60, 0xDF, 0x0C, 0xDD, 0x60, 0xFF, 0xFD, 0xAF,
0x68, 0xCA, 0x76, 0xB8, 0x04, 0x8C, 0xFF, 0x05, 0x62, 0xB8, 0x68, 0xA7, 0xFF, 0x05, 0x00, 0x00,
0xF7, 0x00, 0x00, 0x00 };

SIMJ_U8 salprep_checksum[] = {
0x4D, 0x5D };


