#include "simj_base.h"

#include <stdio.h>


// --- word 0
//			0 - e - error
//			1 - me - mult-bit error = 1
//			2 - cm - 1 correct single bit and not report, else report and don't correct
//			3 - dm - detection mode, 1 = err detection enabled during read
//			4 - bm - byte mode detection during write
//			5-8 - unused
//			9 - ps - 1= power ok
//			10-12 - module number
//			13-15 - module ID 001 = 128 KW module
// 
//	on ICB - DM, CM set, BM reset
//
//			for module 3696-4
//			8 = 1
//			13-15 = 111

#define MEM_STAT_E			0x8000
#define MEM_STAT_ME			0x4000
#define MEM_STAT_CM			0x2000
#define MEM_STAT_DM			0x1000
#define MEM_STAT_BM			0x0800
#define MEM_STAT_MT			0x0080
#define MEM_STAT_PS			0x0040
#define MEM_STAT_MOD_MASK	0x0038
#define MEM_STAT_MOD_SHIFT	3
#define MEM_STAT_ID_MASK	0x0007
#define MEM_STAT_ID_128KW_SS	1
#define MEM_STAT_ID_3696		7

typedef struct {
	SIMJ_U16 memory_plane_status[5];
} MEM_PLANE_STATUS;

//  -------- use 3586-3 memory 256 kw boards each with 2 128 kw module
//  -------- use 2 3696-4 memory 512 kw board  each with 4 128 kw modules
// static MEM_PLANE_STATUS mem_status_registers[8];
static MEM_PLANE_STATUS mem_status_registers[16];


// ===================================================================================================================
void memory_plane_init() {

	int j;

	for (j = 0; j < 8; j++) {
		mem_status_registers[j].memory_plane_status[0] =  MEM_STAT_DM | MEM_STAT_CM | MEM_STAT_MT | MEM_STAT_PS | MEM_STAT_ID_3696 |
															( ( j << MEM_STAT_MOD_SHIFT ) & MEM_STAT_MOD_MASK );
		mem_status_registers[j].memory_plane_status[1] = 0x0000;
		mem_status_registers[j].memory_plane_status[2] = 0x0000;
		mem_status_registers[j].memory_plane_status[3] = 0x0000;
		mem_status_registers[j].memory_plane_status[4] = 0x0000;
	}
	for (j = 8; j < 16; j++) {
		mem_status_registers[j].memory_plane_status[0] = 0x0000; // MEM_STAT_DM | MEM_STAT_CM | MEM_STAT_ID_128KW_SS | ((j << MEM_STAT_MOD_SHIFT) & MEM_STAT_MOD_MASK);
		mem_status_registers[j].memory_plane_status[1] = 0x0000;
		mem_status_registers[j].memory_plane_status[2] = 0x0000;
		mem_status_registers[j].memory_plane_status[3] = 0x0000;
		mem_status_registers[j].memory_plane_status[4] = 0x0000;
	}

}


// ===================================================================================================================
SIMJ_U16 memory_plane_RMPS(SIMJ_U16 first_reg, SIMJ_U16 second_reg) {

	SIMJ_U16 ema;
	SIMJ_U16 ma;
	SIMJ_U16 reg;

	SIMJ_U16 plane;

	// first reg
	//	0 - 10 not used
	// 11 -15 EMA - extended memory address

	// second reg
	// 0 - 10 not used
	// 11-13  memory plane status reg number
	// 14-15  ma - least 2 significant address bits

	ema = first_reg & 0x001f;
	ma = second_reg & 0x0003;
	reg = (second_reg & 0x00e0) >> 5;

	// --------calculate plane number (module number)
	// --------for now ignore modules 9-15....
	// plane = (((ema >> 3) & 0x0003) <<2) | ma;
	if (ema <= 7) {
		plane = ma;
	}
	else if (ema >= 8 && ema <= 0xf) {
		plane = 4 + ma;
	}
	else if (ema >= 0x10 && ema <= 0x17) {
		plane = 8 + ma;
	}
	else {
		plane = 12 + ma;
	}

	// --------DEBUG
	fprintf(stderr, " RMPS R: 0x%04x, Rv1: 0x%04x, ema: 0x%04x, ma: 0x%04x, reg: 0x%04x, plane: 0x%04x, value:  0x%04x\n",
		first_reg, second_reg, ema, ma, reg, plane, mem_status_registers[plane].memory_plane_status[reg]);
	// --------END DEBUG

	return mem_status_registers[plane].memory_plane_status[reg];

}