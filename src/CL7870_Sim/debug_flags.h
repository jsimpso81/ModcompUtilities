#pragma once

// ================================flags for debug CPU===================================================
// --------flags for debug

// ======== cpu
#define DEBUG_BRM				1
#define DEBUG_BRMI				1
#define DEBUG_BX__				0
#define DEBUG_CAR				0
#define DEBUG_CIES				0
#define DEBUG_CIR				0
#define DEBUG_CRI				0
#define DEBUG_CRR				0
#define DEBUG_CRSB				0
#define DEBUG_GET_VIRT_MEM		0
#define DEBUG_DI				0
#define DEBUG_DMPI				0
#define DEBUG_EXI_EXR			0
#define DEBUG_FLOAT_TRAP		0
#define DEBUG_HLT				0
#define DEBUG_ILL_INST			0
#define DEBUG_LBX				0
#define DEBUG_LDAM				0
#define DEBUG_LDVM				0
#define DEBUG_LDX				0
#define DEBUG_LIMP				0
#define DEBUG_LOMP				0
#define DEBUG_MBVV				0
#define DEBUG_MEM_ACCESS_TRAP	0
#define DEBUG_MEM_ACCESS_HALT	0
#define DEBUG_MMRB				0
#define DEBUG_MRBM				0
#define DEBUG_PLM				0
#define DEBUG_PREVENT_HLT		0
#define DEBUG_PRIV_INSTR_TRAP	0
#define DEBUG_PRIV_INSTR_HALT	0
#define DEBUG_PSM				0
#define DEBUG_RIA				0
#define DEBUG_RDI				1
#define DEBUG_RESTARTABLE		0
#define DEBUG_REX_TRAP			0
#define DEBUG_RMI				1
#define DEBUG_RMPS				0
#define DEBUG_SBX				0
#define DEBUG_SCRB				0
#define DEBUG_SGP				0
#define DEBUG_SI				0
#define DEBUG_SIOM				0
#define DEBUG_SLOWER_CPU		0
#define DEBUG_SLP				0
#define DEBUG_SOMP				0
#define DEBUG_SOOM				0
#define DEBUG_STAM				0
#define DEBUG_STVM				0
#define DEBUG_SZOM				0
#define DEBUG_SPR				0
#define DEBUG_SUP				0
#define DEBUG_UIT_TRAP			0
#define DEBUG_WIR				1
#define DEBUG_WMS				0
#define DEBUG_ZIMP				0
#define DEBUG_ZOMP				0

// ======== common device disc functions
#define DEBUG_DISC_COMMON		0

// ======== device disc_mh
// -------- DEFINE DEBUG LEVEL
// ---------1 - physical IO transfers
// ---------2 - commands and 1
// ---------3 - 1 and 2 and much more
// ---------4 ludicous mode.
#define DEBUG_DISC_MH			0

// ======== device null
#define DEBUG_NULL				0

// ======== device tape
#define DEBUG_TAPE				0

// ======== common tape functions
#define DEBUG_TAPE_COMMON		0 

// ======== IOP FUNCTIONS
#define DEBUG_IOP				0


