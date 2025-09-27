// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			modcomp_opcode_string.h
//
//	Description:	Strings for Modcomp classic 16 bit opcodes.
//
//	Externally accessible routines:
// 
// Notes:			This header only includes strings for opcodes that have a unique instruction
//					for the first byte of the instruction.  Other opcode strings are assigned
//					in the routine util_get_opcode_disp.
//	
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

#pragma once

#ifndef SIMJ_MAIN
#define SIMJ_SCOPE extern 
#define SIMJ_INIT( A )
#else
#define SIMJ_SCOPE
#define SIMJ_INIT( A )  = A
#endif



#include "modcomp_opcodes.h"

#ifndef SIMJ_MAIN
//extern char opcode_string[NUMB_OP][20];
char opcode_si_string[NUMB_OPSI][20];
#else
/* --------obsolete 
char opcode_string[NUMB_OP][20] = {
	"HLT",
	"AUG01",
	"LXR",
	"RMPS_RMWS",
	"WMS",
	"DMPI",
	"MMRB",
	"MRBM",
	"MBR",
	"MBL",
	"IBR",
	"MUR",
	"MLR",
	"TOR",
	"AUG0E",
	"LRS",

"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",



"MPR",
"DVR",
"DAR",
"REX",
"CAR",
"CIR",
"SIA_SIE_SIR",
"RIA_RIE_RIR",
"RLD_RLQ",
"RLS",
"RAD_RAQ",
"RAS",
"LLD_LLQ",
"LLS",
"LAD_LAQ",
"LAS",

"FAR_CDIF",
"FSR",
"FMR",
"FDR",
"FARD_FARQ_CFDI",
"FSRD_FSRQ_CQFF",
"FMRD_FMRQ_CDFI",
"FDRD_FDRQ_CQFI",
"FAM_FAI",
"FSM_FSI",
"FMM_FMI",
"FDM_FDI",
"FAMD_FAMQ_FAID_FAIQ",
"FSMD_FSMQ_FSID_FSIQ",
"FMMD_FMMQ_FMID_FMIQ",
"FDMD_FDMQ_FDID_FDIQ",

"OCA",
"OCB",
"OCC",
"OCD",
"ODA",
"ODB",
"ODC",
"ODD",
"ISA",
"ISB",
"ISC",
"ISD",
"IDA",
"IDB",
"IDC",
"IDD",

"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",




"ABR",
"SBR",
"ZBR",
"OBR",
"XBR",
"LBR",
"TBR",
"GMR",
"ADR",
"SUR",
"ETR",
"ORR",
"XOR",
"TRR",
"CRR",
"TTR",

"ABRB",
"SBRB",
"ZBRB",
"OBRB",
"XBRB",
"LBRB",
"TBRB",
"GMRB",
"ADRB",
"SURB",
"ETRB",
"ORRB",
"XORB",
"TRRB",
"TERB",
"TTRB",

"ABMM",
"ZBMM",
"OBMM",
"TBMM",
"ABMB",
"ZBMB",
"TBMB",
"CBMB",
"LDXT_STXT_DMT_STMT",
"NOP",
"IRRD_TTRD",
"CRRT_CRRQ_TTRQ",
"ESD_ESS",
"TRRQ_LDXD",
"CRXD_STXD",
"AUG8F",

"ABSM",
"ZBSM",
"OBSM",
"TBSM",
"ABSB",
"ZBSB",
"TBSB",
"CBSB",
"ABXM",
"ZBXM",
"OBXM",
"TBXM",
"ABXB",
"ZBXB",
"TBXB",
"CBXB",

"MPM",
"DVM",
"MPRD_MPMD",
"DVRD_DVMD",
"LFM",
"SFM",
"HHI_HNH",
"AUGA7",
"HNS_HNR",
"HZS_HZR",
"HOS_HOR",
"HCS_HCR",
"HLS_HGE",
"HLE_HGT",
"LBX",
"SBX",

"MPS",
"DVS",
"SCCC",
"EXR",
"LFS",
"SFS",
"IRM",
"IRR",
"MPX",
"DVX",
"PLM",
"PSM",
"LFX",
"SFX",
"LDAM_LDVM",
"STAM_STVM",

"ADMM",
"ETMM",
"ORMM",
"CRM",
"ADMB",
"ETMB",
"TRMB",
"CRMB",
"ADRD_ADMD",
"SURD_SUMD",
"AUGCA",
"UIT",
"TSBM",
"TRRD_LDMD",
"CLM_STMD_CLMD",
"CRRD_CRMD",

"ADSM",
"ETSM",
"ORSM",
"CRS",
"ADSB",
"ETSB",
"TRSB",
"CRSB",
"ADXM",
"ETXM",
"ORXM",
"CRX",
"ADXB",
"ETXB",
"TRXB",
"CRXB",

"ADM",
"SUM",
"ETM",
"ORM",
"XOM",
"LDM",
"STM",
"BRU_BLM",
"AUGE8",
"SUI_CRI",
"ETI_TETI",
"ORI_TORI",
"XOI_TXOI",
"LDI_LDF_LDFD_FDFQ",
"STI",
"BLI",

"ADS",
"SUS",
"ETS",
"ORS",
"XOS",
"LDS",
"STS",
"HOP_BLT",
"ADX",
"SUX",
"ETX",
"ORX",
"XOX",
"LDX",
"STX",
"BRX_BLX"

};
*/

// --------separated opcodes..
char opcode_si_string[NUMB_OPSI][20] = {
	"HLT",
	"AUG01",
	"LXR",
	"RMPS",
	"WMS",
	"DMPI",
	"MMRB",
	"MRBM",
	"MBR",
	"MBL",
	"IBR",
	"MUR",
	"MLR",
	"TOR",
	"AUG0E",
	"LRS",

"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",

"MPR",
"DVR",
"DAR",
"REX",
"CAR",
"CIR",
"SIA",
"RIA",
"RLD",
"RLS",
"RAD",
"RAS",
"LLD",
"LLS",
"LAD",
"LAS",

"FAR",
"FSR",
"FMR",
"FDR",
"FARD",
"FSRD",
"FMRD",
"FDRD",
"FAM",
"FSM",
"FMM",
"FDM",
"FAMD",
"FSMD",
"FMMD",
"FDMD",

"OCA",
"OCB",
"OCC",
"OCD",
"ODA",
"ODB",
"ODC",
"ODD",
"ISA",
"ISB",
"ISC",
"ISD",
"IDA",
"IDB",
"IDC",
"IDD",

"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",
"RESRVD",

"ABR",
"SBR",
"ZBR",
"OBR",
"XBR",
"LBR",
"TBR",
"GMR",
"ADR",
"SUR",
"ETR",
"ORR",
"XOR",
"TRR",
"CRR",
"TTR",

"ABRB",
"SBRB",
"ZBRB",
"OBRB",
"XBRB",
"LBRB",
"TBRB",
"GMRB",
"ADRB",
"SURB",
"ETRB",
"ORRB",
"XORB",
"TRRB",
"TERB",
"TTRB",

"ABMM",
"ZBMM",
"OBMM",
"TBMM",
"ABMB",
"ZBMB",
"TBMB",
"CBMB",
"LDXT",
"NOP",
"IRRD",
"CRRT",
"ESD",
"TRRQ",
"CRXD",
"AUG8F",

"ABSM",
"ZBSM",
"OBSM",
"TBSM",
"ABSB",
"ZBSB",
"TBSB",
"CBSB",
"ABXM",
"ZBXM",
"OBXM",
"TBXM",
"ABXB",
"ZBXB",
"TBXB",
"CBXB",

"MPM",
"DVM",
"MPRD",
"DVRD",
"LFM",
"SFM",
"HHI",
"AUGA7",
"HNS",
"HZS",
"HOS",
"HCS",
"HLS",
"HLE",
"LBX",
"SBX",

"MPS",
"DVS",
"SCCC",
"EXR",
"LFS",
"SFS",
"IRM",
"IRR",
"MPX",
"DVX",
"PLM",
"PSM",
"LFX",
"SFX",
"LDAM",
"STAM",

"ADMM",
"ETMM",
"ORMM",
"CRM",
"ADMB",
"ETMB",
"TRMB",
"CRMB",
"ADRD",
"SURD",
"AUGCA",
"UIT",
"TSBM",
"TRRD",
"CLM",
"CRRD",

"ADSM",
"ETSM",
"ORSM",
"CRS",
"ADSB",
"ETSB",
"TRSB",
"CRSB",
"ADXM",
"ETXM",
"ORXM",
"CRX",
"ADXB",
"ETXB",
"TRXB",
"CRXB",

"ADM",
"SUM",
"ETM",
"ORM",
"XOM",
"LDM",
"STM",
"BRU",
"AUGE8",
"SUI",
"ETI",
"ORI",
"XOI",
"LDI",
"STI",
"BLI",

"ADS",
"SUS",
"ETS",
"ORS",
"XOS",
"LDS",
"STS",
"HOP",
"ADX",
"SUX",
"ETX",
"ORX",
"XOX",
"LDX",
"STX",
"BRX",

// --------extra opcodes..
"SPR",      // 0x0100	
"SGP",      // 0x0101
"RMWS",      // 0x0102
"SLP",      // 0x0103
"SUP",      // 0x0104
"SIE",      // 0x0105
"SIR",      // 0x0106
"RIE",      // 0x0107
"RIR",      // 0x0108

"RLQ",      // 0x0109
"RAQ",      // 0x010a
"LLQ",      // 0x010b
"LAQ",      // 0x010c

"CDIF",      // 0x010d
"FARQ",      // 0x010e
"CFDI",      // 0x010f
"FSRQ",      // 0x0110
"CQFF",      // 0x0111
"FMRQ",      // 0x0112
"CDFI",      // 0x0113
"FDRQ",      // 0x0114
"CQFI",      // 0x0115
"FAI",      // 0x0116
"FSI",      // 0x0117
"FMI",      // 0x0118
"FDI",      // 0x0119
"FAMQ",      // 0x011a
"FAID",      // 0x011b
"FAIQ",      // 0x011c
"FSMQ",      // 0x011d
"FSID",      // 0x011e
"FSIQ",      // 0x011f
"FMMQ",      // 0x0120
"FMID",      // 0x0121
"FMIQ",      // 0x0122
"FDMQ",      // 0x0123
"FDID",      // 0x0124
"FDIQ",      // 0x0125

"STXT",      // 0x0126
"LDMT",      // 0x0127
"STMT",      // 0x0128
"TTRD",      // 0x0129
"CRRQ",      // 0x012a
"TTRQ",      // 0x012b
"ESS",      // 0x012c
"LDXD",      // 0x012d
"STXD",      // 0x012e

"MPMD",      // 0x012f
"DVMD",      // 0x0130
"HNH",      // 0x0131
"HNR",      // 0x0132
"HZR",      // 0x0133
"HOR",      // 0x0134
"HCR",      // 0x0135
"HGE",      // 0x0136
"HGT",      // 0x0137

"LDVM",      // 0x0138
"STVM",      // 0x0139

"ADMD",      // 0x013a
"SUMD",      // 0x013b
"LDMD",      // 0x013c
"STMD",      // 0x013d
"CLMD",      // 0x013e
"CRMD",      // 0x013f

"BLM",      // 0x0140
"CRI",      // 0x0141
"TETI",      // 0x0142
"TORI",      // 0x0143
"TXOI",      // 0x0144
"LDF",      // 0x0145
"LDFD",      // 0x0146
"LDFQ",      // 0x0147
"BLT",      // 0x0148
"BLX",      // 0x0149

"RMI",      // 0x014a	// --  0	RMI -- Request Remote Multiprocessor Interrupt
"EVMO",      // 0x014b	// --  1	EVMO -- Enter Virtual Mode of CPU Execution
"SIOM",      // 0x014c	// --  2	SIOM -- Select Another Program's IM as Current OM
"SOOM",      // 0x014d	// --  3	SOOM -- Select Another Program's OM as Current OM
"SZOM",      // 0x014e	// --  4	SZOM -- Select Map Zero as Current OM
"SCRB",      // 0x014f	// --  5	SCRB -- Select Current Register Block in PSD
"EXMA",      // 0x0150	// --  6	EXMA -- Enter Extended Memory Addressing Mode
"EXMA2",      // 0x0151	// --  7	EXMA -- Enter Extended Memory Addressing Mode
"XVMO",      // 0x0152	// --  8	XVMO -- Exit Virtual Mode of CPU Execution
"ZIMP",      // 0x0153	// --  9	ZIMP -- Zero Section of Instruction Map
"UIT01A",      // 0x0154	// --  A	UIT
"ZOMP",      // 0x0155	// --  B	ZOMP -- Zero Section of Operand Map
"UIT01C",      // 0x0156	// --  C	UIT
"LIMP",      // 0x0157	// --  D	LIMP -- Load Instruction Map Image into Hardware Map
"LOMP",      // 0x0158	// --  E	LOMP -- Load Operand Map Image Into Hardware Map
"SOMP",      // 0x0159	// --  F	SOMP -- Store Operand Map into Map Image

"TRO",      // 0x015a	// -------- 0	TRO  -- Transfer and Reset Overflow Status History      
"LCPS",      // 0x015b	// -------- 1	LCPS  -- Load Register with Current Program Status Register of PSD   
"LCPR",      // 0x015c	// -------- 2	LCPR  -- Load Register with Current Program Register of PSD   
"LCCC",      // 0x015d	// -------- 3	LCCC  -- Load Register with Current Condition Code of PSD   
"LCIA",      // 0x015e	// -------- 4	LCIA  -- Load Register with Current interrupt Active Latches    
"LCIE",      // 0x015f	// -------- 5	LCIE  -- Load Register with Current interrupt Enable Latches     
"LCIR",      // 0x0160	// -------- 6	LCIR  -- Load Register with Current interrupt Request Latches     
"MBVV",      // 0x0161	// -------- 7	MBVV  -- Move Virtual Block to Virtual Block       
"MBVE",      // 0x0162	// -------- 8	MBVE  -- Move Block from Virtual to Extended Memory     
"MBEV",      // 0x0163	// -------- 9	MBEV  -- Move Block from Extended to Virtual Memory     
"MPES",      // 0x0164	// -------- A	MPES  -- Multiply Immediate with Extended Sign
"UIT0EB",      // 0x0165	// -------- B   UIT      
"RDI",      // 0x0166	// -------- C	RDI  -- Read Internal Registers         
"WIR",      // 0x0167	// -------- D	WIR  -- Write Internal Register         
"BRM",      // 0x0168	// -------- E	BRM  -- Branch to Microroutine Immediate        
"BRMI",      // 0x0169	// -------- F	BRMI  -- Branch to Microroutine Immediate        

"BXNS",      // 0x016a // --  BXNS	Branch (Short-Indexed) on Condition Code N Set     
"BXZS",      // 0x016b // --  BXZS	Branch (Short-Indexed) on Condition Code Z Set     
"BXOS",      // 0x016c // --  BXOS	Branch (Short-Indexed) On Condition Code O Set     
"BXCS",      // 0x016d // --  BXCS	Branch (Short-Indexed) on Condition Code C Set     
"BXLS",      // 0x016e // --  BXLS	Branch (Short-Indexed) on Less Than Condition      
"BXLE",      // 0x016f // --  BXLE	Branch (Short-Indexed) on Less Than or Equal Condition    
"BXHI",      // 0x0170 // --  BXHI	Branch (Short-Indexed) on Magnitude Higher Condition      
"UIT8F7",      // 0x0171 // -- 07 -- UIT
"BXNR",      // 0x0172 // --  BXNR	Branch (Short-Indexed) on Condition Code N Reset     
"BXZR",      // 0x0173 // --  BXZR	Branch (Short-Indexed) on Condition Code Z Reset     
"BXOR",      // 0x0174 // --  BXOR	Branch (Short-Indexed) On Condition Code O Reset     
"BXCR",      // 0x0175 // --  BXCR	Branch (Short-Indexed) on Condition Code C Reset     
"BXGE",      // 0x0176 // --  BXGE	Branch (Short-Indexed) on Greater Than or Equal Condition    
"BXGT",      // 0x0177 // --  BXGT	Branch (Short-Indexed) on Greater Than Condition      
"BXNH",      // 0x0178 // --  BXNH	Branch (Short-Indexed) on Magnitude Not Higher Condition     
"UIT8FF",      // 0x0179 // -- 15 -- UIT

"BLNS",      // 0x017a // --  BLNS	Branch and Link on Condition Code N Set     
"BLZS",      // 0x017b // --  BLZS	Branch and Link on Condition Code Z Set     
"BLOS",      // 0x017c // --  BLOS	Branch and Link On Condition Code O Set     
"BLCS",      // 0x017d // --  BLCS	Branch and Link on Condition Code C Set     
"BLLS",      // 0x017e // --  BLLS	Branch and Link on Less Than Condition      
"BLLE",      // 0x017f // --  BLLE	Branch and Link on Less Than or Equal Condition    
"BLHI",      // 0x0180 // --  BLHI	Branch and Link on Magnitude Higher Condition      
"UITA77",      // 0x0181 // -- 07 -- UIT
"BLNR",      // 0x0182 // --  BLNR	Branch and Link on Condition Code N Reset     
"BLZR",      // 0x0183 // --  BLZR	Branch and Link on Condition Code Z Reset     
"BLOR",      // 0x0184 // --  BLOR	Branch and Link On Condition Code O Reset     
"BLCR",      // 0x0185 // --  BLCR	Branch and Link on Condition Code C Reset     
"BLGE",      // 0x0186 // --  BLGE	Branch and Link on Greater Than or Equal Condition    
"BLGT",      // 0x0187 // --  BLGT	Branch and Link on Greater Than Condition      
"BLNH",      // 0x0188 // --  BLNH	Branch and Link on Magnitude Not Higher Condition     
"UITA7F",      // 0x0189 // -- 15 -- UIT

"SRNS",      // 0x018a // --  SRNS	Set Register if Condition Code N Set
"SRZS",      // 0x018b // --  SRZS	Set Register if Condition Code Z Set
"SROS",      // 0x018c // --  SROS	Set Register if Condition Code O Set
"SRCS",      // 0x018e // --  SRCS	Set Register if Code C Set
"SRLS",      // 0x018e // --  SRLS	Set Register on Less than Condition
"SRLE",      // 0x018f // --  SRLE	Set Register on Less than or Equal Condition
"SRHI",      // 0x0190 // --  SRHI	Set Register on Magnitude Higher Condition
"UITCA7",      // 0x0191 // -- 07 -- UIT
"SRNR",      // 0x0192 // --  SRNR	Set Register if Condition Code N Reset
"SRZR",      // 0x0193 // --  SRZR	Set Register if Condition Code Z Reset
"SROR",      // 0x0194 // --  SROR	Set Register if Condition Code O Reset
"SRCR",      // 0x0195 // --  SRCR	Set Register if Condition Code C Reset 
"SRGE",      // 0x0196 // --  SRGE	Set Register on Greater than or Equal Condition
"SRGT",      // 0x0197 // --  SRGT	Set Register on Greater than Condition
"SRNH",      // 0x0198 // --  SRNH	Set Register on Magnitude not Higher Condition
"SR00",      // 0x0199 // -- UNDOCUMENTED -- Don't know what this really does, set reg to -1

"ADI",      // 0x019a // --  0	ADI  --  Add Memory(Immediate) to Register	
"LDES",      // 0x019b // --  1	LDES  --  Load Immediate and Extend Sign	
"ADES",      // 0x019c // --  2	ADES  --  Add Immediate with Extended Sign	
"SUES",      // 0x019d // --  3	SUES  --  Subtract Immediate with Extended Sign	
"CIES",      // 0x019e // --  4	CIES  --  Compare Immediate with Extended Sign	
"EXI",      // 0x019f // --  5	EXI  --  Execute Immediate	
"MPI",      // 0x01a0 // --  6	MPI  --  Multiply Immediate	
"DVI",      // 0x01a1 // --  7	DVI  --  Divide Immediate	
"EPMD",      // 0x01a2 // --  8	EPMD  --  Enter Pipeline Mode of Execution	
"CRZ",      // 0x01a3 // --  9	CRZ  --  Compare Register to Zero	
"CRZD",      // 0x01a4 // --  A	CRZD  --  Compare Double Register to Zero	
"XMPD",      // 0x01a5 // --  B	XPMD  --  Exit Pipeline Mode of Execution	
"LTIL",      // 0x01a6 // --  C	LTIL  --  Loop Termination with Indirectly Addressed Control Variable and Literal Terminal Value	
"LTDL",      // 0x01a7 // --  D	LTDL  --  Loop Terminationwit h Directly Addressed Control - Variable - and Literal Terminal Value	
"LTLD",      // 0x01a8 // --  E	LTLD  --  Loop Termination with Indirectly Addressed Control Variable and Directly Addressed Terminal Value	
"LTDD",      // 0x01a9 // --  F	LTDD  --  Loop Termination with Directly Addressed - Control Variable and Directly Addressed Terminal Value

"BadOpcode" // 0x01aa // --  opcode was not decoded.

};
#endif