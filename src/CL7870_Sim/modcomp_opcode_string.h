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


char opcode_string[256][20] = {
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

