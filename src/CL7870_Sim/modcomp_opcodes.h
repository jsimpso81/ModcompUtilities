// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			modcomp_opcodes.h
//
//	Description:	Defines for modcomp classic 16 bit opcodes.
//
//	Externally accessible routines:
// 
// Notes:
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

#pragma once


#define OP_HLT			            0x00
#define OP_AUG01		            0x01	// augment
#define OP_LXR			            0x02
#define OP_RMPS_RMWS	            0x03
#define OP_WMS			            0x04
#define OP_DMPI			            0x05
#define OP_MMRB			            0x06
#define OP_MRBM			            0x07
#define OP_MBR			            0x08
#define OP_MBL			            0x09
#define OP_IBR			            0x0a
#define OP_MUR			            0x0b
#define OP_MLR			            0x0c
#define OP_TOR			            0x0d
#define OP_AUG0E		            0x0e
#define OP_LRS			            0x0f

// 0x1X -- reserved for decimal arithmetic

#define OP_MPR			            0x20
#define OP_DVR			            0x21
#define OP_DAR			            0x22
#define OP_REX			            0x23
#define OP_CAR			            0x24
#define OP_CIR			            0x25
#define OP_SIA_SIE_SIR	            0x26
#define OP_RIA_RIE_RIR	            0x27
#define OP_RLD_RLQ		            0x28
#define OP_RLS			            0x29
#define OP_RAD_RAQ		            0x2a
#define OP_RAS			            0x2b
#define OP_LLD_LLQ		            0x2c
#define OP_LLS			            0x2d
#define OP_LAD_LAQ		            0x2e
#define OP_LAS			            0x2f

#define OP_FAR_CDIF		            0x30
#define OP_FSR			            0x31
#define OP_FMR			            0x32
#define OP_FDR			            0x33
#define OP_FARD_FARQ_CFDI           0x34
#define OP_FSRD_FSRQ_CQFF           0x35
#define OP_FMRD_FMRQ_CDFI           0x36
#define OP_FDRD_FDRQ_CQFI           0x37
#define OP_FAM_FAI		            0x38
#define OP_FSM_FSI		            0x39
#define OP_FMM_FMI		            0x3a
#define OP_FDM_FDI					0x3b
#define OP_FAMD_FAMQ_FAID_FAIQ      0x3c
#define OP_FSMD_FSMQ_FSID_FSIQ		0x3d
#define OP_FMMD_FMMQ_FMID_FMIQ      0x3e
#define OP_FDMD_FDMQ_FDID_FDIQ      0x3f

#define OP_OCA			            0x40
#define OP_OCB			            0x41
#define OP_OCC			            0x42
#define OP_OCD			            0x43
#define OP_ODA			            0x44
#define OP_ODB			            0x45
#define OP_ODC			            0x46
#define OP_ODD			            0x47
#define OP_ISA			            0x48
#define OP_ISB			            0x49
#define OP_ISC			            0x4a
#define OP_ISD			            0x4b
#define OP_IDA			            0x4c
#define OP_IDB			            0x4d
#define OP_IDC			            0x4e
#define OP_IDD			            0x4f

// -- 0x5x -- reserved for communications

#define OP_ABR			            0x60
#define OP_SBR			            0x61
#define OP_ZBR			            0x62
#define OP_OBR			            0x63
#define OP_XBR			            0x64
#define OP_LBR			            0x65
#define OP_TBR			            0x66
#define OP_GMR			            0x67
#define OP_ADR			            0x68
#define OP_SUR			            0x69
#define OP_ETR			            0x6a
#define OP_ORR			            0x6b
#define OP_XOR			            0x6c
#define OP_TRR			            0x6d
#define OP_CRR			            0x6e
#define OP_TTR			            0x6f

#define OP_ABRB			            0x70
#define OP_SBRB			            0x71
#define OP_ZBRB			            0x72
#define OP_OBRB		                0x73
#define OP_XBRB		                0x74
#define OP_LBRB		                0x75
#define OP_TBRB			            0x76
#define OP_GMRB			            0x77
#define OP_ADRB			            0x78
#define OP_SURB			            0x79
#define OP_ETRB			            0x7a
#define OP_ORRB			            0x7b
#define OP_XORB			            0x7c
#define OP_TRRB			            0x7d
#define OP_TERB			            0x7e
#define OP_TTRB			            0x7f

#define OP_ABMM				        0x80
#define OP_ZBMM				        0x81
#define OP_OBMM						0x82
#define OP_TBMM						0x83
#define OP_ABMB					    0x84
#define OP_ZBMB					    0x85
#define OP_TBMB					    0x86
#define OP_CBMB					    0x87
#define OP_LDXT_STXT_LDMT_STMT		0x88
#define OP_NOP						0x89
#define OP_IRRD_TTRD				0x8a
#define OP_CRRT_CRRQ_TTRQ			0x8b
#define OP_ESD_ESS			        0x8c
#define OP_TRRQ_LDXD		        0x8d
#define OP_CRXD_STXD		        0x8e
#define OP_AUG8F			        0x8f

#define OP_ABSM				        0x90
#define OP_ZBSM				        0x91
#define OP_OBSM				        0x92
#define OP_TBSM				        0x93
#define OP_ABSB				        0x94
#define OP_ZBSB				        0x95
#define OP_TBSB				        0x96
#define OP_CBSB				        0x97
#define OP_ABXM				        0x98
#define OP_ZBXM				        0x99
#define OP_OBXM				        0x9a
#define OP_TBXM				        0x9b
#define OP_ABXB				        0x9c
#define OP_ZBXB				        0x9d
#define OP_TBXB				        0x9e
#define OP_CBXB				        0x9f

#define OP_MPM				        0xa0
#define OP_DVM				        0xa1
#define OP_MPRD_MPMD		        0xa2
#define OP_DVRD_DVMD		        0xa3
#define OP_LFM				        0xa4
#define OP_SFM				        0xa5
#define OP_HHI_HNH				    0xa6
#define OP_AUGA7			        0xa7
#define OP_HNS_HNR			        0xa8
#define OP_HZS_HZR			        0xa9
#define OP_HOS_HOR			        0xaa
#define OP_HCS_HCR			        0xab
#define OP_HLS_HGE			        0xac
#define OP_HLE_HGT			        0xad
#define OP_LBX				        0xae
#define OP_SBX				        0xaf

#define OP_MPS				        0xb0
#define OP_DVS				        0xb1
#define OP_SCCC				        0xb2
#define OP_EXR				        0xb3
#define OP_LFS				        0xb4
#define OP_SFS				        0xb5
#define OP_IRM				        0xb6
#define OP_IRR				        0xb7
#define OP_MPX				        0xb8
#define OP_DVX				        0xb9
#define OP_PLM				        0xba
#define OP_PSM				        0xbb
#define OP_LFX				        0xbc
#define OP_SFX				        0xbd
#define OP_LDAM_LDVM		        0xbe
#define OP_STAM_STVM		        0xbf

#define OP_ADMM				        0xc0
#define OP_ETMM				        0xc1
#define OP_ORMM				        0xc2
#define OP_CRM				        0xc3
#define OP_ADMB				        0xc4
#define OP_ETMB				        0xc5
#define OP_TRMB				        0xc6
#define OP_CRMB				        0xc7
#define OP_ADRD_ADMD		        0xc8
#define OP_SURD_SUMD		        0xc9
#define OP_AUGCA			        0xca
#define OP_UIT				        0xcb
#define OP_TSBM				        0xcc
#define OP_TRRD_LDMD		        0xcd
#define OP_CLM_STMD_CLMD	        0xce
#define OP_CRRD_CRMD		        0xcf

#define OP_ADSM				        0xd0
#define OP_ETSM				        0xd1
#define OP_ORSM				        0xd2
#define OP_CRS				        0xd3
#define OP_ADSB				        0xd4
#define OP_ETSB				        0xd5
#define OP_TRSB				        0xd6
#define OP_CRSB				        0xd7
#define OP_ADXM				        0xd8
#define OP_ETXM				        0xd9
#define OP_ORXM				        0xda
#define OP_CRX				        0xdb
#define OP_ADXB				        0xdc
#define OP_ETXB				        0xdd
#define OP_TRXB				        0xde
#define OP_CRXB				        0xdf

#define OP_ADM				        0xe0
#define OP_SUM				        0xe1
#define OP_ETM				        0xe2
#define OP_ORM				        0xe3
#define OP_XOM				        0xe4
#define OP_LDM				        0xe5
#define OP_STM				        0xe6
#define OP_BRU_BLM			        0xe7
#define OP_AUGE8			        0xe8
#define OP_SUI_CRI			        0xe9
#define OP_ETI_TETI			        0xea
#define OP_ORI_TORI			        0xeb
#define OP_XOI_TXOI			        0xec
#define OP_LDI_LDF_LDFD_FDFQ        0xed
#define OP_STI				        0xee
#define OP_BLI				        0xef

#define OP_ADS				        0xf0
#define OP_SUS				        0xf1
#define OP_ETS				        0xf2
#define OP_ORS				        0xf3
#define OP_XOS				        0xf4
#define OP_LDS				        0xf5
#define OP_STS				        0xf6
#define OP_HOP_BLT			        0xf7
#define OP_ADX				        0xf8
#define OP_SUX				        0xf9
#define OP_ETX				        0xfa
#define OP_ORX				        0xfb
#define OP_XOX				        0xfc
#define OP_LDX				        0xfd
#define OP_STX				        0xfe
#define OP_BRX_BLX				    0xff
