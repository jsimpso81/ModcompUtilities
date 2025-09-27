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


// --------this is based solely on the upper 8 bits of the instruction.
// --------augments have multiple different instructions with the same
// --------opcode.
#define OP_HLT			            0x00
#define OP_AUG01		            0x01	// augment
#define OP_LXR_SPR_SGP				0x02
#define OP_RMPS_RMWS_SLP            0x03
#define OP_WMS_SUP		            0x04
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


// --------completely parsed opcodes.  Single instruction
#define OPSI_HLT			            0x0000
//#define OPSI_AUG01					0x0001	// augment
#define OPSI_LXR						0x0002
#define OPSI_RMPS						0x0003
#define OPSI_WMS			            0x0004
#define OPSI_DMPI			            0x0005
#define OPSI_MMRB			            0x0006
#define OPSI_MRBM			            0x0007
#define OPSI_MBR			            0x0008
#define OPSI_MBL			            0x0009
#define OPSI_IBR			            0x000a
#define OPSI_MUR			            0x000b
#define OPSI_MLR			            0x000c
#define OPSI_TOR			            0x000d
//#define OPSI_AUG0E		            0x000e
#define OPSI_LRS			            0x000f

// 0x001X -- reserved for decimal arithmetic

#define OPSI_MPR			            0x0020
#define OPSI_DVR			            0x0021
#define OPSI_DAR			            0x0022
#define OPSI_REX			            0x0023
#define OPSI_CAR			            0x0024
#define OPSI_CIR			            0x0025
#define OPSI_SIA			            0x0026
#define OPSI_RIA			            0x0027
#define OPSI_RLD			            0x0028
#define OPSI_RLS			            0x0029
#define OPSI_RAD			            0x002a
#define OPSI_RAS			            0x002b
#define OPSI_LLD			            0x002c
#define OPSI_LLS			            0x002d
#define OPSI_LAD			            0x002e
#define OPSI_LAS			            0x002f

#define OPSI_FAR			            0x0030
#define OPSI_FSR			            0x0031
#define OPSI_FMR			            0x0032
#define OPSI_FDR			            0x0033
#define OPSI_FARD						0x0034
#define OPSI_FSRD						0x0035
#define OPSI_FMRD						0x0036
#define OPSI_FDRD						0x0037
#define OPSI_FAM			            0x0038
#define OPSI_FSM			            0x0039
#define OPSI_FMM			            0x003a
#define OPSI_FDM						0x003b
#define OPSI_FAMD						0x003c
#define OPSI_FSMD						0x003d
#define OPSI_FMMD						0x003e
#define OPSI_FDMD						0x003f

#define OPSI_OCA			            0x0040
#define OPSI_OCB			            0x0041
#define OPSI_OCC			            0x0042
#define OPSI_OCD			            0x0043
#define OPSI_ODA			            0x0044
#define OPSI_ODB			            0x0045
#define OPSI_ODC			            0x0046
#define OPSI_ODD			            0x0047
#define OPSI_ISA			            0x0048
#define OPSI_ISB			            0x0049
#define OPSI_ISC			            0x004a
#define OPSI_ISD			            0x004b
#define OPSI_IDA			            0x004c
#define OPSI_IDB			            0x004d
#define OPSI_IDC			            0x004e
#define OPSI_IDD			            0x004f

// -- 0x005x -- reserved for communications

#define OPSI_ABR			            0x0060
#define OPSI_SBR			            0x0061
#define OPSI_ZBR			            0x0062
#define OPSI_OBR			            0x0063
#define OPSI_XBR			            0x0064
#define OPSI_LBR			            0x0065
#define OPSI_TBR			            0x0066
#define OPSI_GMR			            0x0067
#define OPSI_ADR			            0x0068
#define OPSI_SUR			            0x0069
#define OPSI_ETR			            0x006a
#define OPSI_ORR			            0x006b
#define OPSI_XOR			            0x006c
#define OPSI_TRR			            0x006d
#define OPSI_CRR			            0x006e
#define OPSI_TTR			            0x006f

#define OPSI_ABRB			            0x0070
#define OPSI_SBRB			            0x0071
#define OPSI_ZBRB			            0x0072
#define OPSI_OBRB		                0x0073
#define OPSI_XBRB		                0x0074
#define OPSI_LBRB		                0x0075
#define OPSI_TBRB			            0x0076
#define OPSI_GMRB			            0x0077
#define OPSI_ADRB			            0x0078
#define OPSI_SURB			            0x0079
#define OPSI_ETRB			            0x007a
#define OPSI_ORRB			            0x007b
#define OPSI_XORB			            0x007c
#define OPSI_TRRB			            0x007d
#define OPSI_TERB			            0x007e
#define OPSI_TTRB			            0x007f

#define OPSI_ABMM				        0x0080
#define OPSI_ZBMM				        0x0081
#define OPSI_OBMM						0x0082
#define OPSI_TBMM						0x0083
#define OPSI_ABMB					    0x0084
#define OPSI_ZBMB					    0x0085
#define OPSI_TBMB					    0x0086
#define OPSI_CBMB					    0x0087
#define OPSI_LDXT						0x0088
#define OPSI_NOP						0x0089
#define OPSI_IRRD						0x008a
#define OPSI_CRRT						0x008b
#define OPSI_ESD				        0x008c
#define OPSI_TRRQ				        0x008d
#define OPSI_CRXD				        0x008e
//#define OPSI_AUG8F			        0x008f

#define OPSI_ABSM				        0x0090
#define OPSI_ZBSM				        0x0091
#define OPSI_OBSM				        0x0092
#define OPSI_TBSM				        0x0093
#define OPSI_ABSB				        0x0094
#define OPSI_ZBSB				        0x0095
#define OPSI_TBSB				        0x0096
#define OPSI_CBSB				        0x0097
#define OPSI_ABXM				        0x0098
#define OPSI_ZBXM				        0x0099
#define OPSI_OBXM				        0x009a
#define OPSI_TBXM				        0x009b
#define OPSI_ABXB				        0x009c
#define OPSI_ZBXB				        0x009d
#define OPSI_TBXB				        0x009e
#define OPSI_CBXB				        0x009f

#define OPSI_MPM				        0x00a0
#define OPSI_DVM				        0x00a1
#define OPSI_MPRD				        0x00a2
#define OPSI_DVRD				        0x00a3
#define OPSI_LFM				        0x00a4
#define OPSI_SFM				        0x00a5
#define OPSI_HHI					    0x00a6
//#define OPSI_AUGA7			        0x00a7
#define OPSI_HNS				        0x00a8
#define OPSI_HZS				        0x00a9
#define OPSI_HOS				        0x00aa
#define OPSI_HCS				        0x00ab
#define OPSI_HLS				        0x00ac
#define OPSI_HLE				        0x00ad
#define OPSI_LBX					    0x00ae
#define OPSI_SBX				        0x00af

#define OPSI_MPS				        0x00b0
#define OPSI_DVS				        0x00b1
#define OPSI_SCCC				        0x00b2
#define OPSI_EXR				        0x00b3
#define OPSI_LFS				        0x00b4
#define OPSI_SFS				        0x00b5
#define OPSI_IRM				        0x00b6
#define OPSI_IRR				        0x00b7
#define OPSI_MPX				        0x00b8
#define OPSI_DVX				        0x00b9
#define OPSI_PLM				        0x00ba
#define OPSI_PSM				        0x00bb
#define OPSI_LFX				        0x00bc
#define OPSI_SFX				        0x00bd
#define OPSI_LDAM				        0x00be
#define OPSI_STAM				        0x00bf

#define OPSI_ADMM				        0x00c0
#define OPSI_ETMM				        0x00c1
#define OPSI_ORMM				        0x00c2
#define OPSI_CRM				        0x00c3
#define OPSI_ADMB				        0x00c4
#define OPSI_ETMB				        0x00c5
#define OPSI_TRMB				        0x00c6
#define OPSI_CRMB				        0x00c7
#define OPSI_ADRD				        0x00c8
#define OPSI_SURD				        0x00c9
//#define OPSI_AUGCA			        0x00ca
#define OPSI_UIT				        0x00cb
#define OPSI_TSBM				        0x00cc
#define OPSI_TRRD				        0x00cd
#define OPSI_CLM				        0x00ce
#define OPSI_CRRD				        0x00cf

#define OPSI_ADSM				        0x00d0
#define OPSI_ETSM				        0x00d1
#define OPSI_ORSM				        0x00d2
#define OPSI_CRS				        0x00d3
#define OPSI_ADSB				        0x00d4
#define OPSI_ETSB				        0x00d5
#define OPSI_TRSB				        0x00d6
#define OPSI_CRSB				        0x00d7
#define OPSI_ADXM				        0x00d8
#define OPSI_ETXM				        0x00d9
#define OPSI_ORXM				        0x00da
#define OPSI_CRX				        0x00db
#define OPSI_ADXB				        0x00dc
#define OPSI_ETXB				        0x00dd
#define OPSI_TRXB				        0x00de
#define OPSI_CRXB				        0x00df

#define OPSI_ADM				        0x00e0
#define OPSI_SUM				        0x00e1
#define OPSI_ETM				        0x00e2
#define OPSI_ORM				        0x00e3
#define OPSI_XOM				        0x00e4
#define OPSI_LDM				        0x00e5
#define OPSI_STM				        0x00e6
#define OPSI_BRU				        0x00e7
//#define OPSI_AUGE8			        0x00e8
#define OPSI_SUI				        0x00e9
#define OPSI_ETI				        0x00ea
#define OPSI_ORI				        0x00eb
#define OPSI_XOI				        0x00ec
#define OPSI_LDI				        0x00ed
#define OPSI_STI				        0x00ee
#define OPSI_BLI				        0x00ef

#define OPSI_ADS				        0x00f0
#define OPSI_SUS				        0x00f1
#define OPSI_ETS				        0x00f2
#define OPSI_ORS				        0x00f3
#define OPSI_XOS				        0x00f4
#define OPSI_LDS				        0x00f5
#define OPSI_STS				        0x00f6
#define OPSI_HOP				        0x00f7
#define OPSI_ADX				        0x00f8
#define OPSI_SUX				        0x00f9
#define OPSI_ETX				        0x00fa
#define OPSI_ORX				        0x00fb
#define OPSI_XOX				        0x00fc
#define OPSI_LDX				        0x00fd
#define OPSI_STX				        0x00fe
#define OPSI_BRX					    0x00ff

// --------start of extended opcode indexes...
#define OPSI_SPR						0x0100	
#define OPSI_SGP						0x0101
#define OPSI_RMWS						0x0102
#define OPSI_SLP						0x0103
#define OPSI_SUP						0x0104
#define OPSI_SIE						0x0105
#define OPSI_SIR						0x0106
#define OPSI_RIE						0x0107
#define OPSI_RIR						0x0108

#define OPSI_RLQ						0x0109
#define OPSI_RAQ						0x010a
#define OPSI_LLQ						0x010b
#define OPSI_LAQ						0x010c

#define OPSI_CDIF						0x010d
#define OPSI_FARQ						0x010e
#define OPSI_CFDI						0x010f
#define OPSI_FSRQ						0x0110
#define OPSI_CQFF						0x0111
#define OPSI_FMRQ						0x0112
#define OPSI_CDFI						0x0113
#define OPSI_FDRQ						0x0114
#define OPSI_CQFI						0x0115
#define OPSI_FAI						0x0116
#define OPSI_FSI						0x0117
#define OPSI_FMI						0x0118
#define OPSI_FDI						0x0119
#define OPSI_FAMQ						0x011a
#define OPSI_FAID						0x011b
#define OPSI_FAIQ						0x011c
#define OPSI_FSMQ						0x011d
#define OPSI_FSID						0x011e
#define OPSI_FSIQ						0x011f
#define OPSI_FMMQ						0x0120
#define OPSI_FMID						0x0121
#define OPSI_FMIQ						0x0122
#define OPSI_FDMQ						0x0123
#define OPSI_FDID						0x0124
#define OPSI_FDIQ						0x0125

#define OPSI_STXT						0x0126
#define OPSI_LDMT						0x0127
#define OPSI_STMT						0x0128
#define OPSI_TTRD						0x0129
#define OPSI_CRRQ						0x012a
#define OPSI_TTRQ						0x012b
#define OPSI_ESS						0x012c
#define OPSI_LDXD						0x012d
#define OPSI_STXD						0x012e

#define OPSI_MPMD						0x012f
#define OPSI_DVMD						0x0130
#define OPSI_HNH						0x0131
#define OPSI_HNR						0x0132
#define OPSI_HZR						0x0133
#define OPSI_HOR						0x0134
#define OPSI_HCR						0x0135
#define OPSI_HGE						0x0136
#define OPSI_HGT						0x0137

#define OPSI_LDVM						0x0138
#define OPSI_STVM						0x0139

#define OPSI_ADMD						0x013a
#define OPSI_SUMD						0x013b
#define OPSI_LDMD						0x013c
#define OPSI_STMD						0x013d
#define OPSI_CLMD						0x013e
#define OPSI_CRMD						0x013f

#define OPSI_BLM						0x0140
#define OPSI_CRI						0x0141
#define OPSI_TETI						0x0142
#define OPSI_TORI						0x0143
#define OPSI_TXOI						0x0144
#define OPSI_LDF						0x0145
#define OPSI_LDFD						0x0146
#define OPSI_LDFQ						0x0147
#define OPSI_BLT						0x0148
#define OPSI_BLX						0x0149

#define OPSI_AUG01						0x014a	// augment start

#define OPSI_RMI						0x014a	// --  0	RMI -- Request Remote Multiprocessor Interrupt
#define OPSI_EVMO						0x014b	// --  1	EVMO -- Enter Virtual Mode of CPU Execution
#define OPSI_SIOM						0x014c	// --  2	SIOM -- Select Another Program's IM as Current OM
#define OPSI_SOOM						0x014d	// --  3	SOOM -- Select Another Program's OM as Current OM
#define OPSI_SZOM						0x014e	// --  4	SZOM -- Select Map Zero as Current OM
#define OPSI_SCRB						0x014f	// --  5	SCRB -- Select Current Register Block in PSD
#define OPSI_EXMA						0x0150	// --  6	EXMA -- Enter Extended Memory Addressing Mode
#define OPSI_EXMA2						0x0151	// --  7	EXMA -- Enter Extended Memory Addressing Mode
#define OPSI_XVMO						0x0152	// --  8	XVMO -- Exit Virtual Mode of CPU Execution
#define OPSI_ZIMP						0x0153	// --  9	ZIMP -- Zero Section of Instruction Map
#define OPSI_UIT01A						0x0154	// --  A	UIT
#define OPSI_ZOMP						0x0155	// --  B	ZOMP -- Zero Section of Operand Map
#define OPSI_UIT01C						0x0156	// --  C	UIT
#define OPSI_LIMP						0x0157	// --  D	LIMP -- Load Instruction Map Image into Hardware Map
#define OPSI_LOMP						0x0158	// --  E	LOMP -- Load Operand Map Image Into Hardware Map
#define OPSI_SOMP						0x0159	// --  F	SOMP -- Store Operand Map into Map Image

#define OPSI_AUG0E						0x015a	// augment start

#define OPSI_TRO						0x015a	// -------- 0	TRO  -- Transfer and Reset Overflow Status History      
#define OPSI_LCPS						0x015b	// -------- 1	LCPS  -- Load Register with Current Program Status Register of PSD   
#define OPSI_LCPR						0x015c	// -------- 2	LCPR  -- Load Register with Current Program Register of PSD   
#define OPSI_LCCC						0x015d	// -------- 3	LCCC  -- Load Register with Current Condition Code of PSD   
#define OPSI_LCIA						0x015e	// -------- 4	LCIA  -- Load Register with Current interrupt Active Latches    
#define OPSI_LCIE						0x015f	// -------- 5	LCIE  -- Load Register with Current interrupt Enable Latches     
#define OPSI_LCIR						0x0160	// -------- 6	LCIR  -- Load Register with Current interrupt Request Latches     
#define OPSI_MBVV						0x0161	// -------- 7	MBVV  -- Move Virtual Block to Virtual Block       
#define OPSI_MBVE						0x0162	// -------- 8	MBVE  -- Move Block from Virtual to Extended Memory     
#define OPSI_MBEV						0x0163	// -------- 9	MBEV  -- Move Block from Extended to Virtual Memory     
#define OPSI_MPES						0x0164	// -------- A	MPES  -- Multiply Immediate with Extended Sign
#define OPSI_UIT0EB						0x0165	// -------- B   UIT      
#define OPSI_RDI						0x0166	// -------- C	RDI  -- Read Internal Registers         
#define OPSI_WIR						0x0167	// -------- D	WIR  -- Write Internal Register         
#define OPSI_BRM						0x0168	// -------- E	BRM  -- Branch to Microroutine Immediate        
#define OPSI_BRMI						0x0169	// -------- F	BRMI  -- Branch to Microroutine Immediate        

#define OPSI_AUG8F						0x016a // augment start

#define OPSI_BXNS						0x016a // --  BXNS	Branch (Short-Indexed) on Condition Code N Set     
#define OPSI_BXZS						0x016b // --  BXZS	Branch (Short-Indexed) on Condition Code Z Set     
#define OPSI_BXOS						0x016c // --  BXOS	Branch (Short-Indexed) On Condition Code O Set     
#define OPSI_BXCS						0x016d // --  BXCS	Branch (Short-Indexed) on Condition Code C Set     
#define OPSI_BXLS						0x016e // --  BXLS	Branch (Short-Indexed) on Less Than Condition      
#define OPSI_BXLE						0x016f // --  BXLE	Branch (Short-Indexed) on Less Than or Equal Condition    
#define OPSI_BXHI						0x0170 // --  BXHI	Branch (Short-Indexed) on Magnitude Higher Condition      
#define OPSI_UIT8F7						0x0171 // -- 07 -- UIT
#define OPSI_BXNR						0x0172 // --  BXNR	Branch (Short-Indexed) on Condition Code N Reset     
#define OPSI_BXZR						0x0173 // --  BXZR	Branch (Short-Indexed) on Condition Code Z Reset     
#define OPSI_BXOR						0x0174 // --  BXOR	Branch (Short-Indexed) On Condition Code O Reset     
#define OPSI_BXCR						0x0175 // --  BXCR	Branch (Short-Indexed) on Condition Code C Reset     
#define OPSI_BXGE						0x0176 // --  BXGE	Branch (Short-Indexed) on Greater Than or Equal Condition    
#define OPSI_BXGT						0x0177 // --  BXGT	Branch (Short-Indexed) on Greater Than Condition      
#define OPSI_BXNH						0x0178 // --  BXNH	Branch (Short-Indexed) on Magnitude Not Higher Condition     
#define OPSI_UIT8FF						0x0179 // -- 15 -- UIT

#define OPSI_AUGA7						0x017A // augment start

#define OPSI_BLNS						0x017a // --  BLNS	Branch and Link on Condition Code N Set     
#define OPSI_BLZS						0x017b // --  BLZS	Branch and Link on Condition Code Z Set     
#define OPSI_BLOS						0x017c // --  BLOS	Branch and Link On Condition Code O Set     
#define OPSI_BLCS						0x017d // --  BLCS	Branch and Link on Condition Code C Set     
#define OPSI_BLLS						0x017e // --  BLLS	Branch and Link on Less Than Condition      
#define OPSI_BLLE						0x017f // --  BLLE	Branch and Link on Less Than or Equal Condition    
#define OPSI_BLHI						0x0180 // --  BLHI	Branch and Link on Magnitude Higher Condition      
#define OPSI_UITA77						0x0181 // -- 07 -- UIT
#define OPSI_BLNR						0x0182 // --  BLNR	Branch and Link on Condition Code N Reset     
#define OPSI_BLZR						0x0183 // --  BLZR	Branch and Link on Condition Code Z Reset     
#define OPSI_BLOR						0x0184 // --  BLOR	Branch and Link On Condition Code O Reset     
#define OPSI_BLCR						0x0185 // --  BLCR	Branch and Link on Condition Code C Reset     
#define OPSI_BLGE						0x0186 // --  BLGE	Branch and Link on Greater Than or Equal Condition    
#define OPSI_BLGT						0x0187 // --  BLGT	Branch and Link on Greater Than Condition      
#define OPSI_BLNH						0x0188 // --  BLNH	Branch and Link on Magnitude Not Higher Condition     
#define OPSI_UITA7F						0x0189 // -- 15 -- UIT

#define OPSI_AUGCA						0x018a // augment start

#define OPSI_SRNS						0x018a // --  SRNS	Set Register if Condition Code N Set
#define OPSI_SRZS						0x018b // --  SRZS	Set Register if Condition Code Z Set
#define OPSI_SROS						0x018c // --  SROS	Set Register if Condition Code O Set
#define OPSI_SRCS						0x018e // --  SRCS	Set Register if Code C Set
#define OPSI_SRLS						0x018e // --  SRLS	Set Register on Less than Condition
#define OPSI_SRLE						0x018f // --  SRLE	Set Register on Less than or Equal Condition
#define OPSI_SRHI						0x0190 // --  SRHI	Set Register on Magnitude Higher Condition
#define OPSI_UITCA7						0x0191 // -- 07 -- UIT
#define OPSI_SRNR						0x0192 // --  SRNR	Set Register if Condition Code N Reset
#define OPSI_SRZR						0x0193 // --  SRZR	Set Register if Condition Code Z Reset
#define OPSI_SROR						0x0194 // --  SROR	Set Register if Condition Code O Reset
#define OPSI_SRCR						0x0195 // --  SRCR	Set Register if Condition Code C Reset 
#define OPSI_SRGE						0x0196 // --  SRGE	Set Register on Greater than or Equal Condition
#define OPSI_SRGT						0x0197 // --  SRGT	Set Register on Greater than Condition
#define OPSI_SRNH						0x0198 // --  SRNH	Set Register on Magnitude not Higher Condition
#define OPSI_SR00						0x0199 // -- UNDOCUMENTED -- Don't know what this really does, set reg to -1

#define OPSI_AUGE8						0x019a // augment start

#define OPSI_ADI						0x019a // --  0	ADI  --  Add Memory(Immediate) to Register	
#define OPSI_LDES						0x019b // --  1	LDES  --  Load Immediate and Extend Sign	
#define OPSI_ADES						0x019c // --  2	ADES  --  Add Immediate with Extended Sign	
#define OPSI_SUES						0x019d // --  3	SUES  --  Subtract Immediate with Extended Sign	
#define OPSI_CIES						0x019e // --  4	CIES  --  Compare Immediate with Extended Sign	
#define OPSI_EXI						0x019f // --  5	EXI  --  Execute Immediate	
#define OPSI_MPI						0x01a0 // --  6	MPI  --  Multiply Immediate	
#define OPSI_DVI						0x01a1 // --  7	DVI  --  Divide Immediate	
#define OPSI_EPMD						0x01a2 // --  8	EPMD  --  Enter Pipeline Mode of Execution	
#define OPSI_CRZ						0x01a3 // --  9	CRZ  --  Compare Register to Zero	
#define OPSI_CRZD						0x01a4 // --  A	CRZD  --  Compare Double Register to Zero	
#define OPSI_XMPD						0x01a5 // --  B	XPMD  --  Exit Pipeline Mode of Execution	
#define OPSI_LTIL						0x01a6 // --  C	LTIL  --  Loop Termination with Indirectly Addressed Control Variable and Literal Terminal Value	
#define OPSI_LTDL						0x01a7 // --  D	LTDL  --  Loop Terminationwit h Directly Addressed Control - Variable - and Literal Terminal Value	
#define OPSI_LTLD						0x01a8 // --  E	LTLD  --  Loop Termination with Indirectly Addressed Control Variable and Directly Addressed Terminal Value	
#define OPSI_LTDD						0x01a9 // --  F	LTDD  --  Loop Termination with Directly Addressed - Control Variable and Directly Addressed Terminal Value

#define OPSI_NOINST						0x01aa // --  Opcode was not decoded.
