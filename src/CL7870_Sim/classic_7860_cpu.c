#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_opcodes.h"
#include "modcomp_sim_procedures.h"

void classic_7860_cpu() {

	// -------- local constants
	const unsigned __int16 bit[16] =   { 0x8000, 0x4000, 0x2000, 0x1000, 0x0800, 0x0400, 0x0200, 0x0100,
										 0x0080, 0x0040, 0x0020, 0x0010, 0x0008, 0x0004, 0x0002, 0x0001 };
	const unsigned __int16 bitnot[16] = { 0x7fff, 0xbfff, 0xdfff, 0xefff, 0xf7ff, 0xfbff, 0xfdff, 0xfeff,
										  0xff7f, 0xffbf, 0xffdf, 0xffef, 0xfff7, 0xfffb, 0xfffd, 0xfffe };
	const unsigned __int16 mask[16] =  { 0x8000, 0xc000, 0xe000, 0xf000, 0xf800, 0xfc00, 0xfe00, 0xff00,
						 				 0xff80, 0xffc0, 0xffe0, 0xfff0, 0xfff8, 0xfffc, 0xfffe, 0xffff };


	// -------- local values	
	static unsigned __int16 program_counter = 0;	// use local mapping, value 0=65k
	static unsigned __int16 instruction = 0;
	static unsigned __int16 opcode;
	// unsigned __int16 source_reg = 0;
	// unsigned __int16 dest_reg = 0;

	// -------- potentially global values
	static unsigned __int16 cpu_interrupt_active = 0;
	static unsigned __int16 cpu_interrupt_enabled = 0;
	static unsigned __int16 cpu_interrupt_request = 0;
	static unsigned __int16 cpu_register[16][16] = { 0 }; // [register][registerblock] TODO: Ensure initializtion is complete.
	static unsigned __int16 cpu_current_register_block = 0;
	static unsigned __int16 cpu_current_operand_map = 0;
	static unsigned __int16 cpu_current_instruction_map = 0;
	static bool cpu_virtual_mode = false;
	// static bool cpu_run = false;
	// static bool cpu_single_step = false;
	static bool cpu_cond_code_n = false;
	static bool cpu_cond_code_z = false;
	static bool cpu_cond_code_o = false;
	static bool cpu_cond_code_c = false;


	// -------- for temporary use
	static unsigned __int16		tempu16_regs = 0;
	static unsigned __int16		tempu16_regd = 0;
	static unsigned __int16		tempu16_val1 = 0;
	static unsigned __int16		tempu16_val2 = 0;


#define UNIMPLEMENTED_INSTRUCTION {\
					printf("\n unimplemented instruction %04x\n",instruction);\
					}

#define GET_SOURCE_REGISTER (instruction & 0x000f)

#define GET_SOURCE_REGISTER_VALUE ( cpu_register[GET_SOURCE_REGISTER][cpu_current_register_block]) 

#define GET_DESTINATION_REGISTER (( instruction >> 4) & 0x000f )

#define GET_REGISTER_VALUE( REG ) ( cpu_register[REG][cpu_current_register_block])
			
#define GET_MEMORY( A ) (gbl_mem[(A)])

#define SET_REGISTER_VALUE(ZREG,ZVAL) {\
					cpu_register[ZREG][cpu_current_register_block] = ZVAL; \
					}

#define SET_DESTINATION_REGISTER( V1 ) {\
					SET_REGISTER_VALUE( GET_DESTINATION_REGISTER, V1 ); \
					}

#define SET_DESTINATION_REGISTER_DOUBLE( V1, V2 ) {}

#define SET_DESTINATION_REGISTER_TRIPLE( V1, V2, V3 ) {}

#define SET_DESTINATION_REGISTER_QUAD( V1, V2, V3, V4 ) {}

#define SET_NEXT_PROGRAM_COUNTER(A)	{\
					program_counter = (A);\
					}

#define	SET_CC_Z(COND_Z) {\
				cpu_cond_code_z = (COND_Z); \
				}

#define	SET_CC_N(COND_N) {\
				cpu_cond_code_n = (COND_N); \
				}

#define	SET_CC_O(COND_O) {\
				cpu_cond_code_o = (COND_O); \
				}

#define TEST_VALID_DOUBLE_REGISTER(ZREG) (ZREG != 0 && ((ZREG & 0x0001) == 0))
					
#define TEST_VALID_TRIPLE_REGISTER(ZREG) (ZREG != 0 && ((ZREG & 0x0003) == 0))
					
#define TEST_VALID_QUAD_REGISTER(ZREG) (ZREG != 0 && ((ZREG & 0x0003) == 0))

#define CONDITIONAL_BRANCH( TEST_VALUE,  BRANCH_PC, NO_BRANCH_PC ) {\
					if ((TEST_VALUE)) {\
						SET_NEXT_PROGRAM_COUNTER( BRANCH_PC );\
					}\
					else {\
						SET_NEXT_PROGRAM_COUNTER( NO_BRANCH_PC );\
					}\
					} 


	// printf("\n CPU started.\n");

	while (gbl_fp_runlight | gbl_fp_single_step) {
		// -------- get next instruction
		// -------- check for new interrupts

		if ((cpu_interrupt_request & cpu_interrupt_enabled) > cpu_interrupt_active) {

			// --------find out which interrupt we are activatiing 

			// --------save current process_status_double_word in dedicated memory location

			// --------get current process status double word from dedicated memory location

			// --------get register file index
		}


		instruction = gbl_mem[program_counter];

		// -------- decode opcode
		opcode = (instruction >> (unsigned __int16)8) & 0xff;

		printf("pc: %04X, instruction: %04x, op code: %04x\n", program_counter, instruction, opcode);

		switch (opcode) {

			// --------00 -- HLT
		case  OP_HLT:			// 0x00
			// cpu_run = false;
			gbl_fp_runlight = false;
			printf("\nCpu halted.\n");
			cmd_process_print_prompt();
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_AUG01:			// 0x01	

		case  OP_LXR:			// 0x02

		case  OP_RMPS_RMWS:	    // 0x03

		case  OP_WMS:			// 0x04

		case  OP_DMPI:			// 0x05
		case  OP_MMRB:			// 0x06
		case  OP_MRBM:			// 0x07
		case  OP_MBR:			// 0x08
		case  OP_MBL:			// 0x09
		case  OP_IBR:			// 0x0a
		case  OP_MUR:			// 0x0b
		case  OP_MLR:			// 0x0c
		case  OP_TOR:			// 0x0d
		case  OP_AUG0E:			// 0x0e
		case  OP_LRS:			// 0x0f

			// 0x1X -- reserved for decimal arithmetic

		case  OP_MPR:			// 0x20
		case  OP_DVR:			// 0x21
		case  OP_DAR:			// 0x22
		case  OP_REX:			// 0x23
		case  OP_CAR:			// 0x24**
		case  OP_CIR:			// 0x25
		case  OP_SIA_SIE_SIR:		// 0x26
		case  OP_RIA_RIE_RIR:		//  0x27
		case  OP_RLD_RLQ:			// 0x28
		case  OP_RLS:			// 0x29
		case  OP_RAD_RAQ:		//            0x2a
		case  OP_RAS:			// 0x2b
		case  OP_LLD_LLQ:		//            0x2c
		case  OP_LLS:			// 0x2d
		case  OP_LAD_LAQ:		//            0x2e
		case  OP_LAS:			// 0x2f

		case  OP_FAR_CDIF:		// 0x30
		case  OP_FSR:			// 0x31
		case  OP_FMR:			// 0x32
		case  OP_FDR:			// 0x33
		case  OP_FARD_FARQ_CFDI:	//          0x34
		case  OP_FSRD_FSRQ_CQFF:	//           0x35
		case  OP_FMRD_FMRQ_CDFI:	//           0x36
		case  OP_FDRD_FDRQ_CQFI:	//           0x37
		case  OP_FAM_FAI:	//		            0x38
		case  OP_FSM_FSI:	//		            0x39
		case  OP_FMM_FMI:	//		            0x3a
		case  OP_FDM_FDI:	//		0x3b
		case  OP_FAMD_FAMQ_FAID_FAIQ:	//      0x3c
		case  OP_FSMD_FSMQ_FSID_FSIQ:	//		0x3d
		case  OP_FMMD_FMMQ_FMID_FMIQ:	//      0x3e
		case  OP_FDMD_FDMQ_FDID_FDIQ:	//      0x3f

		case  OP_OCA:			// 0x40
		case  OP_OCB:			// 0x41
		case  OP_OCC:			// 0x42
		case  OP_OCD:			// 0x43
		case  OP_ODA:			// 0x44
		case  OP_ODB:			// 0x45
		case  OP_ODC:			// 0x46
		case  OP_ODD:			// 0x47
		case  OP_ISA:			// 0x48
		case  OP_ISB:			// 0x49
		case  OP_ISC:			// 0x4a
		case  OP_ISD:			// 0x4b
		case  OP_IDA:			// 0x4c
		case  OP_IDB:			// 0x4d
		case  OP_IDC:			// 0x4e
		case  OP_IDD:			// 0x4f

			// -- 0x5x -- reserved for communications

		case  OP_ABR:			// 0x60
		case  OP_SBR:			// 0x61
		case  OP_ZBR:			// 0x62
		case  OP_OBR:			// 0x63
		case  OP_XBR:			// 0x64

		case  OP_LBR:			// 0x65 -- DONE
			tempu16_regs = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER(bit[tempu16_regs]);
			SET_CC_Z(false);
			SET_CC_N(tempu16_regs != 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBR:			// 0x66

		case  OP_GMR:			// 0x67 -- DONE
			tempu16_regs = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER(mask[tempu16_regs]);
			SET_CC_Z(false);
			SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
			SET_CC_O(tempu16_regs == 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADR:			// 0x68
		case  OP_SUR:			// 0x69
		case  OP_ETR:			// 0x6a
		case  OP_ORR:			// 0x6b
		case  OP_XOR:			// 0x6c

		case  OP_TRR:			// 0x6d -- DONE
			tempu16_val1 = GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER(tempu16_val1);
			SET_CC_Z(tempu16_val1 == 0);
			SET_CC_N(tempu16_val1 & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRR:			// 0x6e
		case  OP_TTR:			// 0x6f

		case  OP_ABRB:			// 0x70
		case  OP_SBRB:			// 0x71
		case  OP_ZBRB:			// 0x72
		case  OP_OBRB:			// 0x73
		case  OP_XBRB:			// 0x74

		case  OP_LBRB:			// 0x75 -- DONE
			tempu16_regs = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER(bit[tempu16_regs]);
			SET_CC_Z(false);
			SET_CC_N(tempu16_regs != 0);
			SET_NEXT_PROGRAM_COUNTER( GET_MEMORY(program_counter + 1) );
			break;

		case  OP_TBRB:			// 0x76

		case  OP_GMRB:			// 0x77 - DONE
			tempu16_regs = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER(mask[tempu16_regs]);
			SET_CC_Z(false);
			SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
			SET_CC_O(tempu16_regs == 0);
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			break;

		case  OP_ADRB:			// 0x78
		case  OP_SURB:			// 0x79
		case  OP_ETRB:			// 0x7a
		case  OP_ORRB:			// 0x7b
		case  OP_XORB:			// 0x7c

		case  OP_TRRB:			// 0x7d -- DONE
			tempu16_val1 = GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER(tempu16_val1);
			SET_CC_Z( tempu16_val1 == 0 );
			SET_CC_N( (tempu16_val1 & 0x8000) );
			CONDITIONAL_BRANCH( !cpu_cond_code_z, GET_MEMORY(program_counter + 1), program_counter + 2);
			break;

		case  OP_TERB:			// 0x7e
		case  OP_TTRB:			// 0x7f

		case  OP_ABMM:			// 	        0x80
		case  OP_ZBMM:			// 	        0x81
		case  OP_OBMM:			//  0x82
		case  OP_TBMM:			// x83
		case  OP_ABMB:			// 		    0x84
		case  OP_ZBMB:			// 		    0x85
		case  OP_TBMB:			// 		    0x86
		case  OP_CBMB:			// 		    0x87
		case  OP_LDXT_STXT_DMT_STMT:	//	0x88
		case  OP_NOP:			// :			// 0x89
		case  OP_IRRD_TTRD:			// 	0x8a
		case  OP_CRRT_CRRQ_TTRQ:			// 0x8b
		case  OP_ESD_ESS:			//         0x8c

		case  OP_TRRQ_LDXD:		//        0x8d



		case  OP_CRXD_STXD:		//        0x8e
		case  OP_AUG8F:			//         0x8f

		case  OP_ABSM:			// 	        0x90
		case  OP_ZBSM:			// 	        0x91
		case  OP_OBSM:			// 	        0x92
		case  OP_TBSM:			// 	        0x93
		case  OP_ABSB:			// 	        0x94
		case  OP_ZBSB:			// 	        0x95
		case  OP_TBSB:			// 	        0x96
		case  OP_CBSB:			// 	        0x97
		case  OP_ABXM:			// 	        0x98
		case  OP_ZBXM:			// 	        0x99
		case  OP_OBXM:			// 	        0x9a
		case  OP_TBXM:			// 	        0x9b
		case  OP_ABXB:			// 	        0x9c
		case  OP_ZBXB:			// 	        0x9d
		case  OP_TBXB:			// 	        0x9e
		case  OP_CBXB:			// 	        0x9f

		case  OP_MPM:			// 	        0xa0
		case  OP_DVM:			// 	        0xa1
		case  OP_MPRD_MPMD:		//        0xa2
		case  OP_DVRD_DVMD:		//        0xa3
		case  OP_LFM:			// 	        0xa4
		case  OP_SFM:			// 	        0xa5
		case  OP_HHI:			// 	        0xa6
		case  OP_AUGA7:			//         0xa7
		case  OP_HNS_HNR:			//         0xa8
		case  OP_HZS_HZR:			//         0xa9
		case  OP_HOS_HOR:			//         0xaa
		case  OP_HCS_HCR:			//         0xab
		case  OP_HLS_HGE:			//         0xac
		case  OP_HLE_HGT:			//         0xad
		case  OP_LBX:			// 	        0xae
		case  OP_SBX:			// 	        0xaf

		case  OP_MPS:			// 	        0xb0
		case  OP_DVS:			// 	        0xb1
		case  OP_SCCC:			// 	        0xb2
		case  OP_EXR:			// 	        0xb3
		case  OP_LFS:			// 	        0xb4
		case  OP_SFS:			// 	        0xb5
		case  OP_IRM:			// 	        0xb6
		case  OP_IRR:			// 	        0xb7
		case  OP_MPX:			// 	        0xb8
		case  OP_DVX:			// 	        0xb9
		case  OP_PLM:			// 	        0xba
		case  OP_PSM:			// 	        0xbb
		case  OP_LFX:			// 	        0xbc
		case  OP_SFX:			// 	        0xbd
		case  OP_LDAM_LDVM:		//        0xbe
		case  OP_STAM_STVM:		//        0xbf

		case  OP_ADMM:			// 	        0xc0
		case  OP_ETMM:			// 	        0xc1
		case  OP_ORMM:			// 	        0xc2
		case  OP_CRM:			// 	        0xc3
		case  OP_ADMB:			// 	        0xc4
		case  OP_ETMB:			// 	        0xc5
		case  OP_TRMB:			// 	        0xc6
		case  OP_CRMB:			// 	        0xc7
		case  OP_ADRD_ADMD:		//        0xc8
		case  OP_SURD_SUMD:		//        0xc9
		case  OP_AUGCA:			//         0xca
		case  OP_UIT:			// 	        0xcb
		case  OP_TSBM:			// 	        0xcc

		case  OP_TRRD_LDMD:		//        0xcd
			// LDMD
			if (instruction && 0x0010) {

			}
			// TRRD -- DONE
			else {
				tempu16_regs = GET_SOURCE_REGISTER & 0x000e;
				tempu16_val1 = GET_REGISTER_VALUE(tempu16_regs);
				tempu16_val2 = GET_REGISTER_VALUE(tempu16_regs+1);
			}
			SET_CC_Z(tempu16_val1 == 0 && tempu16_val2 == 0);
			SET_CC_N(tempu16_val1 & 0x8000);
			SET_DESTINATION_REGISTER_DOUBLE( tempu16_val1, tempu16_val2);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CLM_STMD_CLMD:	//        0xce
		case  OP_CRRD_CRMD:		//        0xcf

		case  OP_ADSM:			// 	        0xd0
		case  OP_ETSM:			// 	        0xd1
		case  OP_ORSM:			// 	        0xd2
		case  OP_CRS:			// 	        0xd3
		case  OP_ADSB:			// 	        0xd4
		case  OP_ETSB:			// 	        0xd5
		case  OP_TRSB:			// 	        0xd6
		case  OP_CRSB:			// 	        0xd7
		case  OP_ADXM:			// 	        0xd8
		case  OP_ETXM:			// 	        0xd9
		case  OP_ORXM:			// 	        0xda
		case  OP_CRX:			// 	        0xdb
		case  OP_ADXB:			// 	        0xdc
		case  OP_ETXB:			// 	        0xdd
		case  OP_TRXB:			// 	        0xde
		case  OP_CRXB:			// 	        0xdf

		case  OP_ADM:			// 	        0xe0
		case  OP_SUM:			// 	        0xe1
		case  OP_ETM:			// 	        0xe2
		case  OP_ORM:			// 	        0xe3
		case  OP_XOM:			// 	        0xe4
		case  OP_LDM:			// 	        0xe5
		case  OP_STM:			// 	        0xe6
		case  OP_BRU_BLM:			//         0xe7
		case  OP_AUGE8:			//         0xe8
		case  OP_SUI_CRI:			//         0xe9
		case  OP_ETI_TETI:			//         0xea
		case  OP_ORI_TORI:			//         0xeb
		case  OP_XOI_TXOI:			//         0xec

		case  OP_LDI_LDF_LDFD_FDFQ:    //    0xed
			switch (GET_SOURCE_REGISTER) {
			case 0:	// -------- LDI   load immediate
				SET_DESTINATION_REGISTER( GET_MEMORY(program_counter + 1) );
				SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
				break;
			case 1: // -------- LDF  load floating immediate
				if (TEST_VALID_DOUBLE_REGISTER(GET_DESTINATION_REGISTER)) {
					SET_DESTINATION_REGISTER_DOUBLE(GET_MEMORY(program_counter + 1), 0);
				}
				else {
					UNIMPLEMENTED_INSTRUCTION;
				}
				break;
			case 2:	// -------- LDFD load floating double immediate 
				if (TEST_VALID_TRIPLE_REGISTER(GET_DESTINATION_REGISTER)) {
					SET_DESTINATION_REGISTER_TRIPLE(GET_MEMORY(program_counter + 1), 0, 0);
				}
				else {
					UNIMPLEMENTED_INSTRUCTION;
				}
				break;
			case 3: // -------- LDFQ load floating quad immediate
				if (TEST_VALID_QUAD_REGISTER(GET_DESTINATION_REGISTER)) {
					SET_DESTINATION_REGISTER_QUAD(GET_MEMORY(program_counter + 1), 0, 0, 0);
				}
				else {
					UNIMPLEMENTED_INSTRUCTION;
				}
				break;
			default:
				UNIMPLEMENTED_INSTRUCTION;
				break;
			}
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_STI:			// 	        0xee
		case  OP_BLI:			// 	        0xef

		case  OP_ADS:			// 	        0xf0
		case  OP_SUS:			// 	        0xf1
		case  OP_ETS:			// 	        0xf2
		case  OP_ORS:			// 	        0xf3
		case  OP_XOS:			// 	        0xf4
		case  OP_LDS:			// 	        0xf5
		case  OP_STS:			// 	        0xf6
		case  OP_HOP_BLT:		//         0xf7
		case  OP_ADX:			// 	        0xf8
		case  OP_SUX:			// 	        0xf9
		case  OP_ETX:			// 	        0xfa
		case  OP_ORX:			// 	        0xfb
		case  OP_XOX:			// 	        0xfc
		case  OP_LDX:			// 	        0xfd
		case  OP_STX:			// 	        0xfe
		case  OP_BRX_BLX:			// 	    0xff
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		default:
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		}

		// -------- reset single step if active
		if (gbl_fp_single_step) {
			gbl_fp_single_step = false;
		}

		// --------update some front panel values
		gbl_fp_cc_n_light = cpu_cond_code_n;
		gbl_fp_cc_z_light = cpu_cond_code_z;
		gbl_fp_cc_o_light = cpu_cond_code_o;
		gbl_fp_cc_c_light = cpu_cond_code_c;
	}
	// printf("\n CPU exiting.\n");
}