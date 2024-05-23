#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_opcodes.h"
#include "modcomp_sim_procedures.h"

typedef union {
	unsigned __int16 reg16[16];
	unsigned __int32 reg32[8];
	unsigned __int64 reg64[4];
} REG_BLOCK;

static unsigned __int16 program_counter = 0;	// use local mapping, value 0=65k
static unsigned __int16 cpu_current_register_block = 0;
// static unsigned __int16 cpu_register[16][16] = { 0 }; // [register][registerblock] TODO: Ensure initializtion is complete.
static REG_BLOCK cpu_register[16];	// indexed by register block.

// #define GET_REGISTER_VALUE( REG ) ( cpu_register[REG][cpu_current_register_block])
// #define SET_REGISTER_VALUE( ZREG,ZVAL) {\
// 					cpu_register[ZREG][cpu_current_register_block] = ZVAL; \
// 					}
#define GET_REGISTER_VALUE( REG ) ( cpu_register[cpu_current_register_block].reg16[REG])
#define SET_REGISTER_VALUE( REG,VAL) {\
					cpu_register[cpu_current_register_block].reg16[REG] = VAL; \
					}
// TODO: Make sure endian things don't mess this up!
#define GET_REGISTER_VALUE_DOUBLE( DREG ) ( cpu_register[cpu_current_register_block].reg32[DREG])
#define SET_REGISTER_VALUE_DOUBLE( DREG,DVAL) {\
					cpu_register[cpu_current_register_block].reg32[DREG] = DVAL; \
					}
// TODO: Make sure endian things don't mess this up!
#define GET_REGISTER_VALUE_QUAD( QREG ) ( cpu_register[cpu_current_register_block].reg64[QREG])
#define SET_REGISTER_VALUE_QUAD( QREG,QVAL) {\
					cpu_register[cpu_current_register_block].reg64[QREG] = QVAL; \
					}


// TODO: only set if halted.
void cpu_set_register_value(unsigned __int16 reg_index, unsigned __int16 reg_value) {
	SET_REGISTER_VALUE( reg_index, reg_value );
}

unsigned __int16 cpu_get_register_value(unsigned __int16 reg_index) {
	return GET_REGISTER_VALUE(reg_index);
}

void cpu_set_program_counter(unsigned __int16 pc) {
	program_counter = pc;
}

unsigned __int16 cpu_get_program_counter() {
	return program_counter;
}

void classic_7860_cpu() {

	// -------- local constants
	const unsigned __int16 bit[16] = { 0x8000, 0x4000, 0x2000, 0x1000, 0x0800, 0x0400, 0x0200, 0x0100,
										 0x0080, 0x0040, 0x0020, 0x0010, 0x0008, 0x0004, 0x0002, 0x0001 };
	const unsigned __int16 bitnot[16] = { 0x7fff, 0xbfff, 0xdfff, 0xefff, 0xf7ff, 0xfbff, 0xfdff, 0xfeff,
										  0xff7f, 0xffbf, 0xffdf, 0xffef, 0xfff7, 0xfffb, 0xfffd, 0xfffe };
	const unsigned __int16 mask[16] = { 0x8000, 0xc000, 0xe000, 0xf000, 0xf800, 0xfc00, 0xfe00, 0xff00,
										 0xff80, 0xffc0, 0xffe0, 0xfff0, 0xfff8, 0xfffc, 0xfffe, 0xffff };


	// -------- local values	
	static unsigned __int16 instruction = 0;
	static unsigned __int16 opcode;

	// -------- potentially global values
	static unsigned __int16 cpu_interrupt_active = 0;
	static unsigned __int16 cpu_interrupt_enabled = 0;
	static unsigned __int16 cpu_interrupt_request = 0;
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
	static __int32              temp32_addr_calc = 0;

	// -------- parsed instruction values
	static unsigned __int16		tmp_instr_src = 0;
	static unsigned __int16		tmp_instr_src_dbl = 0;
	static unsigned __int16		tmp_instr_dest = 0;
	static unsigned __int16		tmp_instr_dest_dbl = 0;

	static VAL16				tmp16_src_value = { .uval = 0 };

	static VAL16				tmp16_dest_value = { .uval = 0 };

	static VAL16				tmp16_val1 = { .uval = 0 };
	static VAL16				tmp16_val2 = { .uval = 0 };
	static VAL16				tmp16_val3 = { .uval = 0 };
	static VAL16				tmp16_val4 = { .uval = 0 };
	static VAL16				tmp16_val5 = { .uval = 0 };

	static unsigned __int32		tempu32_val1 = 0;
	static unsigned __int32		tempu32_val2 = 0;
	static unsigned __int32		tempu32_val3 = 0;
	static unsigned __int32		tempu32_val4 = 0;

	static unsigned __int64		tempu64_val1 = 0;
	static unsigned __int64		tempu64_val2 = 0;
	static unsigned __int64		tempu64_val3 = 0;
	static unsigned __int64		tempu64_val4 = 0;

	static signed	__int16		temp16_val10 = 0;
	static int					temp_bit = 0;

	static int j = 0;


#define UNIMPLEMENTED_INSTRUCTION {\
					printf("\n unimplemented instruction %04x\n",instruction);\
					}
#define ILLEGAL_INSTRUCTION {\
					printf("\n Illegal instruction %04x\n",instruction);\
					}

// -------- source register 
#define GET_SOURCE_REGISTER (instruction & 0x000f)
#define GET_SOURCE_REGISTER_DOUBLE ((instruction >> 1 ) & 0x0007)
#define GET_SOURCE_REGISTER_QUAD ((instruction >> 2 ) & 0x0003)

#define GET_SOURCE_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_SOURCE_REGISTER )) 
#define GET_SOURCE_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_SOURCE_REGISTER_DOUBLE )) 
#define GET_SOURCE_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_SOURCE_REGISTER_QUAD )) 

#define SET_SOURCE_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_SOURCE_REGISTER, V1 ); \
					}

// -------- destination register 
#define GET_DESTINATION_REGISTER (( instruction >> 4) & 0x000f )
#define GET_DESTINATION_REGISTER_DOUBLE (( instruction >> 5) & 0x0007 )
#define GET_DESTINATION_REGISTER_QUAD (( instruction >> 6) & 0x0003 )

#define GET_DESTINATION_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_DESTINATION_REGISTER ))
#define GET_DESTINATION_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_DESTINATION_REGISTER_DOUBLE ))
#define GET_DESTINATION_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_DESTINATION_REGISTER_QUAD ))

#define SET_DESTINATION_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_DESTINATION_REGISTER, V1 ); \
					}

#define SET_DESTINATION_REGISTER_VALUE_DOUBLE( DV1 ) {\
					SET_REGISTER_VALUE_DOUBLE(  GET_DESTINATION_REGISTER_DOUBLE, DV1); \
					}

// TODO: finish set_destination_register_value_triple
#define SET_DESTINATION_REGISTER_VALUE_TRIPLE( V1, V2, V3 ) {}

// TODO: finish set_destination_register_value_quad
#define SET_DESTINATION_REGISTER_VALUE_QUAD( QV1 ) {\
					SET_REGISTER_VALUE_QUAD(GET_DESTINATION_REGISTER_QUAD, QV1); \
					}

#define GET_MEMORY_IM( A ) (gbl_mem[(A)])
#define GET_MEMORY_OM( A ) (gbl_mem[(A)])
#define SET_MEMORY_IM( A, VAL ) {\
				gbl_mem[(A)] = VAL;\
				}
#define SET_MEMORY_OM( A, VAL ) {\
				gbl_mem[(A)] = VAL;\
				}

#define GET_MEMORY_IMMEDIATE (GET_MEMORY_IM( program_counter + 1))
#define SET_MEMORY_IMMEDIATE( VAL ) {\
				SET_MEMORY_IM( program_counter + 1, VAL);\
				}

#define GET_MEMORY_SHORT_DISPLACED 	(GET_MEMORY_OM( GET_MEMORY_IMMEDIATE + ( instruction & 0x000f) ))
#define SET_MEMORY_SHORT_DISPLACED( VAL ) {\
				SET_MEMORY_OM( GET_MEMORY_IMMEDIATE + ( instruction & 0x000f) , VAL);\
				}

// TODO: fix when Rs = 0 
#define GET_MEMORY_SHORT_INDEXED (GET_MEMORY_OM( GET_SOURCE_REGISTER_VALUE ))
#define SET_MEMORY_SHORT_INDEXED(VAL) {\
					SET_MEMORY_OM( GET_SOURCE_REGISTER_VALUE, VAL );\
					}

// --------note that the first two are for only for use by GET_MEMORY_DIRECT !!!
#define GET_MEMORY_DIRECT_ADDR_PARITAL 	((instruction & 0x0007) == 0 ? GET_MEMORY_IMMEDIATE : ( (__int32)GET_MEMORY_IMMEDIATE + (__int32)GET_REGISTER_VALUE(instruction & 0x0007)) & 0x0000ffff )
#define GET_MEMORY_DIRECT_ADDR ((instruction & 0x0008) == 0 ? GET_MEMORY_DIRECT_ADDR_PARITAL : GET_MEMORY_OM( GET_MEMORY_DIRECT_ADDR_PARITAL ))
#define GET_MEMORY_DIRECT (GET_MEMORY_OM( GET_MEMORY_DIRECT_ADDR ))
#define SET_MEMORY_DIRECT( VAL ) {\
					SET_MEMORY_OM( GET_MEMORY_DIRECT_ADDR, VAL );\
					}

#define SET_NEXT_PROGRAM_COUNTER(A)	{\
					program_counter = (A);\
					}

#define	SET_CC_N(COND_N) {\
				cpu_cond_code_n = (COND_N); \
				}

#define	SET_CC_Z(COND_Z) {\
				cpu_cond_code_z = (COND_Z); \
				}

#define	SET_CC_O(COND_O) {\
				cpu_cond_code_o = (COND_O); \
				}

#define	SET_CC_C(COND_C) {\
				cpu_cond_code_c = (COND_C); \
				}

// TODO: finish SET_CC_CHAR macro
#define SET_CC_CHAR( VAL ) {\
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

		if ( gbl_verbose_debug )
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
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch ( tmp_instr_dest ) {

				// --  0	RMI
				case 0:
					break;
				// --  1	EVMO
				case 1:
					break;
				// --  2	SIOM
				case 2:
					break;
				// --  3	SOOM
				case 3:
					break;
				// --  4	SZOM
				case 4:
					break;
				// --  5	SCRB
				case 5:
					break;
				// --  6	EXMA
				case 6:
					break;
				// --  7	EXMA
				case 7:
					break;
				// --  8	XVMO
				case 8:
					break;
				// --  9	ZIMP
				case 9:
					break;
				// --  A	UIT
				case 10:
					break;
				// --  B	ZOMP
				case 11:
					break;
				// --  C	UIT
				case 12:
					break;
				// --  D	LIMP
				case 13:
					break;
				// --  E	LOMP
				case 14:
					break;
				// --  F	SOMP
				case 15:
					break;
			}
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LXR:			// 0x02 -- Load Extended Memory Control Register
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RMPS_RMWS:	    // 0x03
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_WMS:			// 0x04 --  Write Memory Status
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DMPI:			// 0x05  --  Initialize Direct Memory Processor
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MMRB:			// 0x06  --  Move Memory File to RegisterBlock Section of Context
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MRBM:			// 0x07  --  Move Register-Block Section of Context File to Memory
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MBR:			// 0x08 -- Move byte right register to register
			tmp16_val1.uval = ( GET_SOURCE_REGISTER_VALUE >> 8 ) & 0x00ff;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set char CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MBL:			// 0x09 -- Move byte left register to register
			tmp16_val1.uval = (GET_SOURCE_REGISTER_VALUE << 8) & 0xff00;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set char CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IBR:			// 0x0a  --  Interchange Bytes Register to Register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val2.uval = ((tmp16_val1.uval << 8) & 0xff00) | ((tmp16_val1.uval >> 8) & 0x00ff);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_CHAR(tmp16_val2.uval & 0x00ff);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MUR:			// 0x0b -- Move upper byte register to register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE  & 0xff00;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MLR:			// 0x0c -- Move lower byte register to register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE & 0x00ff;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TOR:			// 0x0d  --  Transfer One's Complement Register to Register
			tmp16_val1.uval = ~GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_AUG0E:			// 0x0e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LRS:			// 0x0f  --  Left Rotate Single-Register to Register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.uval = (tmp16_val1.uval << 1) | (tmp16_val1.uval >> 15);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_CC_C(tmp16_val1.uval & 0x0001);
			SET_CC_O(false);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


			// 0x1X -- reserved for decimal arithmetic

		case  OP_MPR:			// 0x20  --  Multiply Register By Register 
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DVR:			// 0x21  --  Divide Register By Register
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DAR:			// 0x22
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_REX:			// 0x23  --  Request Executive Service 
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CAR:			// 0x24**
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CIR:			// 0x25
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SIA_SIE_SIR:		// 0x26
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RIA_RIE_RIR:		//  0x27
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RLD_RLQ:			// 0x28
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RLS:			// 0x29  --  Shift Right Logical SingleRegister 
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE >> GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			// TODO Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RAD_RAQ:		//            0x2a
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RAS:			// 0x2b  --  Shift Right Arithmetic Single-Registe
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LLD_LLQ:		//            0x2c
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LLS:			// 0x2d  --  Shift Left Logical SingleRegister 
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE << GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			// TODO Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LAD_LAQ:		//            0x2e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LAS:			// 0x2f  --  Shift Left Arithmetic Single-Register
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_FAR_CDIF:		// 0x30
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FSR:			// 0x31
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FMR:			// 0x32
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FDR:			// 0x33
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FARD_FARQ_CFDI:	//          0x34
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FSRD_FSRQ_CQFF:	//           0x35
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FMRD_FMRQ_CDFI:	//           0x36
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FDRD_FDRQ_CQFI:	//           0x37
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FAM_FAI:	//		            0x38
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FSM_FSI:	//		            0x39
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FMM_FMI:	//		            0x3a
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FDM_FDI:	//		0x3b
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FAMD_FAMQ_FAID_FAIQ:	//      0x3c
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FSMD_FSMQ_FSID_FSIQ:	//		0x3d
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FMMD_FMMQ_FMID_FMIQ:	//      0x3e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_FDMD_FDMQ_FDID_FDIQ:	//      0x3f
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_OCA:			// 0x40  --  Output Command to I/O Group A
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
				(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OCB:			// 0x41  --  Output Command to I/O Group B
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0010;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
				(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OCC:			// 0x42  --  Output Command to I/O Group C
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0020;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
				(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OCD:			// 0x43  --  Output Command to I/O Group D
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0030;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
				(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ODA:			// 0x44  --  Output Data to I/O Group A
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_data_proc[tmp_instr_src] != NULL) {
				(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ODB:			// 0x45  --  Output Data to I/O Group B
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0010;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_data_proc[tmp_instr_src] != NULL) {
				(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ODC:			// 0x46  --  Output Data to I/O Group C
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0020;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_data_proc[tmp_instr_src] != NULL) {
				(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ODD:			// 0x47  --  Output Data to I/O Group D
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0030;
			tmp16_val1.uval = GET_REGISTER_VALUE(GET_DESTINATION_REGISTER);
			if (iop_output_data_proc[tmp_instr_src] != NULL) {
				(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ISA:			// 0x48 -- Input Status from 1/0 Group A
			tmp_instr_src = GET_SOURCE_REGISTER;
			if ( iop_input_status_proc[tmp_instr_src] != NULL) {
				tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE( tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ISB:			// 0x49  --  Input Status from 1/0 Group B
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0010;
			if (iop_input_status_proc[tmp_instr_src] != NULL) {
				tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ISC:			// 0x4a  --  Input Status from 1/0 Group C
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0020;
			if (iop_input_status_proc[tmp_instr_src] != 0) {
				tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ISD:			// 0x4b  --  Input Status from 1/0 Group D
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0030;
			if (iop_input_status_proc[tmp_instr_src] != 0) {
				tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IDA:			// 0x4c  --  Input Data from 1/0 Group A
			tmp_instr_src = GET_SOURCE_REGISTER;
			if (iop_input_data_proc[tmp_instr_src] != 0) {
				tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IDB:			// 0x4d  --  Input Data from 1/0 Group B
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0010;
			if (iop_input_data_proc[tmp_instr_src] != 0) {
				tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IDC:			// 0x4e  --  Input Data from 1/0 Group C
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0020;
			if (iop_input_data_proc[tmp_instr_src] != 0) {
				tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IDD:			// 0x4f  --  Input Data from 1/0 Group D
			tmp_instr_src = GET_SOURCE_REGISTER | 0x0030;
			if (iop_input_data_proc[tmp_instr_src] != 0) {
				tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
			}
			else {
				tmp16_val1.uval = 0;
			}
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

			// -- 0x5x -- reserved for communications

		case  OP_ABR:			// 0x60  --  Add Bit in Register
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = bit[tmp_instr_src];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.sval += tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.sval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SBR:			// 0x61  --  Subtract Bit in Register
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = bit[tmp_instr_src];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.sval -= tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.sval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBR:			// 0x62  --  Zero Bit in Register
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = bit[tmp_instr_src];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval &= ~tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.sval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBR:			// 0x63  --  OR Bit in Register
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = bit[tmp_instr_src];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval |= tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.sval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XBR:			// 0x64
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LBR:			// 0x65 -- DONE
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(tmp_instr_src != 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBR:			// 0x66
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_GMR:			// 0x67 -- DONE
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
			SET_CC_O(tmp_instr_src == 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADR:			// 0x68
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.sval += tmp16_val1.sval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			// TODO Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUR:			// 0x69
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETR:			// 0x6a
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORR:			// 0x6b
			tmp16_src_value.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_dest_value.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_dest_value.uval | tmp16_src_value.uval);
			// TODO Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOR:			// 0x6c
			tmp16_src_value.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_dest_value.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_dest_value.uval ^ tmp16_src_value.uval);
			// TODO Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TRR:			// 0x6d -- DONE
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRR:			// 0x6e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TTR:			// 0x6f
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.sval *= -1;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000));
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ABRB:			// 0x70
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + bit[GET_SOURCE_REGISTER];
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z( tmp16_val1.uval == 0 );
			// SET_CC_N(tempu16_regs != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_SBRB:			// 0x71
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = bit[tmp_instr_src];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.sval -= tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.sval == 0 );
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_ZBRB:			// 0x72
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBRB:			// 0x73
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XBRB:			// 0x74
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LBRB:			// 0x75 -- DONE
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(tmp_instr_src != 0);
			SET_NEXT_PROGRAM_COUNTER( GET_MEMORY_IMMEDIATE );
			break;

		case  OP_TBRB:			// 0x76
			tmp16_val1.uval = GET_REGISTER_VALUE( GET_DESTINATION_REGISTER );
			tmp16_val2.uval = tmp16_val1.uval & bit[GET_SOURCE_REGISTER];
			CONDITIONAL_BRANCH( tmp16_val2.uval !=0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_GMRB:			// 0x77 - DONE
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
			SET_CC_O(tmp_instr_src == 0);
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_IMMEDIATE);
			break;

		case  OP_ADRB:			// 0x78
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.sval += tmp16_val1.sval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			// TODO: This needs to be signed add!
			// TODO: Set CC
			CONDITIONAL_BRANCH(tmp16_val2.sval != 0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_SURB:			// 0x79
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETRB:			// 0x7a
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORRB:			// 0x7b
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XORB:			// 0x7c
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TRRB:			// 0x7d -- DONE
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z( tmp16_val1.uval == 0 );
			SET_CC_N( (tmp16_val1.uval & 0x8000) );
			CONDITIONAL_BRANCH( !cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_TERB:			// 0x7e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TTRB:			// 0x7f
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.sval *= -1;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;


		case  OP_ABMM:			// 	        0x80
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBMM:			// 	        0x81
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBMM:			//  0x82
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBMM:			// x83
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABMB:			// 		    0x84
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBMB:			// 		    0x85
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBMB:			// 		    0x86
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CBMB:			// 		    0x87
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDXT_STXT_DMT_STMT:	//	0x88
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_NOP:			// :			// 0x89
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IRRD_TTRD:			// 	0x8a
			// -------- IRRD
			if ((instruction & 0x0010) != 0) {
				tmp_instr_src_dbl = GET_SOURCE_REGISTER_DOUBLE;
				tmp_instr_dest_dbl = GET_DESTINATION_REGISTER_DOUBLE;
				tempu32_val1 = GET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl);
				SET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl, GET_REGISTER_VALUE_DOUBLE(tmp_instr_dest_dbl));	// DEST -> SRC
				SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest_dbl, tempu32_val1);									// SRC -> DEST
				// TODO: IRRD set cond codes
			}
			// -------- TRRD
			else {
				tmp_instr_src_dbl = GET_SOURCE_REGISTER_DOUBLE;
				tmp_instr_dest_dbl = GET_DESTINATION_REGISTER_DOUBLE;
				tempu32_val1 = GET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl);
				SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest_dbl, tempu32_val1);
				// TODO: TRRD set cond codes
			}
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRRT_CRRQ_TTRQ:			// 0x8b
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ESD_ESS:			//         0x8c
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_TRRQ_LDXD:		//        0x8d
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRXD_STXD:		//        0x8e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_AUG8F:			//         0x8f
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ABSM:			// 	        0x90
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBSM:			// 	        0x91
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBSM:			// 	        0x92
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBSM:			// 	        0x93
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABSB:			// 	        0x94
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBSB:			// 	        0x95
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBSB:			// 	        0x96
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CBSB:			// 	        0x97
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABXM:			// 	        0x98
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBXM:			// 	        0x99
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBXM:			// 	        0x9a
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBXM:			// 	        0x9b
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABXB:			// 	        0x9c
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBXB:			// 	        0x9d
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBXB:			// 	        0x9e
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CBXB:			// 	        0x9f
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_MPM:			// 	        0xa0
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DVM:			// 	        0xa1
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MPRD_MPMD:		//        0xa2
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DVRD_DVMD:		//        0xa3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LFM:			// 	        0xa4
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					SET_REGISTER_VALUE(j, GET_MEMORY_OM(tmp16_val1.uval++));
				}
			}
			else if (tmp_instr_dest >= 1) {
				for (j = tmp_instr_dest; j < 8; j++) {
					SET_REGISTER_VALUE(j, GET_MEMORY_OM(tmp16_val1.uval++));
				}
			}
			else {
				ILLEGAL_INSTRUCTION;
			}
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_SFM:			// 	        0xa5
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					SET_MEMORY_OM(tmp16_val1.uval++, GET_REGISTER_VALUE(j));
				}
			}
			else if (tmp_instr_dest >= 1) {
				for (j = tmp_instr_dest; j < 8; j++) {
					SET_MEMORY_OM(tmp16_val1.uval++, GET_REGISTER_VALUE(j));
				}
			}
			else {
				ILLEGAL_INSTRUCTION;
			}
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_HHI:			// 	        0xa6
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_AUGA7:			//         0xa7
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HNS_HNR:			//         0xa8
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HZS_HZR:			//         0xa9
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HOS_HOR:			//         0xaa
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HCS_HCR:			//         0xab
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HLS_HGE:			//         0xac
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HLE_HGT:			//         0xad
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LBX:			// 	        0xae
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SBX:			// 	        0xaf
			tmp_instr_src = GET_SOURCE_REGISTER;
			if (tmp_instr_src & 0x0001) {
				ILLEGAL_INSTRUCTION;
			}
			else {
				tmp16_val4.uval = GET_DESTINATION_REGISTER_VALUE & 0x00ff;

				tmp16_val1.uval = GET_REGISTER_VALUE(tmp_instr_src);
				tmp16_val5.uval = GET_REGISTER_VALUE(tmp_instr_src + 1);
				memcpy(&temp16_val10, &tmp16_val5.uval, sizeof(temp16_val10));
				// printf("\n after memcpy dest 0x%04x, src 0x%04x\n", temp16_val10, tempu16_val5);
				temp_bit = temp16_val10 & 0x0001;
				temp16_val10 = temp16_val10 >> 1;
				tempu32_val1 = tmp16_val1.uval;
				temp32_addr_calc = (__int32)tempu32_val1 + (__int32)temp16_val10;
				// printf("\n addr calc base 0x%08x, offset 0x%04x, calc 0x%08x\n", tempu32_val1, temp16_val10, temp32_addr_calc);
				tmp16_val3.uval = (unsigned __int16)(temp32_addr_calc & 0x0000ffff);
				// printf("\n to 16 bit 0x%04x, calc 0x%08x\n", tmp16_val3.uval, temp32_addr_calc);
				tmp16_val2.uval = GET_MEMORY_OM(tmp16_val3.uval);
				if (temp_bit != 0) {
					tmp16_val2.uval = (tmp16_val2.uval & 0xff00) | (tmp16_val4.uval);
				}
				else {
					tmp16_val2.uval = (tmp16_val2.uval & 0x00ff) | (tmp16_val4.uval<<8);
				}
				if ( gbl_verbose_debug )
					printf("\n Set byte in mem addr 0x%04x, valu 0x%04x, rx 0x%04x, rx+1 0x%04x\n", tmp16_val3.uval, tmp16_val2.uval,tmp16_val1.uval, tmp16_val5.uval);
				SET_MEMORY_OM(tmp16_val3.uval, tmp16_val2.uval);
				// TODO: make this a macro...
				// TODO: Set CC
			}
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_MPS:			// 	        0xb0
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DVS:			// 	        0xb1
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SCCC:			// 	        0xb2
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_EXR:			// 	        0xb3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LFS:			// 	        0xb4
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SFS:			// 	        0xb5
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_IRM:			// 	        0xb6
			temp32_addr_calc = GET_MEMORY_DIRECT_ADDR;
			tmp16_val1.uval = GET_MEMORY_OM(temp32_addr_calc);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_MEMORY_OM(temp32_addr_calc, tmp16_val2.uval);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		// --------interchange register to register
		case  OP_IRR:			// 	        0xb7
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_SOURCE_REGISTER_VALUE(GET_DESTINATION_REGISTER_VALUE);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set cc based on new dest reg value
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_MPX:			// 	        0xb8
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_DVX:			// 	        0xb9
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_PLM:			// 	        0xba
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_PSM:			// 	        0xbb
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LFX:			// 	        0xbc
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SFX:			// 	        0xbd
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDAM_LDVM:		//        0xbe
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_STAM_STVM:		//        0xbf
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ADMM:			// 	        0xc0
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETMM:			// 	        0xc1
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORMM:			// 	        0xc2
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRM:			// 	        0xc3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADMB:			// 	        0xc4
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETMB:			// 	        0xc5
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TRMB:			// 	        0xc6
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRMB:			// 	        0xc7
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADRD_ADMD:		//        0xc8
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SURD_SUMD:		//        0xc9
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_AUGCA:			//         0xca
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_UIT:			// 	        0xcb
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TSBM:			// 	        0xcc
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_TRRD_LDMD:		//        0xcd
			// LDMD
			if (instruction && 0x0010) {
				UNIMPLEMENTED_INSTRUCTION;
			}
			// TRRD -- 
			// TODO: Take care of illegal register value in instruction
			else {
				tmp_instr_src_dbl = GET_SOURCE_REGISTER_DOUBLE;
				tempu32_val1 = GET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl);
			}
			SET_DESTINATION_REGISTER_VALUE_DOUBLE(tempu32_val1);
			SET_CC_Z(tempu32_val1 == 0 );
			// TODO: check endian
			SET_CC_N(tempu32_val1 & 0x80000000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CLM_STMD_CLMD:	//        0xce
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch (tmp_instr_dest) {
				//--------CLM
				case 0:
					SET_MEMORY_DIRECT(0);
					// TODO: Set CC
					break;
				default:
					UNIMPLEMENTED_INSTRUCTION;
					break;
			}
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_CRRD_CRMD:		//        0xcf
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ADSM:			// 	        0xd0
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETSM:			// 	        0xd1
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORSM:			// 	        0xd2
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRS:			// 	        0xd3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADSB:			// 	        0xd4
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETSB:			// 	        0xd5
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TRSB:			// 	        0xd6
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRSB:			// 	        0xd7
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADXM:			// 	        0xd8
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETXM:			// 	        0xd9
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORXM:			// 	        0xda
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRX:			// 	        0xdb
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADXB:			// 	        0xdc
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETXB:			// 	        0xdd
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TRXB:			// 	        0xde
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRXB:			// 	        0xdf
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ADM:			// 	        0xe0
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUM:			// 	        0xe1
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETM:			// 	        0xe2
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORM:			// 	        0xe3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOM:			// 	        0xe4
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDM:			// 	        0xe5
			SET_DESTINATION_REGISTER_VALUE( GET_MEMORY_DIRECT);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_STM:			// 	        0xe6
			SET_MEMORY_DIRECT( GET_DESTINATION_REGISTER_VALUE );
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_BRU_BLM:			//         0xe7
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
			// BLM
			if (tmp_instr_dest != 0) {
				SET_DESTINATION_REGISTER_VALUE(program_counter + 2);
			}
			// BRU
			else {
				// nothing to do here.
			}
			SET_NEXT_PROGRAM_COUNTER(tmp16_val1.uval);
			break;

		case  OP_AUGE8:			//         0xe8
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUI_CRI:			//         0xe9
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETI_TETI:			//         0xea
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORI_TORI:			//         0xeb
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOI_TXOI:			//         0xec
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDI_LDF_LDFD_FDFQ:    //    0xed
			switch (GET_SOURCE_REGISTER) {
			case 0:	// -------- LDI   load immediate
				SET_DESTINATION_REGISTER_VALUE( GET_MEMORY_IMMEDIATE );
				SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
				break;

			case 1: // -------- LDF  load floating immediate
				if (TEST_VALID_DOUBLE_REGISTER(GET_DESTINATION_REGISTER)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER & 0x000e;
					SET_REGISTER_VALUE(tmp_instr_dest, GET_MEMORY_IM(program_counter + 1));
					SET_REGISTER_VALUE(tmp_instr_dest+1, GET_MEMORY_IM(program_counter + 2));
				}
				else {
					UNIMPLEMENTED_INSTRUCTION;
				}
				SET_NEXT_PROGRAM_COUNTER(program_counter + 3);
				break;

			case 2:	// -------- LDFD load floating double immediate 
				if (TEST_VALID_TRIPLE_REGISTER(GET_DESTINATION_REGISTER)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER & 0x000c;
					SET_REGISTER_VALUE(tmp_instr_dest, GET_MEMORY_IM(program_counter + 1));
					SET_REGISTER_VALUE(tmp_instr_dest + 1, GET_MEMORY_IM(program_counter + 2));
					SET_REGISTER_VALUE(tmp_instr_dest + 2, GET_MEMORY_IM(program_counter + 3));
				}
				else {
					UNIMPLEMENTED_INSTRUCTION;
				}
				SET_NEXT_PROGRAM_COUNTER(program_counter + 4);
				break;

			case 3: // -------- LDFQ load floating quad immediate
				if (TEST_VALID_QUAD_REGISTER(GET_DESTINATION_REGISTER)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER & 0x000c;
					SET_REGISTER_VALUE(tmp_instr_dest, GET_MEMORY_IM(program_counter + 1));
					SET_REGISTER_VALUE(tmp_instr_dest + 1, GET_MEMORY_IM(program_counter + 2));
					SET_REGISTER_VALUE(tmp_instr_dest + 2, GET_MEMORY_IM(program_counter + 3));
					SET_REGISTER_VALUE(tmp_instr_dest + 3, GET_MEMORY_IM(program_counter + 4));
				}
				else {
					UNIMPLEMENTED_INSTRUCTION;
				}
				SET_NEXT_PROGRAM_COUNTER(program_counter + 5);
				break;

			default:
				UNIMPLEMENTED_INSTRUCTION;
				SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
				break;
			}
			break;

		case  OP_STI:			// 	        0xee
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_MEMORY_IMMEDIATE(tmp16_val1.uval);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_BLI:			// 	        0xef
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ADS:			// 	        0xf0
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUS:			// 	        0xf1
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETS:			// 	        0xf2
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORS:			// 	        0xf3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOS:			// 	        0xf4
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDS:			// 	        0xf5
			SET_DESTINATION_REGISTER_VALUE( GET_MEMORY_SHORT_DISPLACED );
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_STS:			// 	        0xf6
			SET_MEMORY_SHORT_DISPLACED( GET_DESTINATION_REGISTER_VALUE );
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_HOP_BLT:		//         0xf7
			tmp16_val1.uval = instruction & 0x007f;
			if (tmp16_val1.uval & 0x0040) {
				tmp16_val1.uval |= 0xff80;	// sign extend.
			}
			tmp16_val2.uval = program_counter;
			tmp16_val2.sval += tmp16_val1.sval;
			// TODO Make hop calc macro.
			SET_NEXT_PROGRAM_COUNTER( tmp16_val2.uval );
			break;

		case  OP_ADX:			// 	        0xf8
			tmp16_val1.uval = GET_MEMORY_SHORT_INDEXED;
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.sval += tmp16_val1.sval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUX:			// 	        0xf9
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETX:			// 	        0xfa
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORX:			// 	        0xfb
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOX:			// 	        0xfc
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDX:			// 	        0xfd
			SET_DESTINATION_REGISTER_VALUE(GET_MEMORY_SHORT_INDEXED);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_STX:			// 	        0xfe
			SET_MEMORY_SHORT_INDEXED( GET_DESTINATION_REGISTER_VALUE );
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_BRX_BLX:			// 	    0xff
			tmp_instr_dest = GET_DESTINATION_REGISTER;

			// BRX
			if (tmp_instr_dest == 0) {
				// nothing extra to do here...
			}
			// BLX
			else {
				SET_DESTINATION_REGISTER_VALUE(program_counter + 1);
			}
			SET_NEXT_PROGRAM_COUNTER(GET_SOURCE_REGISTER_VALUE);
			break;

		default:
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		}

		// -------- reset single step if active
		if (gbl_fp_single_step) {
			gbl_fp_single_step = false;
			WakeByAddressSingle(&gbl_fp_single_step);
		}

		// --------update some front panel values
		// TODO: make these getters so not to slow down CPU.
		gbl_fp_cc_n_light = cpu_cond_code_n;
		gbl_fp_cc_z_light = cpu_cond_code_z;
		gbl_fp_cc_o_light = cpu_cond_code_o;
		gbl_fp_cc_c_light = cpu_cond_code_c;
	}
	// printf("\n CPU exiting.\n");
}