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

// -------- individual parts of the PSD
static unsigned __int16 program_counter = 0;	// use local mapping, value 0=65k
// -------- individual parts os the PWD
static unsigned __int8 cpu_register_block = 0;		// GRB
static unsigned __int8 cpu_operand_map = 0;		// OM
static unsigned __int8 cpu_instruction_map = 0;	// IM
static bool cpu_priv_mode = true;							// PRV
static bool cpu_overflow_hist = false;						// OH
static bool cpu_cond_code_n = false;						// CC N
static bool cpu_cond_code_z = false;						// CC Z
static bool cpu_cond_code_o = false;						// CC O
static bool cpu_cond_code_c = false;						// CC C

static REG_BLOCK cpu_register[16];	// indexed by register block.

// #define GET_REGISTER_VALUE( REG ) ( cpu_register[REG][cpu_register_block])
// #define SET_REGISTER_VALUE( ZREG,ZVAL) {\
// 					cpu_register[ZREG][cpu_register_block] = ZVAL; \
// 					}
#define GET_REGISTER_VALUE( REG ) ( cpu_register[cpu_register_block].reg16[REG])
#define SET_REGISTER_VALUE( REG,VAL) {\
					cpu_register[cpu_register_block].reg16[REG] = VAL; \
					}
// TODO: Make sure endian things don't mess this up!
#define GET_REGISTER_VALUE_DOUBLE( DREG ) ( cpu_register[cpu_register_block].reg32[DREG])
#define SET_REGISTER_VALUE_DOUBLE( DREG,DVAL) {\
					cpu_register[cpu_register_block].reg32[DREG] = DVAL; \
					}
// TODO: Make sure endian things don't mess this up!
#define GET_REGISTER_VALUE_QUAD( QREG ) ( cpu_register[cpu_register_block].reg64[QREG])
#define SET_REGISTER_VALUE_QUAD( QREG,QVAL) {\
					cpu_register[cpu_register_block].reg64[QREG] = QVAL; \
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

PSW cpu_get_current_PSW() {
	PSW loc_psw = { .all = 0 };
	loc_psw.sep.cc_c = cpu_cond_code_c;
	loc_psw.sep.cc_o = cpu_cond_code_o;
	loc_psw.sep.cc_z = cpu_cond_code_z;
	loc_psw.sep.cc_n = cpu_cond_code_n;
	loc_psw.sep.oh = cpu_overflow_hist;
	loc_psw.sep.om = cpu_operand_map;
	loc_psw.sep.im = cpu_instruction_map;
	loc_psw.sep.grb = cpu_register_block;
	loc_psw.sep.prv = cpu_priv_mode;
	return loc_psw;
}

// --------this should be local to this module only!!
void cpu_set_current_PSW( PSW new_psw ) {
	cpu_cond_code_c = new_psw.sep.cc_c;
	cpu_cond_code_o = new_psw.sep.cc_o;
	cpu_cond_code_z = new_psw.sep.cc_z;
	cpu_cond_code_n = new_psw.sep.cc_n;
	cpu_overflow_hist = new_psw.sep.oh;
	cpu_operand_map = new_psw.sep.om;
	cpu_instruction_map = new_psw.sep.im;
	cpu_register_block = new_psw.sep.grb;
	cpu_priv_mode = new_psw.sep.prv;
	return;
}


void cpu_classic_7860() {

	// -------- local constants
	const unsigned __int16 bit[16] = { 0x8000, 0x4000, 0x2000, 0x1000, 0x0800, 0x0400, 0x0200, 0x0100,
										 0x0080, 0x0040, 0x0020, 0x0010, 0x0008, 0x0004, 0x0002, 0x0001 };
	const unsigned __int16 bitnot[16] = { 0x7fff, 0xbfff, 0xdfff, 0xefff, 0xf7ff, 0xfbff, 0xfdff, 0xfeff,
										  0xff7f, 0xffbf, 0xffdf, 0xffef, 0xfff7, 0xfffb, 0xfffd, 0xfffe };
	const unsigned __int16 mask[16] = { 0x8000, 0xc000, 0xe000, 0xf000, 0xf800, 0xfc00, 0xfe00, 0xff00,
										 0xff80, 0xffc0, 0xffe0, 0xfff0, 0xfff8, 0xfffc, 0xfffe, 0xffff };


	// -------- local values
	
	INSTRUCTION instruction = { .all = 0 };

	//	static unsigned __int16 instruction = 0;
	// static unsigned __int16 opcode;

	// -------- potentially global values

	// -------- interrupts
	//		0	Power Fail Safe / Auto Start
	//		1	Memory Parity
	//		2	System Protect
	//		3	Multiprocessor Communications
	//		4	Unimplemented Instruction Trap
	//		5	Floating Point Overflow
	//		6	Real Time Clock
	//		7	External
	//		8	External
	//		9	External
	//		10	External
	//		11	External
	//		12	I/O Data Party Line
	//		13	I/O Service Party Line
	//		14	Console Interrupt
	//		15	Task Scheduler
	//

	static unsigned __int16 cpu_interrupt_active = 0;
	static unsigned __int16 cpu_interrupt_enabled = 0;
	static unsigned __int16 cpu_interrupt_request = 0;

	static bool cpu_virtual_mode = false;


	// -------- for temporary use
	static __int32              temp32_addr_calc = 0;

	// -------- parsed instruction values
	static unsigned __int16		tmp_instr_src = 0;
	static unsigned __int16		tmp_instr_src_dbl = 0;
	static unsigned __int16		tmp_instr_dest = 0;
	static unsigned __int16		tmp_instr_dest_dbl = 0;

	static VAL16				tmp16_src_value = { .uval = 0 };
	static VAL16				tmp16_dest_value = { .uval = 0 };
	static VAL16				tmp16_result_value = { .uval = 0 };

	static VAL16				tmp16_val1 = { .uval = 0 };
	static VAL16				tmp16_val2 = { .uval = 0 };
	static VAL16				tmp16_val3 = { .uval = 0 };
	static VAL16				tmp16_val4 = { .uval = 0 };
	static VAL16				tmp16_val5 = { .uval = 0 };

	static VAL32				tmp32_val1 = { .uval = 0 };
	static VAL32				tmp32_val2 = { .uval = 0 };
	static VAL32				tmp32_val3 = { .uval = 0 };
	static VAL32				tmp32_val4 = { .uval = 0 };

	static unsigned __int64		tempu64_val1 = 0;
	static unsigned __int64		tempu64_val2 = 0;
	static unsigned __int64		tempu64_val3 = 0;
	static unsigned __int64		tempu64_val4 = 0;

	static signed	__int16		temp16_val10 = 0;
	static int					temp_bit = 0;

	static int j = 0;
	bool do_branch = false;

	char op_code_string[20] = "";

#define UNIMPLEMENTED_INSTRUCTION {\
					printf("\n unimplemented instruction %04x\n",instruction.all);\
					gbl_fp_runlight = false;\
					printf("\nCpu halted.  pc = 0x%04x\n", program_counter);\
					cmd_process_print_prompt();\
					}
#define ILLEGAL_INSTRUCTION {\
					printf("\n Illegal instruction %04x\n",instruction.all);\
					gbl_fp_runlight = false;\
					printf("\nCpu halted.  pc = 0x%04x\n", program_counter);\
					cmd_process_print_prompt();\
					}

// -------- source register 
#define GET_SOURCE_REGISTER (instruction.parts.src_reg )
#define GET_SOURCE_REGISTER_DOUBLE ( (instruction.parts.src_reg >> 1 ) )
#define GET_SOURCE_REGISTER_QUAD ( (instruction.parts.src_reg >> 2 ) )

#define GET_SOURCE_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_SOURCE_REGISTER )) 
#define GET_SOURCE_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_SOURCE_REGISTER_DOUBLE )) 
#define GET_SOURCE_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_SOURCE_REGISTER_QUAD )) 

#define SET_SOURCE_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_SOURCE_REGISTER, V1 ); \
					}

// -------- destination register 
#define GET_DESTINATION_REGISTER ( instruction.parts.dest_reg  )
#define GET_DESTINATION_REGISTER_DOUBLE (( instruction.parts.dest_reg >> 1)  )
#define GET_DESTINATION_REGISTER_QUAD (( instruction.parts.dest_reg >> 2) )  

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
#define GET_MEMORY_IMMEDIATE_2ND (GET_MEMORY_IM( program_counter + 2))

#define GET_MEMORY_SHORT_DISPLACED 	(GET_MEMORY_OM( GET_REGISTER_VALUE(1) + ( instruction.parts.src_reg) ))
#define SET_MEMORY_SHORT_DISPLACED( VAL ) {\
				SET_MEMORY_OM( GET_REGISTER_VALUE(1) + ( instruction.parts.src_reg) , VAL);\
				}

// TODO: fix when Rs = 0 
#define GET_MEMORY_SHORT_INDEXED (GET_MEMORY_OM( GET_SOURCE_REGISTER_VALUE ))
#define SET_MEMORY_SHORT_INDEXED(VAL) {\
					SET_MEMORY_OM( GET_SOURCE_REGISTER_VALUE, VAL );\
					}

// --------note that the first two are for only for use by GET_MEMORY_DIRECT !!!
#define GET_MEMORY_DIRECT_ADDR_PARITAL 	((instruction.all & 0x0007) == 0 ? GET_MEMORY_IMMEDIATE : ( (__int32)GET_MEMORY_IMMEDIATE + (__int32)GET_REGISTER_VALUE(instruction.all & 0x0007)) & 0x0000ffff )
#define GET_MEMORY_DIRECT_ADDR ((instruction.all & 0x0008) == 0 ? GET_MEMORY_DIRECT_ADDR_PARITAL : GET_MEMORY_OM( GET_MEMORY_DIRECT_ADDR_PARITAL ))
#define GET_MEMORY_DIRECT (GET_MEMORY_OM( GET_MEMORY_DIRECT_ADDR ))
#define SET_MEMORY_DIRECT( VAL ) {\
					SET_MEMORY_OM( GET_MEMORY_DIRECT_ADDR, VAL );\
					}

#define GET_HOP_OFFSET ( ( instruction.all & 0x0040 ) ? (instruction.all & 0x007f) | 0xff80 : instruction.all & 0x007f )
#define GET_NEXT_PROGRAM_COUNTER_HOP  ( program_counter + GET_HOP_OFFSET )

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

//	TODO: Verify these CC test macros !!!
// -------- less than or equal to
#define TEST_CC_LE (   cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) )
// -------- greater than
#define TEST_CC_GT ( !(cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) ))
			
// -------- greater than or equal to
#define TEST_CC_GE (cpu_cond_code_z || ( !cpu_cond_code_z && ( cpu_cond_code_n != cpu_cond_code_c ) ) )
// -------- less than
#define TEST_CC_LT (cpu_cond_code_n != cpu_cond_code_o) 
				    
// -------- Not higher
#define TEST_CC_NH	( !cpu_cond_code_c || cpu_cond_code_z )
// -------- Higher than
#define TEST_CC_HI	( !(!cpu_cond_code_c || cpu_cond_code_z) )
					
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

// --------allowed set interrupt masks
#define SIA_ALLOWED 0x7fff
#define SIE_ALLOWED 0xffff		// some disables are prohibited.
#define SIR_ALLOWED 0xffff		// level C and D should not be program requested, but it isn't prevented.

#define RIA_ALLOWED_NOT	( ~0xffff )		
#define RIE_ALLOWED_NOT	( ~0x53ff )		// -- not allowed to reset enable 0, 2, 4, 5 	
#define RIR_ALLOWED_NOT ( ~0xffff )

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

		// -------- fetch the next instruction
		instruction.all = gbl_mem[program_counter];


		// -------- if verbose display the instruction.
		if (gbl_verbose_debug) {

			util_get_opcode_disp(instruction.all, op_code_string, 20);

			printf("\npc: %04X \t%s  \tinstruction: %04x,  op code: %04x   \n",
				program_counter, op_code_string, instruction.all, instruction.parts.op_code  );
		}

		switch ( instruction.parts.op_code) {

			// --------00 -- HLT -- Halt (Privileged)          
		case  OP_HLT:			// 0x00
			// cpu_run = false;
			gbl_fp_runlight = false;
			printf("\nCpu halted.  pc = 0x%04x\n",program_counter);
			cmd_process_print_prompt();
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_AUG01:			// 0x01	
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch ( tmp_instr_dest ) {

				// --  0	RMI -- Request Multi·processor Interrupt
				case 0:
					break;
				// --  1	EVMO -- Enter Virtual Mode of CPU Execution
				case 1:
					break;
				// --  2	SIOM -- Select Another Program's IM
				case 2:
					break;
				// --  3	SOOM -- Select Another Program's OM as Current OM
				case 3:
					break;
				// --  4	SZOM -- Select Map Zero as Current OM
				case 4:
					break;
				// --  5	SCRB -- Select Current Register Block in PSD
				case 5:
					break;
				// --  6	EXMA -- Enter Extended Memory Addressing Mode
				// --  7	EXMA -- Enter Extended Memory Addressing Mode
				case 6:
				case 7:
					break;
				// --  8	XVMO -- Exit Virtual Mode of CPU Execution
				case 8:
					break;
				// --  9	ZIMP -- Zero Section of Instruction Map
				case 9:
					break;
				// --  B	ZOMP -- Zero Section of Operand Map
				case 11:
					break;
				// --  D	LIMP -- Load Instruction Map Image into Hardware Map
				case 13:
					break;
				// --  E	LOMP -- Load Operand Map Image Into Hardware Map
				case 14:
					break;
				// --  F	SOMP -- Store Operand Map into Map Image
				case 15:
					break;
					// --  A	UIT
				case 10:
					// --  C	UIT
				case 12:
				default:
					break;
			}
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LXR:			// 0x02 -- Load Extended Memory Control Register
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RMPS_RMWS:	    // 0x03 - RMPS -- Read Memory Plane Status        
								//		  RMWS -- Read Memory Word Status
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
			tmp_instr_src = GET_SOURCE_REGISTER;
			switch (tmp_instr_src) {
				// -------- 0	TRO  -- Transfer and Reset Overflow Status History      
				case 0:
					SET_DESTINATION_REGISTER_VALUE( ( cpu_overflow_hist ? 1 : 0 ) );
					cpu_overflow_hist = 0;
					break;
				// -------- 1	LCPS  -- Load Register with Current Program Status Register of PSD   
				case 1:
					SET_DESTINATION_REGISTER_VALUE( cpu_get_current_PSW().all );
					break;
				// -------- 2	LCPR  -- Load Register with Current Program Register of PSD   
				case 2:
					SET_DESTINATION_REGISTER_VALUE(program_counter);
					// TODO: Set CC
					break;
				// -------- 3	LCCC  -- Load Register with Current Condition Code of PSD   
				case 3:
					SET_DESTINATION_REGISTER_VALUE( cpu_get_current_PSW().all & 0x000f);
					break;
				// -------- 4	LCIA  -- Load Register with Current Interrupt Active Latches    
				case 4:
					SET_DESTINATION_REGISTER_VALUE(cpu_interrupt_active);
					break;
				// -------- 5	LCIE  -- Load Register with Current Interrupt Enable Latches     
				case 5:
					SET_DESTINATION_REGISTER_VALUE(cpu_interrupt_enabled);
					break;
				// -------- 6	LCIR  -- Load Register with Current Interrupt Request Latches     
				case 6:
					SET_DESTINATION_REGISTER_VALUE(cpu_interrupt_request);
					break;
				// -------- 7	MBVV  -- Move Virtual Block to Virtual Block       
				case 7:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- 8	MBVE  -- Move Block from Virtual to Extended Memory     
				case 8:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- 9	MBEV  -- Move Block from Extended to Virtual Memory     
				case 9:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- A	MPES  -- Multiply Immediate with Extended Sign       
				case 10:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- B	DVES  -- Divide lmmediate with Extended Sign       
				case 11:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- C	RDIR  -- Read Internal Registers         
				case 12:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- D	WIR  -- Write Internal Register         
				case 13:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- E	BRM  -- Branch to Microroutine Immediate        
				case 14:
					UNIMPLEMENTED_INSTRUCTION;
					break;
				// -------- F	BRMI  -- Branch to Microroutine Immediate        
				case 15:
					UNIMPLEMENTED_INSTRUCTION; // TODO: THis is actually a two word instruction.
					break;
				default:
					ILLEGAL_INSTRUCTION;
					break;
			}
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

		case  OP_DAR:			// 0x22 ???? MAYBE NOT
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
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			tmp16_val1.uval = bit[GET_SOURCE_REGISTER];
			switch (tmp_instr_dest) {
				// SIA  --  Set Interrupt Active         
				case 0:
					cpu_interrupt_active |= ( tmp16_val1.uval & SIA_ALLOWED );
					break;
				// SIE  --  Set Interrupt Enable         
				case 4:
					cpu_interrupt_enabled |= ( tmp16_val1.uval & SIE_ALLOWED );
					break;
				// SIR  --  Set Interrupt Request         
				case 8:
					cpu_interrupt_request |= ( tmp16_val1.uval & SIR_ALLOWED );
					break;
				default:
					UNIMPLEMENTED_INSTRUCTION;
					break;
			}
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_RIA_RIE_RIR:		//  0x27
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			tmp16_val1.uval = bitnot[GET_SOURCE_REGISTER];
			switch (tmp_instr_dest) {
				// RIA  --  Reset Interrupt Active         
			case 0:
				cpu_interrupt_active &= (tmp16_val1.uval | RIA_ALLOWED_NOT);
				break;
				// RIE  --  Reset Interrupt Enable         
			case 4:
				cpu_interrupt_enabled &= (tmp16_val1.uval | RIE_ALLOWED_NOT);
				break;
				// RIR  --  Reset Interrupt Request         
			case 8:
				cpu_interrupt_request &= (tmp16_val1.uval | RIR_ALLOWED_NOT);
				break;
			default:
				UNIMPLEMENTED_INSTRUCTION;
				break;
			}
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
			// TODO: Set CC
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
			// TODO: Set CC
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

		case  OP_ISA:			// 0x48
			tmp_instr_src = GET_SOURCE_REGISTER;
			// -------- ISZ - Input Status from Device Zero
			// TODO: or in "relocatable mode"
			if (tmp_instr_src == 0) {
				tmp16_val1.uval = 0x2000;		// classic cpu.  
			}
			// -------- ISA - Input Status from 1/0 Group A
			else {
				if (iop_input_status_proc[tmp_instr_src] != NULL) {
					tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
				}
				else {
					tmp16_val1.uval = 0;
				}
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
			tmp16_val1.uval = bit[GET_SOURCE_REGISTER];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE + tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SBR:			// 0x61  --  Subtract Bit in Register
			tmp_instr_src = GET_SOURCE_REGISTER;
			tmp16_val1.uval = bit[tmp_instr_src];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval -= tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBR:			// 0x62  --  Zero Bit in Register
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & bitnot[GET_SOURCE_REGISTER];
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
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
			SET_CC_Z(tmp16_val2.uval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XBR:			// 0x64  --  Exclusive OR Bit in Register       
			tmp16_val1.uval = bit[GET_SOURCE_REGISTER];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE ^ tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LBR:			// 0x65 -- Load Bit in Register        
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(tmp_instr_src != 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBR:			// 0x66 -- Test Bit(s) in Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000) != 0);
			SET_CC_C((tmp16_val1.uval & bit[GET_SOURCE_REGISTER]) != 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_GMR:			// 0x67 -- Generate· Mask in Register (Load Negative. Power of Two)   
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
			SET_CC_O(tmp_instr_src == 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADR:			// 0x68  --  Add Register to Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUR:			// 0x69 --  Subtract Register from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE - GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETR:			// 0x6a  -- Extract Register from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORR:			// 0x6b  -- OR Register to Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOR:			// 0x6c -- Exclusive OR Register to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TRR:			// 0x6d -- Transfer Register to Register        
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRR:			// 0x6e
			tmp16_src_value.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_dest_value.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_result_value.uval =  tmp16_dest_value.uval - tmp16_src_value.uval;
			// TODO: Set CC
			SET_CC_Z(tmp16_result_value.uval == 0);
			SET_CC_N( (tmp16_result_value.uval & 0x8000) != 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TTR:			// 0x6f  --  Transfer Two's Complement of Register to Register     
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.sval *= -1;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000));
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABRB:			// 0x70  --  Add Bit in Register and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + bit[GET_SOURCE_REGISTER];
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z( tmp16_val1.uval == 0 );
			// SET_CC_N(tempu16_regs != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_SBRB:			// 0x71  --  Subtract Bit in Register and Branch if Nonzero    
			tmp16_val1.uval = bit[GET_SOURCE_REGISTER];
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE - tmp16_val1.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0 );
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_ZBRB:			// 0x72  --  Zero Bit in Register and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & bitnot[GET_SOURCE_REGISTER];
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_OBRB:			// 0x73  --  OR Bit in Register and Branch Unconditionally     
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | bit[GET_SOURCE_REGISTER];
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(false);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_IMMEDIATE);
			break;

		case  OP_XBRB:			// 0x74  --       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ bit[GET_SOURCE_REGISTER];
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			// SET_CC_N(tmp_instr_src != 0);
			// SET_NEXT_PROGRAM_COUNTER(GET_MEMORY(program_counter + 1));
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_LBRB:			// 0x75 -- Load Bit in Register and Branch Unconditionally
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(tmp_instr_src != 0);
			SET_NEXT_PROGRAM_COUNTER( GET_MEMORY_IMMEDIATE );
			break;

		case  OP_TBRB:			// 0x76  --  Test Bit in Register and Branch if One    
			tmp16_val1.uval = GET_REGISTER_VALUE( GET_DESTINATION_REGISTER );
			tmp16_val2.uval = tmp16_val1.uval & bit[GET_SOURCE_REGISTER];
			// TODO: Set other CC
			CONDITIONAL_BRANCH( tmp16_val2.uval !=0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_GMRB:			// 0x77  --  Generate Mask in Register and Branch Unconditionally     
			tmp_instr_src = GET_SOURCE_REGISTER;
			SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
			SET_CC_Z(false);
			SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
			SET_CC_O(tmp_instr_src == 0);
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_IMMEDIATE);
			break;

		case  OP_ADRB:			// 0x78  --  Add Register to Register and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: This needs to be signed add!
			// TODO: Set CC
			CONDITIONAL_BRANCH(tmp16_val1.uval != 0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_SURB:			// 0x79  --  Subtract Register from Register and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE - GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			CONDITIONAL_BRANCH(tmp16_val1.uval != 0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_ETRB:			// 0x7a  --  Extract Register from Register and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			CONDITIONAL_BRANCH(tmp16_val1.uval != 0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_ORRB:			// 0x7b  --  OR Register to Register and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000));
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_XORB:			// 0x7c  --  Exclusive OR Register to Register and Branch if Nonzero   
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_TRRB:			// 0x7d --  Transfer Register to Register and Branch if Nonzero    
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z( tmp16_val1.uval == 0 );
			SET_CC_N( (tmp16_val1.uval & 0x8000) );
			CONDITIONAL_BRANCH( !cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_TERB:			// 0x7e  --  Test Register and Branch if any Ones Compare    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_SOURCE_REGISTER_VALUE;
			// TODO: Set CC
			CONDITIONAL_BRANCH(tmp16_val1.uval != 0, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_TTRB:			// 0x7f  --  Transfer Two's Complement of Register to Register and Branch if Nonzero 
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.sval *= -1;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N((tmp16_val1.uval & 0x8000));
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;


		case  OP_ABMM:			// 	        0x80  --  Add Bit in Memory            
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT + tmp16_val1.uval;
			SET_MEMORY_DIRECT(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_ZBMM:			// 	        0x81  --  Zero Bit in Memory        
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT & tmp16_val1.uval;
			SET_MEMORY_DIRECT(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_OBMM:			//  0x82  --  OR Bit in Memory·        
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT | tmp16_val1.uval;
			SET_MEMORY_DIRECT(tmp16_val2.uval);
			SET_CC_Z(false);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_TBMM:			// x83  --  Test Bit(s) in Memory        
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT & tmp16_val1.uval;
			SET_CC_Z(tmp16_val2.uval != 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_ABMB:			// 		    0x84  --  Add Bit in Memory and Branch if Nonzero    
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT + tmp16_val1.uval;
			SET_MEMORY_DIRECT(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_ZBMB:			// 		    0x85  --  Zero Bit in Memory and Branch if Nonzero    
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT & tmp16_val1.uval;
			SET_MEMORY_DIRECT(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_TBMB:			// 		    0x86  --  Test Bit(s) in Memory and Branch if One    
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_DIRECT & tmp16_val1.uval;
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
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
			// -------- IRRD  --  Interchange Double Register and Double Register      
			if ((instruction.all & 0x0010) != 0) {
				tmp_instr_src_dbl = GET_SOURCE_REGISTER_DOUBLE;
				tmp_instr_dest_dbl = GET_DESTINATION_REGISTER_DOUBLE;
				tmp32_val1.uval = GET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl);
				SET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl, GET_REGISTER_VALUE_DOUBLE(tmp_instr_dest_dbl));	// DEST -> SRC
				SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest_dbl, tmp32_val1.uval);									// SRC -> DEST
				// TODO: IRRD set cond codes
			}
			// -------- TTRD  --  Transfer Two's Complement of Double- Register to Double-Register    
			else {
				tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE; 
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val1.uval);
				// TODO: check double word order in registers.
				SET_CC_Z(tmp32_val1.uval == 0);
				SET_CC_N( (tmp32_val1.uval & 0x80000000) != 0);
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
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch (tmp_instr_dest) {
				case 0:		// --  BXNS	Branch (Short-Indexed) on Condition Code N Set     
					do_branch = cpu_cond_code_n;
					break;
				case 1:		// --  BXZS	Branch (Short-Indexed) on Condition Code Z Set     
					do_branch = cpu_cond_code_z;
					break;
				case 2:		// --  BXOS	Branch (Short-Indexed) On Condition Code O Set     
					do_branch = cpu_cond_code_o;
					break;
				case 3:		// --  BXCS	Branch (Short-Indexed) on Condition Code C Set     
					do_branch = cpu_cond_code_c;
					break;
				case 4:		// --  BXLS	Branch (Short-Indexed) on Less Than Condition      
					do_branch = TEST_CC_LT;
					break;
				case 5:		// --  BXLE	Branch (Short-Indexed) on Less Than or Equal Condition    
					do_branch = TEST_CC_LE;  // CCZ .OR. (CCN .XOR. CCO)
					break;
				case 6:		// --  BXHI	Branch (Short-Indexed) on Magnitude Higher Condition      
					do_branch = TEST_CC_HI;	// !CCC .or. CCZ
					break;
				case 8:		// --  BXNR	Branch (Short-Indexed) on Condition Code N Reset     
					do_branch = !cpu_cond_code_n;
					break;
				case 9:		// --  BXZR	Branch (Short-Indexed) on Condition Code Z Reset     
					do_branch = !cpu_cond_code_z;
					break;
				case 10:		// --  BXOR	Branch (Short-Indexed) On Condition Code O Reset     
					do_branch = !cpu_cond_code_o;
					break;
				case 11:		// --  BXCR	Branch (Short-Indexed) on Condition Code C Reset     
					do_branch = !cpu_cond_code_c;
					break;
				case 12:		// --  BXGE	Branch (Short-Indexed) on Greater Than or Equal Condition    
					do_branch = TEST_CC_GE;  //  CCZ .or. (!CCZ .AND. (!CCN .XOR. CCO))
					break;
				case 13:		// --  BXGT	Branch (Short-Indexed) on Greater Than Condition      
					do_branch = TEST_CC_GT;  //  CCZ .or. (CCN .XOR. CCO)
					break;
				case 14:		// --  BXNH	Branch (Short-Indexed) on Magnitude Not Higher Condition     
					do_branch = TEST_CC_NH;  // !CCC .or CCZ
					break;
				default:
					ILLEGAL_INSTRUCTION;
					do_branch = false;
					break;
			}
			if (do_branch) {
				SET_DESTINATION_REGISTER_VALUE(program_counter + 1);
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_SHORT_INDEXED);
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			}
			break;


		case  OP_ABSM:			// 	        0x90  --  Add Bit in Memory (Short-Displaced)       
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED + tmp16_val1.uval;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBSM:			// 	        0x91  --  Zero Bit in Memory (Short-Displaced}       
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED & tmp16_val1.uval;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBSM:			// 	        0x92  --  OR Bit in Memory (Short-Displaced)       
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED | tmp16_val1.uval;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val2.uval);
			SET_CC_Z(false);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBSM:			// 	        0x93  --  Test Bit(s) in Memory (Short-Displaced)       
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED & tmp16_val1.uval;
			SET_CC_Z(tmp16_val2.uval != 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABSB:			// 	        0x94  --  Add Bit in Memory (Short-Displaced) and Branch if Nonzero   
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED + tmp16_val1.uval;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_ZBSB:			// 	        0x95  --  Zero Bit in Memory (Short-Displaced} and Branch     
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED & tmp16_val1.uval;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_TBSB:			// 	        0x96  --  Test Bit(s) in Memory (Short-Displaced) and Branch if One   
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_DISPLACED & tmp16_val1.uval;
			SET_CC_Z(tmp16_val2.uval != 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_CBSB:			// 	        0x97
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABXM:			// 	        0x98  --  Add Bit in Memory (Short-Indexed)       
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED + tmp16_val1.uval;
			SET_MEMORY_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ZBXM:			// 	        0x99  --  Zero Bit in Memory (Short-Indexed}       
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED & tmp16_val1.uval;
			SET_MEMORY_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_OBXM:			// 	        0x9a  --  OR Bit in Memory (Short-Indexed)       
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED | tmp16_val1.uval;
			SET_MEMORY_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(false);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_TBXM:			// 	        0x9b  --  Test Bit(s) in Memory (Short-Indexed)       
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED & tmp16_val1.uval;
			SET_CC_Z(tmp16_val2.uval != 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ABXB:			// 	        0x9c  --  Add Bit in Memory (Short-Indexed) and Branch if Nonzero   
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED + tmp16_val1.uval;
			SET_MEMORY_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_ZBXB:			// 	        0x9d  --  Zero Bit in Memory (Short-Indexed} and Branch if Nonzero   
			tmp16_val1.uval = bitnot[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED & tmp16_val1.uval;
			SET_MEMORY_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(tmp16_val2.uval == 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
			break;

		case  OP_TBXB:			// 	        0x9e  --  Test Bit in Memory (Short-Indexed) and Branch if One   
			tmp16_val1.uval = bit[GET_DESTINATION_REGISTER];
			tmp16_val2.uval = GET_MEMORY_SHORT_INDEXED & tmp16_val1.uval;
			SET_CC_Z(tmp16_val2.uval != 0);
			SET_CC_N(tmp16_val2.uval & 0x8000);
			// TODO: Set CC
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE, program_counter + 2);
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

		case  OP_HHI_HNH:			// 	        0xa6
			// HNH  --  Hop on Magnitude Not Higher Condition      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(TEST_CC_NH, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			// HHI  --  Hop on Magnitude Higher Condition        
			else {
				CONDITIONAL_BRANCH(TEST_CC_HI && !cpu_cond_code_z, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_AUGA7:			//         0xa7
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch (tmp_instr_dest) {
				case 0:		// --  BLNS	Branch and Link on Condition Code N Set
					do_branch = cpu_cond_code_n;
					break;
				case 1:		// --  BLZS	Branch and Link on Condition Code Z Set    
					do_branch = cpu_cond_code_z;
					break;
				case 2:		// --  BLOS	Branch and Link on Condition Code O Set    
					do_branch = cpu_cond_code_o;
					break;
				case 3:		// --  BLCS	Branch and Link on Condition Code C Set    
					do_branch = cpu_cond_code_c;
					break;
				case 4:		// --  BLLS	Branch and Link on Less Than Condition     
					do_branch = TEST_CC_LT;
					break;
				case 5:		// --  BLLE	Branch and Link on Less Than or Equal Condition   
					do_branch = TEST_CC_LE;	// CCZ .OR. (CCN .XOR. CCO)
					break;
				case 6:		// --  BLHI	Branch and Link on Magnitude Higher Condition     
					do_branch = TEST_CC_HI;	// !CCC .or. CCZ
					break;
				case 8:		// --  BLNR	Branch and Link on Condition Code N Reset    
					do_branch = !cpu_cond_code_n;
					break;
				case 9:		// --  BLZR	Branch and Link on Condition Code Z Reset    
					do_branch = !cpu_cond_code_z;
					break;
				case 10:		// --  BLOR	Branch and Link on Condition Code O Reset    
					do_branch = !cpu_cond_code_o;
					break;
				case 11:		// --  BLCR	Branch and Link on Condition Code C Reset    
					do_branch = !cpu_cond_code_c;
					break;
				case 12:		// --  BLGE	Branch and Link on Greater Than or Equal Condition   
					do_branch = TEST_CC_GE;  //  CCZ .or. (!CCZ .AND. (!CCN .XOR. CCO))
					break;
				case 13:		// --  BLGT	Branch and Link on Greater Than Condition     
					do_branch = TEST_CC_GT;  //  CCZ .or. (CCN .XOR. CCO)
					break;
				case 14:		// --  BLNH	Branch and Link on Magnitude Not Higher Condition    
					do_branch = TEST_CC_NH;  // !CCC .or CCZ
					break;
				default:
					ILLEGAL_INSTRUCTION;
					do_branch = false;
					break;
			}
			if (do_branch) {
				SET_DESTINATION_REGISTER_VALUE(program_counter + 2);
				SET_NEXT_PROGRAM_COUNTER( GET_MEMORY_IMMEDIATE );
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			}
			break;

		case  OP_HNS_HNR:			//         0xa8
			// HNR  --  Hop on Condition Code N Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(!cpu_cond_code_n, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			// HNS  --  Hop on Condition Code N Set      
			else {
				CONDITIONAL_BRANCH(cpu_cond_code_n, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_HZS_HZR:			//         0xa9
			// HZR  --  Hop on Condition Code Z Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(  !cpu_cond_code_z, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			// HZS  --  Hop on Condition Code Z Set      
			else {
				CONDITIONAL_BRANCH( cpu_cond_code_z, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_HOS_HOR:			//         0xaa
			// HOR  --  Hop on Condition Code O Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(!cpu_cond_code_o, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			// HOS  --  Hop on Condition Code O Set      
			else {
				CONDITIONAL_BRANCH(cpu_cond_code_o, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_HCS_HCR:			//         0xab
			// HCR  --  Hop on Condition Code C Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(!cpu_cond_code_c, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			// HCS  --  Hop on Condition Code C Set      
			else {
				CONDITIONAL_BRANCH(cpu_cond_code_c, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_HLS_HGE:			//         0xac
			//       HGE	Hop oh Greater than or Equal Condition     
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH( TEST_CC_GE, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			//  HLS	Hop on Less Than Condition            
			else {
				CONDITIONAL_BRANCH( TEST_CC_LT, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_HLE_HGT:			//         0xad
			//       HGT	Hop on Greater Than Condition       
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH( TEST_CC_GT, GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			//       HLE	Hop on Less Than or Equal Condition     
			else {
				CONDITIONAL_BRANCH( TEST_CC_LE , GET_NEXT_PROGRAM_COUNTER_HOP, program_counter + 1);
			}
			break;

		case  OP_LBX:			// 	        0xae  --  Load Byte from Memory (Byte-Indexed)       
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SBX:			// 	        0xaf  --  Store Byte in Memory (Byte-Indexed)       
			tmp_instr_src = GET_SOURCE_REGISTER;
			if (tmp_instr_src & 0x0001) {
				ILLEGAL_INSTRUCTION;
			}
			else {
				tmp16_val4.uval = GET_DESTINATION_REGISTER_VALUE & 0x00ff;
				// TODO: Fix all this !
				tmp16_val1.uval = GET_REGISTER_VALUE(tmp_instr_src);
				tmp16_val5.uval = GET_REGISTER_VALUE(tmp_instr_src + 1);
				memcpy(&temp16_val10, &tmp16_val5.uval, sizeof(temp16_val10));
				// printf("\n after memcpy dest 0x%04x, src 0x%04x\n", temp16_val10, tempu16_val5);
				temp_bit = temp16_val10 & 0x0001;
				temp16_val10 = temp16_val10 >> 1;
				tmp32_val1.uval = tmp16_val1.uval;
				temp32_addr_calc = tmp32_val1.sval + (__int32)temp16_val10;
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

		case  OP_IRM:			// 	        0xb6  --  Interchange Register and Memory        
			temp32_addr_calc = GET_MEMORY_DIRECT_ADDR;
			tmp16_val1.uval = GET_MEMORY_OM(temp32_addr_calc);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_MEMORY_OM(temp32_addr_calc, tmp16_val2.uval);
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_IRR:			// 	        0xb7  --  Interchange Register and Register    -    
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


		case  OP_ADMM:			// 	        0xc0  --  Add Register to Memory        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_DIRECT;
			SET_MEMORY_DIRECT(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_ETMM:			// 	        0xc1  --  Extract Register from Memory        
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_DIRECT;
			SET_MEMORY_DIRECT(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_ORMM:			// 	        0xc2  --  OR Register to Memory        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_DIRECT;
			SET_MEMORY_DIRECT(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(false);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_CRM:			// 	        0xc3  --
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADMB:			// 	        0xc4  --  Add Register to Memory and Branch if Nonzero      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_DIRECT;
			SET_MEMORY_DIRECT(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_ETMB:			// 	        0xc5  --  Extract Register from Memory and Branch if Nonzero    
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_DIRECT;
			SET_MEMORY_DIRECT(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_TRMB:			// 	        0xc6  --  Test Register and Memory and Branch if any Ones Compare  
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_DIRECT;
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
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
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch (tmp_instr_dest) {
				case 0:		// --  SRNS	Set Register if Condition Code N Set
					tmp16_val1.uval = (cpu_cond_code_n ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 1:		// --  SRZS	Set Register if Condition Code Z Set
					tmp16_val1.uval = (cpu_cond_code_z ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 2:		// --  SROS	Set Register if Condition Code O Set
					tmp16_val1.uval = (cpu_cond_code_o ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 3:		// --  SRCS	Set Register if Code C Set
					tmp16_val1.uval = (cpu_cond_code_c ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 4:		// --  SRLS	Set Register on Less than Condition
					tmp16_val1.uval = (cpu_cond_code_n != cpu_cond_code_o ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 5:		// --  SRLE	Set Register on Less than or Equal Condition
					tmp16_val1.uval = (cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o) ? 0xffff : 0);	// CCZ .OR. (CCN .XOR. CCO)
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 6:		// --  SRHI	Set Register on Magnitude Higher Condition
					tmp16_val1.uval = (!cpu_cond_code_c || cpu_cond_code_z ? 0xffff : 0);	// !CCC .or. CCZ
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 8:		// --  SRNR	Set Register if Condition Code N Reset
					tmp16_val1.uval = (!cpu_cond_code_n ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 9:		// --  SRZR	Set Register if Condition Code Z Reset
					tmp16_val1.uval = (!cpu_cond_code_z ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 10:		// --  SROR	Set Register if Condition Code O Reset
					tmp16_val1.uval = (!cpu_cond_code_o ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 11:		// --  SRCR	Set Register if Condition Code C Reset 
					tmp16_val1.uval = (!cpu_cond_code_c ? 0xffff : 0);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 12:		// --  SRGE	Set Register on Greater than or Equal Condition
					tmp16_val1.uval = (cpu_cond_code_z || (!cpu_cond_code_z && (cpu_cond_code_n != cpu_cond_code_c)) ? 0xffff : 0);  //  CCZ .or. (!CCZ .AND. (!CCN .XOR. CCO))
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 13:		// --  SRGT	Set Register on Greater than Condition
					tmp16_val1.uval = (!(cpu_cond_code_z || (cpu_cond_code_n != cpu_cond_code_o)) ? 0xffff : 0);  //  CCZ .or. (CCN .XOR. CCO)
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				case 14:		// --  SRNH	Set Register on Magnitude not Higher Condition
					tmp16_val1.uval = (!cpu_cond_code_c || cpu_cond_code_z ? 0xffff : 0);  // !CCC .or CCZ
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					break;
				default:
					ILLEGAL_INSTRUCTION;
					break;
			}
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
			// LDMD  -- Load Double-Register from Memory Doubleword        
			if (instruction.all && 0x0010) {
				UNIMPLEMENTED_INSTRUCTION;
			}
			// TRRD -- Transfer Double-Register to Double- Register       
			// TODO: Take care of illegal register value in instruction
			else {
				tmp_instr_src_dbl = GET_SOURCE_REGISTER_DOUBLE;
				tmp32_val1.uval = GET_REGISTER_VALUE_DOUBLE(tmp_instr_src_dbl);
			}
			SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val1.uval);
			SET_CC_Z(tmp32_val1.uval == 0 );
			// TODO: check endian
			SET_CC_N(tmp32_val1.uval & 0x80000000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CLM_STMD_CLMD:	//        0xce
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			switch (tmp_instr_dest) {
				//--------CLM  --  Clear Memory          
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


		case  OP_ADSM:			// 	        0xd0  --  Add Register to Memory (Short-Displaced)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_SHORT_DISPLACED;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETSM:			// 	        0xd1  --  Extract Register from Memory (Short- Displaced)      
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_SHORT_DISPLACED;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORSM:			// 	        0xd2  --  OR Register to Memory (Short-Displaced)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_SHORT_DISPLACED;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(false);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRS:			// 	        0xd3
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADSB:			// 	        0xd4  --  Add Register to Memory (Short-Displaced) and Branch if Nonzero   
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_SHORT_DISPLACED;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_ETSB:			// 	        0xd5  --  Extract Register from Memory (Short- Displaced) and Branch if Nonzero  
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_SHORT_DISPLACED;
			SET_MEMORY_SHORT_DISPLACED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_TRSB:			// 	        0xd6  --  Test Register and Memory (Short- Displaced)  and Branch if any ones Compare        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_SHORT_DISPLACED;
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_CRSB:			// 	        0xd7
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADXM:			// 	        0xd8  --  Add Register to Memory (Short-Indexed)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_SHORT_INDEXED;
			SET_MEMORY_SHORT_INDEXED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETXM:			// 	        0xd9  --  Extract Register from Memory (Short- Indexed)      
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_SHORT_INDEXED;
			SET_MEMORY_SHORT_INDEXED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORXM:			// 	        0xda  --  OR Register to Memory (Short-Indexed)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_SHORT_INDEXED;
			SET_MEMORY_SHORT_INDEXED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(false);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_CRX:			// 	        0xdb
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ADXB:			// 	        0xdc  --  Add Register to Memory (Short-Indexed) and Branch if Nonzero   
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_SHORT_INDEXED;
			SET_MEMORY_SHORT_INDEXED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_ETXB:			// 	        0xdd  --  Extract Register from Memory Short- Indexed and Branch if Nonzero  
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_SHORT_INDEXED;
			SET_MEMORY_SHORT_INDEXED(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_TRXB:			// 	        0xde  --  Test Register and Memory (Short-Indexed) and Branch if any Ones Compare 
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_SHORT_INDEXED;
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			CONDITIONAL_BRANCH(!cpu_cond_code_z, GET_MEMORY_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_CRXB:			// 	        0xdf
			UNIMPLEMENTED_INSTRUCTION;
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;


		case  OP_ADM:			// 	        0xe0  --  Add Memory to Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_SUM:			// 	        0xe1  --  Subtract Memory from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE - GET_MEMORY_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_ETM:			// 	        0xe2  --  Extract Memory from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_MEMORY_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_ORM:			// 	        0xe3  --  OR Memory to Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_XOM:			// 	        0xe4  --  Exclusive OR Memory to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_MEMORY_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_LDM:			// 	        0xe5  --   Load Register from Memory        
			tmp16_val1.uval = GET_MEMORY_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_STM:			// 	        0xe6  --  Store Register in Memory        
			SET_MEMORY_DIRECT( GET_DESTINATION_REGISTER_VALUE );
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_BRU_BLM:			//         0xe7
			// BLM  --  Branch and Link 
			tmp_instr_dest = GET_DESTINATION_REGISTER;
			if (tmp_instr_dest != 0) {
				SET_DESTINATION_REGISTER_VALUE(program_counter + 2);
			}
			// BRU  --  Branch Unconditionally          
			else {
				// nothing to do here.
			}
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_DIRECT_ADDR);
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
				// TODO: Set cc
				break;

			case 1: // -------- LDF  load floating immediate
				if (TEST_VALID_DOUBLE_REGISTER(GET_DESTINATION_REGISTER)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER & 0x000e;
					SET_REGISTER_VALUE(tmp_instr_dest, GET_MEMORY_IM(program_counter + 1));
					SET_REGISTER_VALUE(tmp_instr_dest+1, GET_MEMORY_IM(program_counter + 2));
					// TODO: Set cc
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
					// TODO: Set cc
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
					// TODO: Set cc
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

		case  OP_STI:			// 	        0xee  --  Store Register in Memory (Immediate)       
			SET_MEMORY_IMMEDIATE(GET_DESTINATION_REGISTER_VALUE);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 2);
			break;

		case  OP_BLI:			// 	        0xef  --  Branch and Link (Immediate)        
			SET_DESTINATION_REGISTER_VALUE( program_counter + 2)
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_IMMEDIATE);
			break;


		case  OP_ADS:			// 	        0xf0  --  Add Memory (Short-Displaced) to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_SHORT_DISPLACED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_SUS:			// 	        0xf1  --  Subtract Memory (Short-Displaced) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE - GET_MEMORY_SHORT_DISPLACED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETS:			// 	        0xf2  --  Extract Memory (Short-Displaced) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_MEMORY_SHORT_DISPLACED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORS:			// 	        0xf3  --  OR Memory (Short-Displaced) to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_SHORT_DISPLACED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOS:			// 	        0xf4  --  Exclusive OR Memory to Register (Short-Displaced)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_MEMORY_SHORT_DISPLACED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDS:			// 	        0xf5  --  Load Register from Memory        
			tmp16_val1.uval = GET_MEMORY_SHORT_DISPLACED;
			SET_DESTINATION_REGISTER_VALUE( tmp16_val1.uval );
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_STS:			// 	        0xf6
			SET_MEMORY_SHORT_DISPLACED( GET_DESTINATION_REGISTER_VALUE );
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_HOP_BLT:		//         0xf7
			// BLT  --  Branch and Link (Indexed Through-Table)       
			if (instruction.all & 0x0080) {
				SET_REGISTER_VALUE(8, program_counter + 1);
				// TODO: check the address calculation
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_OM(GET_REGISTER_VALUE(2)+GET_HOP_OFFSET));
			}
			// HOP  --  Hop Unconditionally          
			else {
				SET_NEXT_PROGRAM_COUNTER(GET_NEXT_PROGRAM_COUNTER_HOP);
			}
			break;

		case  OP_ADX:			// 	        0xf8  --  Add Memory (Short-Indexed) to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE + GET_MEMORY_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			// TODO: Set cc
			break;

		case  OP_SUX:			// 	        0xf9  --  Subtract Memory (Short-Indexed) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE - GET_MEMORY_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ETX:			// 	        0xfa  --  Extract Memory (Short-Indexed) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_MEMORY_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_ORX:			// 	        0xfb  --  OR Memory (Short-Indexed) to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_XOX:			// 	        0xfc  --  Exclusive OR Memory to Register (Short-Displaced)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_MEMORY_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_CC_N(tmp16_val1.uval & 0x8000);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_LDX:			// 	        0xfd  --  Load Register from Memory (Short-Indexed)       
			tmp16_val1.uval = GET_MEMORY_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			// TODO: Set CC
			SET_CC_N((tmp16_val1.uval & 0x8000) != 0);
			SET_CC_Z(tmp16_val1.uval == 0);
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_STX:			// 	        0xfe  --  Store Register in Memory (Short-Indexed)       
			SET_MEMORY_SHORT_INDEXED( GET_DESTINATION_REGISTER_VALUE );
			// TODO: Set CC
			SET_NEXT_PROGRAM_COUNTER(program_counter + 1);
			break;

		case  OP_BRX_BLX:			// 	    0xff
			tmp_instr_dest = GET_DESTINATION_REGISTER;

			// BRX  --  Branch (Short-Indexed) Unconditionally         
			if (tmp_instr_dest == 0) {
				// nothing extra to do here...
			}
			// BLX  --  Branch and Link (Short-Indexed)        
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