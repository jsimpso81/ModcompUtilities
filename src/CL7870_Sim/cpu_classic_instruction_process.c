// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			cpu_classic_instruction_process.c
//
//	Description:	THis module contains the CPU instruction set processing.  This
//                  can be compiled to simulate Classic 7860, 7830, II/15 cpus, with or
//                  without EAU floating point instructions.
//
//	Externally accessible routines:
//					char* cpu_get_debug_string()
//					void cpu_trigger_power_interrupt(SIMJ_U16 power_interrupt_type)
//					void cpu_trigger_memory_parity_interrupt(SIMJ_U16 parity_interrupt_type)
//					bool cpu_get_virtual_mode()
//					void cpu_get_virtual_map(SIMJ_U16 map, MEM_MAP* copied_map)
//					void cpu_get_instruction_trace( SIMJ_U16 *inx, SIMJ_U16 trace[1024], 
//							SIMJ_U16 trace_w1[1024], SIMJ_U16 trace_w2[1024], SIMJ_U16 trace_w3[1024], SIMJ_U16 trace_w4[1024])
//					void cpu_init_data()
//					void cpu_stop_data()
//					void cpu_set_power_on()
//					void cpu_master_clear()
//					bool cpu_get_power_on()
//					void cpu_get_active_interrupt(SIMJ_U16* act )
//					void cpu_get_interrupt(SIMJ_U16* act, SIMJ_U16* req, SIMJ_U16* ena,
//									SIMJ_U32* di_req, SIMJ_U32* di_prc, SIMJ_U32* si_req, SIMJ_U32* si_prc )
//					void cpu_request_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr)
//					void cpu_request_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr)
//					void cpu_reset_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr)
//					void cpu_reset_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr)
//					SIMJ_U32 cpu_get_instruction_count()
//					SIMJ_U16 cpu_get_clock_trigger_count()
//					void cpu_trigger_console_interrupt()
//					void cpu_trigger_clock_interrupt()
//					void cpu_set_register_value(SIMJ_U16 reg_index, SIMJ_U16 reg_value)
//					SIMJ_U16 cpu_get_register_value(SIMJ_U16 reg_index)
//					SIMJ_U16 cpu_get_register_block_value(SIMJ_U16, reg_block, SIMJ_U16 reg_index)
//					void cpu_set_program_counter(SIMJ_U16 pc)
//					SIMJ_U16 cpu_get_program_counter()
//					PSW cpu_get_current_PSW()
//					SIMJ_U16 cpu_read_internal_register(SIMJ_U16 front_panel_address)
//					void cpu_classic_instruction_process()
//					SIMJ_U16 cpu_get_virtual_mem( SIMJ_U16 map_numb, SIMJ_U16 virt_addr );
//                  
// 
//	Internal only routines:
//					SIMJ_INLINE SIMJ_U16 cpu_get_next_DI_request()
//					SIMJ_INLINE SIMJ_U16 cpu_get_next_SI_request()
//					SIMJ_INLINE void cpu_clear_all_DI_request()
//					SIMJ_INLINE void cpu_clear_all_SI_request()
//					SIMJ_INLINE void cpu_switch_register_blocks(SIMJ_U8 new_reg_block)
//					SIMJ_INLINE void cpu_set_current_PSW(PSW new_psw)
//					SIMJ_INLINE SIMJ_U16 cpu_find_bit(SIMJ_U16 bit_value)
//					SIMJ_U16 cpu_convert_to_front_panel_address(SIMJ_U16 cpu_internal_reg_addr)
// 
// Notes:			
//					Thir module also contains static variables used by routines in this module.
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

#include "simj_base.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "modcomp_opcodes.h"


// --------- CPU OPTIONS
#define OPTION_CPU_TIMING					0
#define OPTION_CPU_TRACE					0
#define OPTION_CPU_VERBOSE					0
#define OPTION_CPU_DISABLE_INTERRUPTABLE	0

// #define SIMJ_INLINE		inline
// #define SIMJ_REGISTER	register
#define SIMJ_INLINE	
#define SIMJ_REGISTER

// ================================global constants===================================================
// -------- global constants
const SIMJ_U16 bit[16] = { 0x8000, 0x4000, 0x2000, 0x1000, 0x0800, 0x0400, 0x0200, 0x0100,
						   0x0080, 0x0040, 0x0020, 0x0010, 0x0008, 0x0004, 0x0002, 0x0001 };
const SIMJ_U16 bitnot[16] = { 0x7fff, 0xbfff, 0xdfff, 0xefff, 0xf7ff, 0xfbff, 0xfdff, 0xfeff,
							  0xff7f, 0xffbf, 0xffdf, 0xffef, 0xfff7, 0xfffb, 0xfffd, 0xfffe };
const SIMJ_U16 mask[16] = { 0x8000, 0xc000, 0xe000, 0xf000, 0xf800, 0xfc00, 0xfe00, 0xff00,
							0xff80, 0xffc0, 0xffe0, 0xfff0, 0xfff8, 0xfffc, 0xfffe, 0xffff };

const SIMJ_U32 bit32[32] = { 0x80000000, 0x40000000, 0x20000000, 0x10000000, 0x08000000, 0x04000000, 0x02000000, 0x01000000,
							 0x00800000, 0x00400000, 0x00200000, 0x00100000, 0x00080000, 0x00040000, 0x00020000, 0x00010000, 
							 0x00008000, 0x00004000, 0x00002000, 0x00001000, 0x00000800, 0x00000400, 0x00000200, 0x00000100,
							 0x00000080, 0x00000040, 0x00000020, 0x00000010, 0x00000008, 0x00000004, 0x00000002, 0x00000001 };

const SIMJ_U32 mask32[32] = { 0x80000000, 0xc0000000, 0xe0000000, 0xf0000000, 0xf8000000, 0xfc000000, 0xfe000000, 0xff000000,
							  0xff800000, 0xffc00000, 0xffe00000, 0xfff00000, 0xfff80000, 0xfffc0000, 0xfffe0000, 0xffff0000,
							  0xffff8000, 0xffffc000, 0xffffe000, 0xfffff000, 0xfffff800, 0xfffffc00, 0xfffffe00, 0xffffff00,
							  0xffffff80, 0xffffffc0, 0xffffffe0, 0xfffffff0, 0xfffffff8, 0xfffffffc, 0xfffffffe, 0xffffffff };

const SIMJ_U64 bit64[64] = { 0x8000000000000000, 0x4000000000000000, 0x2000000000000000, 0x1000000000000000, 
							 0x0800000000000000, 0x0400000000000000, 0x0200000000000000, 0x0100000000000000,
							 0x0080000000000000, 0x0040000000000000, 0x0020000000000000, 0x0010000000000000,
							 0x0008000000000000, 0x0004000000000000, 0x0002000000000000, 0x0001000000000000,
							 0x0000800000000000, 0x0000400000000000, 0x0000200000000000, 0x0000100000000000,
							 0x0000080000000000, 0x0000040000000000, 0x0000020000000000, 0x0000010000000000,
							 0x0000008000000000, 0x0000004000000000, 0x0000002000000000, 0x0000001000000000,
							 0x0000000800000000, 0x0000000400000000, 0x0000000200000000, 0x0000000100000000,
							 0x0000000080000000, 0x0000000040000000, 0x0000000020000000, 0x0000000010000000,
							 0x0000000008000000, 0x0000000004000000, 0x0000000002000000, 0x0000000001000000,
							 0x0000000000800000, 0x0000000000400000, 0x0000000000200000, 0x0000000000100000, 
							 0x0000000000080000, 0x0000000000040000, 0x0000000000020000, 0x0000000000010000,
							 0x0000000000008000, 0x0000000000004000, 0x0000000000002000, 0x0000000000001000, 
							 0x0000000000000800, 0x0000000000000400, 0x0000000000000200, 0x0000000000000100,
							 0x0000000000000080, 0x0000000000000040, 0x0000000000000020, 0x0000000000000010, 
							 0x0000000000000008, 0x0000000000000004, 0x0000000000000002, 0x0000000000000001 };

const SIMJ_U64 mask64[64] = { 0x8000000000000000, 0xc000000000000000, 0xe000000000000000, 0xf000000000000000, 
							  0xf800000000000000, 0xfc00000000000000, 0xfe00000000000000, 0xff00000000000000,
							  0xff80000000000000, 0xffc0000000000000, 0xffe0000000000000, 0xfff0000000000000,
							  0xfff8000000000000, 0xfffc000000000000, 0xfffe000000000000, 0xffff000000000000,
							  0xffff800000000000, 0xffffc00000000000, 0xffffe00000000000, 0xfffff00000000000,
							  0xfffff80000000000, 0xfffffc0000000000, 0xfffffe0000000000, 0xffffff0000000000,
							  0xffffff8000000000, 0xffffffc000000000, 0xffffffe000000000, 0xfffffff000000000,
							  0xfffffff800000000, 0xfffffffc00000000, 0xfffffffe00000000, 0xffffffff00000000,
							  0xffffffff80000000, 0xffffffffc0000000, 0xffffffffe0000000, 0xfffffffff0000000,
							  0xfffffffff8000000, 0xfffffffffc000000, 0xfffffffffe000000, 0xffffffffff000000,
							  0xffffffffff800000, 0xffffffffffc00000, 0xffffffffffe00000, 0xfffffffffff00000,
							  0xfffffffffff80000, 0xfffffffffffc0000, 0xfffffffffffe0000, 0xffffffffffff0000,
							  0xffffffffffff8000, 0xffffffffffffc000, 0xffffffffffffe000, 0xfffffffffffff000,
							  0xfffffffffffff800, 0xfffffffffffffc00, 0xfffffffffffffe00, 0xffffffffffffff00,
							  0xffffffffffffff80, 0xffffffffffffffc0, 0xffffffffffffffe0, 0xfffffffffffffff0,
							  0xfffffffffffffff8, 0xfffffffffffffffc, 0xfffffffffffffffe, 0xffffffffffffffff };


//const SIMJ_U32 bitnot32[32] = { 0x7fff, 0xbfff, 0xdfff, 0xefff, 0xf7ff, 0xfbff, 0xfdff, 0xfeff,
//									  0xff7f, 0xffbf, 0xffdf, 0xffef, 0xfff7, 0xfffb, 0xfffd, 0xfffe };

// -------- interrupts
//		0	0x8000	Power Fail Safe / Auto Start
#define CPU_INTR_power_fail (SIMJ_U16)0x8000
#define CPU_INTR_power_fail_num			0
//		1	0x4000	Memory Parity
#define CPU_INTR_mem_parity (SIMJ_U16)0x4000
#define CPU_INTR_mem_parity_num			1
//		2	0x2000	System Protect (priv instruction)
#define CPU_INTR_sys_protect (SIMJ_U16)0x2000
#define CPU_INTR_sys_protect_num		2
//		3	0x1000	Multiprocessor Communications
#define CPU_INTR_RMI (SIMJ_U16)0x1000
#define CPU_INTR_RMI_num				3
//		4	0x0800	Unimplemented Instruction Trap
#define CPU_INTR_UIT (SIMJ_U16)0x0800
#define CPU_INTR_UIT_num				4
//		5	0x0400	Floating Point Overflow
#define CPU_INTR_floating_overflow (SIMJ_U16)0x0400
#define CPU_INTR_floating_overflow_num	5
//		6	0x0200	Real Time Clock
#define CPU_INTR_clock (SIMJ_U16)0x0200
#define CPU_INTR_clock_num				6
//		7	0x0100	External
#define CPU_INTR_ext_7 (SIMJ_U16)0x0100
#define CPU_INTR_ext_7_num				7
//		8	0x0080	External
#define CPU_INTR_ext_8 (SIMJ_U16)0x0080
#define CPU_INTR_ext_8_num				8
//		9	0x0040	External
#define CPU_INTR_ext_9 (SIMJ_U16)0x0040
#define CPU_INTR_ext_9_num				9
//		10	0x0020	External
#define CPU_INTR_ext_A (SIMJ_U16)0x0020
#define CPU_INTR_ext_A_num				10
//		11	0x0010	External
#define CPU_INTR_ext_B (SIMJ_U16)0x0010
#define CPU_INTR_ext_B_num				11
//		12	0x0008	I/O Data Party Line
#define CPU_INTR_DI (SIMJ_U16)0x0008
#define CPU_INTR_DI_num					12
//		13	0x0004	I/O Service Party Line
#define CPU_INTR_SI (SIMJ_U16)0x0004
#define CPU_INTR_SI_num					13
//		14	0x0002	Console interrupt
#define CPU_INTR_console (SIMJ_U16)0x0002
#define CPU_INTR_console_num			14
//		15	0x0001	Task Scheduler
#define CPU_INTR_task_sch (SIMJ_U16)0x0001
#define CPU_INTR_task_sch_num			15

#define CPU_INTR_none_num				16
//
#define DEFAULT_INTR_ENABLED (CPU_INTR_power_fail | CPU_INTR_sys_protect | CPU_INTR_UIT | CPU_INTR_floating_overflow)

#define CPU_INTR_BASE_ENTRY_PS	(SIMJ_U32)0x00000040		// Status -  BASE + INT_VAL * 2
#define CPU_INTR_BASE_ENTRY_PC	(SIMJ_U32)0x00000021		// Pgm counter - BASE + INT_VAL * 2
#define CPU_INTR_DI_BASE_ENTRY_PC	(SIMJ_U32)0x00000080	// Pgm counter - BASE + DEV_ADDR
#define CPU_INTR_SI_BASE_ENTRY_PC	(SIMJ_U32)0x000000C0	// Pgm counter - BASE + DEV_ADDR

#define CPU_INTR_BASE_RETURN_PC	(SIMJ_U32)0x00000020		// Pgm counter - BASE + INT_VAL * 2
#define CPU_INTR_BASE_RETURN_PS	(SIMJ_U32)0x00000041		// Status - BASE + INT_VAL * 2



// ================================global cpu data===================================================
// 
// -------- individual parts of the PSD
static SIMJ_U16 program_counter = 0;	// use local mapping, value 0=65k
// -------- individual parts os the PWD
static SIMJ_U8 cpu_operand_map = 0;		// OM
static SIMJ_U8 cpu_instruction_map = 0;	// IM
static bool cpu_priv_mode = true;							// PRV
static bool cpu_overflow_hist = false;						// OH
static bool cpu_cond_code_n = false;						// CC N
static bool cpu_cond_code_z = false;						// CC Z
static bool cpu_cond_code_o = false;						// CC O
static bool cpu_cond_code_c = false;						// CC C

static REG_BLOCK cpu_register;						// register block in use by CPU.
static REG_BLOCK cpu_register_blocks[16];			// indexed by register block.
static SIMJ_U8 cpu_register_current_block = 0;		// GRB

static volatile SIMJ_U16 cpu_interrupt_active = 0;							// interrupt active bits.
static volatile SIMJ_U16 cpu_interrupt_enabled = DEFAULT_INTR_ENABLED;		// interrupt enabled bits.
static volatile SIMJ_U16 cpu_interrupt_request = 0;							// interrupt requested bits.
static volatile SIMJ_U16 cpu_interrupt_request_external = 0;				// external interrupt requested bits.
static volatile SIMJ_U16 cpu_interrupt_active_mask = 0xffff;
static volatile SIMJ_U16 cpu_interrupt_active_num = CPU_INTR_none_num;	// currently active interrupt level, 16 = nothing active.
											// NOTE this is a write only variable.. not used...

static volatile SIMJ_U16 cpu_interrupt_DI_request_dev_addr[16][4] = { 0 };
static volatile SIMJ_U32 cpu_interrupt_DI_request_count[16][4] = {0};
static volatile SIMJ_U32 cpu_interrupt_DI_proc_count[16][4] = { 0 };
static volatile SIMJ_U32 cpu_interrupt_DI_total_request_count =  0;
static volatile SIMJ_U32 cpu_interrupt_DI_total_proc_count = 0;

static volatile SIMJ_U16 cpu_interrupt_SI_request_dev_addr[16][4] = { 0 };
static volatile SIMJ_U32 cpu_interrupt_SI_request_count[16][4] = { 0 };
static volatile SIMJ_U32 cpu_interrupt_SI_proc_count[16][4] = { 0 };
static volatile SIMJ_U32 cpu_interrupt_SI_total_request_count = 0;
static volatile SIMJ_U32 cpu_interrupt_SI_total_proc_count = 0;

static volatile SIMJ_U16 cpu_interrupt_entry_cond_codes[16] = { 0 };

static bool skip_interrupt_determination = false;

// --------variables for instruction interrupt and restart.
// --------this needs to be a stack.  For now make the stack 48 deep.
static SIMJ_S16 cpu_inst_stack_index = -1;
#define CPU_INST_STACK_MAX 48
// --------the PC of the restartable instruction.  Helps to determine if a 
// --------a new stack item is needed.
static SIMJ_U16 cpu_inst_intr_pc[CPU_INST_STACK_MAX] = { 0 };
// --------the full opcode of the restartable instruction.  Helps to
// --------determine if a new stack item is needed.
static SIMJ_U16 cpu_inst_intr_opcode[CPU_INST_STACK_MAX] = { 0 };
// -------- one more thing to help determine uniqueness.  Not sure what just
// -------- yet.  
static SIMJ_U16 cpu_inst_intr_XXX[CPU_INST_STACK_MAX] = { 0 };
// -------- saved next starting loop location.
static SIMJ_S32 cpu_inst_next_start[CPU_INST_STACK_MAX] = { 0 };
// -------- saved end loop count (new)
static SIMJ_S32 cpu_inst_loop_end[CPU_INST_STACK_MAX] = { 0 };

// --------if stack has not been used then use it..
#if DEBUG_RESTARTABLE >= 1
#define RESTARTABLE_SETUP_DEBUG {\
	fprintf( stderr, " restartable new entry, stack ptr %d\n",cpu_inst_stack_index);\
}
#define RESTARTABLE_COMPLETE_DEBUG {\
	fprintf( stderr, " restartable complete, stack ptr %d\n",cpu_inst_stack_index);\
}
#else
#define RESTARTABLE_SETUP_DEBUG {\
}
#define RESTARTABLE_COMPLETE_DEBUG {\
}
#endif
#define RESTART_INSTRUCTION_SETUP( RESTART_PC, RESTART_INSTR, RESTART_EXTRA, LOOP_START, LOOP_END ) {\
		if ( cpu_inst_stack_index == -1 ) {\
			cpu_inst_stack_index++;\
			if ( cpu_inst_stack_index >= CPU_INST_STACK_MAX ){\
				cpu_inst_stack_index = CPU_INST_STACK_MAX-1;\
				printf(" *** ERROR *** Restart instruction stack out of space.\n");\
			}\
			RESTARTABLE_SETUP_DEBUG\
			cpu_inst_intr_pc[cpu_inst_stack_index] = RESTART_PC;\
			cpu_inst_intr_opcode[cpu_inst_stack_index] = RESTART_INSTR;\
			cpu_inst_intr_XXX[cpu_inst_stack_index] = RESTART_EXTRA;\
			cpu_inst_next_start[cpu_inst_stack_index] = LOOP_START;\
			cpu_inst_loop_end[cpu_inst_stack_index] = LOOP_END;\
		}\
		else if ( cpu_inst_intr_pc[cpu_inst_stack_index] != RESTART_PC ||\
				  cpu_inst_intr_opcode[cpu_inst_stack_index] != RESTART_INSTR ||\
				  cpu_inst_intr_XXX[cpu_inst_stack_index] != RESTART_EXTRA ) {\
			cpu_inst_stack_index++;\
			if ( cpu_inst_stack_index >= CPU_INST_STACK_MAX ){\
				cpu_inst_stack_index = CPU_INST_STACK_MAX-1;\
				printf(" *** ERROR *** Restart instruction stack out of space.\n");\
			}\
			RESTARTABLE_SETUP_DEBUG\
			cpu_inst_intr_pc[cpu_inst_stack_index] = RESTART_PC;\
			cpu_inst_intr_opcode[cpu_inst_stack_index] = RESTART_INSTR;\
			cpu_inst_intr_XXX[cpu_inst_stack_index] = RESTART_EXTRA;\
			cpu_inst_next_start[cpu_inst_stack_index] = LOOP_START;\
			cpu_inst_loop_end[cpu_inst_stack_index] = LOOP_END;\
		}\
}

#define RESTART_INSTRUCTION_GET_LOOP( LOOP_START, LOOP_END ) {\
		LOOP_START = cpu_inst_next_start[cpu_inst_stack_index];\
		LOOP_END = cpu_inst_loop_end[cpu_inst_stack_index];\
}

#define RESTART_INSTRUCTION_SAVE_LOOP( NEXT_LOOP_START, NEXT_LOOP_END ) {\
		cpu_inst_next_start[cpu_inst_stack_index] = NEXT_LOOP_START;\
		cpu_inst_loop_end[cpu_inst_stack_index] = NEXT_LOOP_END;\
}
#define RESTART_INSTRUCTION_COMPLETE {\
		cpu_inst_stack_index--;\
		if (cpu_inst_stack_index < -1)\
			cpu_inst_stack_index = -1;\
		RESTARTABLE_COMPLETE_DEBUG\
}

// -------- CHECK FOR A NEW INTERRUPT WITHIN AN INSTRUCTION.
// TODO: Is it really necessary to sync interrupt requests.
// -------- TAKE RESOURCE
// -------- OR IN THE INTERRUPT REUEST
// -------- RESET EXTERNAL INTERRUPT REQUEST 
// -------- GIVE BACK RESOURCE.
// -------- CHECK IF THERE IS A NEW INTERRUPT.
#if OPTION_CPU_DISABLE_INTERRUPTABLE == 1
#define IS_THERE_A_NEW_INTERRUPT {\
					if ( 1 == 0 ) {\
					}

// -------- END OF CHECKING FOR A NEW INTERRUPT REQUEST WITHIN INSTRUCTION.
#define END_IS_THERE_A_NEW_INTERRUPT {\
					}\
					}
#else
#define IS_THERE_A_NEW_INTERRUPT {\
					TAKE_RESOURCE(ResourceInterruptRequest);\
					cpu_interrupt_request |= cpu_interrupt_request_external;\
					cpu_interrupt_request_external = 0;\
					GIVE_RESOURCE(ResourceInterruptRequest);\
					if (((cpu_interrupt_request & cpu_interrupt_enabled & cpu_interrupt_active_mask) > (cpu_interrupt_active & cpu_interrupt_active_mask))) {\
					}

// -------- END OF CHECKING FOR A NEW INTERRUPT REQUEST WITHIN INSTRUCTION.
#define END_IS_THERE_A_NEW_INTERRUPT {\
						goto end_instruction;\
					}\
					}
#endif


static volatile bool cpu_virtual_mode = false;			// true when in virtual mode
static volatile MEM_MAP cpu_virtual_mem_map[8];			// space for the memory maps.

static volatile bool cpu_pipeline_mode = false;			// true when in pipeline mode.

// TODO: finish implementing extended memory addressing for 7860
static volatile SIMJ_U16 cpu_extended_memory_ctrl_reg = 0;	// extended memory control register 7860 only

static volatile SIMJ_U16 cpu_trigger_clock_int = 0;		// updated by external call.  Count of clock interrupts triggered
// static volatile SIMJ_U16 cpu_last_clock_int = 0;		// updated by cpu -- NEVER USED...

static volatile SIMJ_U32 cpu_instruction_count = 0;		// how many instructions executed.

static SIMJ_U16 saved_reg_0 = 0;						// TODO: look at saved_reg_0 -- not used.

DEFINE_RESOURCE(  ResourceInterruptRequest );			// resource for syncing access to int request updates.

static int use_extended_addressing_mode = 0;			// state machine 0 = none, 1 = next instr to use, 2 = after next instruction
														// TODO: finish extended addressing for 7860

static bool cpu_power_on = false;						// simulates powering on the CPU.  

DEFINE_RESOURCE( ResourceInstructionTrace );			// for syncronizing access to instruction trace data.

// --------instruction trace data.
static SIMJ_U16		cpu_pc_trace[1024] = { 0 };			// virtual pc
static PSW			cpu_psw_trace[1024] = { 0 };		// current ps 
static SIMJ_U16		cpu_actint_trace[1024] = { 0 };		// active interrupts.
static SIMJ_U32		instruction_trace[1024] = { 0 };	// list of ABS memory addresses containing instructions.
static SIMJ_U32		instruction_trace_w1[1024] = { 0 };	// list of ABS memory addresses containing instructions.
static SIMJ_U32		instruction_trace_w2[1024] = { 0 };	// list of ABS memory addresses containing instructions.
static SIMJ_U32		instruction_trace_w3[1024] = { 0 };	// list of ABS memory addresses containing instructions.
static SIMJ_U32		instruction_trace_w4[1024] = { 0 };	// list of ABS memory addresses containing instructions.
static SIMJ_U16		instruction_trace_index = 0;		// next index into instruction trace list 

// --------data for simulating Modcomp II style memory protection registers.
#if SIMJ_SIM_CPU == 7830
static bool			cpu_nonvirt_prot_enabled = false;
static SIMJ_U16		cpu_nonvirt_prot_lowprot_reg = 0xffff;
static SIMJ_U16		cpu_nonvirt_prot_upprot_reg = 0xffff;
static SIMJ_U16		cpu_nonvirt_prot_gbl_reg = 0xffff;
#endif

static SIMJ_U16 cpu_mem_last_access_rights = 0;			// storage - NOT USED - for last memory access rights...
														// TODO: review and fix last memory access rights.

static SIMJ_U16 volatile cpu_mem_last_IM_data = 0;				// storage - for last IM memory accessed.  for front panel
static SIMJ_U32 volatile cpu_mem_last_IM_abs_addr = 0;			// storage - for last IM memory accessed.  for front panel
static SIMJ_U16 volatile cpu_mem_last_OM_data = 0;				// storage - for last IM memory accessed.  for front panel
static SIMJ_U32 volatile cpu_mem_last_OM_abs_addr = 0;			// storage - for last IM memory accessed.  for front panel
static SIMJ_U32 volatile cpu_mem_last_abs_addr = 0;			// storage - for last ABS memory accessed.  for front panel


// ===========================================================================================================
// --------define macros
#include "cpu_macro_register_access.h"

// ===========================================================================================================
// -------- private function declarations
SIMJ_INLINE void cpu_clear_all_DI_request();
SIMJ_INLINE void cpu_clear_all_SI_request();
SIMJ_U16 cpu_convert_to_front_panel_address(SIMJ_U16 cpu_internal_reg_addr);
SIMJ_INLINE SIMJ_U16 cpu_find_bit(SIMJ_U16 bit_value);
SIMJ_INLINE SIMJ_U16 cpu_get_next_DI_request();
SIMJ_INLINE SIMJ_U16 cpu_get_next_SI_request();
SIMJ_INLINE void cpu_switch_register_blocks(SIMJ_U8 new_reg_block);
SIMJ_INLINE void cpu_copy_register_current_to_block();
SIMJ_INLINE void cpu_set_current_PSW(PSW new_psw);

// ===========================================================================================================
// ======== PRIVATE FUNCTIONS
// ===========================================================================================================

// ===========================================================================================================
// --------returns with device address or 0xffff
// -------- THIS MUST BE CALLED WHILE IN POSSESSION OF THE RESORCE FOR THE INTERRUPT REQUEST.
// --------private
SIMJ_INLINE SIMJ_U16 cpu_get_next_DI_request() {
	SIMJ_U16 fnd_dev_addr = 0xffff;
	int j = 0;

	// --------loop over all priorities and buses
	// j = priority
	for (j = 0; j < 16; j++) {
		if (cpu_interrupt_DI_request_count[j][0] != cpu_interrupt_DI_proc_count[j][0]) {
			fnd_dev_addr = cpu_interrupt_DI_request_dev_addr[j][0];
			cpu_interrupt_DI_proc_count[j][0]++;
			cpu_interrupt_DI_total_proc_count++;
			break;
		}
		if (cpu_interrupt_DI_request_count[j][1] != cpu_interrupt_DI_proc_count[j][1]) {
			fnd_dev_addr = cpu_interrupt_DI_request_dev_addr[j][1];
			cpu_interrupt_DI_proc_count[j][1]++;
			cpu_interrupt_DI_total_proc_count++;
			break;
		}
		if (cpu_interrupt_DI_request_count[j][2] != cpu_interrupt_DI_proc_count[j][2]) {
			fnd_dev_addr = cpu_interrupt_DI_request_dev_addr[j][2];
			cpu_interrupt_DI_proc_count[j][2]++;
			cpu_interrupt_DI_total_proc_count++;
			break;
		}
		if (cpu_interrupt_DI_request_count[j][3] != cpu_interrupt_DI_proc_count[j][3]) {
			fnd_dev_addr = cpu_interrupt_DI_request_dev_addr[j][3];
			cpu_interrupt_DI_proc_count[j][3]++;
			cpu_interrupt_DI_total_proc_count++;
			break;
		}
	}
	return fnd_dev_addr;
}

// ===========================================================================================================
// --------returns with device address or 0xffff
// -------- THIS MUST BE CALLED WHILE IN POSSESSION OF THE RESOURCE FOR THE INTERRUPT REQUEST.
// -------- private
SIMJ_INLINE SIMJ_U16 cpu_get_next_SI_request() {
	SIMJ_U16 fnd_dev_addr = 0xffff;
	int j = 0;

	// --------loop over all priorities and buses
	for (j = 0; j < 16; j++) {
		if (cpu_interrupt_SI_request_count[j][0] != cpu_interrupt_SI_proc_count[j][0]) {
			fnd_dev_addr = cpu_interrupt_SI_request_dev_addr[j][0];
			cpu_interrupt_SI_proc_count[j][0]++;
			cpu_interrupt_SI_total_proc_count++;
			break;
		}
		if (cpu_interrupt_SI_request_count[j][1] != cpu_interrupt_SI_proc_count[j][1]) {
			fnd_dev_addr = cpu_interrupt_SI_request_dev_addr[j][1];
			cpu_interrupt_SI_proc_count[j][1]++;
			cpu_interrupt_SI_total_proc_count++;
			break;
		}
		if (cpu_interrupt_SI_request_count[j][2] != cpu_interrupt_SI_proc_count[j][2]) {
			fnd_dev_addr = cpu_interrupt_SI_request_dev_addr[j][2];
			cpu_interrupt_SI_proc_count[j][2]++;
			cpu_interrupt_SI_total_proc_count++;
			break;
		}
		if (cpu_interrupt_SI_request_count[j][3] != cpu_interrupt_SI_proc_count[j][3]) {
			fnd_dev_addr = cpu_interrupt_SI_request_dev_addr[j][3];
			cpu_interrupt_SI_proc_count[j][3]++;
			cpu_interrupt_SI_total_proc_count++;
			break;
		}
	}

	return fnd_dev_addr;
}

// ===========================================================================================================
// -------- Clears all DI requests (part of master clear)
// -------- THIS MUST BE CALLED WHILE IN POSSESSION OF THE RESOURCE FOR THE INTERRUPT REQUEST.
// --------private
SIMJ_INLINE void cpu_clear_all_DI_request() {

	int j = 0;

	// --------loop over all priorities and buses
	// j = priority
	for (j = 0; j < 16; j++) {
		cpu_interrupt_DI_request_dev_addr[j][0] = 0;
		cpu_interrupt_DI_proc_count[j][0] = 0;
		cpu_interrupt_DI_total_proc_count = 0;

		cpu_interrupt_DI_request_dev_addr[j][1] = 0;
		cpu_interrupt_DI_proc_count[j][1] = 0;
		cpu_interrupt_DI_total_proc_count = 0;

		cpu_interrupt_DI_request_dev_addr[j][2] = 0;
		cpu_interrupt_DI_proc_count[j][2] = 0;
		cpu_interrupt_DI_total_proc_count = 0;

		cpu_interrupt_DI_request_dev_addr[j][3] = 0;
		cpu_interrupt_DI_proc_count[j][3] = 0;
		cpu_interrupt_DI_total_proc_count = 0;
	}
	cpu_interrupt_DI_total_request_count = 0;
	cpu_interrupt_DI_total_proc_count = 0;

	return;
}

// ===========================================================================================================
// -------- Clears all SI requests (part of master clear)
// -------- THIS MUST BE CALLED WHILE IN POSSESSION OF THE RESOURCE FOR THE INTERRUPT REQUEST.
// --------private
SIMJ_INLINE void cpu_clear_all_SI_request() {

	int j = 0;

	// --------loop over all priorities and buses
	// j = priority
	for (j = 0; j < 16; j++) {
		cpu_interrupt_SI_request_dev_addr[j][0] = 0;
		cpu_interrupt_SI_proc_count[j][0] = 0;
		cpu_interrupt_SI_total_proc_count = 0;

		cpu_interrupt_SI_request_dev_addr[j][1] = 0;
		cpu_interrupt_SI_proc_count[j][1] = 0;
		cpu_interrupt_SI_total_proc_count = 0;

		cpu_interrupt_SI_request_dev_addr[j][2] = 0;
		cpu_interrupt_SI_proc_count[j][2] = 0;
		cpu_interrupt_SI_total_proc_count = 0;

		cpu_interrupt_SI_request_dev_addr[j][3] = 0;
		cpu_interrupt_SI_proc_count[j][3] = 0;
		cpu_interrupt_SI_total_proc_count = 0;
	}
	cpu_interrupt_SI_total_request_count = 0;
	cpu_interrupt_SI_total_proc_count = 0;

	return;
}


// ===========================================================================================================
// --------this should be local to this module only!!
// --------private
SIMJ_INLINE void cpu_switch_register_blocks(SIMJ_U8 new_reg_block) {

	// int j = 0;

	// -------- copy current registers to register block.
	//for (j = 0; j < 16; j++) {
	//	cpu_register_blocks[cpu_register_current_block].reg16[j] = cpu_register.reg16[j];
	//}
	cpu_register_blocks[cpu_register_current_block].reg64[0] = cpu_register.reg64[0];
	cpu_register_blocks[cpu_register_current_block].reg64[1] = cpu_register.reg64[1];
	cpu_register_blocks[cpu_register_current_block].reg64[2] = cpu_register.reg64[2];
	cpu_register_blocks[cpu_register_current_block].reg64[3] = cpu_register.reg64[3];

	// -------- copy new register block to current registers.
	//for (j = 0; j < 16; j++) {
	//	cpu_register.reg16[j] = cpu_register_blocks[new_reg_block].reg16[j];
	//}
	cpu_register.reg64[0] = cpu_register_blocks[new_reg_block].reg64[0];
	cpu_register.reg64[1] = cpu_register_blocks[new_reg_block].reg64[1];
	cpu_register.reg64[2] = cpu_register_blocks[new_reg_block].reg64[2];
	cpu_register.reg64[3] = cpu_register_blocks[new_reg_block].reg64[3];

	// -------- set current register block.
	cpu_register_current_block = new_reg_block;
}

// ===========================================================================================================
// --------this should be local to this module only!!
// --------private
SIMJ_INLINE void cpu_copy_register_current_to_block() {

	// -------- copy current registers to register block.
	cpu_register_blocks[cpu_register_current_block].reg64[0] = cpu_register.reg64[0];
	cpu_register_blocks[cpu_register_current_block].reg64[1] = cpu_register.reg64[1];
	cpu_register_blocks[cpu_register_current_block].reg64[2] = cpu_register.reg64[2];
	cpu_register_blocks[cpu_register_current_block].reg64[3] = cpu_register.reg64[3];

}


// ===========================================================================================================
// --------this should be local to this module only!!
// --------private
SIMJ_INLINE void cpu_set_current_PSW(PSW new_psw) {
	cpu_cond_code_c = new_psw.sep.cc_c;
	cpu_cond_code_o = new_psw.sep.cc_o;
	cpu_cond_code_z = new_psw.sep.cc_z;
	cpu_cond_code_n = new_psw.sep.cc_n;
	cpu_overflow_hist = new_psw.sep.oh;
	cpu_operand_map = new_psw.sep.om;
	cpu_instruction_map = new_psw.sep.im;
	if (new_psw.sep.grb != cpu_register_current_block) {
		cpu_switch_register_blocks((SIMJ_U8)new_psw.sep.grb);
	}
	cpu_priv_mode = new_psw.sep.prv;
	return;
}

// ===========================================================================================================
// --------private
SIMJ_INLINE SIMJ_U16 cpu_find_bit(SIMJ_U16 bit_value) {
	SIMJ_U16 new_bit = 16;
	SIMJ_U16 j;
	for (j = 0; j < 16; j++) {
		if ((bit[j] & bit_value) != 0) {
			new_bit = j;
			break;
		}
	}
	return new_bit;
}

// ===========================================================================================================
// cpu_convert_to_front_panel_address -- as address by CPU commands, not front panel.
// 
//	int_reg_addr
// [ 0 0 0 0 ] [ GRF ]                [IOP, MBC, CTXT, MAP] [ EAU, CPU]
//
// front panel format
// [ 0 0 0 0 ] [ MAP, IOP, MBC, CTXT] [ EAU, CPU]           [ GRF ] 
// 
// --------private
SIMJ_U16 cpu_convert_to_front_panel_address(SIMJ_U16 cpu_internal_reg_addr) {

	return(((cpu_internal_reg_addr & 0x0010) < 7) |			// map
		((cpu_internal_reg_addr & 0x0080) < 3) |			// iop
		((cpu_internal_reg_addr & 0x0040) < 3) |			// mbc
		((cpu_internal_reg_addr & 0x0020) < 3) |			// ctx
		((cpu_internal_reg_addr & 0x000f) < 4) |			// eau, cpu
		((cpu_internal_reg_addr & 0x0f00) > 8));			// grf

}



// ===========================================================================================================
// ======== public functions
// ===========================================================================================================

// ===========================================================================================================
// --------public
char* cpu_get_debug_string() {

	char* retValue;

	switch (gbl_verbose_debug) {
	case debugging_off:
		retValue = "Off";
		break;
	case debugging_automatic:
		retValue = "Automatic";
		break;
	case debugging_on:
		retValue = "On";
		break;
	default:
		retValue = "Unknown";
		break;
	}
	return retValue;
}


// ===========================================================================================================
// --------public
//
//	powerfail auto / restart trap condition codes
//
//		n z o c
//		0 0 0 x		external user, power recovering, DC power change
//		0 0 1 x		external user, power recovering, AC power change
//		0 1 0 x		external user, power failing, DC power change
//		0 1 1 x		external user, power failing, AC power change
//		
//		1 0 0 x		local, power recovering, DC power change
//		1 0 1 x		local, power recovering, AC power change
//		1 1 0 x		local, power failing, DC power change
//		1 1 1 x		local, power failing, AC power change
// 
// for the 32/85 the condition codes are as follows:
// 
//		n z o c
//		x 0 x x		power is recovering
//		x 1 x x		power is failing
//
void cpu_trigger_power_interrupt(SIMJ_U16 power_interrupt_type) {

	if (cpu_power_on) {
		// -------- Request ownership of the resource.
		TAKE_RESOURCE(ResourceInterruptRequest);
		cpu_interrupt_request_external |= (CPU_INTR_power_fail & cpu_interrupt_enabled);
		cpu_interrupt_entry_cond_codes[CPU_INTR_power_fail_num] = power_interrupt_type & PSW_MASK_CC_ALL;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE(ResourceInterruptRequest);
	}

}

// ===========================================================================================================
// --------public
//
// memory parity trap condition codes
//		
//		n z o c
//		0 x 0 0		global - pc points to next instruction, err in inst but not opcode
//		0 x 0 1		"", error in opcode
//		0 x 1 0		"", err in operand
//		0 x 1 1		"", undefined
//		
//		0 x 0 0		instruction abort, pc points to aborted instruction, err in inst but not opcode
//		0 x 0 1		"", error in opcode
//		0 x 1 0		"", err in operand
//		0 x 1 1		"", undefined
//
// for the 32/85 condition codes are as follow
// 
//		n z o c
//		0 x x x		not used
//		1 x x x		instruction aborted
//		x x 0 0		undefined
//		x x 0 1		failure in opcode word
//		x x 1 0		failure in other instruction word
//		x x 1 1		undefined
//
void cpu_trigger_memory_parity_interrupt(SIMJ_U16 parity_interrupt_type) {

	if (cpu_power_on) {
		// -------- Request ownership of the resource.
		TAKE_RESOURCE(ResourceInterruptRequest);
		cpu_interrupt_request_external |= (CPU_INTR_mem_parity & cpu_interrupt_enabled);
		cpu_interrupt_entry_cond_codes[CPU_INTR_mem_parity_num] = parity_interrupt_type & PSW_MASK_CC_ALL;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE(ResourceInterruptRequest);
	}

}


// ===========================================================================================================
// --------pubic
bool cpu_get_virtual_mode() {

	return cpu_virtual_mode;
}


// ===========================================================================================================
// --------pubic
bool cpu_get_pipeline_mode() {

	return cpu_pipeline_mode;
}


// ===========================================================================================================
// --------public
void cpu_get_virtual_map(SIMJ_U16 map, MEM_MAP* copied_map) {

	SIMJ_U16 j = 0;

	if (map >= 0 && map <= 7) {
		for (j = 0; j < 256; j++) {
			copied_map->entry[j].all = cpu_virtual_mem_map[map].entry[j].all;
		}
	}
	else {
		printf(" *** ERROR ***  cpu_get_virtual_map - Requested map %d is invalid.\n", map);
	}

	return;
}


// ===========================================================================================================
// --------public
void cpu_get_instruction_trace( SIMJ_U16 *inx, 
				SIMJ_U16 pc_trace[1024], PSW psw_trace[1024], SIMJ_U16 actint_trace[1024],
				SIMJ_U32 trace[1024], SIMJ_U32 trace_w1[1024], SIMJ_U32 trace_w2[1024], 
				SIMJ_U32 trace_w3[1024], SIMJ_U32 trace_w4[1024]) {

	int j = 0;

	TAKE_RESOURCE( ResourceInstructionTrace );

	for (j = 0; j < 1024; j++) {
		pc_trace[j] = cpu_pc_trace[j];
		psw_trace[j] = cpu_psw_trace[j];
		actint_trace[j] = cpu_actint_trace[j];
		trace[j] = instruction_trace[j];
		trace_w1[j] = instruction_trace_w1[j];
		trace_w2[j] = instruction_trace_w2[j];
		trace_w3[j] = instruction_trace_w3[j];
		trace_w4[j] = instruction_trace_w4[j];
	}
	*inx = instruction_trace_index;
	GIVE_RESOURCE( ResourceInstructionTrace );

	return;
}

// ===========================================================================================================
// --------public
void cpu_init_data() {

	bool status = false;

	// --------initialize the resource for interrupt requests.
	// Initialize the resource one time only.
	status = INIT_RESOURCE( ResourceInterruptRequest );
	if (!status) {
		printf(" *** ERROR *** Cpu could not create locking mechanism for interrupt request.\n");
	}
	status = INIT_RESOURCE( ResourceInstructionTrace );
	if (!status) {
		printf(" *** ERROR *** Cpu could not create locking mechanism for instruction trace.\n");
	}


}


// ===========================================================================================================
// --------public
void cpu_stop_data() {

	// -------- set virtual cpu power off.
	cpu_power_on = false;

	// --------delete the resource for interrupt requests.
	DELETE_RESOURCE( ResourceInterruptRequest );
	DELETE_RESOURCE( ResourceInstructionTrace );

}

// ===========================================================================================================
// --------public
void cpu_set_power_on() {

	// -------- set virtual power on.
	cpu_power_on = true;

	// --------do a master clear -- real power on probably does some extra stuff.
	cpu_master_clear();

	return;

}

// ===========================================================================================================
// --------public
void cpu_master_clear() {

	SIMJ_U16 j = 0;

	// --------can't master clear unless cpu virtual power is on.
	if (!cpu_power_on) {
		printf(" *** ERROR ***  Cant Master Clear cpu.  Cpu must be powered on first.\n");
		return;
	}

	// --------set actual mode (non-virtual mode)
	cpu_virtual_mode = false;

	// --------reset (or set??) pipeline mode.
	cpu_pipeline_mode = false;

	// --------set priv mode
	cpu_priv_mode = true;

	// --------set instruction map and operand map
	cpu_operand_map = 0;		// OM
	cpu_instruction_map = 0;	// IM

	// --------set general register block number.
	cpu_register_current_block = 0;		// GRB

	// --------set default enabled interrupts
	cpu_interrupt_enabled = DEFAULT_INTR_ENABLED;

	// --------reset all active interrupts
	cpu_interrupt_active = 0;
	cpu_interrupt_active_mask = 0xffff;
	cpu_interrupt_active_num = 16;

	// --------reset all interrupt requests
	// -------- Request ownership of the resource.
	TAKE_RESOURCE( ResourceInterruptRequest );
	cpu_interrupt_request = 0;
	cpu_interrupt_request_external = 0;
	for (j = 0; j < 16; j++) {
		cpu_interrupt_entry_cond_codes[j] = 0;
	}

	cpu_clear_all_DI_request();
	cpu_clear_all_SI_request();

	// -------- Release ownership of the resource.
	GIVE_RESOURCE( ResourceInterruptRequest );
	
	// --------condition codes ??
	cpu_overflow_hist = false;						// OH
	cpu_cond_code_n = false;						// CC N
	cpu_cond_code_z = false;						// CC Z
	cpu_cond_code_o = false;						// CC O
	cpu_cond_code_c = false;						// CC C

	// --------set program counter.
	program_counter = 1;

	// --------extended memory control reg
	cpu_extended_memory_ctrl_reg = 0;

	// --------send ICB to all devices.
	// TODO: Does MC send ICB to all devices?  For now it does.
	for (j = 0; j < 64; j++) {
		if (iop_device_buffer[j] != NULL) {
			if (iop_output_cmd_proc[j] != NULL) {
				(*iop_output_cmd_proc[j])(j, 0x4400);		// terminate with ICB
			}
		}
	}

#if SIMJ_SIM_CPU == 7830
	cpu_nonvirt_prot_enabled = false;
	cpu_nonvirt_prot_lowprot_reg = 0xffff;
	cpu_nonvirt_prot_upprot_reg = 0xffff;
	cpu_nonvirt_prot_gbl_reg = 0xffff;
#endif

	return;
}


// ===========================================================================================================
// --------public
bool cpu_get_power_on() {
	return cpu_power_on;
}


// ===========================================================================================================
// --------public
void cpu_get_active_interrupt(SIMJ_U16* act) {
	*act = cpu_interrupt_active;
	return;
}


// ===========================================================================================================
// --------public
void cpu_get_interrupt(SIMJ_U16* act, SIMJ_U16* req, SIMJ_U16* ena,
				SIMJ_U32* di_req, SIMJ_U32* di_prc, SIMJ_U32* si_req, SIMJ_U32* si_prc ) {
	*act = cpu_interrupt_active;
	*req = cpu_interrupt_request;
	*ena = cpu_interrupt_enabled;
	*di_req = cpu_interrupt_DI_total_request_count;
	*di_prc = cpu_interrupt_DI_total_proc_count;
	*si_req = cpu_interrupt_SI_total_request_count;
	*si_prc = cpu_interrupt_SI_total_proc_count;
	return;
}


// ===========================================================================================================
// --------public
void cpu_request_DI( SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr ) {


	// ------- prio, bus combinations should result in unique dev-addr...

	if ( (( CPU_INTR_DI & cpu_interrupt_enabled ) != 0) && cpu_power_on ) {

		// -------- Request ownership of the resource.
		TAKE_RESOURCE( ResourceInterruptRequest );
		cpu_interrupt_DI_request_dev_addr[prio][bus] = dev_addr;
		cpu_interrupt_DI_request_count[prio][bus]++;
		cpu_interrupt_DI_total_request_count++;
		cpu_interrupt_request_external |= (CPU_INTR_DI); // &cpu_interrupt_enabled);
		cpu_interrupt_entry_cond_codes[CPU_INTR_DI_num] = 0;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE( ResourceInterruptRequest );

		// fprintf(stderr, " DI requested da %d, bus %d, pri %d\n", dev_addr, bus, prio);
	}
	else {
		fprintf(stderr, " DI requested, but NOT enabled da %d, bus %d, pri %d\n", dev_addr, bus, prio);
	}
	return;
}


// ===========================================================================================================
// --------public
void cpu_request_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr) {

	// ------- prio, bus combinations should result in unique dev-addr...

	if ( ( ( CPU_INTR_SI & cpu_interrupt_enabled ) != 0 ) && cpu_power_on ) {
		// -------- Request ownership of the resource.
		TAKE_RESOURCE( ResourceInterruptRequest );
		cpu_interrupt_SI_request_dev_addr[prio][bus] = dev_addr;
		cpu_interrupt_SI_request_count[prio][bus]++;
		cpu_interrupt_SI_total_request_count++;
		cpu_interrupt_request_external |= (CPU_INTR_SI); // &cpu_interrupt_enabled);
		cpu_interrupt_entry_cond_codes[CPU_INTR_SI_num] = 0;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE( ResourceInterruptRequest );

		// fprintf(stderr, " SI requested da %d, bus %d, pri %d\n", dev_addr, bus, prio);
	}
	else {
		fprintf(stderr, " SI requested, but NOT enabled da %d, bus %d, pri %d\n", dev_addr, bus, prio);
	}
	return;
}


// ===========================================================================================================
// --------public
void cpu_reset_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr) {

	SIMJ_U16 loc_outstanding_cnt = 0;

	// TODO: NOT SURE THIS IS CORRECT FOR RESETTING DI

	// ------- prio, bus combinations should result in unique dev-addr...

	// -------- Request ownership of the resource.
	TAKE_RESOURCE(ResourceInterruptRequest);
	loc_outstanding_cnt = cpu_interrupt_DI_request_count[prio][bus] - cpu_interrupt_DI_proc_count[prio][bus];
	if (loc_outstanding_cnt != 0) {
		cpu_interrupt_DI_request_dev_addr[prio][bus] = 0;
		cpu_interrupt_DI_request_count[prio][bus] = cpu_interrupt_DI_proc_count[prio][bus];
		if (cpu_interrupt_DI_total_request_count >= loc_outstanding_cnt)
			cpu_interrupt_DI_total_request_count -= loc_outstanding_cnt;
		else
			cpu_interrupt_DI_total_request_count = 0;
		if (cpu_interrupt_DI_total_request_count == cpu_interrupt_DI_total_proc_count)
			cpu_interrupt_request_external &= (~CPU_INTR_DI); // &cpu_interrupt_enabled);
		// TODO: check on this DI reset!
		// cpu_interrupt_entry_cond_codes[CPU_INTR_DI_num] = 0;
	}
	// -------- Release ownership of the resource.
	GIVE_RESOURCE(ResourceInterruptRequest);

#if DEBUG_DI == 1
	fprintf(stderr, " DI reset da %d, bus %d, pri %d\n", dev_addr, bus, prio);
#endif

	return;
}


// ===========================================================================================================
// --------public
void cpu_reset_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr) {

	SIMJ_U16 loc_outstanding_cnt = 0;

	// TODO: NOT SURE THIS IS CORRECT FOR RESETTING SI

	// ------- prio, bus combinations should result in unique dev-addr...

	// -------- Request ownership of the resource.
	TAKE_RESOURCE(ResourceInterruptRequest);
	loc_outstanding_cnt = cpu_interrupt_SI_request_count[prio][bus] - cpu_interrupt_SI_proc_count[prio][bus];
	if (loc_outstanding_cnt != 0) {
		cpu_interrupt_SI_request_dev_addr[prio][bus] = 0;
		cpu_interrupt_SI_request_count[prio][bus] = cpu_interrupt_SI_proc_count[prio][bus];
		if (cpu_interrupt_SI_total_request_count >= loc_outstanding_cnt)
			cpu_interrupt_SI_total_request_count -= loc_outstanding_cnt;
		else
			cpu_interrupt_SI_total_request_count = 0;
		if (cpu_interrupt_SI_total_request_count == cpu_interrupt_SI_total_proc_count)
			cpu_interrupt_request_external &= (~CPU_INTR_SI); // &cpu_interrupt_enabled);
		// TODO: check on this SI reset!
		// cpu_interrupt_entry_cond_codes[CPU_INTR_SI_num] = 0;
	}
	// -------- Release ownership of the resource.
	GIVE_RESOURCE(ResourceInterruptRequest);

#if DEBUG_SI == 1
	fprintf(stderr, " SI reset da %d, bus %d, pri %d\n", dev_addr, bus, prio);
#endif

	return;
}

// ===========================================================================================================
// -------- public
// -------- convience function to return virtual memory via a hardware memory map.
//					
SIMJ_U16 cpu_get_virtual_mem(SIMJ_U16 hardware_map_numb, SIMJ_U16 virt_addr) {

	// -------- bad map number
	if (hardware_map_numb > 7) {
		printf(" *** ERROR ***  cpu_get_virtual_mem - Bad hardware map number (0-7) %d\n", 
				hardware_map_numb);
		return 0;
	}

	// --------

	SIMJ_U32 page_offset = (((cpu_virtual_mem_map[hardware_map_numb].entry[(virt_addr >> 8) & 0x00ff].parts.mem_page) << 8) & 0xffffff00);
	SIMJ_U32 page_word = (virt_addr & 0x00ff);
	SIMJ_U16 mem_value = gbl_mem[(page_offset | page_word)];
#if DEBUG_GET_VIRT_MEM == 1
	fprintf(stderr, " cpu-get-virtual-mem: addr 0x%08x: val: 0x%04x\n", (page_offset | page_word), mem_value);
#endif
	return mem_value;
}
// ===========================================================================================================
// --------public
SIMJ_U32 cpu_get_instruction_count() {
	return cpu_instruction_count;
}


// ===========================================================================================================
// --------public
SIMJ_U16 cpu_get_clock_trigger_count() {
	return cpu_trigger_clock_int;
}


// ===========================================================================================================
// --------public
void cpu_trigger_console_interrupt() {

	if (cpu_power_on) {
		// -------- Request ownership of the resource.
		TAKE_RESOURCE( ResourceInterruptRequest );
		cpu_interrupt_request_external |= (CPU_INTR_console & cpu_interrupt_enabled);
		cpu_interrupt_entry_cond_codes[CPU_INTR_console_num] = 0;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE( ResourceInterruptRequest );
	}
}



// ===========================================================================================================
// --------public
void cpu_trigger_clock_interrupt() {

	// -------only trigger if cpu is powered on and interrupt is enabled..
	if (cpu_power_on) {
		if (cpu_interrupt_enabled & CPU_INTR_clock ) {
			cpu_trigger_clock_int++;
			// -------- Request ownership of the resource.
			TAKE_RESOURCE(ResourceInterruptRequest);
			cpu_interrupt_request_external |= (CPU_INTR_clock & cpu_interrupt_enabled);
			cpu_interrupt_entry_cond_codes[CPU_INTR_clock_num] = 0;
			// -------- Release ownership of the resource.
			GIVE_RESOURCE(ResourceInterruptRequest);
		}
	}
}

// ===========================================================================================================
// --------public
void cpu_set_register_value(SIMJ_U16 reg_index, SIMJ_U16 reg_value) {
	if ( ( !gbl_fp_runlight && !gbl_fp_single_step ) || reg_index == 0) {
		SET_REGISTER_VALUE(reg_index, reg_value);
	}
	else {
		printf(" *** ERROR ***  Cant set register value while CPU is running.\n");
	}
}


// ===========================================================================================================
// --------public
SIMJ_U16 cpu_get_register_value(SIMJ_U16 reg_index) {
	return GET_REGISTER_VALUE(reg_index);
}

// ===========================================================================================================
// --------public
SIMJ_U16 cpu_get_register_block_value(SIMJ_U16 reg_block_numb, SIMJ_U16 reg_numb) {
	return cpu_register_blocks[reg_block_numb].reg16[reg_numb];
}


// ===========================================================================================================
// --------public
void cpu_set_program_counter(SIMJ_U16 pc) {
	if (!gbl_fp_runlight && !gbl_fp_single_step) {
		program_counter = pc;
	}
	else {
		printf(" *** ERROR ***  Cant set program counter value while CPU is running.\n");
	}
}


// ===========================================================================================================
// --------public
SIMJ_U16 cpu_get_program_counter() {
	return program_counter;
}


// ===========================================================================================================
// --------public
PSW cpu_get_current_PSW() {
	PSW loc_psw = { .all = 0 };
	loc_psw.sep.cc_c = cpu_cond_code_c;
	loc_psw.sep.cc_o = cpu_cond_code_o;
	loc_psw.sep.cc_z = cpu_cond_code_z;
	loc_psw.sep.cc_n = cpu_cond_code_n;
	loc_psw.sep.oh = cpu_overflow_hist;
	loc_psw.sep.om = cpu_operand_map;
	loc_psw.sep.im = cpu_instruction_map;
	loc_psw.sep.grb = cpu_register_current_block;
	loc_psw.sep.prv = cpu_priv_mode;
	return loc_psw;
}



// ===========================================================================================================
// read internal register -- as address by front panel.
//
// front panel format
// [ 0 0 0 0 ] [ MAP, IOP, MBC, CTXT] [ EAU, CPU] [ GRF ] 
//
// This may be specific to CPU 7860
//
// --------public
SIMJ_U16 cpu_read_internal_register(SIMJ_U16 front_panel_address) {

	SIMJ_U16 retval = 0;

	if (front_panel_address == 0) {												// -------- front panel switches
		retval = gbl_fp_switches;
	}
	else if (front_panel_address >= 0x001 && front_panel_address <= 0x00f) {	// -------- general registers
		retval = cpu_get_register_value(front_panel_address);
	}
	// else if (front_panel_address == 0x010) {									// -------- MAP RAM INPUT/OUTPUT
	// 	retval = xxxx;
	// }
	else if (front_panel_address == 0x011) {									// -------- EMCR
		retval = cpu_extended_memory_ctrl_reg;
	}
	else if (front_panel_address == 0x012) {									// -------- PSW
		retval = cpu_get_current_PSW().all;
	}
	// else if (front_panel_address == 0x013) {									// -------- EMA
	// 	retval = cpu_interrupt_active;
	// }
	else if (front_panel_address == 0x014) {									// -------- ACT
		retval = cpu_interrupt_active;
	}
	else if (front_panel_address == 0x015) {									// -------- RQT
		retval = cpu_interrupt_request;
	}
	else if (front_panel_address == 0x016) {									// -------- ENA
		retval = cpu_interrupt_enabled;
	}
	// else if (front_panel_address == 0x017) {									// -------- MA
	// 	retval = xxxx;
	// }
	else if (front_panel_address == 0x018) {									// -------- OS
 		retval = cpu_mem_last_OM_data;
	}
	else if (front_panel_address == 0x019) {									// -------- IS
		retval = cpu_mem_last_IM_data;
	}
	// else if (front_panel_address == 0x01A) {									// -------- IR
	// 	retval = xxxx;
	// }
	// else if (front_panel_address == 0x01B) {									// -------- PTB (PR)
	// 	retval = xxx;
	// }
	// else if (front_panel_address == 0x01C) {									// -------- LIT / AG
	// 	retval = xxx;
	// }
	// else if (front_panel_address == 0x01D) {									// -------- TW
	// 	retval = xxx;
	// }
	// else if (front_panel_address == 0x01E) {									// -------- DSW
	// 	retval = xxx;
	// }
	// else if (front_panel_address == 0x01F) {									// -------- MDB
	// 	retval = xxxx;
	// }
	else if (front_panel_address >= 0x041 && front_panel_address <= 0x07f) {	// -------- GRF copies
		retval = cpu_get_register_value(front_panel_address & 0x000f);
	}
	// else if (front_panel_address >= 0x100 && front_panel_address <= 0x1ff) {	// -------- EAU Context files
	// 	retval = cpu_get_register_value(front_panel_address & 0x000f);
	// }
	// else if (front_panel_address >= 0x400 && front_panel_address <= 0x7ff) {	// -------- DMP info.
	// 	retval = cpu_get_register_value(front_panel_address & 0x000f);
	// }
	// else if (front_panel_address >= 0x800 && front_panel_address <= 0x8ff) {	// -------- VIRT MAP RAMS
	// 	retval = cpu_get_register_value(front_panel_address & 0x000f);
	// }
	else {																		// -------- invalid or unsupported
		printf(" *** ERROR ***  Read internal register not implemented for address. FP Addr: 0x%04x\n", front_panel_address);
	}

	return retval;

}

// ===========================================================================================================
// --------public
// cpu_get_last_im_abs_addr()
SIMJ_U32 cpu_get_last_im_abs_addr() {
	return   cpu_mem_last_IM_abs_addr;
}


// ===========================================================================================================
// --------public
// cpu_get_last_om_abs_addr()
SIMJ_U32 cpu_get_last_om_abs_addr() {
	return   cpu_mem_last_OM_abs_addr;
}


// ===========================================================================================================
// --------public
// cpu_get_last_abs_addr()
SIMJ_U32 cpu_get_last_abs_addr() {
	return   cpu_mem_last_abs_addr;
}


// ===========================================================================================================
// --------public

void cpu_classic_instruction_process() {


	// -------- potentially global values  -- for now local, may need to be global later



	// -------- local variables
	INSTRUCTION				instruction = { .all = 0 };

	// -------- for temporary use
	static SIMJ_S32				temp32_addr_calc = 0;

	// -------- parsed instruction values
	// -------- these 4 were static, make register.
	SIMJ_REGISTER SIMJ_U16			tmp_instr_src = 0;
	SIMJ_REGISTER SIMJ_U16			tmp_instr_src_dbl = 0;
	SIMJ_REGISTER SIMJ_U16			tmp_instr_dest = 0;
	SIMJ_REGISTER SIMJ_U16			tmp_instr_dest_dbl = 0;

	// --------these three were static, make register....
	SIMJ_REGISTER VAL16			tmp16_src_value = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_dest_value = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_result_value = { .uval = 0 };

	SIMJ_U64				tempu64_val1 = 0;
	SIMJ_U64				tempu64_val2 = 0;
	SIMJ_U64				tempu64_val3 = 0;
	SIMJ_U64				tempu64_val4 = 0;

	SIMJ_REGISTER SIMJ_U16		tmp16_STK_ADDR;
	SIMJ_REGISTER SIMJ_U16		tmp16_STK_HSA;
	SIMJ_REGISTER SIMJ_U16		tmp16_STK_CSP;
	SIMJ_REGISTER SIMJ_U16		tmp16_STK_LSA;
	SIMJ_REGISTER SIMJ_U16		tmp16_STK_NW;		// numb of words to allocate, same as NV
	SIMJ_REGISTER SIMJ_U16		tmp16_STK_NR;		// numb of registers to copy.

	SIMJ_REGISTER VAL16			tmp16_val1 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val2 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val3 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val4 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val5 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val6 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val7 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val8 = { .uval = 0 };
	SIMJ_REGISTER VAL16			tmp16_val9 = { .uval = 0 };

	SIMJ_REGISTER VAL32			tmp32_val1 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val2 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val3 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val4 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val5 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val6 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val7 = { .uval = 0 };
	SIMJ_REGISTER VAL32			tmp32_val8 = { .uval = 0 };

	SIMJ_REGISTER VAL64			tmp64_val1 = { .uval = 0 };
	SIMJ_REGISTER VAL64			tmp64_val2 = { .uval = 0 };
	SIMJ_REGISTER VAL64			tmp64_val3 = { .uval = 0 };
	SIMJ_REGISTER VAL64			tmp64_val4 = { .uval = 0 };
	SIMJ_REGISTER VAL64			tmp64_val5 = { .uval = 0 };
	SIMJ_REGISTER VAL64			tmp64_val6 = { .uval = 0 };


	SIMJ_S32				temp_bit = 0;

	SIMJ_S32				j = 0;
	// SIMJ_S32				jj = 0;
	SIMJ_U16				j16 = 0;
	bool					do_branch = false;

	char op_code_string[20] = "";

	SIMJ_U16 new_int = 0;
	SIMJ_U16 old_int = 0;

	PSW tmp_PSW = { .all = 0 };
	SIMJ_U16 tmp_dev_addr = 0xffff;

	SIMJ_U8 tmpu8;

	bool print_delayed_message = false;

	SIMJ_U32	fstatus1 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32	fstatus2 = SIMJ_FLTCVT_OTHER_ERR;
	SIMJ_U32	fstatus3 = SIMJ_FLTCVT_OTHER_ERR;

	SIMJ_F64 tmp_f64_val1 = 0.0;
	SIMJ_F64 tmp_f64_val2 = 0.0;
	SIMJ_F64 tmp_f64_val3 = 0.0;

	SIMJ_U16	tmp_new_prog_count = 0;
	SIMJ_U16	tmp_next_instruction = 0;
	SIMJ_U16	tmp_addr_direct = 0;
	SIMJ_U16	tmp_val_direct_1 = 0;
	SIMJ_U16	tmp_val_direct_2 = 0;
	SIMJ_U16	tmp_val_direct_3 = 0;
	SIMJ_U16	tmp_val_direct_4 = 0;

	LARGE_INTEGER StartingTime = { .QuadPart = 0 };

// -------- DEBUG
	char junkxx[200] = { 0 };
// -------- END DEUBG

#include "cpu_instruction_memory_macros.h"
#include "cpu_cond_code_macros.h"

//
//	unimplemented instruction trap
//
//		n z o c
//		x 0 0 x		not rex, not exr / exi
//		x 0 1 x		not rex, exr / exi
//		x 1 0 x		rex, not exr / exi
//		x 1 1 x		rex, exi / exr
//
//  for the 32/85 condition codes as follows
//		n z o c
//		x 0 0 x		not rex, not EXR/EXI
//		x 0 1 x		not rex, instruction is EXR/EXI 
// 	    x 1 0 x		REX instruction, not EXR/EXI
// 	    x 1 1 x		REX instruction, EXR/EXI
//

// -------- UIT
#if DEBUG_UIT_TRAP == 1
#define UNIMPLEMENTED_INSTRUCTION_DEBUG {\
	fprintf(stderr, "\n unimplemented instruction 0x%04x @  0x%04x\n\n",instruction.all, program_counter);\
	fprintf(stdout, "\n unimplemented instruction 0x%04x @  0x%04x\n\n",instruction.all, program_counter);\
}
#else
#define UNIMPLEMENTED_INSTRUCTION_DEBUG {}
#endif
#define UNIMPLEMENTED_INSTRUCTION {\
					UNIMPLEMENTED_INSTRUCTION_DEBUG\
					if ( cpu_interrupt_active >= CPU_INTR_UIT ) {\
						gbl_fp_runlight = false;\
						printf("\nCpu halted.  UIT while interrupt 0-4 active.\n\n");\
						cmd_process_print_prompt();\
					}\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_UIT;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_UIT_num] = 0;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					goto end_instruction;\
					}

#if DEBUG_REX_TRAP == 1
#define REX_INSTRUCTION_DEBUG {\
	fprintf(stderr, "\n unimplemented (REX) instruction 0x%04x @  0x%04x\n\n",instruction.all, program_counter);\
	fprintf(stdout, "\n unimplemented (REX) instruction 0x%04x @  0x%04x\n\n",instruction.all, program_counter);\
}
#else
#define REX_INSTRUCTION_DEBUG {}
#endif
#define REX_INSTRUCTION {\
					REX_INSTRUCTION_DEBUG\
					if ( cpu_interrupt_active >= CPU_INTR_UIT ) {\
						gbl_fp_runlight = false;\
						printf("\nCpu halted.  UIT while interrupt 0-4 active.\n\n");\
						cmd_process_print_prompt();\
					}\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_UIT;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_UIT_num] = PSW_MASK_CC_Z;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					goto end_instruction;\
					}

// -------- this is really the same as UIT.
#if DEBUG_ILL_INST == 1
#define ILLEGAL_INSTRUCTION_DEBUG {\
	fprintf(stderr, "\n Illegal instruction 0x%04x @ 0x%04x\n\n",instruction.all, program_counter);\
}
#else
#define ILLEGAL_INSTRUCTION_DEBUG {}
#endif
#define ILLEGAL_INSTRUCTION {\
					ILLEGAL_INSTRUCTION_DEBUG\
					if ( cpu_interrupt_active >= CPU_INTR_UIT ) {\
						gbl_fp_runlight = false;\
						printf("\nCpu halted.  UIT while interrupt 0-4 active.\n\n");\
						cmd_process_print_prompt();\
					}\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_UIT;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_UIT_num] = 0;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					goto end_instruction;\
					}

#if DEBUG_EXI_EXR == 1
#define EXI_EXR_INSTRUCTION_DEBUG {\
		fprintf(stderr, "\n EXI/EXR inside EXI/ExR instruction 0x%04x @  0x%04x\n\n",instruction.all, program_counter);\
		fprintf(stdout, "\n EXI/EXR inside EXI/ExR instruction 0x%04x @  0x%04x\n\n",instruction.all, program_counter);\
}
#else
#define EXI_EXR_INSTRUCTION_DEBUG {}
#endif
#define EXI_EXR_INSTRUCTION {\
					EXI_EXR_INSTRUCTION_DEBUG\
					if ( cpu_interrupt_active >= CPU_INTR_UIT ) {\
						gbl_fp_runlight = false;\
						printf("\nCpu halted.  UIT while interrupt 0-4 active.\n\n");\
						cmd_process_print_prompt();\
					}\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_UIT;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_UIT_num] = PSW_MASK_CC_O;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					goto end_instruction;\
					}

//	protection trap condition codes
//
//		n z o c
//
//		0 0 0 0
//		0 0 0 1
//		0 0 1 0
//
//		0 x x x		system stall alarm(ambiguous!!)
//
//		0 1 x x		priv instuction trap
//
//		1 0 0 0		virt mode instruction read violation  (jim - no exec)
//		1 0 0 1		virt mode instruction write violation  (jim - ??? no rwe)
//		1 0 1 0		virt mode operand read violation      (jim - no read)
//		1 0 1 1		virt mode operand write violation     (jim - no write)
//
//		1 1 0 0		extended mode instruction read violation
//		1 1 0 1		extended mode instruction write violation
//		1 1 1 0		extended mode operand read violation
//		1 1 1 1		extended mode operand write violation
// 
// for the 32/85 condition codes are defined as follows
// 
// 	   n z o c
// 	   0 0 0 0		memory interface stall alarm
// 	   0 0 0 1		page fault, memory page not resident
// 	   0 0 1 0		page fault, page table not resident
// 	   0 0 1 1		page fault, directory page not resident
// 	   0 1 0 0		mapping fault, no valid entry for state
// 	   0 1 0 1		unrecoverable memory error in page table access during page miss routine
// 	   0 1 1 0		priv instruction violation EXI, EXR
// 	   0 1 1 1		priv instruction violation
// 	   1 0 0 0		address protect, instruction read
// 	   1 0 0 1		CAR/CIR with no active interrupt
// 	   1 0 1 0		address protect, operand read
// 	   1 0 1 1		address protect, operand write
//	   1 1 0 0		not used
//     1 1 0 1		not used
//     1 1 1 0		address protect, EXMA operand read
//     1 1 1 1		address protect, EXMA operand write
// 	


#if DEBUG_PRIV_INSTR_TRAP == 1
#define PRIV_INSTR_TRAP_DEBUG {\
		fprintf(stderr, "\n privileged instruction 0x%04x @ 0x%04x\n\n",instruction.all, program_counter);\
		disp_psw(stdout, cpu_get_current_PSW());\
		disp_interrupts(stderr);\
}
#else
#define PRIV_INSTR_TRAP_DEBUG {}
#endif
#if DEBUG_PRIV_INSTR_HALT == 1
#define PRIV_INSTR_HALT_DEBUG {\
		gbl_fp_runlight = false;\
		printf("\n PRIV INST TRAP -- cpu halted\n");\
}
#else
#define PRIV_INSTR_HALT_DEBUG {}
#endif
#define PRIV_INSTR_TRAP {\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_sys_protect;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_sys_protect_num] = PSW_MASK_CC_Z | PSW_MASK_CC_O | PSW_MASK_CC_C;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					PRIV_INSTR_TRAP_DEBUG\
					PRIV_INSTR_HALT_DEBUG\
					goto end_instruction;\
					}


#if DEBUG_MEM_ACCESS_TRAP == 1
#define MEM_ACCESS_TRAP_NO_READ_DEBUG {\
		fprintf(stderr, "\n mem access trap - no read - inst: 0x%04x @ 0x%04x, acc:  0x%04x\n\n",instruction.all, program_counter, cpu_mem_last_access_rights);\
		disp_psw(stderr, cpu_get_current_PSW());\
		disp_interrupts(stderr);\
}
#else
#define MEM_ACCESS_TRAP_NO_READ_DEBUG {}
#endif
#if DEBUG_MEM_ACCESS_HALT == 1
#define MEM_ACCESS_HALT_DEBUG {\
		gbl_fp_runlight = false;\
		printf("\n MEM ACCESS TRAP -- cpu halted\n");\
}
#else
#define MEM_ACCESS_HALT_DEBUG {}
#endif

// --------this is for operand map...
#define MEM_ACCESS_TRAP_NO_READ {\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_sys_protect;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_sys_protect_num] = PSW_MASK_CC_N | PSW_MASK_CC_O;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					MEM_ACCESS_TRAP_NO_READ_DEBUG\
					MEM_ACCESS_HALT_DEBUG\
					goto end_instruction;\
					}

#define MEM_ACCESS_TRAP_NO_READ_IM {\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_sys_protect;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_sys_protect_num] = PSW_MASK_CC_N ;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					MEM_ACCESS_TRAP_NO_READ_DEBUG\
					MEM_ACCESS_HALT_DEBUG\
					goto end_instruction;\
					}

#define MEM_ACCESS_TRAP_NO_READ_OM {\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_sys_protect;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_sys_protect_num] = PSW_MASK_CC_N | PSW_MASK_CC_O;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					MEM_ACCESS_TRAP_NO_READ_DEBUG\
					MEM_ACCESS_HALT_DEBUG\
					goto end_instruction;\
					}


#if DEBUG_MEM_ACCESS_TRAP == 1
#define MEM_ACCESS_TRAP_NO_EXEC_DEBUG {\
		disp_psw(stderr, cpu_get_current_PSW());\
		fprintf(stderr, "\n mem access trap - no execute - inst: 0x%04x @ 0x%04x, acc:  0x%04x\n\n",instruction.all, program_counter, cpu_mem_last_access_rights);\
		disp_interrupts(stderr);\
}
#else
#define MEM_ACCESS_TRAP_NO_EXEC_DEBUG {}
#endif
#define MEM_ACCESS_TRAP_NO_EXEC {\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_sys_protect;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_sys_protect_num] = PSW_MASK_CC_N;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					MEM_ACCESS_TRAP_NO_EXEC_DEBUG\
					MEM_ACCESS_HALT_DEBUG\
					goto end_instruction;\
					}

#if DEBUG_MEM_ACCESS_TRAP == 1
#define MEM_ACCESS_TRAP_NO_WRITE_DEBUG {\
		disp_psw(stderr, cpu_get_current_PSW());\
		fprintf(stderr, "\n mem access trap - no write - inst: 0x%04x @ 0x%04x, acc:  0x%04x\n\n",instruction.all, program_counter, cpu_mem_last_access_rights);\
		disp_interrupts(stderr);\
}
#else
#define MEM_ACCESS_TRAP_NO_WRITE_DEBUG {}
#endif
#define MEM_ACCESS_TRAP_NO_WRITE {\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_sys_protect;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_sys_protect_num] = PSW_MASK_CC_N | PSW_MASK_CC_O | PSW_MASK_CC_C;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					skip_interrupt_determination = false;\
					MEM_ACCESS_TRAP_NO_WRITE_DEBUG\
					MEM_ACCESS_HALT_DEBUG\
					goto end_instruction;\
					}

//
//	float trap
//
//			N	0 = result >= zero
//				1 = result < zero
//			Z	0 = result not zero
//				1 = result Is zero
//			O	1 always set
//			C	0 = exponent overflow
//				1 = exponent underflow
//
//  for the 32/85 condition codes as follows
//		
//			N	0 = result >= zero
//				1 = result < zero
//			Z	0 = result not zero
//				1 = result Is zero
//			O	1 always set
//			C	0 = exponent underflow
//				1 = exponent overflow
//				**> NOTE <** C is different !!!
//

// TODO: Set CC for FLOAT TRAP
#if DEBUG_FLOAT_TRAP == 1
#define FLOAT_TRAP_DEBUG {\
		fprintf(stderr, "\n Float trap 0x%04x @ 0x%04x\n\n",instruction.all, program_counter);\
}
#else
#define FLOAT_TRAP_DEBUG {}
#endif
#define FLOAT_TRAP {\
					FLOAT_TRAP_DEBUG\
					TAKE_RESOURCE( ResourceInterruptRequest );\
					cpu_interrupt_request |= CPU_INTR_floating_overflow;\
					cpu_interrupt_entry_cond_codes[CPU_INTR_floating_overflow_num] = 0;\
					GIVE_RESOURCE( ResourceInterruptRequest );\
					goto end_instruction;\
					}


//#define BAD_SHORT_DISPLACED_ADDR_TRAP {\
//					fprintf(stderr, "\n bad address trap 0x%04x @  0x%04x\n",instruction.all, program_counter);\
//					fprintf(stderr,"    memory including instruction  0x%04x  0x%04x  0x%04x  0x%04x\n",\
//									GET_MEMORY_VALUE_ABS(program_counter), \
//									GET_MEMORY_VALUE_ABS(program_counter + 1), \
//									GET_MEMORY_VALUE_ABS(program_counter + 2), \
//									GET_MEMORY_VALUE_ABS(program_counter + 3) ); \
//					fprintf(stderr, "    register R1 0x%04x\n",GET_REGISTER_VALUE(1) );\
//					fprintf(stderr, "    short displaced addr calc  0x%04x\n",  GET_MEMORY_ADDR_SHORT_DISPLACED );\
//					TAKE_RESOURCE( ResourceInterruptRequest );\
//					cpu_interrupt_request |= CPU_INTR_sys_protect;\
//					GIVE_RESOURCE( ResourceInterruptRequest );\
//					goto end_instruction;\
//					}
//

// --------get the highest active interrupt 0-15, 16 is returned if nothing active.
// TODO: Use cpu_interrupt_active_num instead of search.
#define GET_HIGHEST_ACTIVE_INT( INT_NUMB ) {\
		INT_NUMB = 16;\
		for (j16 = 0; j16 < 16; j16++) {\
			if ((bit[j16] & cpu_interrupt_active) != 0) {\
				INT_NUMB = j16;\
				break;\
			}\
		}\
		}

// --------clear bit in active register.
// --------update helper variables...
// --------nothing active, reset masks.
// --------set information for newest high active interrupt.
// TODO: should the new active number determination be delayed until after 1 inst execution occurs.
#define CLEAR_ACTIVE_INT( INT_NUMB ) {\
	cpu_interrupt_active &= bitnot[INT_NUMB];\
	if (cpu_interrupt_active == 0) {\
		cpu_interrupt_active_mask = mask[15];\
		cpu_interrupt_active_num = 16;\
	}\
	else {\
		SIMJ_U16 new_high_int = cpu_find_bit(cpu_interrupt_active);\
		cpu_interrupt_active_mask = mask[new_high_int];\
		cpu_interrupt_active_num = new_high_int;\
	}\
	}

// --------clear request
// -------- Request ownership of the resource.
// -------- Release ownership of the resource.
// TODO: maybe clear DI/SI anyway and have new variable that holds want DI/SI and reset at top of int determine.
#define CLEAR_REQUEST_INT( INT_NUMB ) {\
		TAKE_RESOURCE(ResourceInterruptRequest);\
		if ((INT_NUMB == CPU_INTR_DI_num) && (cpu_interrupt_DI_total_request_count == cpu_interrupt_DI_total_proc_count)) {\
			cpu_interrupt_request &= bitnot[ INT_NUMB ];\
		}\
		else if ((INT_NUMB == CPU_INTR_SI_num) && (cpu_interrupt_SI_total_request_count == cpu_interrupt_SI_total_proc_count)) {\
			cpu_interrupt_request &= bitnot[INT_NUMB];\
		}\
		else {\
			cpu_interrupt_request &= bitnot[INT_NUMB];\
		}\
		GIVE_RESOURCE(ResourceInterruptRequest);\
		}

// -------- GET THE HOP OFFSET FROM THE INSTRUCTION.
#define GET_HOP_OFFSET ( ( instruction.all & 0x0040 ) ? (SIMJ_U16)((instruction.all & 0x007f) | 0xff80) : (SIMJ_U16)(instruction.all & 0x007f) )

// -------- CALCULATE THE NEXT PROGRAM COUNTER WHEN HOP INSTRUCTION BRANCHES.
#define GET_NEXT_PROGRAM_COUNTER_HOP  ( (SIMJ_U16)(program_counter + GET_HOP_OFFSET) )

// -------- SET THE NEXT PROGRAM COUNTER VALUE.
#define SET_NEXT_PROGRAM_COUNTER(A)	{\
					program_counter = (A);\
					}

// -------- CALCULATE NEXT PROGRAM COUNTER FOR ONE WORD INSTRUCTIONS
#define PROGRAM_COUNTER_ONE_WORD_INSTRUCT ( program_counter+1 )

// -------- CALCULATE NEXT PROGRAM COUNTER FOR TWO WORD INSTRUCTIONS
#define PROGRAM_COUNTER_TWO_WORD_INSTRUCT ( program_counter+2 )

// -------- CALCULATE NEXT PROGRAM COUNTER FOR THREE WORD INSTRUCTIONS
#define PROGRAM_COUNTER_THREE_WORD_INSTRUCT ( program_counter+3 )

// -------- CALCULATE NEXT PROGRAM COUNTER FOR FOUR WORD INSTRUCTIONS
#define PROGRAM_COUNTER_FOUR_WORD_INSTRUCT ( program_counter+4 )

// -------- SET NEXT PROGRAM COUNTER FOR BRANCHING INSTRUCTIONS.
#define CONDITIONAL_BRANCH( TEST_VALUE,  BRANCH_PC, NO_BRANCH_PC ) {\
					if ( (TEST_VALUE) ) {\
						SET_NEXT_PROGRAM_COUNTER( BRANCH_PC );\
					}\
					else {\
						SET_NEXT_PROGRAM_COUNTER( NO_BRANCH_PC );\
					}\
					} 

#define TEST_VALID_DOUBLE_REGISTER(ZREG) (ZREG != 0 && ((ZREG & 0x0001) == 0))

#define TEST_VALID_TRIPLE_REGISTER(ZREG) (ZREG != 0 && ((ZREG & 0x0003) == 0))

#define TEST_VALID_QUAD_REGISTER(ZREG) (ZREG != 0 && ((ZREG & 0x0003) == 0))



// -------- test if cpu mode is privileged
// TODO: check 7830 priv mode instructions...
#if SIMJ_SIM_CPU == 7830
#define IS_PRIV_MODE  ( ( cpu_priv_mode && cpu_virtual_mode ) || ( !cpu_virtual_mode && ( !cpu_nonvirt_prot_enabled || ( cpu_nonvirt_prot_enabled && ( program_counter < cpu_nonvirt_prot_lowprot_reg || ( program_counter >= cpu_nonvirt_prot_upprot_reg && program_counter < cpu_nonvirt_prot_gbl_reg) ) ) ) ) )
#else
#define IS_PRIV_MODE  (cpu_priv_mode || !cpu_virtual_mode) 
#endif



// --------allowed set interrupt masks
#define SIA_ALLOWED 0xffff		// docs say level 0 cant be set active, but diags expect it.	
#define SIE_ALLOWED 0xffff		// some disables are prohibited.
#define SIR_ALLOWED 0xffff		// level C and D should not be program requested, but it isn't prevented.

#define RIA_ALLOWED_NOT	( ~0xffff )		
#define RIE_ALLOWED_NOT	( ~0x13ff )		// -- not allowed to reset enable 0, 1, 2, 4, 5 	
#define RIR_ALLOWED_NOT ( ~0xffff )


	// --------------------------------------------------------
	// printf("\n CPU started.\n");

	// --------execute instructions while running or single stepping.
	while (gbl_fp_runlight | gbl_fp_single_step) {

		// -------- get starting time for wait.
		StartingTime = util_high_res_spin_wait_get_start();

		// -------- this is a kludge for REG 0
		saved_reg_0 = GET_REGISTER_VALUE(0);

		// ---------------------------------------------------------------------
		// -------- check for new interrupts
		// TODO: optimize interrupt determination.
		if (!skip_interrupt_determination ) {

			// -------- make interrupt request register synchronous.
			// TODO: Is it really necessary to sync interrupt requests.
			TAKE_RESOURCE( ResourceInterruptRequest );
			cpu_interrupt_request |= cpu_interrupt_request_external;
			cpu_interrupt_request_external = 0;
			GIVE_RESOURCE( ResourceInterruptRequest );

			// TODO: Not sure new interupt determination and processing is correct.
			// Do we have a new request....
			tmp16_val1.uval = (cpu_interrupt_request & cpu_interrupt_enabled & cpu_interrupt_active_mask);
			if ( tmp16_val1.uval > (cpu_interrupt_active & cpu_interrupt_active_mask) ) {

				// --------find out which interrupt we are requesting
				new_int = cpu_find_bit(tmp16_val1.uval);

				// --------save current process_status_double_word in dedicated memory location
				if (new_int < 16) {

					// --------store the current program counter and status double word to dedicated memory location
					SET_MEMORY_VALUE_ABS(CPU_INTR_BASE_RETURN_PC + (new_int * 2), program_counter);
					SET_MEMORY_VALUE_ABS(CPU_INTR_BASE_RETURN_PS + (new_int * 2), (cpu_get_current_PSW().all) );

					// --------set new PSW and new PC -- to go to the interrupt processing routine.

					// -------- DI
					if (new_int == CPU_INTR_DI_num) {
						print_delayed_message = false;
						// -------- turn off DI request and report error.
						// -------- Request ownership of the resource.
						TAKE_RESOURCE( ResourceInterruptRequest );
						tmp_dev_addr = cpu_get_next_DI_request();
						// -------- if nothing found, report error  (try device address 0 )
						if (tmp_dev_addr == 0xffff) {
							tmp_dev_addr = 0;
						}
						//	//  cpu_interrupt_request &= ~CPU_INTR_DI;
						//	print_delayed_message = true;
						//	// -------- set the normal address for DI if no devices requested...
						//	program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PC + (new_int * 2));
						//	tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PS + (new_int * 2));
						//	cpu_set_current_PSW(tmp_PSW);

						//	// --------set that the interrupt is active!
						//	cpu_interrupt_active |= bit[new_int];
						//	cpu_interrupt_active_mask = mask[new_int];
						//  cpu_interrupt_active_num = new_int;
						//}
						//else {
						program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_DI_BASE_ENTRY_PC + tmp_dev_addr);
						tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PS + (new_int * 2));
						//--debug
						//--debug--fprintf(stderr, " -- old PSW - ");
						//--debug--disp_psw(stderr, cpu_get_current_PSW());
						//--debug--fprintf(stderr, " -- new PSW - ");
						//--debug--disp_psw(stderr, tmp_PSW);
						//--debug
						cpu_set_current_PSW(tmp_PSW);
						cpu_priv_mode = true;	// DEBUG !!!

						// --------set that the interrupt is active!
						cpu_interrupt_active |= bit[new_int];
						cpu_interrupt_active_mask = mask[new_int];
						cpu_interrupt_active_num = new_int;
						//}
						// -------- Release ownership of the resource.
						GIVE_RESOURCE( ResourceInterruptRequest );
#if DEBUG_DI == 1
						if (print_delayed_message) {
							fprintf(stderr, "\n cpu - Erroneous DI interrupt request, ignored.\n");
						}
#endif
					}

					// -------- SI
					else if (new_int == CPU_INTR_SI_num) {
						print_delayed_message = false;
						// -------- Request ownership of the resource.
						TAKE_RESOURCE( ResourceInterruptRequest );
						tmp_dev_addr = cpu_get_next_SI_request();
						// -------- if nothing found, report error
						if (tmp_dev_addr == 0xffff) {
							tmp_dev_addr = 0;
						}
						// 	// -------- turn off SI request and report error.
						// 	//  cpu_interrupt_request &= ~CPU_INTR_SI;
						// 	print_delayed_message = true;
						// 	// -------- set the normal address for DI if no devices requested...
						// 	program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PC + (new_int * 2));
						// 	tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PS + (new_int * 2));
						// 	cpu_set_current_PSW(tmp_PSW);

						// 	// --------set that the interrupt is active!
						// 	cpu_interrupt_active |= bit[new_int];
						// 	cpu_interrupt_active_mask = mask[new_int];
						//	cpu_interrupt_active_num = new_int;
						// }
						// else {
						program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_SI_BASE_ENTRY_PC + tmp_dev_addr);
						tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PS + (new_int * 2));
						//--debug
						//--debug--fprintf(stderr, " -- old PSW - ");
						//--debug--disp_psw(stderr, cpu_get_current_PSW());
						//--debug--fprintf(stderr, " -- new PSW - ");
						//--debug--disp_psw(stderr, tmp_PSW);
						//--debug
						cpu_set_current_PSW(tmp_PSW);
						cpu_priv_mode = true;	// DEBUG !!!

						// --------set that the interrupt is active!
						cpu_interrupt_active |= bit[new_int];
						cpu_interrupt_active_mask = mask[new_int];
						cpu_interrupt_active_num = new_int;
						// }
						// -------- Release ownership of the resource.
						GIVE_RESOURCE( ResourceInterruptRequest );
#if DEBUG_SI == 1
						if (print_delayed_message) {
							fprintf(stderr, "\n cpu - Erroneous SI interrupt request, ignored.\n");
						}
#endif
					}

					// -------- All other interrupts
					else {
						// --------get instruction that caused issue..
						// THIS WILL FAIL ON MEM PROTECT TRAP...
						// JAS commented this out 9/4/2025
						// ---need??-- GET_MEMORY_VALUE_IM(tmp16_val1.uval, program_counter);
						program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PC + (new_int * 2));
						tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_ENTRY_PS + (new_int * 2));

						// --------set condition codes for different interrupt numbers.
						if (new_int >= CPU_INTR_power_fail_num && new_int <= CPU_INTR_floating_overflow_num) {
							tmp_PSW.all = (tmp_PSW.all & ~PSW_MASK_CC_ALL) | (cpu_interrupt_entry_cond_codes[new_int] & PSW_MASK_CC_ALL);
						}

						//--debug
						//--debug--fprintf(stderr, " -- old PSW - ");
						//--debug--disp_psw(stderr, cpu_get_current_PSW());
						//--debug--fprintf(stderr, " -- new PSW - ");
						//--debug--disp_psw(stderr, tmp_PSW);
						//--debug
						cpu_set_current_PSW(tmp_PSW);
						cpu_priv_mode = true;	// DEBUG !!!

						//if (new_int == 1) {
						//	SET_CC_N(true);		// global
						//}
						//else if (new_int == 2) {
						//	SET_CC_N(false);
						//	SET_CC_Z(true);
						//}
						//else if (new_int == 4) {
						//	SET_CC_Z(((tmp16_val1.uval & 0xff00) == 0x2300));  // rex
						//	SET_CC_O(((tmp16_val1.uval & 0xff00) == 0xb300) || ((tmp16_val1.uval & 0xff0f) == 0xe805)); // exi, exr
						//}
						 
						// --------set that the interrupt is active!
						cpu_interrupt_active |= bit[new_int];
						cpu_interrupt_active_mask = mask[new_int];
						cpu_interrupt_active_num = new_int;
					}

					// fprintf(stderr, "\n cpu - new interrupt level %d, new pc 0x%04x, new psw 0x%04x\n", new_int, program_counter, cpu_get_current_PSW().all);

				}
				else {
					// --------DEBUG
					fprintf(stderr, "\n cpu - erroneous interrupt determination.  new_int: %d, new pc 0x%04x, req: 0x%04x, act: 0x%04x, ena: 0x%04x\n", new_int, program_counter, cpu_interrupt_request, cpu_interrupt_active, cpu_interrupt_enabled);
					// --------END DEBUG
				}
			}
		}

		// -------- reset skip interrupt determination (this is used so we get one instruction executed before the next interrupt)
		skip_interrupt_determination = false;


		// -------------------------------------------------
		// --------normal instruction processing.

		// -------- fetch the next instruction
		GET_MEMORY_VALUE_IM(instruction.all, program_counter);

		// -------- add to instruction trace.
		// TODO: fix trace - use data not address....
		TAKE_RESOURCE( ResourceInstructionTrace );
		cpu_pc_trace[instruction_trace_index] = program_counter;
		cpu_psw_trace[instruction_trace_index] = cpu_get_current_PSW();
		cpu_actint_trace[instruction_trace_index] = cpu_interrupt_active;
		instruction_trace[instruction_trace_index] = GET_ABS_MEMORY_ADDR_IM(program_counter);
		instruction_trace_w1[instruction_trace_index] = GET_ABS_MEMORY_ADDR_IM(program_counter+1);
		instruction_trace_w2[instruction_trace_index] = GET_ABS_MEMORY_ADDR_IM(program_counter+2);
		instruction_trace_w3[instruction_trace_index] = GET_ABS_MEMORY_ADDR_IM(program_counter+3);
		instruction_trace_w4[instruction_trace_index] = GET_ABS_MEMORY_ADDR_IM(program_counter+4);
		instruction_trace_index++;
		instruction_trace_index &= 0x03ff;		// 0 -  1023
		GIVE_RESOURCE( ResourceInstructionTrace );

		// -------- check for EXI and EXR instructions.  These are special.  Replace the value
		// -------- of instruction with the results of that instruction.
		if (instruction.parts.op_code == OP_EXR) {		// EXR
			cpu_inst_used[opcode_get_opcode_index(instruction.all)]++;	// debug
#if DEBUG_EXI_EXR == 1
			fprintf(stderr," EXR - src reg numb %d, dst reg numb %d, src val 0x%04hx, dst val 0x%04hx\n",
				instruction.parts.src_reg, instruction.parts.dest_reg,
				GET_SOURCE_REGISTER_VALUE, GET_DESTINATION_REGISTER_VALUE);
#endif
			// --------yes instruction is the or of the src and dest register values.
			// --------does this include register 0.
			instruction.all = GET_SOURCE_REGISTER_VALUE | GET_DESTINATION_REGISTER_VALUE;
		}
		else if ((instruction.all & 0xff0f) == 0xe805) {	// EXI
			cpu_inst_used[opcode_get_opcode_index(instruction.all)]++;	// debug
			GET_MEMORY_VALUE_IMMEDIATE(tmp_next_instruction);
#if DEBUG_EXI_EXR == 1
			fprintf(stderr," EXI - dst reg numb %d, dst val 0x%04hx, immed val 0x%04hx, \n",
				instruction.parts.dest_reg,
				GET_DESTINATION_REGISTER_VALUE, tmp_next_instruction);
#endif
			instruction.all = tmp_next_instruction | GET_DESTINATION_REGISTER_VALUE;
			program_counter++;		// it only makes sense to do this.  document doesn't mention this.
		}


		// -------- debug -- fill array of used instrutions
		cpu_inst_used[opcode_get_opcode_index(instruction.all)]++;

		// -------- if verbose display the instruction.
		if (gbl_verbose_debug == debugging_on) {

			disp_psw(stderr, cpu_get_current_PSW());
			disp_cur_reg(stderr);
			util_get_opcode_disp(instruction.all, op_code_string, 20);

			printf("pc: %04X,  %s,\tinstruction: %04x,  op code: %04x\n",
				program_counter, op_code_string, instruction.all, instruction.parts.op_code);
			cmd_process_print_prompt();
		}

		// --------process each opcode.  
		switch (instruction.parts.op_code) {

		// --------00 -- HLT -- Halt (Privileged)          
		case  OP_HLT:			// 0x00
			if (IS_PRIV_MODE) {
#if DEBUG_PREVENT_HLT != 1
				gbl_fp_runlight = false;
#endif
#if DEBUG_HLT == 1
				fprintf(stderr,"\n");
				disp_pc(stderr, program_counter);
				disp_psw(stderr,cpu_get_current_PSW());
				disp_cur_reg(stderr);
				disp_interrupts(stderr);
#endif
				printf("\nCpu halted.  pc = 0x%04x\n", program_counter);
				cmd_process_print_prompt();
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				PRIV_INSTR_TRAP;
			}
			break;

		case  OP_AUG01:			// 0x01	
			switch (instruction.parts.dest_reg) {

			// --  0	RMI -- Request Multi·processor interrupt
			case 0:
				if (IS_PRIV_MODE) {
					rmi_request(instruction.parts.src_reg);
#if DEBUG_RMI >= 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, RMI: 0x%04x,  \n",
						junkxx, instruction.all, program_counter,
						instruction.parts.src_reg);
					gbl_fp_runlight = false;
					printf("\n RMI instruction - debug halt\n");
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  1	EVMO -- Enter Virtual Mode of CPU Execution
			case 1:
				if (IS_PRIV_MODE) {
					cpu_virtual_mode = true;
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  2	SIOM -- Select Another Program's IM as Current OM
			case 2:
				if (IS_PRIV_MODE) {
					tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
					cpu_operand_map = (tmp16_val1.uval >> PSW_SHIFT_IMAP) & 0x0007;
#if DEBUG_SIOM == 1
					fprintf(stderr, " SIOM  pc: 0x%04x, new omap %d, reg value: 0x%04x\n", program_counter, cpu_operand_map, tmp16_val1.uval);
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  3	SOOM -- Select Another Program's OM as Current OM
			case 3:
				if (IS_PRIV_MODE) {
					tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
					cpu_operand_map = (tmp16_val1.uval >> PSW_SHIFT_OMAP) & 0x0007;
#if DEBUG_SOOM == 1
					fprintf(stderr, " SOOM  pc: 0x%04x, new omap %d, reg value: 0x%04x\n", program_counter, cpu_operand_map, tmp16_val1.uval);
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  4	SZOM -- Select Map Zero as Current OM
			case 4:
				if (IS_PRIV_MODE) {
					cpu_operand_map = 0;
#if DEBUG_SZOM == 1
					//--debug--fprintf(stderr, " SZOM  pc: 0x%04x\n", program_counter);
#endif 
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  5	SCRB -- Select Current Register Block in PSD
			case 5:
				if (IS_PRIV_MODE) {
					tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
					tmpu8 = (SIMJ_U8)((tmp16_val1.uval >> PSW_SHIFT_GRB) & 0x000f);
#if DEBUG_SCRB == 1
					fprintf(stderr, "\n   SCRB cur reg block %d, new block %d  \n", cpu_register_current_block, tmpu8);
#endif
					if (tmpu8 != cpu_register_current_block) {
						cpu_switch_register_blocks(tmpu8);
					}
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  6	EXMA -- Enter Extended Memory Addressing Mode
			// --  7	EXMA -- Enter Extended Memory Addressing Mode
			// TODO: EXMA instructions !!!
			case 6:
			case 7:
#if SIMJ_SIM_CPU == 7830
				UNIMPLEMENTED_INSTRUCTION;
#else
				UNIMPLEMENTED_INSTRUCTION;
#endif
				break;

			// --  8	XVMO -- Exit Virtual Mode of CPU Execution
			case 8:
				if (IS_PRIV_MODE) {
					cpu_virtual_mode = false;
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  9	ZIMP -- Zero Section of Instruction Map
			case 9:
				if (IS_PRIV_MODE) {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> PSW_SHIFT_IMAP) & 0x0007;	// contains map number. (IMAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;					// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val6.sval = -1 * tmp32_val5.sval;									// positive length
#if DEBUG_ZIMP == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, map: 0x%04x \n",
							junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val6.uval);
#endif
					RESTART_INSTRUCTION_SETUP(program_counter, (instruction.all), 0, tmp32_val4.sval, tmp32_val4.sval + tmp32_val6.sval)
					RESTART_INSTRUCTION_GET_LOOP(tmp32_val4.sval, tmp32_val6.sval)
					for (j = tmp32_val4.sval; j < tmp32_val6.sval; j++) {
						cpu_virtual_mem_map[tmp16_val2.uval].entry[j].all = 0;
						// -------update count in source register.
						tmp16_val8.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
						tmp16_val9.uval = GET_REGISTER_VALUE(tmp16_val8.uval);
						//tmp16_val9.uval = (tmp16_val9.uval & 0xff00) | (((tmp16_val9.uval | 0xff00)+1) & 0x00ff);
						tmp16_val9.uval = ((tmp16_val9.uval + (SIMJ_U16)0x0100) & (SIMJ_U16)0xff00) | (((tmp16_val9.uval | (SIMJ_U16)0xff00) + (SIMJ_U16)1) & (SIMJ_U16)0x00ff);
						SET_REGISTER_VALUE(tmp16_val8.uval, tmp16_val9.uval);

						// --------see if time to check interrupts.
						if (j > tmp32_val4.sval && (((j - tmp32_val4.sval+1) % 16) == 0) && ((j + 1) < tmp32_val6.sval)) {
							// --------see there is a new interrupt.
							IS_THERE_A_NEW_INTERRUPT
								// -------- update register with new starting offset and length, then quit instruction
								//tmp32_val4.uval += j;		// starting offset.
								//tmp32_val5.sval += j;		// negative length
								//tmp32_val8.uval = tmp32_val1.uval & 0x0000ffff | ((tmp32_val5.uval << 16) & 0x00ff0000) | ((tmp32_val4.uval << 24) & 0xff000000);
								//SET_SOURCE_REGISTER_VALUE_DOUBLE(tmp32_val8.uval);
								//fprintf(stderr, "       interrupting   updated reg: 0x%08x, new off: 0x%08x, new -len: 0x%08x \n",
								//	tmp32_val8.uval, tmp32_val4.uval, tmp32_val5.uval);
								RESTART_INSTRUCTION_SAVE_LOOP(j + 1, tmp32_val6.sval)
#if DEBUG_ZIMP == 1
								fprintf(stderr, "       interrupting   next start: %d, next end: %d \n", j + 1, tmp32_val6.sval);
#endif
							END_IS_THERE_A_NEW_INTERRUPT
						}
					}
					RESTART_INSTRUCTION_COMPLETE
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  B	ZOMP -- Zero Section of Operand Map
			case 11:
				if (IS_PRIV_MODE) {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> PSW_SHIFT_OMAP) & 0x0007;	// contains map number. (OMAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;					// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val6.sval = -1 * tmp32_val5.sval;									// positive length
#if DEBUG_ZOMP == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, map: 0x%04x \n",
							junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val6.uval);
#endif
					RESTART_INSTRUCTION_SETUP(program_counter, (instruction.all), 0, tmp32_val4.sval, tmp32_val4.sval + tmp32_val6.sval)
					RESTART_INSTRUCTION_GET_LOOP(tmp32_val4.sval, tmp32_val6.sval)
					for (j = tmp32_val4.sval; j < tmp32_val6.sval; j++) {
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val4.uval].all = 0;
						// -------update count in source register.
						tmp16_val8.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
						tmp16_val9.uval = GET_REGISTER_VALUE(tmp16_val8.uval);
						// tmp16_val9.uval = (tmp16_val9.uval & 0xff00) | (((tmp16_val9.uval | 0xff00)+1) & 0x00ff);
						tmp16_val9.uval = ((tmp16_val9.uval + (SIMJ_U16)0x0100) & (SIMJ_U16)0xff00) | (((tmp16_val9.uval | (SIMJ_U16)0xff00) + (SIMJ_U16)1) & (SIMJ_U16)0x00ff);
						SET_REGISTER_VALUE(tmp16_val8.uval, tmp16_val9.uval);
						// --------see if time to check interrupts.
						if (j > tmp32_val4.sval && (((j - tmp32_val4.sval+1) % 16) == 0) && ((j + 1) < tmp32_val6.sval)) {
							// --------see there is a new interrupt.
							IS_THERE_A_NEW_INTERRUPT
								// -------- update register with new starting offset and length, then quit instruction
								// tmp32_val4.uval += j;		// starting offset.
								// tmp32_val5.sval += j;		// negative length
								// tmp32_val8.uval = tmp32_val1.uval & 0x0000ffff | ((tmp32_val5.uval << 16) & 0x00ff0000) | ((tmp32_val4.uval << 24) & 0xff000000);
								// SET_SOURCE_REGISTER_VALUE_DOUBLE(tmp32_val8.uval);
								// fprintf(stderr, "       interrupting   updated reg: 0x%08x, new off: 0x%08x, new -len: 0x%08x \n",
								//	tmp32_val8.uval, tmp32_val4.uval, tmp32_val5.uval);
								RESTART_INSTRUCTION_SAVE_LOOP(j + 1, tmp32_val6.sval)
#if DEBUG_ZOMP == 1
								fprintf(stderr, "       interrupting   next start: %d, next end: %d \n", j + 1, tmp32_val6.sval);
#endif
							END_IS_THERE_A_NEW_INTERRUPT
						}
					}
					RESTART_INSTRUCTION_COMPLETE
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  D	LIMP -- Load Instruction Map Image into Hardware Map
			case 13:
				if (IS_PRIV_MODE) {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> PSW_SHIFT_IMAP) & 0x0007;	// contains map number. (IMAP)
					tmp32_val3.uval = (tmp32_val1.uval & 0x00001fff) << 8;					// abs page address (MIAP) offset
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;					// VP (starting offset in map and miap) 
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val6.sval = -1 * tmp32_val5.sval;									// positive length
#if DEBUG_LIMP == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, map: 0x%04x \n", 
							junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start page:   0x%08x \n", tmp32_val3.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val6.uval);
#endif
					RESTART_INSTRUCTION_SETUP(program_counter, (instruction.all), 0, tmp32_val4.sval, tmp32_val4.sval+tmp32_val6.sval)
					RESTART_INSTRUCTION_GET_LOOP(tmp32_val4.sval, tmp32_val6.sval)

					for (j = tmp32_val4.sval; j < tmp32_val6.sval; j++) {
						//tmp32_val7.uval = tmp32_val4.uval + j;
						tmp32_val7.sval = j;
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val7.uval].all = GET_MEMORY_VALUE_ABS(tmp32_val3.uval | tmp32_val7.uval);
						// -------update count in source register.
						tmp16_val8.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
						tmp16_val9.uval = GET_REGISTER_VALUE(tmp16_val8.uval);
						// tmp16_val9.uval = (tmp16_val9.uval & 0xff00) | (((tmp16_val9.uval | (SIMJ_U16)0xff00)+(SIMJ_U16)1) & (SIMJ_U16)0x00ff);
						tmp16_val9.uval = ((tmp16_val9.uval + (SIMJ_U16)0x0100) & (SIMJ_U16)0xff00) | (((tmp16_val9.uval | (SIMJ_U16)0xff00) + (SIMJ_U16)1) & (SIMJ_U16)0x00ff);
						SET_REGISTER_VALUE(tmp16_val8.uval, tmp16_val9.uval);
#if DEBUG_LIMP == 1
						fprintf(stderr, "        map: %3d  entry: 0x%04x Value: 0x%04x \n", tmp16_val2.uval, tmp32_val7.uval, cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val7.uval].all);
#endif
						// --------see if time to check interrupts.
						if (j > tmp32_val4.sval && ( ((j - tmp32_val4.sval+1) % 16 ) == 0) && ((j + 1) < tmp32_val6.sval) ) {
							// --------see there is a new interrupt.
							IS_THERE_A_NEW_INTERRUPT
								RESTART_INSTRUCTION_SAVE_LOOP(j+1, tmp32_val6.sval)
#if DEBUG_LIMP == 1
								fprintf(stderr, "       interrupting   next start: %d, next end: %d \n", j + 1, tmp32_val6.sval);
#endif
							END_IS_THERE_A_NEW_INTERRUPT
						}
					}
					RESTART_INSTRUCTION_COMPLETE
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  E	LOMP -- Load Operand Map Image Into Hardware Map
			case 14:
				if (IS_PRIV_MODE) {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> PSW_SHIFT_OMAP) & 0x0007;	// contains map number. (OMAP)
					tmp32_val3.uval = (tmp32_val1.uval & 0x00001fff) << 8;					// abs page address (MIAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;					// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val6.sval = -1 * tmp32_val5.sval;									// positive length
#if DEBUG_LOMP == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, map: 0x%04x \n",
							junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start page:   0x%08x \n", tmp32_val3.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val6.uval);
#endif
					RESTART_INSTRUCTION_SETUP(program_counter, (instruction.all), 0, tmp32_val4.sval, tmp32_val4.sval + tmp32_val6.sval)
					RESTART_INSTRUCTION_GET_LOOP(tmp32_val4.sval, tmp32_val6.sval)

					for (j = tmp32_val4.sval; j < tmp32_val6.sval; j++) {
						//tmp32_val7.uval = tmp32_val4.uval + j;
						tmp32_val7.sval = j;
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val7.uval].all = GET_MEMORY_VALUE_ABS(tmp32_val3.uval | tmp32_val7.uval);
						// -------update count in source register.
						tmp16_val8.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
						tmp16_val9.uval = GET_REGISTER_VALUE(tmp16_val8.uval);
						// tmp16_val9.uval = (tmp16_val9.uval & 0xff00) | (((tmp16_val9.uval | 0xff00)+1) & 0x00ff);
						tmp16_val9.uval = ((tmp16_val9.uval + (SIMJ_U16)0x0100) & (SIMJ_U16)0xff00) | (((tmp16_val9.uval | (SIMJ_U16)0xff00) + (SIMJ_U16)1) & (SIMJ_U16)0x00ff);
						SET_REGISTER_VALUE(tmp16_val8.uval, tmp16_val9.uval);
#if DEBUG_LOMP == 1
						fprintf(stderr, "        map: %3d  entry: 0x%04x Value: 0x%04x \n", tmp16_val2.uval, tmp32_val7.uval, cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val7.uval].all);
#endif					
						// --------see if time to check interrupts.   not first time, not last time and every 16...
						if (j > tmp32_val4.sval && (((j - tmp32_val4.sval+1) % 16) == 0) && ((j+1) < tmp32_val6.sval) ) {
							// --------see there is a new interrupt.
							IS_THERE_A_NEW_INTERRUPT
								// -------- update register with new starting offset and length, then quit instruction
								//tmp32_val4.uval += j;		// starting offset.
								//tmp32_val5.sval += j;		// negative length
								//tmp32_val8.uval = tmp32_val1.uval & 0x0000ffff | ((tmp32_val5.uval << 16) & 0x00ff0000) | ((tmp32_val4.uval << 24) & 0xff000000);
								//SET_SOURCE_REGISTER_VALUE_DOUBLE(tmp32_val8.uval);
								//fprintf(stderr, "       interrupting   updated reg: 0x%08x, new off: 0x%08x, new -len: 0x%08x \n",
								//	tmp32_val8.uval, tmp32_val4.uval, tmp32_val5.uval);
								RESTART_INSTRUCTION_SAVE_LOOP(j + 1, tmp32_val6.sval)
#if DEBUG_LOMP == 1
								fprintf(stderr, "       interrupting   next start: %d, next end: %d \n", j + 1, tmp32_val6.sval);
#endif
							END_IS_THERE_A_NEW_INTERRUPT
						}
					}
					RESTART_INSTRUCTION_COMPLETE
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  F	SOMP -- Store Operand Map into Map Image
			case 15:
				if (IS_PRIV_MODE) {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> PSW_SHIFT_OMAP) & 0x0007;	// contains map number. (OMAP)
					tmp32_val3.uval = (tmp32_val1.uval & 0x00001fff) << 8;					// abs page address (MIAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;					// VP (starting offset in map and miap)
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val6.sval = -1 * tmp32_val5.sval;									// positive length
#if DEBUG_SOMP == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, map: 0x%04x \n",
						junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start page:   0x%08x \n", tmp32_val3.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val6.uval);
#endif
					RESTART_INSTRUCTION_SETUP(program_counter, (instruction.all), 0, tmp32_val4.sval, tmp32_val4.sval + tmp32_val6.sval)
					RESTART_INSTRUCTION_GET_LOOP(tmp32_val4.sval, tmp32_val6.sval)

					for (j = tmp32_val4.sval; j < tmp32_val6.sval; j++) {
						// tmp32_val7.uval = tmp32_val4.uval + j;
						tmp32_val7.sval = j;
						SET_MEMORY_VALUE_ABS(tmp32_val3.uval + tmp32_val7.uval, cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val7.uval].all );
						// -------update count in source register.
						tmp16_val8.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
						tmp16_val9.uval = GET_REGISTER_VALUE(tmp16_val8.uval);
						//tmp16_val9.uval = (tmp16_val9.uval & 0xff00) | (((tmp16_val9.uval | 0xff00)+1) & 0x00ff);
						tmp16_val9.uval = ((tmp16_val9.uval + (SIMJ_U16)0x0100) & (SIMJ_U16)0xff00) | (((tmp16_val9.uval | (SIMJ_U16)0xff00) + (SIMJ_U16)1) & (SIMJ_U16)0x00ff);
						SET_REGISTER_VALUE(tmp16_val8.uval, tmp16_val9.uval);
						// --------see if time to check interrupts.
						if (j > tmp32_val4.sval && (((j - tmp32_val4.sval+1) % 16) == 0) && ((j + 1) < tmp32_val6.sval)) {
							// --------see there is a new interrupt.
							IS_THERE_A_NEW_INTERRUPT
								// -------- update register with new starting offset and length, then quit instruction
								//tmp32_val4.uval += j;		// starting offset.
								//tmp32_val5.sval += j;		// negative length
								//tmp32_val8.uval = tmp32_val1.uval & 0x0000ffff | ((tmp32_val5.uval << 16) & 0x00ff0000) | ((tmp32_val4.uval << 24) & 0xff000000);
								//SET_SOURCE_REGISTER_VALUE_DOUBLE(tmp32_val8.uval);
								//fprintf(stderr, "       interrupting   updated reg: 0x%08x, new off: 0x%08x, new -len: 0x%08x \n",
								//	tmp32_val8.uval, tmp32_val4.uval, tmp32_val5.uval);
								RESTART_INSTRUCTION_SAVE_LOOP(j + 1, tmp32_val6.sval)
#if DEBUG_SOMP == 1
								fprintf(stderr, "       interrupting   next start: %d, next end: %d \n", j + 1, tmp32_val6.sval);
#endif
							END_IS_THERE_A_NEW_INTERRUPT
						}
					}
					SET_CC_N(false);
					SET_CC_Z(false);
					RESTART_INSTRUCTION_COMPLETE
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// --  A	UIT
			case 10:
				UNIMPLEMENTED_INSTRUCTION;
				break;
				// --  C	UIT
			case 12:
				UNIMPLEMENTED_INSTRUCTION;
				break;
			default:
				UNIMPLEMENTED_INSTRUCTION;
				break;
			}
			break;

		case  OP_LXR_SPR_SGP:			// 0x02
			switch (instruction.parts.dest_reg & 0x9) {

// #if SIMJ_SIM_CPU != 7830   -- the 7830 executes this instruction....
			case 0:		// LXR  -- Load Extended Memory Control Register  
					if (IS_PRIV_MODE) {
						cpu_extended_memory_ctrl_reg = GET_SOURCE_REGISTER_VALUE;		// doesn't really do anything on 7830
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						PRIV_INSTR_TRAP;
					}
					break;
// #endif
#if SIMJ_SIM_CPU == 7830
				case 1:		// SPR - Set Protect (IF)  (disable non-virtual protection emulation)  7830 only
					if (IS_PRIV_MODE) {
						cpu_nonvirt_prot_enabled = false;
#if DEBUG_SPR == 1
						fprintf(stderr, "  SPR - set mod II protection off\n");
#endif
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						PRIV_INSTR_TRAP;
					}
					break;

				case 8:		// SGP -- Set Global Protect (Set global protect boundary)  7830 only
					if (IS_PRIV_MODE) {
						cpu_nonvirt_prot_gbl_reg = GET_SOURCE_REGISTER_VALUE & 0xff00;
						cpu_nonvirt_prot_enabled = true;
#if DEBUG_SGP == 1
						fprintf(stderr, "  SGP - set global protect boundary: 0x%04x\n", cpu_nonvirt_prot_gbl_reg);
#endif
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						PRIV_INSTR_TRAP;
					}
					break;
#endif
				default:
					ILLEGAL_INSTRUCTION;
					break;
			}
			break;


		case  OP_RMPS_RMWS_SLP:	    // 0x03 -         
#if SIMJ_SIM_CPU == 7830
			if (instruction.parts.dest_reg == 0) {			// SLP -- Set Lower Protect Value
				if (IS_PRIV_MODE) {
					cpu_nonvirt_prot_enabled = true;
					cpu_nonvirt_prot_lowprot_reg = GET_SOURCE_REGISTER_VALUE & 0xff80;
#if DEBUG_SLP == 1
					fprintf(stderr, "  SLP - set lower protect boundary: 0x%04x\n", cpu_nonvirt_prot_lowprot_reg);
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
			}
			else if (instruction.parts.src_reg & 0x0001) {	//  RMWS -- Read Memory Word Status
// --------not 7830 CPU
#else
			if (instruction.parts.src_reg & 0x0001) {		//  RMWS -- Read Memory Word Status
#endif
				UNIMPLEMENTED_INSTRUCTION;
			}
			else {											// RMPS -- Read Memory Plane Status
				if (IS_PRIV_MODE) {
					tmp16_val4.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
					tmp16_val1.uval = GET_REGISTER_VALUE(tmp16_val4.uval);
					tmp16_val2.uval = GET_REGISTER_VALUE(tmp16_val4.uval+1);
					tmp16_val3.uval = memory_plane_RMPS(tmp16_val1.uval, tmp16_val2.uval);
					SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
#if DEBUG_RMPS == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%04x, reg or 1: 0x%04x , mem status: 0x%04x \n",
							junkxx, instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp16_val3.uval);
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
			}
			break;

		case  OP_WMS_SUP:			// 0x04 --  Set upper Protect Value or Write Memory Status
#if SIMJ_SIM_CPU == 7830
			if (instruction.parts.dest_reg == 0) {			// SUP -- Set Upper Protect Value
				if (IS_PRIV_MODE) {
					cpu_nonvirt_prot_enabled = true;
					cpu_nonvirt_prot_upprot_reg = GET_SOURCE_REGISTER_VALUE & 0xff80;
#if DEBUG_SUP == 1
					fprintf(stderr, "  SUP - set upper protect boundary: 0x%04x\n", cpu_nonvirt_prot_upprot_reg);
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
			}
			else {											// WMS -- Write Memory Status
#endif
				if (IS_PRIV_MODE) {
					tmp16_val4.uval = GET_SOURCE_REGISTER_NUMB & 0x000e;
					tmp16_val1.uval = GET_REGISTER_VALUE(tmp16_val4.uval);
					tmp16_val2.uval = GET_REGISTER_VALUE(tmp16_val4.uval + 1);
					tmp16_val3.uval = GET_DESTINATION_REGISTER_VALUE;
					memory_plane_WMPS(tmp16_val1.uval, tmp16_val2.uval, tmp16_val3.uval);
#if DEBUG_WMS == 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%04x, reg or 1: 0x%04x , mem status: 0x%04x \n",
							junkxx, instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp16_val3.uval);
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
#if SIMJ_SIM_CPU == 7830
			}
#endif
			break;

		case  OP_DMPI:			// 0x05  --  Initialize Direct Memory Processor
			if (IS_PRIV_MODE) {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				SIMJ_U16 tmp_dmp = ((tmp32_val1.uval & 0x3f000000) >> 24);
				iop_vdmp_miap_page[tmp_dmp] = (tmp32_val1.uval & 0x00001fff);
				iop_vdmp_miap_length[tmp_dmp] = (~((SIMJ_U16)((tmp32_val1.uval & 0x00ff0000) >> 16) | 0xff00))+1;
#if DEBUG_DMPI == 1
				fprintf(stderr, "\n pc: 0x%04x  DMPI: 0x%04x  Reg Value: 0x%08x, dmp: 0x%08x, miap: 0x%08x, map len: 0x%08x\n", 
					program_counter, instruction.all, tmp32_val1.uval, 
					tmp_dmp, iop_vdmp_miap_page[tmp_dmp], iop_vdmp_miap_length[tmp_dmp] );
#endif
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				PRIV_INSTR_TRAP;
			}
			break;

		// 0x06 MMRB --  Move Memory File to RegisterBlock Section of Context
		// The contents of the general purpose register file(GRF) are
		//	unaffected by this instruction.If an update of the current
		//	GRF is required, execution of this instruction must be followed
		//	by execution of an SCRB instruction.
		case  OP_MMRB:			// 0x06  --  Move Memory File to RegisterBlock Section of Context
			if (IS_PRIV_MODE) {
				// -------- get dest register value - holding register block value.
				tmp16_val1.uval = (GET_DESTINATION_REGISTER_VALUE >> 8) & 0x000f;
				// -------- get memory address in src register value
				tmp16_val2.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
#if DEBUG_MMRB == 1
				fprintf(stderr, "\n   MMRB virt mem 0x%04x, to reg block %d  \n", tmp16_val2.uval, tmp16_val1.uval );
#endif
				// -------- copy mem (x) style to reg 1-15 in particular block
				// TODO: MMRB Make register block copy more efficient.
				for (j = 1; j < 16; j++) {
					GET_MEMORY_VALUE_OM(cpu_register_blocks[tmp16_val1.uval].reg16[j], tmp16_val2.uval + j - 1);
				}
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				PRIV_INSTR_TRAP;
			}
			break;

		case  OP_MRBM:			// 0x07  --  Move Register-Block Section of Context File to Memory
			if (IS_PRIV_MODE) {
				// TODO: MRBM Current registers may not match current register block.
				// -------- get dest register value - holding register block value.
				tmp16_val1.uval = (GET_DESTINATION_REGISTER_VALUE >> 8) & 0x000f;
				// -------- get memory address in src register value
				tmp16_val2.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
#if DEBUG_MRBM >= 2
				fprintf(stderr, "\n   MRBM reg block %d to virt mem 0x%04x \n", tmp16_val1.uval, tmp16_val2.uval);
#endif
				// -------- if requesting the current register block, then save if first.....
				if (cpu_register_current_block == tmp16_val1.uval) {
#if DEBUG_MRBM >= 1
					fprintf(stderr, "\n   MRBM copy current to block - reg block %d to virt mem 0x%04x \n", tmp16_val1.uval, tmp16_val2.uval);
#endif
					cpu_copy_register_current_to_block();
				}
				// -------- copy reg 1-15 block to mem (x) style
				// -------- TODO: Make register block copy more efficient.
				for (j = 1; j < 16; j++) {
					SET_MEMORY_VALUE_OM(tmp16_val2.uval + j - 1, cpu_register_blocks[tmp16_val1.uval].reg16[j]);
				}
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				PRIV_INSTR_TRAP;
			}
			break;

		case  OP_MBR:			// 0x08 -- Move byte right register to register
			tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.uval = (SIMJ_U16)((tmp16_val2.uval >> 8) & 0x00ff);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_CHAR(tmp16_val1.uval);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_MBL:			// 0x09 -- Move byte left register to register
			tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.uval = (SIMJ_U16)((tmp16_val2.uval << 8) & 0xff00);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_O(false);
			SET_CC_C(false);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_IBR:			// 0x0a  --  Interchange Bytes Register to Register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val2.uval = (SIMJ_U16)(((tmp16_val1.uval << 8) & 0xff00) | ((tmp16_val1.uval >> 8) & 0x00ff));
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_CHAR(tmp16_val2.uval & 0x00ff);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_MUR:			// 0x0b -- Move upper byte register to register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val2.uval = (SIMJ_U16)(tmp16_val1.uval & 0xff00);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val2));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
			SET_CC_O(false);
			SET_CC_C(false);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_MLR:			// 0x0c -- Move lower byte register to register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val2.uval = (SIMJ_U16)(tmp16_val1.uval & 0x00ff);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
			SET_CC_CHAR(tmp16_val2.uval);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_TOR:			// 0x0d  --  Transfer One's Complement Register to Register
			tmp16_val1.uval = ~GET_SOURCE_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_AUG0E:			// 0x0e
			tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
			switch (tmp_instr_src) {

				// -------- 0	TRO  -- Transfer and Reset Overflow Status History      
			case 0:
				SET_DESTINATION_REGISTER_VALUE((cpu_overflow_hist ? 0x8000 : 0));
				cpu_overflow_hist = false;
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 1	LCPS  -- Load Register with Current Program Status Register of PSD   
			case 1:
				SET_DESTINATION_REGISTER_VALUE(cpu_get_current_PSW().all);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 2	LCPR  -- Load Register with Current Program Register of PSD   
			case 2:
				SET_DESTINATION_REGISTER_VALUE(program_counter);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 3	LCCC  -- Load Register with Current Condition Code of PSD   
			case 3:
				SET_DESTINATION_REGISTER_VALUE((cpu_get_current_PSW().all & 0x000f));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 4	LCIA  -- Load Register with Current interrupt Active Latches    
			case 4:
				SET_DESTINATION_REGISTER_VALUE(cpu_interrupt_active);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 5	LCIE  -- Load Register with Current interrupt Enable Latches     
			case 5:
				SET_DESTINATION_REGISTER_VALUE(cpu_interrupt_enabled);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 6	LCIR  -- Load Register with Current interrupt Request Latches     
			case 6:
				SET_DESTINATION_REGISTER_VALUE(cpu_interrupt_request);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// -------- 7	MBVV  -- Move Virtual Block to Virtual Block       
			case 7:
				tmp16_val1.uval = GET_DESTINATION_REGISTER_NUMB & 0xC;
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp16_val1.uval);		// implied neg transfer
				tmp16_val3.uval = GET_REGISTER_VALUE(tmp16_val1.uval|1);	// source virt address
				tmp16_val4.uval = GET_REGISTER_VALUE(tmp16_val1.uval|2);	// destination virt address
				tmp32_val2.uval = (SIMJ_U32)0xffff0000 | (tmp16_val2.uval & (SIMJ_U32)0x0000ffff); // make neg tranfer 32 bit
				tmp32_val2.sval *= -1;										// positive tranfer count.
				tmp32_val5.uval = 0;		// starting loop index
#if DEBUG_MBVV > 0
				util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				fprintf(stderr, " pc: 0x%04x, %s inst: 0x%04x ",
					program_counter, junkxx, instruction.all);
				fprintf(stderr, " impl neg xfer:0x%04x, src addr:0x%04x, dst addr:0x%04x, cnt:0x%08x\n",
					tmp16_val2.uval, tmp16_val3.uval, tmp16_val4.uval, tmp32_val2.uval);
#endif

				RESTART_INSTRUCTION_SETUP(program_counter, (instruction.all), 0, tmp32_val5.uval, tmp32_val2.sval)
				RESTART_INSTRUCTION_GET_LOOP(tmp32_val5.uval, tmp32_val2.sval)

				// --------since the src and dest addresses are updated and the transfer count is too
				// --------the start should always be zero...
				for (j = tmp32_val5.uval; j < tmp32_val2.sval; j++ ) {
					GET_MEMORY_VALUE_OM(tmp16_val6.uval, (tmp16_val3.uval + (SIMJ_U16)j));
					SET_MEMORY_VALUE_OM( (tmp16_val4.uval + (SIMJ_U16)j), tmp16_val6.uval);
#if DEBUG_MBVV > 0
					fprintf(stderr, "       src addr:0x%04x, dst addr:0x%04x, val:0x%04x \n",
						(tmp16_val3.uval + (SIMJ_U16)j), (tmp16_val4.uval + (SIMJ_U16)j),
						tmp16_val6.uval );
#endif
					// --------see if time to check interrupts.
					if (j > 0 && (((j+1) % 16) == 0) && ((j + 1) < tmp32_val2.sval)) {
					// if (j > 0 && (((j+1) % 8) == 0) && ((j + 1) < tmp32_val2.sval)) {
						// --------see there is a new interrupt.
						IS_THERE_A_NEW_INTERRUPT
							// --------update transfer count and src and dest addresses.
							tmp16_val2.uval = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp16_val2.uval += (j + 1);
							SET_REGISTER_VALUE(tmp16_val1.uval, tmp16_val2.uval);
							// --------src address
							tmp16_val3.uval = GET_REGISTER_VALUE((tmp16_val1.uval| (SIMJ_U16)1));
							tmp16_val3.uval += (j + 1);
							SET_REGISTER_VALUE((tmp16_val1.uval|(SIMJ_U16)(SIMJ_U16)1), tmp16_val3.uval);
							// --------dest address
							tmp16_val4.uval = GET_REGISTER_VALUE((tmp16_val1.uval| (SIMJ_U16)2));
							tmp16_val4.uval += (j + 1);
							SET_REGISTER_VALUE((tmp16_val1.uval| (SIMJ_U16)2), tmp16_val4.uval);
							// -------- update register with new starting offset and length, then quit instruction
							RESTART_INSTRUCTION_SAVE_LOOP(0, (tmp32_val2.sval - (SIMJ_S32)(j + 1)))
#if DEBUG_MBVV >= 1
							fprintf(stderr, "       interrupting   next start: %d, next end: %d \n", 0, tmp32_val2.sval-16);
#endif
						END_IS_THERE_A_NEW_INTERRUPT
					}
				}
				// --------update transfer count and src and dest addresses.
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp16_val1.uval);
				tmp16_val2.uval += j;
				SET_REGISTER_VALUE(tmp16_val1.uval, tmp16_val2.uval);
				// --------src address
				tmp16_val3.uval = GET_REGISTER_VALUE((tmp16_val1.uval | (SIMJ_U16)1));
				tmp16_val3.uval += j;
				SET_REGISTER_VALUE((tmp16_val1.uval | (SIMJ_U16)(SIMJ_U16)1), tmp16_val3.uval);
				// --------dest address
				tmp16_val4.uval = GET_REGISTER_VALUE((tmp16_val1.uval | (SIMJ_U16)2));
				tmp16_val4.uval += j;
				SET_REGISTER_VALUE((tmp16_val1.uval | (SIMJ_U16)2), tmp16_val4.uval);
				// --------set condition codes
				SET_CC_N(false);
				SET_CC_Z(true);
				RESTART_INSTRUCTION_COMPLETE
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
#if DEBUG_MBVV >= 1
				fprintf(stderr, "       MBVV done\n");
#endif
				break;

				// -------- 8	MBVE  -- Move Block from Virtual to Extended Memory     
			case 8:
#if SIMJ_SIM_CPU == 7830
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;
#else
				// TODO: Finish MBVE
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;
#endif
				break;

				// -------- 9	MBEV  -- Move Block from Extended to Virtual Memory     
			case 9:
#if SIMJ_SIM_CPU == 7830
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
#else
				// TODO: Finish MBEV
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;
			}
				else {
					PRIV_INSTR_TRAP;
				}
#endif
				break;

				// -------- A	MPES  -- Multiply Immediate with Extended Sign       
			case 10:
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				if ISREGNUM_QUAD(tmp_instr_dest) {
					tmp32_val2.uval = GET_REGISTER_VALUE_DOUBLE((instruction.parts.dest_reg | 0x02) >> 1);
				}
				else {
					tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				}
				tmp64_val1.sval = tmp16_val1.sval;		// sign extend to 64 bits.
				tmp64_val2.sval = tmp32_val2.sval;
				tmp64_val3.sval = tmp64_val1.sval * tmp64_val2.sval;
				if ISREGNUM_QUAD(tmp_instr_dest) {
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
				}
				else {
					// TODO: MPES should sign of truncated value forced to be correct?
					tmp32_val3.uval = (SIMJ_U32)(tmp64_val3.uval & 0x00000000ffffffff);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				}
				SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
				SET_CC_N(ISVAL64_NEG(tmp64_val3));
				SET_CC_O(false);
				SET_CC_C(((tmp64_val3.uval & 0xffffffff80000000) == 0xffffffff80000000) || ((tmp64_val3.uval & 0xffffffff80000000) == 0x0000000000000000));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// -------- B	DVES  -- Divide lmmediate with Extended Sign 
				// -------- NOTE the result of this is 2 32 bit numbers....      
			case 11:
				tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp64_val2.sval = tmp16_val2.sval;		// sign extend to 64 bits.
				if (ISVAL64_ZERO(tmp64_val2)) {
					tmp64_val3.uval = 0;
					tmp64_val4.uval = 0;
					SET_CC_C(true);
				}
				else {
					tmp64_val3.sval = tmp64_val1.sval / tmp64_val2.sval;
					tmp64_val4.sval = tmp64_val1.sval % tmp64_val2.sval;
					SET_CC_C(false);
				}
				tmp32_val3.uval = (SIMJ_U32)(tmp64_val3.uval & 0x00000000ffffffff);		// low 32 bits of quotient.
				tmp32_val4.uval = (SIMJ_U32)(tmp64_val4.uval & 0x00000000ffffffff);		// low 32 bits of remainder.
				SET_REGISTER_VALUE_DOUBLE(GET_DESTINATION_REGISTER_NUMB_DOUBLE, tmp32_val4.uval);		// remainder
				SET_REGISTER_VALUE_DOUBLE(GET_DESTINATION_REGISTER_NUMB_DOUBLE + 1, tmp32_val3.uval);		// quotient
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O(ISVAL32_ZERO(tmp32_val2) || !(((tmp64_val3.uval & 0xffffffff80000000) == 0) || ((tmp64_val3.uval & 0xffffffff80000000) == 0xffffffff80000000)));	// not a 32 bit result
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			// -------- C	RDI  -- Read Internal Registers         
			// read internal register -- as address by CPU commands, not front panel.
			// int_reg_addr
			// [ 0 0 0 0 ] [ GRF ] [IOP, MBC, CTXT, MAP] [ EAU, CPU]
			//
			// front panel format
			// [ 0 0 0 0 ] [ MAP, IOP, MBC, CTXT] [ EAU, CPU] [ GRF ] 
			case 12:
				if (IS_PRIV_MODE) {
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					tmp16_val2.uval = cpu_read_internal_register(cpu_convert_to_front_panel_address(tmp16_val1.uval));
#if DEBUG_RDI > 0
					fprintf(stderr, " RDI -- dest reg 0x%04x, result 0x%04x \n", tmp16_val1.uval, tmp16_val2.uval); 
#endif
					SET_REGISTER_VALUE(instruction.parts.dest_reg | 0x01, tmp16_val2.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

				// -------- D	WIR  -- Write Internal Register         
			case 13:
				if (IS_PRIV_MODE) {
#if DEBUG_WIR > 0
					fprintf(stderr, " WIR -- NOT COMPLETE \n");
#endif
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

				// -------- E	BRM  -- Branch to Microroutine Immediate        
			case 14:
				if (IS_PRIV_MODE) {
#if DEBUG_BRM > 0
					fprintf(stderr, " BRM -- NOT COMPLETE \n");
#endif
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

				// -------- F	BRMI  -- Branch to Microroutine Immediate        
			case 15:
				if (IS_PRIV_MODE) {
#if DEBUG_BRMI > 0
					fprintf(stderr, " BRMI -- NOT COMPLETE \n");
#endif
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_LRS:			// 0x0f  --  Left Rotate Single-Register to Register
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp16_val1.uval = (tmp16_val1.uval << 1) | (tmp16_val1.uval >> 15);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_C(tmp16_val1.uval & 0x0001);
			SET_CC_O(false);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

			// 0x1X -- reserved for decimal arithmetic -- case default will cause unimplemented instruction trap

		case  OP_MPR:			// 0x20  --  Multiply Register By Register 
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest | 0x01);
			}
			else {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
			}
			tmp32_val1.sval = tmp16_val1.sval;
			tmp32_val2.sval = tmp16_val2.sval;
			tmp32_val3.sval = tmp32_val1.sval * tmp32_val2.sval;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
			}
			else {
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			}
			SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
			SET_CC_N(ISVAL32_NEG(tmp32_val3));
			SET_CC_O(false);
			SET_CC_C( ((tmp32_val3.uval & 0xffff8000) == 0xffff8000) || ((tmp32_val3.uval & 0xffff8000) == 0x00000000) );
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

			case  OP_DVR:			// 0x21  --  Divide Register By Register
				if ( ISREGNUM_DOUBLE(GET_DESTINATION_REGISTER_NUMB) ) {
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
					tmp32_val2.sval = tmp16_val2.sval;		// sign extend
					if (tmp32_val2.sval != 0) {		// dont divide by zero.
						tmp32_val3.sval = tmp32_val1.sval / tmp32_val2.sval;	// quotient
						tmp32_val6.sval = tmp32_val1.sval % tmp32_val2.sval;	// remainder
					}
					else {
						tmp32_val3.uval = 0;
						tmp32_val6.uval = 0;
					}
					tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);		// low 16 bits of quotient.
					tmp16_val6.uval = (SIMJ_U16)(tmp32_val6.uval & 0x0000ffff);		// low 16 bits of remainder.
					tmp_instr_dest = (GET_DESTINATION_REGISTER_NUMB & 0x00e);		// must be even.
					SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val6.uval);		// remainder
					SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);		// quotient
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					// TODO: DVR OR IN O if divide by 0
					SET_CC_O(   !( ((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000) ) );	// not a 16 bit result
					SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					ILLEGAL_INSTRUCTION;
				}
				break;

			// TODO: DAR This is the same as ADRD ????
			case  OP_DAR:			// 0x22  --  This might be obsolete....
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
				tmp32_val3.uval = tmp32_val1.uval + tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_REX:			// 0x23  --  Request Executive Service
				// -------- Request ownership of the resource.
				TAKE_RESOURCE( ResourceInterruptRequest) ;
				cpu_interrupt_request |= CPU_INTR_UIT;
				cpu_interrupt_entry_cond_codes[CPU_INTR_UIT_num] = PSW_MASK_CC_Z;
				// -------- Release ownership of the resource.
				GIVE_RESOURCE( ResourceInterruptRequest );
				break;

			case  OP_CAR:			// 0x24  --  CAR  --  Clear Active and Return        
				// --------must be priv to execute this.
				if (IS_PRIV_MODE) {
					// --------find highest active interrupt
					// find the active interrupt 0 - 15, 16 if nothing active.
					GET_HIGHEST_ACTIVE_INT(old_int)
#if DEBUG_CAR >= 1
						fprintf(stderr, " \n");
#endif
#if DEBUG_CAR >= 2
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " pc: 0x%04x, %s inst: 0x%04x, active: 0x%04x, request: 0x%04x\n",
						program_counter, junkxx, instruction.all,
						cpu_interrupt_active, cpu_interrupt_request);
					fprintf(stderr, "        active = %d  alt active = %d\n", old_int, cpu_interrupt_active_num);
					if (old_int != cpu_interrupt_active_num) {
						fprintf(stderr, "          **** Active and ALt Active different *****\n");
					}
#endif
#if DEBUG_CAR >= 1
					char car_junk_psw_char[200] = "";
					format_psw(car_junk_psw_char, sizeof(car_junk_psw_char), cpu_get_current_PSW());
					fprintf(stderr, " CAR old - active: 0x%04x, PSW %s, highest act %d \n",
						cpu_interrupt_active, car_junk_psw_char, old_int );
#endif
					// --------get return process_status_double_word from dedicated memory location or register
					tmp16_val1.uval = (instruction.parts.src_reg & 0x000e);
					// -------- an interrupt was active...
					if (old_int < 16) {
						// --------set return new PSW and PC
						// --------get return pc and status double word from dedicated memory location
						if (tmp16_val1.uval == 0) {
							program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_RETURN_PC + (old_int * 2));
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_RETURN_PS + (old_int * 2));
#if DEBUG_CAR >= 1
							fprintf(stderr, " CAR  normal return  pc loc: 0x%04x, psw loc: 0x%04x\n", 
										CPU_INTR_BASE_RETURN_PC + (old_int * 2), CPU_INTR_BASE_RETURN_PS + (old_int * 2));
#endif
#if DEBUG_CAR >= 3
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						// -------- get return pc and psw from register.
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval | 1);
#if DEBUG_CAR >= 1
							fprintf(stderr, " CAR  register return  reg numb %d\n", tmp16_val1.uval);
#endif							
#if DEBUG_CAR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						cpu_set_current_PSW(tmp_PSW);
						// --------so we don't set the current interrupt active again
						// --------temporarily reset the reqeust bit...
						SIMJ_U16 car_save_request = cpu_interrupt_request;
						cpu_interrupt_request &= bitnot[old_int];
						// --------clear active for that interrupt
						CLEAR_ACTIVE_INT(old_int)
						// --------restore request
						cpu_interrupt_request = car_save_request;
#if DEBUG_CAR >= 2
						fprintf(stderr, "       new active: 0x%04x\n", cpu_interrupt_active);
#endif
					}
					// -------- If no interrupt is active when the CAR instruction is executed, the dedicated 
					// -------- locations O and 1 are used to restore the PSD if register Rs = 0, otherwise
					// -------- return through the registers.
					// -------- For 32/85 behaviour is different -- A sys protect trap is issued.
					// -------- for the II/40 and II/75 (and maybe revised 7860 and 7830 PS from 0x20 is used
					// -------- instead of 1   For now use 0x20
					// ---------The II/45 manual of 11/1983 says to use PR 0, PS #20
					// ---------The II/75 manual of 07/1985 says to use PR 0, PS #20
					// TODO: CAR - resolve the return PS for no active int and no register return. 0x20 for now
					else {
						// --------set return new PSW and PC
						if (tmp16_val1.uval == 0) {
							// --------get current process pc and status double word from dedicated memory location
							program_counter = GET_MEMORY_VALUE_ABS(0);
							//tmp_PSW.all = GET_MEMORY_VALUE_ABS(1);
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0020);
#if DEBUG_CAR >= 1
							fprintf(stderr, " CAR NO INT ACTIVE normal return  pc loc: 0x%04x, psw loc: 0x%04x\n",
								0, 0x0020);
#endif
#if DEBUG_CAR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						// --------return through register values.
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval | 1);
#if DEBUG_CAR >= 1
							fprintf(stderr, " CAR NO INT ACTIVE register return  reg numb %d\n", tmp16_val1.uval);
#endif
#if DEBUG_CAR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						// -------- set new PSW
						cpu_set_current_PSW(tmp_PSW);
						// -------- ensure active mask and active number are set..
						cpu_interrupt_active_mask = mask[15];
						cpu_interrupt_active_num = 16;

#if DEBUG_CAR >= 2
						fprintf(stderr, "       nothing was active!!!\n" );
#endif
					}
					// -------- allow one instruction to execute before the next interrupt.
					skip_interrupt_determination = true;
#if DEBUG_CAR >= 2
					fprintf(stderr, "       new pc 0x%04x, new active: 0x%04x, request: 0x%04x\n",
							program_counter, cpu_interrupt_active, cpu_interrupt_request);
					disp_psw(stderr, cpu_get_current_PSW());
#endif
#if DEBUG_CAR >= 1
					// char car_junk_psw_char[200] = "";   defined above.
					car_junk_psw_char[0] = 0;
					format_psw(car_junk_psw_char, sizeof(car_junk_psw_char), cpu_get_current_PSW());
					fprintf(stderr, " CAR new - active: 0x%04x, PSW %s \n",
						cpu_interrupt_active, car_junk_psw_char );
#endif
				}
			
				// --------not priviledged.
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_CIR:			// 0x25  --  CIR  --  Clear interrupt and Return        
				if (IS_PRIV_MODE) {
					tmp16_val1.uval = instruction.parts.src_reg & 0x000e;
					// --------find highest active interrupt
					GET_HIGHEST_ACTIVE_INT( old_int )
#if DEBUG_CIR >= 1
					char cir_junk_psw_char[200] = "";
					format_psw(cir_junk_psw_char, sizeof(cir_junk_psw_char), cpu_get_current_PSW());
					fprintf(stderr, "\n   CIR old - active: 0x%04x, PSW %s, highest act %d \n",
					cpu_interrupt_active, cir_junk_psw_char, old_int);
#endif
#if DEBUG_CIR >= 2
					if (old_int != 6 && old_int != 4 && old_int != 12) {	// no debug on clock release...
						util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
						fprintf(stderr, " pc: 0x%04x, %s inst: 0x%04x, active: 0x%04x, request: 0x%04x\n",
								program_counter, junkxx, instruction.all,
								cpu_interrupt_active, cpu_interrupt_request);
						fprintf(stderr, "        active = %d\n", old_int);
					}
#endif				
					// --------get return process_status_double_word from dedicated memory location or register
					if (old_int < 16) {
						// --------set return new PSW and PC
						// --------get return process pc and status double word from dedicated memory location
						if (tmp16_val1.uval == 0) {
							program_counter = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_RETURN_PC + (old_int * 2));
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(CPU_INTR_BASE_RETURN_PS + (old_int * 2));
#if DEBUG_CIR >= 1
							if (old_int != 6 && old_int != 4 && old_int != 12) {	// no debug on clock release...
								fprintf(stderr, "   CIR  normal return  pc loc: 0x%04x, psw loc: 0x%04x\n",
									CPU_INTR_BASE_RETURN_PC + (old_int * 2), CPU_INTR_BASE_RETURN_PS + (old_int * 2));
							}
#endif
#if DEBUG_CIR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						// -------- get return pc and psw from register.
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval & 0xe);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval | 1);
#if DEBUG_CIR >= 1
							if (old_int != 6 && old_int != 4 && old_int != 12) {	// no debug on clock release...
								fprintf(stderr, "   CAR  register return  reg numb %d\n", tmp16_val1.uval);
							}
#endif							
#if DEBUG_CIR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif							
						}
						// --------set new PSW.
						cpu_set_current_PSW(tmp_PSW);
						// --------clear active and request for that interrupt
						CLEAR_ACTIVE_INT(old_int)
						CLEAR_REQUEST_INT(old_int)
#if DEBUG_CIR >= 2
						if (old_int != 6 && old_int != 4 && old_int != 12) {	// no debug on clock release...
							fprintf(stderr, "        new active: 0x%04x  request:  0x%04x\n", cpu_interrupt_active, cpu_interrupt_request);
						}
#endif
					}
					// -------- If no interrupt is active when the CIR instruction is executed, the dedicated 
					// -------- locations 0 (PR) and ?? (PS) are used to restore the PSD if register Rs = 0.
					// ---------The II/45 manual of 11/1983 says to use PR 0, PS 1
					// ---------The II/75 manual of 07/1985 says to use PR 0, PS #20
					// -------- For 32/85 behaviour is different -- A sys protect trap is issued.
					else {
						// --------set return new PSW and PC
						if ( tmp16_val1.uval == 0) {
							// --------get current process pc and status double word from dedicated memory location
							program_counter = GET_MEMORY_VALUE_ABS(0);
							// tmp_PSW.all = GET_MEMORY_VALUE_ABS(1);
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0020);
#if DEBUG_CIR >= 1
							fprintf(stderr, "   CIR NO INT ACTIVE normal return  pc loc: 0x%04x, psw loc: 0x%04x\n",
												0, 0x0020);
#endif
#if DEBUG_CIR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval & 0xe);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval | 1);
#if DEBUG_CIR >= 1
							fprintf(stderr, "   CAR NO INT ACTIVE register return  reg numb %d\n", tmp16_val1.uval);
#endif
#if DEBUG_CIR >= 2
							fprintf(stderr, " -- old PSW - ");
							disp_psw(stderr, cpu_get_current_PSW());
							fprintf(stderr, " -- new PSW - ");
							disp_psw(stderr, tmp_PSW);
#endif
						}
						cpu_set_current_PSW(tmp_PSW);
						// TODO: CIR if nothing active is there a request or active to clear.  NO?
#if DEBUG_CIR >= 1
						fprintf(stderr, "        nothing was active branch\n");
#endif
					}
					// -------- allow one instruction to execute before the next interrupt.
					// TODO: CIR allow one instruction to be processed before new interrupt.  For now NO.
					// --DOC DOESNT SAY DO THIS-- skip_interrupt_determination = true;
					// --------DEBUG --- DUPLICATE MESSAGES - DONT NEED.
#if DEBUG_CIR >= 1
					cir_junk_psw_char[0] = 0;
					format_psw(cir_junk_psw_char, sizeof(cir_junk_psw_char), cpu_get_current_PSW());
					fprintf(stderr, "   CIR new - active: 0x%04x, request: 0x%04x, PSW %s \n",
						cpu_interrupt_active, cpu_interrupt_request, car_junk_psw_char);
#endif
#if DEBUG_CIR >= 2
					fprintf(stderr, "     new pc 0x%04x, new active: 0x%04x, request: 0x%04x\n",
						program_counter, cpu_interrupt_active, cpu_interrupt_request);
						disp_psw(stderr, cpu_get_current_PSW());
#endif
				}
				// --------not priviledged. set trap.
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_SIA_SIE_SIR:		// 0x26
				tmp_instr_dest = instruction.parts.dest_reg;		// instruction augment
				tmp16_val1.uval = bit[GET_SOURCE_REGISTER_NUMB];	// interrupt bit to deal with.
				switch (tmp_instr_dest) {

					// SIA  --  Set interrupt Active         
					case 0:
						if (IS_PRIV_MODE) {
							cpu_interrupt_active |= (tmp16_val1.uval & SIA_ALLOWED);	// level 0 is not allowed to be set active -- according to docs
																						// but diagnostics expect it to be set active !.
							// --------if nothing is active, set mask to allow all new interrupts
							if (cpu_interrupt_active == 0) {
								cpu_interrupt_active_mask = mask[15];
							}
							// --------set mask to allow only higher interrupt requests.
							else {
								cpu_interrupt_active_mask = mask[cpu_find_bit(cpu_interrupt_active)];
							}
							// --------DEBUG
							// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							// fprintf(stderr, " % s inst : 0x%04x  level: %02d active: 0x%04x\n", junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_active);
							// --------END DEBUG
							SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						}
						else {
							PRIV_INSTR_TRAP;
						}
						break;

					// SIE  --  Set interrupt Enable         
					case 4:
						if (IS_PRIV_MODE) {
							cpu_interrupt_enabled |= (tmp16_val1.uval & SIE_ALLOWED);
							// -------- if we just enabled SI or DI see if there is a back log that needs to set the request.
							// -------- This really shouldn't be needed, but DIs are getting missed....
							if ((tmp16_val1.uval & SIE_ALLOWED) == CPU_INTR_DI) {
								// -------- Request ownership of the resource.
								TAKE_RESOURCE( ResourceInterruptRequest );
								if (cpu_interrupt_DI_total_proc_count != cpu_interrupt_DI_total_request_count) {
									cpu_interrupt_request |= CPU_INTR_DI;
									fprintf(stderr, " SIE needed to re-request outstanding DI.\n");
								}
								// -------- Release ownership of the resource.
								GIVE_RESOURCE( ResourceInterruptRequest );
							}
							else if ((tmp16_val1.uval & SIE_ALLOWED) == CPU_INTR_SI) {
								// -------- Request ownership of the resource.
								TAKE_RESOURCE( ResourceInterruptRequest );
								if (cpu_interrupt_SI_total_proc_count != cpu_interrupt_SI_total_request_count) {
									cpu_interrupt_request |= CPU_INTR_SI;
									fprintf(stderr, " SIE needed to re-request outstanding SI.\n");
								}
								// -------- Release ownership of the resource.
								GIVE_RESOURCE( ResourceInterruptRequest );
							}
							// TOD: FIX THIS - should not need to poke clock int when enabled.  Likely throttling issue.
							// -------- Poke clock interrupt if enabled.
							// else if ((tmp16_val1.uval & SIE_ALLOWED) == CPU_INTR_clock) {
							// 	TAKE_RESOURCE( ResourceInterruptRequest );
							// 	// -------- Release ownership of the resource.
							// 	cpu_interrupt_request |= CPU_INTR_clock;
							// 	GIVE_RESOURCE( ResourceInterruptRequest );
							// }
							// --------DEBUG
							// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							// fprintf(stderr, " % s inst : 0x%04x  level: %02d enable: 0x%04x\n", junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_enabled);
							// --------END DEBUG
							SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						}
						else {
							PRIV_INSTR_TRAP;
						}
						break;

					// SIR  --  Set interrupt Request         
					case 8:
						if (IS_PRIV_MODE) {
							// -------- Request ownership of the resource.
							TAKE_RESOURCE( ResourceInterruptRequest );
							cpu_interrupt_request |= ( tmp16_val1.uval & SIR_ALLOWED );
							// -------- Release ownership of the resource.
							GIVE_RESOURCE( ResourceInterruptRequest );
							// --------DEBUG
							// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							// fprintf(stderr, " % s inst : 0x%04x  level: %02d request: 0x%04x\n", junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_request);
							// --------END DEBUG
							SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						}
						else {
							PRIV_INSTR_TRAP;
						}
						break;

					default:
						ILLEGAL_INSTRUCTION;
						break;
				}
				break;

			case  OP_RIA_RIE_RIR:		//  0x27
				tmp_instr_dest = instruction.parts.dest_reg;
				tmp16_val1.uval = bitnot[GET_SOURCE_REGISTER_NUMB];
				switch (tmp_instr_dest) {

					// RIA  --  Reset interrupt Active         
					case 0:
						if (IS_PRIV_MODE) {
#if DEBUG_RIA > 0
							util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							fprintf(stderr, " % s inst: 0x%04x  level to reset: %02d was active: 0x%04x", 
									junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_active);
#endif
							cpu_interrupt_active &= ( tmp16_val1.uval | RIA_ALLOWED_NOT);
							if (cpu_interrupt_active == 0) {
								cpu_interrupt_active_mask = mask[15];
							}
							else {
								cpu_interrupt_active_mask = mask[cpu_find_bit(cpu_interrupt_active)];
							}
							// TODO: RIA should this allow one instruction to execute?? For now NO
							// skip_interrupt_determination = true;
#if DEBUG_RIA > 0
							fprintf(stderr, "  new active: 0x%04x \n", cpu_interrupt_active);
#endif
							SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						}
						else {
							PRIV_INSTR_TRAP;
						}
						break;

					// RIE  --  Reset interrupt Enable         
					case 4:
						if (IS_PRIV_MODE) {
							cpu_interrupt_enabled &= ( tmp16_val1.uval | RIE_ALLOWED_NOT);
							// TODO: RIE are current requests removed too ??
							// --------Undocumented, it looks like requests may also be reset...if not active...
							// if ((cpu_interrupt_active & bit[GET_SOURCE_REGISTER_NUMB]) == 0) {
							// 	// -------- Request ownership of the resource.
							// 	TAKE_RESOURCE( ResourceInterruptRequest );
							// 	cpu_interrupt_request &= (tmp16_val1.uval | RIR_ALLOWED_NOT);
							// 	// TODO: RIE make the si/di stuf more efficient.
							// 	if ((cpu_interrupt_DI_total_proc_count != cpu_interrupt_DI_total_request_count) && ((cpu_interrupt_enabled & CPU_INTR_DI) != 0)) {
							// 		cpu_interrupt_request |= CPU_INTR_DI;
							// 		//fprintf(stderr, " RIR needed to re-request outstanding DI.\n");
							// 	}
							// 	if ((cpu_interrupt_SI_total_proc_count != cpu_interrupt_SI_total_request_count) && ((cpu_interrupt_enabled & CPU_INTR_SI) != 0)) {
							// 		cpu_interrupt_request |= CPU_INTR_SI;
							// 		//fprintf(stderr, " RIR needed to re-request outstanding SI.\n");
							// 	}
							// 	// -------- Release ownership of the resource.
							// 	GIVE_RESOURCE( ResourceInterruptRequest );
							// }
							// --------DEBUG
							// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							// fprintf(stderr, " % s inst : 0x%04x  level: %02d enabled: 0x%04x\n", junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_enabled);
							// --------END DEBUG
							SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						}
						else {
							PRIV_INSTR_TRAP;
						}
						break;

					// RIR  --  Reset interrupt Request         
					case 8:
						if (IS_PRIV_MODE) {
							// -------- Request ownership of the resource.
							TAKE_RESOURCE( ResourceInterruptRequest );
							cpu_interrupt_request &= ( tmp16_val1.uval | RIR_ALLOWED_NOT);
							// TODO: RIR make the si/di stuf more efficient.
							if ( (cpu_interrupt_DI_total_proc_count != cpu_interrupt_DI_total_request_count) && ( ( cpu_interrupt_enabled & CPU_INTR_DI ) != 0 ) ) {
								cpu_interrupt_request |= CPU_INTR_DI;
								//fprintf(stderr, " RIR needed to re-request outstanding DI.\n");
							}
							if ((cpu_interrupt_SI_total_proc_count != cpu_interrupt_SI_total_request_count) && ( ( cpu_interrupt_enabled & CPU_INTR_SI ) != 0 ) ) {
								cpu_interrupt_request |= CPU_INTR_SI;
								//fprintf(stderr, " RIR needed to re-request outstanding SI.\n");
							}
							// -------- Release ownership of the resource.
							GIVE_RESOURCE( ResourceInterruptRequest );
							// --------DEBUG
							// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							// fprintf(stderr, " % s inst : 0x%04x  level: %02d request: 0x%04x\n", junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_request);
							// --------END DEBUG
							SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						}
						else {
							PRIV_INSTR_TRAP;
						}
						break;

					default:
						ILLEGAL_INSTRUCTION;
						break;
				}
				break;

			case  OP_RLD_RLQ:			// 0x28
				switch (instruction.parts.dest_reg & 0x0001) {

					case 0:		// RLD  --  Shift Right Logical Double-Register
						tmp_instr_src = instruction.parts.src_reg;
						tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
						tmp32_val3.uval = tmp32_val1.uval >> tmp_instr_src;
						// --------DEBUG
						// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
						// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%08lx, result: 0x%08lx\n",
						// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp32_val1.uval, tmp32_val3.uval);
						// disp_cur_reg(stderr);
						// --------END DEBUG
						SET_DESTINATION_REGISTER_VALUE_DOUBLE( tmp32_val3.uval );
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_O(false);
						SET_CC_C((tmp_instr_src == 0 ? false : ((bit32[32 - tmp_instr_src] & tmp32_val1.uval) != 0)));  // compare unshifted value
						// --------DEBUG
						// disp_psw(stderr, cpu_get_current_PSW());
						// --------END DEBUG
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 1:		// RLQ  --  Shift Right Logical Quadruple-Register        
						tmp_instr_src = instruction.parts.src_reg;
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp64_val3.uval = tmp64_val1.uval >> tmp_instr_src;
						// --------DEBUG
						// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
						// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%16llx, result: 0x%16llx\n",
						// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp64_val1.uval, tmp64_val3.uval);
						// disp_cur_reg(stderr);
						// --------END DEBUG
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_CC_O(false);
						SET_CC_C((tmp_instr_src == 0 ? false : ((bit64[64 - tmp_instr_src] & tmp64_val1.uval) != 0))); // compare unshifted value
						// --------DEBUG
						// disp_psw(stderr, cpu_get_current_PSW());
						// --------END DEBUG
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					default:
						ILLEGAL_INSTRUCTION;
						break;
				}
				break;

			case  OP_RLS:			// 0x29  --  RLS  --  Shift Right Logical Single-Register         
				tmp_instr_src = instruction.parts.src_reg;
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = tmp16_val1.uval >> tmp_instr_src;
				// --------DEBUG
				// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%04x, result: 0x%04x\n",
				// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp16_val1.uval, tmp16_val2.uval);
				// disp_cur_reg(stderr);
				// --------END DEBUG
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C((tmp_instr_src == 0 ? false : ((bit[16 - tmp_instr_src] & tmp16_val1.uval) != 0))); // compare unshifted value
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				// --------DEBUG
				// disp_psw(stderr, cpu_get_current_PSW());
				// --------END DEBUG
				break;

			case  OP_RAD_RAQ:		// 0x2a 
				switch (instruction.parts.dest_reg & 0x0001) {

				case 0:		// RAD -- right arithmetic shift double
					tmp_instr_src = instruction.parts.src_reg;
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					tmp32_val3.sval = tmp32_val1.sval >> tmp_instr_src;
					// --------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%08lx, result: 0x%08lx\n",
					// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp32_val1.uval, tmp32_val2.uval);
					// disp_cur_reg(stderr);
					// --------END DEBUG
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_O(false);
					SET_CC_C((tmp_instr_src == 0 ? false : ((bit32[32 - tmp_instr_src] & tmp32_val1.uval) != 0))); // compare unshifted value
					// --------DEBUG
					// disp_psw(stderr, cpu_get_current_PSW());
					// --------END DEBUG
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				case 1:		// RAQ  --  Shift Right Arithmetic Quadruple-Register             
					tmp_instr_src = instruction.parts.src_reg;
					tmp_instr_dest = instruction.parts.dest_reg;
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					tmp64_val3.sval = tmp64_val1.sval >> tmp_instr_src;
					// --------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%16llx, result: 0x%16llx\n",
					// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp64_val1.uval, tmp64_val3.uval);
					// disp_cur_reg(stderr);
					// --------END DEBUG
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
					SET_CC_N(ISVAL64_NEG(tmp64_val3));
					SET_CC_O(false);
					SET_CC_C((tmp_instr_src == 0 ? false : ((tmp64_val1.uval & bit64[64 - tmp_instr_src]) != 0))); // compare unshifted value
					// --------DEBUG
					// disp_psw(stderr, cpu_get_current_PSW());
					// --------END DEBUG
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				default:
					ILLEGAL_INSTRUCTION;
					break;
				}
				break;

			case  OP_RAS:			// RAS  --  Shift Right Arithmetic Single-Register        
				tmp_instr_src = instruction.parts.src_reg;
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.sval = tmp16_val1.sval >> tmp_instr_src;
				// --------DEBUG
				// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%04x, result: 0x%04x\n",
				// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp16_val1.uval, tmp16_val2.uval);
				// disp_cur_reg(stderr);
				// --------END DEBUG
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C((tmp_instr_src == 0 ? false : ((bit[16 - tmp_instr_src] & tmp16_val1.uval) != 0))); // compare unshifted value
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				// --------DEBUG
				// disp_psw(stderr, cpu_get_current_PSW());
				// --------END DEBUG
				break;

			case  OP_LLD_LLQ:		//            0x2c
				switch (instruction.parts.dest_reg & 0x0001) {

					case 0:		// LLD  --  Shift Left Logical Double-Register        
						tmp_instr_src = instruction.parts.src_reg;
						tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
						tmp32_val3.uval = tmp32_val1.uval  << tmp_instr_src;
						// --------DEBUG
						// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
						// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%08lx, result: 0x%08lx\n",
						// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp32_val1.uval, tmp32_val3.uval);
						// disp_cur_reg(stderr);
						// --------END DEBUG
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_O(false);
						SET_CC_C( (tmp_instr_src == 0 ? false : ((bit32[tmp_instr_src-1] & tmp32_val1.uval) != 0) ) ); // compare unshifted value
						// --------DEBUG
						// disp_psw(stderr, cpu_get_current_PSW());
						// --------END DEBUG
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 1:		// LLQ  --  Shift Left Logical Quadruple-Register        
						tmp_instr_src = instruction.parts.src_reg;
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp64_val3.uval = tmp64_val1.uval << tmp_instr_src;
						// --------DEBUG
						// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
						// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%16llx, result: 0x%16llx\n",
						// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp64_val1.uval, tmp64_val3.uval);
						// disp_cur_reg(stderr);
						// --------END DEBUG
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_O(false);
						SET_CC_C((tmp_instr_src == 0 ? false : ((bit64[tmp_instr_src-1] & tmp64_val1.uval) != 0))); // compare unshifted value
						// --------DEBUG
						// disp_psw(stderr, cpu_get_current_PSW());
						// --------END DEBUG
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					default:
						ILLEGAL_INSTRUCTION;
						break;
				}
				break;

			case  OP_LLS:			// 0x2d  --  LLS  --  Shift Left Logical Single-Register        
				tmp_instr_src = instruction.parts.src_reg;
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = tmp16_val1.uval << tmp_instr_src;
				// --------DEBUG
				// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%04x, result: 0x%04x\n", 
				// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp16_val1.uval, tmp16_val2.uval);
				// disp_cur_reg(stderr);
				// --------END DEBUG
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C((tmp_instr_src == 0 ? false : ((bit[tmp_instr_src-1] & tmp16_val1.uval) != 0))); // compare unshifted value
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				// --------DEBUG
				// disp_psw(stderr, cpu_get_current_PSW());
				// --------END DEBUG
				break;

			case  OP_LAD_LAQ:		//            0x2e
				switch (instruction.parts.dest_reg & 0x0001) {

				case 0:		// LAD - left arithmetic shift double
					tmp_instr_src = instruction.parts.src_reg;
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					tmp32_val3.sval = tmp32_val1.sval << tmp_instr_src;
					if (ISVAL32_NEG(tmp32_val1)) {			// keep sign of original.
						tmp32_val3.uval |= 0x80000000;
					}
					else {
						tmp32_val3.uval &= 0x7fffffff;
					}
					// --------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%08lx, result: 0x%08lx\n",
					// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp32_val1.uval, tmp32_val3.uval);
					// disp_cur_reg(stderr);
					// --------END DEBUG
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_O(!(((mask32[tmp_instr_src] & tmp32_val1.uval) == 0) ||
						((mask32[tmp_instr_src] & tmp32_val1.uval) == mask32[tmp_instr_src])));
					SET_CC_C((tmp_instr_src == 0 ? false : ((bit32[tmp_instr_src] & tmp32_val1.uval) != 0))); // compare unshifted value

					// --------DEBUG
					// disp_psw(stderr, cpu_get_current_PSW());
					// --------END DEBUG
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				case 1:		// LAQ  --  Shift Left Arithmetic Quadruple-Register        
					tmp_instr_src = instruction.parts.src_reg;
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					tmp64_val3.sval = tmp64_val1.sval << tmp_instr_src;
					if (ISVAL64_NEG(tmp64_val1)) {
						tmp64_val3.uval |= 0x8000000000000000;
					}
					else {
						tmp64_val3.uval &= 0x7fffffffffffffff;
					}
					// --------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%16llx, result: 0x%16llx\n",
					// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp64_val1.uval, tmp64_val3.uval);
					// disp_cur_reg(stderr);
					// --------END DEBUG
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
					SET_CC_N(ISVAL64_NEG(tmp64_val3));
					SET_CC_O(!(((mask64[tmp_instr_src] & tmp64_val1.uval) == 0) ||
						((mask64[tmp_instr_src] & tmp64_val1.uval) == mask64[tmp_instr_src])));
					SET_CC_C((tmp_instr_src == 0 ? false : ((bit64[tmp_instr_src] & tmp64_val1.uval) != 0))); // compare unshifted value
					// --------DEBUG
					// disp_psw(stderr, cpu_get_current_PSW());
					// --------END DEBUG
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				default:
					ILLEGAL_INSTRUCTION;
					break;
				}
				break;

			case  OP_LAS:			// 0x2f  --  LAS  --  Shift Left Arithmetic Single-Register        
				tmp_instr_src = instruction.parts.src_reg;
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.sval = tmp16_val1.sval << tmp_instr_src;
				if (ISVAL16_NEG(tmp16_val1)) {
					tmp16_val2.uval |= 0x8000;
				}
				else {
					tmp16_val2.uval &= 0x7fff;
				}
				// --------DEBUG
				// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				// fprintf(stderr, "\n %s inst: 0x%04x, shift:  %02d, orig: 0x%04x, result: 0x%04x\n",
				// 	&junkxx[0], instruction.all, GET_SOURCE_REGISTER_NUMB, tmp16_val1.uval, tmp16_val2.uval);
				// disp_cur_reg(stderr);
				// --------END DEBUG
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_O( !(((mask[tmp_instr_src] & tmp16_val1.uval) == 0) ||
						    ((mask[tmp_instr_src] & tmp16_val1.uval) == mask[tmp_instr_src])));
				SET_CC_C( (tmp_instr_src == 0 ? false : ((bit[tmp_instr_src] & tmp16_val1.uval) != 0)));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				// --------DEBUG
				// disp_psw(stderr, cpu_get_current_PSW());
				// --------END DEBUG
				break;

// -------- if this CPU has an EAU implement floating point instructions.  Otherwise they cause a UIT
#if SIMJ_SIM_HAS_EAU == true

			case  OP_FAR_CDIF:		// 0x30
				switch (instruction.parts.src_reg & 0x1) {
				case 0:		// FAR  --  Floating Point Add Double-Register to Double Register     
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C( (fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 1:		// CDIF  --  Convert Double-Register Integer to Floating Point   
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					fstatus1 = util_cvt_S32_MCS32(tmp32_val1.uval, &tmp32_val2.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val2.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val2));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val2));
					SET_CC_C(fstatus1 != SIMJ_FLTCVT_GOOD);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FSR:			// 0x31
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
				fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
				fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
				tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
				fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_FMR:			// 0x32
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
				fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
				fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
				tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
				fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_FDR:			// 0x33
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
				fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
				fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
				tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
				fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_FARD_FARQ_CFDI:	//          0x34
				switch (instruction.parts.src_reg & 0x01) {
				case 0:				// fard, farq
					switch (instruction.parts.dest_reg & 0x1) {
					case 0:			// FARD  --  Floating Point Add Triple Register to Triple Register    
						tmp64_val1.uval = GET_SOURCE_REGISTER_VALUE_TRIPLE;
						tmp64_val2.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_TRIPLE( tmp_f64_val3 ); 
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					case 1:			// FARQ  --  Floating Point Add Quad Register to Quad Register    
						tmp64_val1.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						tmp64_val2.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					}
 					break;
				case 1:				// CFDI  --  Convert Floating Point to Double-Register Integer      
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					fstatus1 = util_cvt_MCS32_S32(tmp32_val1.uval, &tmp32_val2.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val2.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val2));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val2));
					SET_CC_C(fstatus1 != SIMJ_FLTCVT_GOOD);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FSRD_FSRQ_CQFF:	//           0x35
				switch (instruction.parts.src_reg & 0x01) {
				case 0:				 
					switch (instruction.parts.dest_reg & 0x1) {
					case 0:			// FSRD
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_TRIPLE;
						fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp_f64_val3);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					case 1:			// FSRQ
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					}
					break;
				case 1:				//  CQFF  --  Convert Quad-Register Floating Point to Floating Point     
					tmp64_val1.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
					fstatus1 = util_cvt_MCS64_MCS32(tmp64_val1.uval, &tmp32_val2.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val2.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val2));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val2));
					SET_CC_C(fstatus1 != SIMJ_FLTCVT_GOOD);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FMRD_FMRQ_CDFI:	//           0x36
				switch (instruction.parts.src_reg & 0x01) {
				case 0:				// 
					switch (instruction.parts.dest_reg & 0x1) {
					case 0:			// FMRD
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_TRIPLE;
						fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp_f64_val3);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					case 1:			// FMRQ
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					}
					break;
				case 1:				// CDFI  --  Convert Double Precision Floating Point Operand to Double Integer   
					tmp64_val1.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
					tmp64_val1.uval &= 0xffffffffffff0000;	// only a 3 word floating point value
					fstatus1 = util_cvt_MCS48_S32(tmp64_val1.uval, &tmp32_val2.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val2.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val2));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val2));
					SET_CC_C(fstatus1 != SIMJ_FLTCVT_GOOD);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FDRD_FDRQ_CQFI:	//           0x37
				switch (instruction.parts.src_reg & 0x01) {
				case 0:				// 
					switch (instruction.parts.dest_reg & 0x1) {
					case 0:			// FDRD
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_TRIPLE;
						fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp_f64_val3);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					case 1:			// FDRQ
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
					}
					break;
				case 1:				// CQFI  --  Convert Quad Precision Floating Point Operand to Double Integer   
					tmp64_val1.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
					fstatus1 = util_cvt_MCS64_S32(tmp64_val1.uval, &tmp32_val2.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val2.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val2));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val2));
					SET_CC_C(fstatus1 != SIMJ_FLTCVT_GOOD);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FAM_FAI:	//		            0x38
				switch (instruction.parts.dest_reg & 0x1) {
					case 0:		// fam
						GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val1.uval);
						tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
						fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
					case 1:		// fai
						GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
						tmp32_val1.uval = ((SIMJ_U32)tmp16_val1.uval << 16) & 0xffff0000;
						tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
						fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
				}
				break;

			case  OP_FSM_FSI:	//		            0x39
				switch (instruction.parts.dest_reg & 0x1) {
				case 0:		// FSM
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val2.uval);
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 1:		// FSI
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp32_val2.uval = ((SIMJ_U32)tmp16_val1.uval << 16) & 0xffff0000;
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FMM_FMI:	//		            0x3a
				switch (instruction.parts.dest_reg & 0x1) {
				case 0:		// fmm
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val2.uval);
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 1:		// fmi
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp32_val2.uval = ((SIMJ_U32)tmp16_val1.uval << 16) & 0xffff0000;
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FDM_FDI:	//		0x3b
				switch (instruction.parts.dest_reg & 0x1) {
				case 0:		// fmm
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp16_val2.uval);
					tmp32_val2.uval = ((SIMJ_U32)tmp16_val1.uval << 16) & 0xffff0000;
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 1:		// fdi
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp32_val2.uval = ((SIMJ_U32)tmp16_val1.uval << 16) & 0xffff0000;
					fstatus1 = util_cvt_MCS32_IEEE64(tmp32_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS32_IEEE64(tmp32_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS32(tmp_f64_val3, &tmp32_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FAMD_FAMQ_FAID_FAIQ:	//      0x3c
				switch (instruction.parts.dest_reg & 0x3) {
					case 0:		// famd
						GET_MEMORY_VALUE_DIRECT_TRIPLE(tmp64_val1.uval);
						tmp64_val2.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
					case 1:		// famq
						GET_MEMORY_VALUE_DIRECT_QUAD(tmp64_val1.uval);
						tmp64_val2.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
					case 2:		// faid
						GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
						tmp64_val1.uval = ((SIMJ_U64)tmp16_val1.uval << 48) & 0xffff000000000000;
						tmp64_val2.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
					case 3:		// faiq
						GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
						tmp64_val1.uval = ((SIMJ_U64)tmp16_val1.uval << 48) & 0xffff000000000000;
						tmp64_val2.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
						fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
						tmp_f64_val3 = tmp_f64_val1 + tmp_f64_val2;
						fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
				}
				break;

			case  OP_FSMD_FSMQ_FSID_FSIQ:	//		0x3d
				switch (instruction.parts.dest_reg & 0x3) {
				case 0:		// fsmd
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					GET_MEMORY_VALUE_DIRECT_TRIPLE(tmp64_val2.uval);
					fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 1:		// fsmq
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					GET_MEMORY_VALUE_DIRECT_QUAD(tmp64_val2.uval);
					fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 2:		// fsid
					// TODO: FSID Implement get triple register
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp64_val2.uval = ((SIMJ_U64)tmp16_val2.uval << 48) & 0xffff000000000000;
					fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 3:		// fsiq
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp64_val2.uval = ((SIMJ_U64)tmp16_val2.uval << 48) & 0xffff000000000000;
					fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 - tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FMMD_FMMQ_FMID_FMIQ:	//      0x3e
				switch (instruction.parts.dest_reg & 0x3) {
				case 0:		// fmmd
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					GET_MEMORY_VALUE_DIRECT_TRIPLE(tmp64_val2.uval);
					fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 1:		// fmmq
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					GET_MEMORY_VALUE_DIRECT_QUAD(tmp64_val2.uval);
					fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 2:		// fmid
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp64_val2.uval = ((SIMJ_U64)tmp16_val2.uval << 48) & 0xffff000000000000;
					fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 3:		// fsiq
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp64_val2.uval = ((SIMJ_U64)tmp16_val2.uval << 48) & 0xffff000000000000;
					fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 * tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_FDMD_FDMQ_FDID_FDIQ:	//      0x3f
				switch (instruction.parts.dest_reg & 0x3) {
				case 0:		// fdmd
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					GET_MEMORY_VALUE_DIRECT_TRIPLE(tmp64_val2.uval);
					fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 1:		// fdmq
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					GET_MEMORY_VALUE_DIRECT_QUAD(tmp64_val2.uval);
					fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 2:		// fdid
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp64_val2.uval = ((SIMJ_U64)tmp16_val2.uval << 48) & 0xffff000000000000;
					fstatus1 = util_cvt_MCS48_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS48_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS48(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_TRIPLE(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				case 3:		// fdiq
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
					tmp64_val2.uval = ((SIMJ_U64)tmp16_val2.uval << 48) & 0xffff000000000000;
					fstatus1 = util_cvt_MCS64_IEEE64(tmp64_val1.uval, &tmp_f64_val1);
					fstatus2 = util_cvt_MCS64_IEEE64(tmp64_val2.uval, &tmp_f64_val2);
					tmp_f64_val3 = tmp_f64_val1 / tmp_f64_val2;
					fstatus3 = util_cvt_IEEE64_MCS64(tmp_f64_val3, &tmp64_val3.uval);
					SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_C((fstatus1 != SIMJ_FLTCVT_GOOD) && (fstatus2 != SIMJ_FLTCVT_GOOD) && (fstatus3 != SIMJ_FLTCVT_GOOD));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
				}
				break;

#endif

			case  OP_OCA:			// 0x40  --  Output Command to I/O Group A
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
						(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_OCB:			// 0x41  --  Output Command to I/O Group B
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0010;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
						(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_OCC:			// 0x42  --  Output Command to I/O Group C
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0020;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
						(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_OCD:			// 0x43  --  Output Command to I/O Group D
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0030;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_cmd_proc[tmp_instr_src] != NULL) {
						(*iop_output_cmd_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ODA:			// 0x44  --  Output Data to I/O Group A
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_data_proc[tmp_instr_src] != NULL) {
						(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ODB:			// 0x45  --  Output Data to I/O Group B
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0010;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_data_proc[tmp_instr_src] != NULL) {
						(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ODC:			// 0x46  --  Output Data to I/O Group C
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0020;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_data_proc[tmp_instr_src] != NULL) {
						(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ODD:			// 0x47  --  Output Data to I/O Group D
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0030;
					tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
					if (iop_output_data_proc[tmp_instr_src] != NULL) {
						(*iop_output_data_proc[tmp_instr_src])(tmp_instr_src, tmp16_val1.uval);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ISA:			// 0x48
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
					// -------- ISZ - Input Status from Device Zero
					// CPU type
					//	Bit 0,	Bit 2,	Bit 3	Bit 6	Bit 7
					//	0		0		0		?		?		IV/25
					//	1		0		0		?		?		IV/35
					//	x		1		0		0		?		7830, II/45, II/55
					//	x		1		1		0		?		7860, II/75
					//	x		1		1		1		?		II/15
					//	x		1		1		0		1		32/85
					if (tmp_instr_src == 0) {
#if SIMJ_SIM_CPU == II15
						tmp16_val1.uval = 0x3200;		// classic cpu. II/15
#elif SIMJ_SIM_CPU == 7860
						tmp16_val1.uval = 0x3000;		// classic cpu. 7860
#elif SIMJ_SIM_CPU == 7830
						tmp16_val1.uval = 0x2000;		// classic cpu. 7830
#elif SIMJ_SIM_CPU == 3285
						tmp16_val1.uval = 0x3100;		// classic cpu. 32/85 (maybe others as well.
#elif SIMJ_SIM_CPU == IV25
						tmp16_val1.uval = 0x0000;		// IV/25.
#elif SIMJ_SIM_CPU == IV35
						tmp16_val1.uval = 0x8000;		// IV/35
#else
#error BAD CPU type specifiied
#endif
						if (cpu_virtual_mode)
							tmp16_val1.uval |= 0x4000;		// relocatable mode.
						// -------for the 7830 this indicates on board IOP is disabled.
						SET_CC_N(false);
						SET_CC_Z(true);
						SET_CC_O(false);
						SET_CC_C(false);
					}
					// -------- ISA - Input Status from 1/0 Group A
					else {
						if (iop_input_status_proc[tmp_instr_src] != NULL) {
							tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
							// fprintf(stderr,"   ISA - input status value 0x%04x\n", tmp16_val1.uval);
						}
						else {
							tmp16_val1.uval = 0;
						}
						SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
						SET_CC_N(ISVAL16_NEG(tmp16_val1));
						SET_CC_O((tmp16_val1.uval & bit[7]) != 0);
						SET_CC_C((tmp16_val1.uval & bit[8]) != 0);
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ISB:			// 0x49  --  Input Status from 1/0 Group B
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0010;
					if (iop_input_status_proc[tmp_instr_src] != NULL) {
						tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
						// fprintf(stderr,"   ISB - input status value 0x%04x\n", tmp16_val1.uval);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
					SET_CC_N(ISVAL16_NEG(tmp16_val1));
					SET_CC_O((tmp16_val1.uval & bit[7]) != 0);
					SET_CC_C((tmp16_val1.uval & bit[8]) != 0);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ISC:			// 0x4a  --  Input Status from 1/0 Group C
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0020;
					if (iop_input_status_proc[tmp_instr_src] != 0) {
						tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
						// fprintf(stderr,"   ISC - input status value 0x%04x\n", tmp16_val1.uval);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
					SET_CC_N(ISVAL16_NEG(tmp16_val1));
					SET_CC_O((tmp16_val1.uval & bit[7]) != 0);
					SET_CC_C((tmp16_val1.uval & bit[8]) != 0);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_ISD:			// 0x4b  --  Input Status from 1/0 Group D
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0030;
					if (iop_input_status_proc[tmp_instr_src] != 0) {
						tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
						// fprintf(stderr,"   ISD - input status value 0x%04x\n", tmp16_val1.uval);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
					SET_CC_N(ISVAL16_NEG(tmp16_val1));
					SET_CC_O((tmp16_val1.uval & bit[7]) != 0);
					SET_CC_C((tmp16_val1.uval & bit[8]) != 0);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_IDA:			// 0x4c  --  Input Data from 1/0 Group A
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
					if (iop_input_data_proc[tmp_instr_src] != 0) {
						tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_CHAR(tmp16_val1.uval & 0x00ff);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_IDB:			// 0x4d  --  Input Data from 1/0 Group B
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0010;
					if (iop_input_data_proc[tmp_instr_src] != 0) {
						tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_CHAR(tmp16_val1.uval & 0x00ff);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_IDC:			// 0x4e  --  Input Data from 1/0 Group C
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0020;
					if (iop_input_data_proc[tmp_instr_src] != 0) {
						tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_CHAR(tmp16_val1.uval & 0x00ff);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_IDD:			// 0x4f  --  Input Data from 1/0 Group D
				if (IS_PRIV_MODE) {
					tmp_instr_src = GET_SOURCE_REGISTER_NUMB | 0x0030;
					if (iop_input_data_proc[tmp_instr_src] != 0) {
						tmp16_val1.uval = (*iop_input_data_proc[tmp_instr_src])(tmp_instr_src);
					}
					else {
						tmp16_val1.uval = 0;
					}
					SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
					SET_CC_CHAR(tmp16_val1.uval & 0x00ff);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			// -- 0x5x -- reserved for communications

			case  OP_ABR:			// 0x60  --  Add Bit in Register
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_SBR:			// 0x61  --  Subtract Bit in Register
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_ZBR:			// 0x62  --  Zero Bit in Register
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & bitnot[GET_SOURCE_REGISTER_NUMB];
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C(false);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_OBR:			// 0x63  --  OR Bit in Register
				tmp16_val1.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval |= tmp16_val1.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_Z(false);
				SET_CC_O(false);
				SET_CC_C(true);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_XBR:			// 0x64  --  Exclusive OR Bit in Register       
				tmp16_val1.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE ^ tmp16_val1.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval& tmp16_val2.uval) != 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_LBR:			// 0x65 -- Load Bit in Register        
				tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
				SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
				SET_CC_Z(false);
				SET_CC_N(tmp_instr_src == 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_TBR:			// 0x66 -- Test Bit(s) in Register        
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_SOURCE_REGISTER_NUMB]) != 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_GMR:			// 0x67 -- Generate· Mask in Register (Load Negative. Power of Two)   
				tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
				SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
				SET_CC_Z(false);
				SET_CC_N(true);		// TODO: GMR manual doesn't say CC N is aways set, but it is -- check
				SET_CC_O(tmp_instr_src == 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_ADR:			// 0x68  --  Add Register to Register        
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_SUR:			// 0x69 --  Subtract Register from Register
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3 );
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_ETR:			// 0x6a  -- Extract Register from Register        
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_ORR:			// 0x6b  -- OR Register to Register        
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_XOR:			// 0x6c -- Exclusive OR Register to Register       
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_TRR:			// 0x6d -- Transfer Register to Register        
				tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_CRR:			// 0x6e  -- CRR  --  Compare Register with Register
				{
					SIMJ_REGISTER VAL16 crr_src_value = { .uval = 0 };
					SIMJ_REGISTER VAL16 crr_dest_value = { .uval = 0 };
					SIMJ_REGISTER VAL16 crr_result_value = { .uval = 0 };
					crr_src_value.uval = GET_SOURCE_REGISTER_VALUE;
					crr_dest_value.uval = GET_DESTINATION_REGISTER_VALUE;
					crr_result_value.uval = crr_dest_value.uval - crr_src_value.uval;
					SET_CC_Z(ISVAL16_ZERO(crr_result_value));
					SET_CC_N(ISVAL16_NEG(crr_result_value));
					SET_CC_O_SUB(crr_dest_value, crr_src_value, crr_result_value);
					SET_CC_C_SUB(crr_dest_value, crr_src_value, crr_result_value);
#if DEBUG_CRR >= 1
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, Rs: 0x%04x, Rd: 0x%04x	,  Res: 0x%04x\n",
						junkxx, instruction.all, program_counter,
						crr_src_value.uval, crr_dest_value.uval, crr_result_value.uval);
					disp_psw(stderr, cpu_get_current_PSW());
					disp_cur_reg(stderr);
					if (gbl_verbose_debug == debugging_automatic)
						gbl_verbose_debug = debugging_on;
#endif
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				break;

			case  OP_TTR:			// 0x6f  --  Transfer Two's Complement of Register to Register     
				tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_val3.sval = tmp16_val1.sval * -1;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(ISVAL16_MAXNEG(tmp16_val3));
				tmp16_val1.uval = ~tmp16_val1.uval;
				tmp16_val2.uval = 1;
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_ABRB:			// 0x70  --  Add Bit in Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_SBRB:			// 0x71  --  Subtract Bit in Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ZBRB:			// 0x72  --  Zero Bit in Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & bitnot[GET_SOURCE_REGISTER_NUMB];
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C(false);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_OBRB:			// 0x73  --  OR Bit in Register and Branch Unconditionally     
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | bit[GET_SOURCE_REGISTER_NUMB];
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(false);
				SET_CC_O(false);
				SET_CC_C(true);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
				break;

			case  OP_XBRB:			// 0x74  --  XBRB  --  Exclusive OR Bit in Register and Branch if Nonzero        
				tmp16_val1.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE ^ tmp16_val1.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & tmp16_val2.uval) != 0);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_LBRB:			// 0x75 -- Load Bit in Register and Branch Unconditionally
				tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
				SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
				SET_CC_Z(false);
				SET_CC_N(tmp_instr_src == 0);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				SET_NEXT_PROGRAM_COUNTER( tmp_new_prog_count );
				break;

			case  OP_TBRB:			// 0x76  --  Test Bit in Register and Branch if One    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_SOURCE_REGISTER_NUMB]) != 0);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH( TEST_CC_C, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_GMRB:			// 0x77  --  Generate Mask in Register and Branch Unconditionally     
				tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
				SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
				SET_CC_Z(false);
				SET_CC_N(true);		// TODO: GMRB manual doesn't say CC N is aways set, but it is -- check
				SET_CC_O(tmp_instr_src == 0);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
				break;

			case  OP_ADRB:			// 0x78  --  Add Register to Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_SURB:			// 0x79  --  Subtract Register from Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH( TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ETRB:			// 0x7a  --  Extract Register from Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(tmp16_val1.uval != 0, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;
	
			case  OP_ORRB:			// 0x7b  --  OR Register to Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_XORB:			// 0x7c  --  Exclusive OR Register to Register and Branch if Nonzero   
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TRRB:			// 0x7d --  Transfer Register to Register and Branch if Nonzero    
				tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH( TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TERB:			// 0x7e  --  Test Register and Branch if any Ones Compare    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_SOURCE_REGISTER_VALUE;
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(tmp16_val1.uval != 0, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TTRB:			// 0x7f  --  Transfer Two's Complement of Register to Register and Branch if Nonzero 
				tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_val3.sval = tmp16_val1.sval * -1;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(ISVAL16_MAXNEG(tmp16_val3));
				tmp16_val1.uval = ~tmp16_val1.uval;
				tmp16_val2.uval = 1;
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;


			case  OP_ABMM:			// 	        0x80  --  Add Bit in Memory            
				GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval );
				tmp16_val2.uval = bit[instruction.parts.dest_reg];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ZBMM:			// 	        0x81  --  Zero Bit in Memory        
				tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val2.uval & tmp16_val1.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(false);
				SET_CC_C(false);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_OBMM:			//  0x82  --  OR Bit in Memory·        
				tmp16_val1.uval = bit[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val2.uval | tmp16_val1.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
				SET_CC_Z(false);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(false);
				SET_CC_C(true);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TBMM:			// x83  --  Test Bit(s) in Memory        
				GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ABMB:			// 		    0x84  --  Add Bit in Memory and Branch if Nonzero    
				GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
				tmp16_val2.uval = bit[instruction.parts.dest_reg];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
				break;

			case  OP_ZBMB:			// 		    0x85  --  Zero Bit in Memory and Branch if Nonzero    
				tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val2.uval & tmp16_val1.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(false);
				SET_CC_C(false);
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
				break;

			case  OP_TBMB:			// 		    0x86  --  Test Bit(s) in Memory and Branch if One    
				GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval );
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_C, tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
				break;

			case  OP_CBMB:			// 		    0x87  --  CBMB  --  Compare Bit and Memory and Branch if Equal or Less  
				tmp16_val1.uval = bit[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				if (TEST_CC_Z) {
					GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
				}
				else if (TEST_CC_LT) {
					GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
					SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
				}
				else {
					// GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
				}
				break;

			case  OP_LDXT_STXT_LDMT_STMT:	//	0x88
				switch (instruction.parts.dest_reg & 0x0003) {

					case 0:				//  STXT  --  Store Triple-Register into Memory Tripleword (Short-Indexed)      
						// TODO: STXT Use Triple macros
						tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
						tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000c;
						tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
						tmp16_val3.uval = GET_REGISTER_VALUE(tmp_instr_dest+1);
						tmp16_val4.uval = GET_REGISTER_VALUE(tmp_instr_dest+2);
						SET_MEMORY_VALUE_OM(tmp16_val1.uval, tmp16_val2.uval);
						SET_MEMORY_VALUE_OM(tmp16_val1.uval+1, tmp16_val3.uval);
						SET_MEMORY_VALUE_OM(tmp16_val1.uval+2, tmp16_val4.uval);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 1:				//  LDXT  --  Load Triple-Register from Memory Triple- word (Short-Indexed)
						// TODO: LDXT Use triple macros.
						tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
						tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000c;
						GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
						GET_MEMORY_VALUE_OM(tmp16_val3.uval, tmp16_val1.uval + 1);
						GET_MEMORY_VALUE_OM(tmp16_val4.uval, tmp16_val1.uval + 2);
						SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val2.uval);
						SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);
						SET_REGISTER_VALUE(tmp_instr_dest + 2, tmp16_val4.uval);
						SET_CC_Z((tmp16_val2.uval == 0) && (tmp16_val3.uval == 0) && (tmp16_val4.uval == 0));
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 2:				//  STMT  --  Store Triple-Register into Memory Triple-Word       
						// TODO: STMT Use triple macros.
						GET_MEMORY_DIRECT_ADDR(tmp16_val1.uval);
						tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000c;
						tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
						tmp16_val3.uval = GET_REGISTER_VALUE(tmp_instr_dest + 1);
						tmp16_val4.uval = GET_REGISTER_VALUE(tmp_instr_dest + 2);
						SET_MEMORY_VALUE_OM(tmp16_val1.uval, tmp16_val2.uval);
						SET_MEMORY_VALUE_OM(tmp16_val1.uval + 1, tmp16_val3.uval);
						SET_MEMORY_VALUE_OM(tmp16_val1.uval + 2, tmp16_val4.uval);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;

					case 3:				//  LDMT  --  Load Triple-Register from Memory Tripleword       
						// TODO: LDMT Use triple Macros
						GET_MEMORY_DIRECT_ADDR(tmp16_val1.uval);
						tmp_instr_dest =  GET_DESTINATION_REGISTER_NUMB & 0x000c;
						GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
						GET_MEMORY_VALUE_OM(tmp16_val3.uval, tmp16_val1.uval + 1);
						GET_MEMORY_VALUE_OM(tmp16_val4.uval, tmp16_val1.uval + 2);
						SET_REGISTER_VALUE(tmp_instr_dest,     tmp16_val2.uval);
						SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);
						SET_REGISTER_VALUE(tmp_instr_dest + 2, tmp16_val4.uval);
						SET_CC_Z((tmp16_val2.uval == 0) && (tmp16_val3.uval == 0) && (tmp16_val4.uval == 0));
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
						break;
				}
				break;

			case  OP_NOP:			// :			// 0x89
				SET_REGISTER_VALUE(0, gbl_fp_switches);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

			case  OP_IRRD_TTRD:			// 	0x8a
				// -------- IRRD  --  Interchange Double Register and Double Register      
				if ((instruction.all & 0x0010) != 0) {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					SET_SOURCE_REGISTER_VALUE_DOUBLE(tmp32_val2.uval);	// DEST -> SRC
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val1.uval);		// SRC -> DEST
					SET_CC_N(ISVAL32_NEG(tmp32_val1));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val1));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				// -------- TTRD  --  Transfer Two's Complement of Double- Register to Double-Register    
				else {
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp32_val3.sval = tmp32_val1.sval * -1;
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_O(ISVAL32_MAXNEG(tmp32_val3));
					tmp32_val1.uval = ~tmp32_val1.uval;
					tmp32_val2.uval = 1;
					SET_CC_C_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				break;

			case  OP_CRRT_CRRQ_TTRQ:			// 0x8b
				switch (instruction.parts.dest_reg & 0x0003) {
					case 0:			//  --  TTRQ  --  Transfer Two's Complement of Quadruple- Register to Quadruple-Register    
						tmp64_val1.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						tmp64_val3.sval = tmp64_val1.sval * -1;
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_CC_O(ISVAL64_MAXNEG(tmp64_val3));
						tmp64_val1.uval = ~tmp64_val1.uval;
						tmp64_val2.uval = 1;
						SET_CC_C_ADD_QUAD(tmp64_val1, tmp64_val2, tmp64_val3);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 2:			// --  CRRT  --  Compare Triple Register to Triple Register      
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_TRIPLE;
						// TODO: CRRT Check method for triple compare.
						tmp64_val3.uval = tmp64_val1.uval - tmp64_val2.uval;
						tmp64_val3.uval &= 0xffffffffffff0000;  // use only top 3 word
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_CC_O_SUB_QUAD(tmp64_val1, tmp64_val2, tmp64_val3);
						SET_CC_C_SUB_QUAD(tmp64_val1, tmp64_val2, tmp64_val3);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 3:			// --  CRRQ  --  Compare Quad Register to Quad Register      
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp64_val2.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						tmp64_val3.uval = tmp64_val1.uval - tmp64_val2.uval;
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_CC_O_SUB_QUAD(tmp64_val1, tmp64_val2, tmp64_val3);
						SET_CC_C_SUB_QUAD(tmp64_val1, tmp64_val2, tmp64_val3);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					default:
						ILLEGAL_INSTRUCTION;
						break;
				}
				break;

			case  OP_ESD_ESS:			//         0x8c
				switch (instruction.parts.dest_reg & 0x01) {
	
					case 0:				//  --  ESD  --  Extend Sign Double         
						tmp32_val3.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
						tmp64_val3.sval = tmp32_val3.sval;	// sign extend.
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_CC_O(false);
						SET_CC_C(false);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 1:				//  --  ESS  --  Extend Sign Single         
						tmp16_val3.uval = GET_SOURCE_REGISTER_VALUE;
						tmp32_val3.sval = tmp16_val3.sval;		// sign extend.
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
						SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
						SET_CC_N(ISVAL32_NEG(tmp32_val3));
						SET_CC_O(false);
						SET_CC_C(false);
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
				}
				break;


			case  OP_TRRQ_LDXD:		//			0x8d
				switch (instruction.parts.dest_reg & 0x0001) {
	
					case 0:			// --  TRRQ  --  Transfer Quadruple-Register to Quadruple Register       
						tmp64_val3.uval = GET_SOURCE_REGISTER_VALUE_QUAD;
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 1:			//  --  LDXD  --  Load Double-Register from Memoty Doubleword (Short-Indexed)      
						GET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE(tmp32_val1.uval);
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val1.uval);
						SET_CC_N(ISVAL32_NEG(tmp32_val1));
						SET_CC_Z(ISVAL32_ZERO(tmp32_val1));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;
				}
				break;

			case  OP_CRXD_STXD:		//        0x8e
				switch (instruction.parts.dest_reg & 0x0001) {

				case 0:				//  --  CRXD  --  Compare Double Register to Short-Indexed Memory Doubleword     
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE(tmp32_val2.uval);
					tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;


				case 1:				//  --  STXD  --  Store Double-Register into Memory Doubleword (Short-Indexed)      
					tmp32_val4.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					SET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE( tmp32_val4.uval );
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				break;

			case  OP_AUG8F:			//  -- BX__       0x8f
				switch (instruction.parts.dest_reg) {
					case 0:		// --  BXNS	Branch (Short-Indexed) on Condition Code N Set     
						do_branch = TEST_CC_N;
						break;
					case 1:		// --  BXZS	Branch (Short-Indexed) on Condition Code Z Set     
						do_branch = TEST_CC_Z;
						break;
					case 2:		// --  BXOS	Branch (Short-Indexed) On Condition Code O Set     
						do_branch = TEST_CC_O;
						break;
					case 3:		// --  BXCS	Branch (Short-Indexed) on Condition Code C Set     
						do_branch = TEST_CC_C;
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
						do_branch = TEST_CC_NOT_N;
						break;
					case 9:		// --  BXZR	Branch (Short-Indexed) on Condition Code Z Reset     
						do_branch = TEST_CC_NOT_Z;
						break;
					case 10:		// --  BXOR	Branch (Short-Indexed) On Condition Code O Reset     
						do_branch = TEST_CC_NOT_O;
						break;
					case 11:		// --  BXCR	Branch (Short-Indexed) on Condition Code C Reset     
						do_branch = TEST_CC_NOT_C;
						break;
					case 12:		// --  BXGE	Branch (Short-Indexed) on Greater Than or Equal Condition    
						do_branch = TEST_CC_GE;  //  CCZ .or. (!CCZ .AND. (!CCN .XOR. CCO))
						break;
					case 13:		// --  BXGT	Branch (Short-Indexed) on Greater Than Condition      
						do_branch = TEST_CC_GT;  //  CCZ .or. (CCN .XOR. CCO)
						break;
					case 14:		// --  BXNH	Branch (Short-Indexed) on Magnitude Not Higher Condition     
						do_branch = TEST_CC_NH;  // !CCC .or CCZ
#if DEBUG_BX__ >= 1
						if (gbl_verbose_debug == debugging_automatic)
							gbl_verbose_debug = debugging_on;
#endif
						break;
					default:
#if DEBUG_BX__ >= 1
						fprintf(stderr, " BX__ illegal opcode 0x%04x \n", instruction.parts.dest_reg);
#endif
						ILLEGAL_INSTRUCTION;		// TODO: BX__ Will the PC be wrong when this is executed.
						program_counter--;			// KLUDGE In prep for the increment below.... This will never be executed..
						do_branch = false;
						break;
				}
#if DEBUG_BX__ >= 1
				util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, Rx/branch: 0x%04x, dobranch: %s\n",
					junkxx, instruction.all, program_counter, GET_SOURCE_REGISTER_VALUE, (do_branch ? "true" : "false"));
				disp_psw(stderr, cpu_get_current_PSW());
				disp_cur_reg(stderr);
#endif
				CONDITIONAL_BRANCH(do_branch, GET_SOURCE_REGISTER_VALUE, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;


		case  OP_ABSM:			// 	        0x90  --  Add Bit in Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				tmp16_val2.uval = bit[instruction.parts.dest_reg];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ZBSM:			// 	        0x91  --  Zero Bit in Memory (Short-Displaced}       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val2.uval & tmp16_val1.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(false);
				SET_CC_C(false);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_OBSM:			// 	        0x92  --  OR Bit in Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = bit[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val2.uval| tmp16_val1.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_Z(false);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(false);
				SET_CC_C(true);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_TBSM:			// 	        0x93  --  Test Bit(s) in Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval& bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ABSB:			// 	        0x94  --  Add Bit in Memory (Short-Displaced) and Branch if Nonzero   
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				tmp16_val2.uval = bit[instruction.parts.dest_reg];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_ZBSB:			// 	        0x95  --  Zero Bit in Memory (Short-Displaced} and Branch     
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val2.uval & tmp16_val1.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O(false);
				SET_CC_C(false);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_TBSB:			// 	        0x96  --  Test Bit(s) in Memory (Short-Displaced) and Branch if One   
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval& bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_C, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_CBSB:			// 	        0x97  --  Compare Bit and Memory (Short-Displaced) and Branch if Equal or Less 
			tmp16_val1.uval = bit[instruction.parts.dest_reg];
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else if (TEST_CC_LT) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;

		case  OP_ABXM:			// 	        0x98  --  Add Bit in Memory (Short-Indexed)       
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			tmp16_val2.uval = bit[instruction.parts.dest_reg];
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ZBXM:			// 	        0x99  --  Zero Bit in Memory (Short-Indexed}       
			tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val2.uval & tmp16_val1.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O(false);
			SET_CC_C(false);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_OBXM:			// 	        0x9a  --  OR Bit in Memory (Short-Indexed)       
			tmp16_val1.uval = bit[instruction.parts.dest_reg];
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val2.uval | tmp16_val1.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_Z(false);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O(false);
			SET_CC_C(true);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_TBXM:			// 	        0x9b  --  Test Bit(s) in Memory (Short-Indexed)       
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_O(false);
			SET_CC_C((tmp16_val1.uval& bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ABXB:			// 	        0x9c  --  Add Bit in Memory (Short-Indexed) and Branch if Nonzero   
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			tmp16_val2.uval = bit[instruction.parts.dest_reg];
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ZBXB:			// 	        0x9d  --  Zero Bit in Memory (Short-Indexed} and Branch if Nonzero   
			tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val2.uval & tmp16_val1.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O(false);
			SET_CC_C(false);
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_TBXB:			// 	        0x9e  --  Test Bit in Memory (Short-Indexed) and Branch if One   
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_O(false);
			SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_C, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_CBXB:			// 	        0x9f  --  CBXB  --  Compare Bit and Memory (Short-Indexed) and Branch     
			tmp16_val1.uval = bit[instruction.parts.dest_reg];
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else if (TEST_CC_LT) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;


		case  OP_MPM:			// 	        0xa0  --  MPM  --  Multiply Register by Memory        
			GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval );
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest | 0x01);
			}
			else {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
			}
			tmp32_val1.sval = tmp16_val1.sval;
			tmp32_val2.sval = tmp16_val2.sval;
			tmp32_val3.sval = tmp32_val1.sval * tmp32_val2.sval;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
			}
			else {
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			}
			SET_CC_N(ISVAL32_NEG(tmp32_val3));
			SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
			SET_CC_O(false);
			SET_CC_C(((tmp32_val3.uval & 0xffff8000) == 0xffff8000) || ((tmp32_val3.uval & 0xffff8000) == 0x00000000));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_DVM:			// 	        0xa1  --  DVM  --  Divide Register by Memory        
			if ( ISREGNUM_DOUBLE(GET_DESTINATION_REGISTER_NUMB) ) {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval );
				tmp32_val2.sval = tmp16_val2.sval;	// sign extend.
				if (tmp32_val2.sval != 0) {
					tmp32_val3.sval = tmp32_val1.sval / tmp32_val2.sval;	// quotient
					tmp32_val6.sval = tmp32_val1.sval % tmp32_val2.sval;	// remainder
				}
				else {
					tmp32_val3.uval = 0;
					tmp32_val6.uval = 0;
				}
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);		// low 16 bits of quotient.
				tmp16_val6.uval = (SIMJ_U16)(tmp32_val6.uval & 0x0000ffff);		// low 16 bits of remainder.
				tmp_instr_dest = (GET_DESTINATION_REGISTER_NUMB & 0x00e);		// must be even.
				SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val6.uval);		// remainder
				SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);		// quotient
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				// TODO: DVM OR IN O if divide by 0
				SET_CC_O(!(((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000)));	// not a 16 bit result
				SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			else {
				ILLEGAL_INSTRUCTION;
			}
			break;

		case  OP_MPRD_MPMD:		//        0xa2 --

			switch (instruction.parts.dest_reg & 0x1) {
				case 0:				//  --  MPRD  --  Multiply Double-Register by Double- Register
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
					if ISREGNUM_QUAD(tmp_instr_dest) {
						tmp32_val2.uval = GET_REGISTER_VALUE_DOUBLE((instruction.parts.dest_reg | 0x02) >> 1);
					}
					else {
						tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					}
					tmp64_val1.sval = tmp32_val1.sval;		// sign extend to 64 bits.
					tmp64_val2.sval = tmp32_val2.sval;		// sign extend to 64 bits.
					tmp64_val3.sval = tmp64_val1.sval * tmp64_val2.sval;
					if ISREGNUM_QUAD(tmp_instr_dest) {
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					}
					else {
						tmp32_val3.uval = (SIMJ_U32)(tmp64_val3.uval & 0x00000000ffffffff);
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					}
					SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
					SET_CC_N(ISVAL64_NEG(tmp64_val3));
					SET_CC_O(false);
					SET_CC_C(((tmp64_val3.uval & 0xffffffff80000000) == 0xffffffff80000000) || ((tmp64_val3.uval & 0xffffffff80000000) == 0x0000000000000000));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				case 1:				//  --  MPMD  --  Multiply Double-Register by Memory Double word      
					GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val1.uval);
					tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x0e;
					if ISREGNUM_QUAD(tmp_instr_dest) {
						tmp32_val2.uval = GET_REGISTER_VALUE_DOUBLE((tmp_instr_dest | 0x02 ) >> 1);
					}
					else {
						tmp32_val2.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					}
					tmp64_val1.sval = tmp32_val1.sval;		// sign extend to 64 bits.
					tmp64_val2.sval = tmp32_val2.sval;		// sign extend to 64 bits.
					tmp64_val3.sval = tmp64_val1.sval * tmp64_val2.sval;
					// -------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, "\n %s, instr: 0x%04x, op1: 0x%16llx, op2: 0x%16llx, res: 0x%16llx,",
					// 			&junkxx[0], instruction.all, tmp64_val1.uval, tmp64_val2.uval, tmp64_val3.uval);
					// disp_cur_reg(stderr);
					// -------END DEBUG
					if ISREGNUM_QUAD(tmp_instr_dest) {
						SET_DESTINATION_REGISTER_VALUE_QUAD(tmp64_val3.uval);
					}
					else {
						tmp32_val3.uval = (SIMJ_U32)(tmp64_val3.uval & 0x00000000ffffffff);
						SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
					}
					SET_CC_N(ISVAL64_NEG(tmp64_val3));
					SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
					SET_CC_O(false);
					SET_CC_C(((tmp64_val3.uval & 0xffffffff80000000) == 0xffffffff80000000) || ((tmp64_val3.uval & 0xffffffff80000000) == 0x0000000000000000));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
			}
			break;

		case  OP_DVRD_DVMD:		//        0xa3
			switch (instruction.parts.dest_reg & 0x1) {

				case 0:				//  --  DVRD  --  Divide Quad-Register by Double-Register        
					if ( ISREGNUM_QUAD(GET_DESTINATION_REGISTER_NUMB) && ISREGNUM_DOUBLE(GET_SOURCE_REGISTER_NUMB) ) {
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
						tmp64_val2.sval = tmp32_val2.sval;		// sign extend
						if (tmp64_val2.sval != 0) {		// dont divide by zero.
							tmp64_val3.sval = tmp64_val1.sval / tmp32_val2.sval;	// quotient
							tmp64_val6.sval = tmp64_val1.sval % tmp32_val2.sval;	// remainder
						}
						else {
							tmp64_val3.uval = 0;
							tmp64_val6.uval = 0;
						}
						tmp32_val3.uval = (SIMJ_U32)(tmp64_val3.uval & 0x00000000ffffffff);		// low 32 bits of quotient.
						tmp32_val6.uval = (SIMJ_U32)(tmp64_val6.uval & 0x00000000ffffffff);		// low 32 bits of remainder.
						tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB_DOUBLE & 0x000e;			// must be even
						SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest, tmp32_val6.uval);				// remainder
						SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest + 1, tmp32_val3.uval);			// quotient
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						// TODO: DVRD OR IN O if divide by 0
						SET_CC_O(!(((tmp64_val3.uval & 0xffffffff80000000) == 0) || ((tmp64_val3.uval & 0xffffffff80000000) == 0xffffffff80000000)));	// not a 32 bit result
						SET_CC_C(ISVAL64_ZERO(tmp64_val2));		// divide by zero
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						ILLEGAL_INSTRUCTION;
					}
					break;

				case 1:				//  --  DVMD  --  Divide Quad-Register by Memory Doubleword       
					if (ISREGNUM_QUAD(GET_DESTINATION_REGISTER_NUMB & 0xfffe) ) {
						tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_QUAD;
						GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val2.uval );
						tmp64_val2.sval = tmp32_val2.sval;		// sign extend
						if (tmp64_val2.sval != 0) {		// dont divide by zero.
							tmp64_val3.sval = tmp64_val1.sval / tmp32_val2.sval;	// quotient
							tmp64_val6.sval = tmp64_val1.sval % tmp32_val2.sval;	// remainder
						}
						else {
							tmp64_val3.uval = 0;
							tmp64_val6.uval = 0;
						}
						tmp32_val3.uval = (SIMJ_U32)(tmp64_val3.uval & 0x00000000ffffffff);		// low 32 bits of quotient.
						tmp32_val6.uval = (SIMJ_U32)(tmp64_val6.uval & 0x00000000ffffffff);		// low 32 bits of remainder.
						tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB_DOUBLE & 0x00e;			// must be even
						SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest, tmp32_val6.uval);				// remainder
						SET_REGISTER_VALUE_DOUBLE(tmp_instr_dest + 1, tmp32_val3.uval);			// quotient
						SET_CC_Z(ISVAL64_ZERO(tmp64_val3));
						SET_CC_N(ISVAL64_NEG(tmp64_val3));
						// TODO: DVMD OR IN O if divide by 0
						SET_CC_O(!(((tmp64_val3.uval & 0xffffffff80000000) == 0) || ((tmp64_val3.uval & 0xffffffff80000000) == 0xffffffff80000000)));	// not a 32 bit result
						SET_CC_C(ISVAL64_ZERO(tmp64_val2));		// divide by zero
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					}
					else {
						ILLEGAL_INSTRUCTION;
					}
					break;
			}
			break;

		case  OP_LFM:			// 	        0xa4
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			GET_MEMORY_DIRECT_ADDR(tmp16_val1.uval);
			tmp16_val5.uval = 0;	// temp register to check for all zero.
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
					tmp16_val1.uval++;
					if (j == tmp_instr_dest) {
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
					}
					tmp16_val5.uval |= tmp16_val2.uval;
					SET_REGISTER_VALUE(j, tmp16_val2.uval);
				}
			}
			else { // if (tmp_instr_dest >= 1) {
				for (j = tmp_instr_dest; j < 8; j++) {
					GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
					tmp16_val1.uval++;
					if (j == tmp_instr_dest) {
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
					}
					tmp16_val5.uval |= tmp16_val2.uval;
					SET_REGISTER_VALUE(j, tmp16_val2.uval);
				}
			}
			SET_CC_Z(ISVAL16_ZERO(tmp16_val5));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_SFM:			// 	        0xa5
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			GET_MEMORY_DIRECT_ADDR(tmp16_val1.uval);
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					SET_MEMORY_VALUE_OM(tmp16_val1.uval, GET_REGISTER_VALUE(j));
					tmp16_val1.uval++;
				}
			}
			else {  // if (tmp_instr_dest >= 1) {
				for (j = tmp_instr_dest; j < 8; j++) {
					SET_MEMORY_VALUE_OM(tmp16_val1.uval, GET_REGISTER_VALUE(j));
					tmp16_val1.uval++;
				}
			}
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_HHI_HNH:			// 	        0xa6
			// HNH  --  Hop on Magnitude Not Higher Condition      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(TEST_CC_NH, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			// HHI  --  Hop on Magnitude Higher Condition        
			else {
				CONDITIONAL_BRANCH(TEST_CC_HI, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_AUGA7:			//         0xa7
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			switch (tmp_instr_dest) {
				case 0:		// --  BLNS	Branch and Link on Condition Code N Set
					do_branch = TEST_CC_N;
					break;
				case 1:		// --  BLZS	Branch and Link on Condition Code Z Set    
					do_branch = TEST_CC_Z;
					break;
				case 2:		// --  BLOS	Branch and Link on Condition Code O Set    
					do_branch = TEST_CC_O;
					break;
				case 3:		// --  BLCS	Branch and Link on Condition Code C Set    
					do_branch = TEST_CC_C;
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
					do_branch = TEST_CC_NOT_N;
					break;
				case 9:		// --  BLZR	Branch and Link on Condition Code Z Reset    
					do_branch = TEST_CC_NOT_Z;
					break;
				case 10:		// --  BLOR	Branch and Link on Condition Code O Reset    
					do_branch = TEST_CC_NOT_O;
					break;
				case 11:		// --  BLCR	Branch and Link on Condition Code C Reset    
					do_branch = TEST_CC_NOT_C;
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
				SET_SOURCE_REGISTER_VALUE(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				SET_NEXT_PROGRAM_COUNTER( tmp_new_prog_count );
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			break;

		case  OP_HNS_HNR:			//         0xa8
			// HNR  --  Hop on Condition Code N Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(TEST_CC_NOT_N, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			// HNS  --  Hop on Condition Code N Set      
			else {
				CONDITIONAL_BRANCH(TEST_CC_N, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_HZS_HZR:			//         0xa9
			// HZR  --  Hop on Condition Code Z Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(  TEST_CC_NOT_Z, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			// HZS  --  Hop on Condition Code Z Set      
			else {
				CONDITIONAL_BRANCH( TEST_CC_Z, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_HOS_HOR:			//         0xaa
			// HOR  --  Hop on Condition Code O Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(TEST_CC_NOT_O, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			// HOS  --  Hop on Condition Code O Set      
			else {
				CONDITIONAL_BRANCH(TEST_CC_O, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_HCS_HCR:			//         0xab
			// HCR  --  Hop on Condition Code C Reset      
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH(TEST_CC_NOT_C, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			// HCS  --  Hop on Condition Code C Set      
			else {
				CONDITIONAL_BRANCH(TEST_CC_C, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_HLS_HGE:			//         0xac
			//       HGE	Hop oh Greater than or Equal Condition     
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH( TEST_CC_GE, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			//  HLS	Hop on Less Than Condition            
			else {
				CONDITIONAL_BRANCH( TEST_CC_LT, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_HLE_HGT:			//         0xad
			//       HGT	Hop on Greater Than Condition       
			if (instruction.all & 0x0080) {
				CONDITIONAL_BRANCH( TEST_CC_GT, GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			//       HLE	Hop on Less Than or Equal Condition     
			else {
				CONDITIONAL_BRANCH( TEST_CC_LE , GET_NEXT_PROGRAM_COUNTER_HOP, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_LBX:			// 	        0xae  --  Load Byte from Memory (Byte-Indexed)       
			tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
			if (tmp_instr_src & 0x0001) {
				ILLEGAL_INSTRUCTION;
			}
			else {

				// -------- calculate memory address 
				// TODO: LBX make this a macro...
				tmp16_val1.uval = GET_REGISTER_VALUE(tmp_instr_src);		//  base address
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_src + 1);	//  word offset and byte numb
				temp_bit = tmp16_val2.uval & 0x0001;						// byte numb 0=high/first or 1=low/second
				tmp16_val2.sval >>= 1;										// get word offset
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;		// complete word address
#if DEBUG_LBX >=2
				memcpy(&temp16_val10, &tmp16_val5.uval, sizeof(temp16_val10));
				fprintf(stderr,"\n after memcpy dest 0x%04x, src 0x%04x\n", temp16_val10, tempu16_val5);
				temp16_val10 = temp16_val10 >> 1;
				tmp32_val1.uval = tmp16_val1.uval;
				temp32_addr_calc = tmp32_val1.sval + (SIMJ_S32)temp16_val10;
				fprintf(stderr,"\n addr calc base 0x%08x, offset 0x%04x, calc 0x%08x\n", tempu32_val1, temp16_val10, temp32_addr_calc);
				tmp16_val3.uval = (SIMJ_U16)(temp32_addr_calc & 0x0000ffff);
				fprintf(stderr,"\n to 16 bit 0x%04x, calc 0x%08x\n", tmp16_val3.uval, temp32_addr_calc);
#endif
				// --------read memory value
				GET_MEMORY_VALUE_OM(tmp16_val4.uval, tmp16_val3.uval);
				if (temp_bit == 0) {
					tmp16_val5.uval = (tmp16_val4.uval >> 8 ) & 0x00ff;
				}
				else {
					tmp16_val5.uval = (tmp16_val4.uval & 0x00ff);
				}
				// -- STORE BYTE IN RIGHT SIDE OF REG (0 in left half )...
				SET_DESTINATION_REGISTER_VALUE(tmp16_val5.uval);
				SET_CC_CHAR(tmp16_val5.uval);
				if (gbl_verbose_debug == debugging_on) {
#if DEBUG_LBX >=1
					fprintf(stderr, " LBX pc: 0x%04x  inst: 0x%04x - from mem addr 0x%04x, valu 0x%04x, rx 0x%04x, rx+1 0x%04x\n",
							program_counter, instruction.all, tmp16_val3.uval, tmp16_val2.uval, tmp16_val1.uval, tmp16_val5.uval);
#endif
					fprintf(stderr, " LBX pc: 0x%04x  inst: 0x%04x, calc addr: 0x%04x, value: 0x%04x, base addr: 0x%04x, off: 0x%04x\n",
						program_counter, instruction.all, tmp16_val3.uval, tmp16_val4.uval, tmp16_val1.uval, tmp16_val2.uval);
				}
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;

		case  OP_SBX:			// 	        0xaf  --  Store Byte in Memory (Byte-Indexed)       
			tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
			if (tmp_instr_src & 0x0001) {
				ILLEGAL_INSTRUCTION;
			}
			else {
				// TODO: SBX Fix all this !
				// TODO: SBX make this a macro...
				tmp16_val1.uval = GET_REGISTER_VALUE(tmp_instr_src);		//  base address
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_src + 1);	//  word offset and byte numb
				temp_bit = tmp16_val2.uval & 0x0001;						// byte numb 0=high/first or 1=low/second
				tmp16_val2.sval >>= 1;										// get word offset
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;		// complete word address
#if DEBUG_SBX >= 2
				memcpy(&temp16_val10, &tmp16_val5.uval, sizeof(temp16_val10));
				fprintf(stderr,"\n after memcpy dest 0x%04x, src 0x%04x\n", temp16_val10, tempu16_val5);
				temp_bit = temp16_val10 & 0x0001;
				temp16_val10 = temp16_val10 >> 1;
				tmp32_val1.uval = tmp16_val1.uval;
				temp32_addr_calc = tmp32_val1.sval + (SIMJ_S32)temp16_val10;
				fprintf(stderr,"\n addr calc base 0x%08x, offset 0x%04x, calc 0x%08x\n", tempu32_val1, temp16_val10, temp32_addr_calc);
				tmp16_val3.uval = (SIMJ_U16)(temp32_addr_calc & 0x0000ffff);
				fprintf(stderr,"\n to 16 bit 0x%04x, calc 0x%08x\n", tmp16_val3.uval, temp32_addr_calc);
#endif
				tmp16_val4.uval = GET_DESTINATION_REGISTER_VALUE & 0x00ff;
				GET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val3.uval);
				if (temp_bit != 0) {
					tmp16_val5.uval = (tmp16_val5.uval & 0xff00) | (tmp16_val4.uval);
				}
				else {
					tmp16_val5.uval = (tmp16_val5.uval & 0x00ff) | (tmp16_val4.uval<<8);
				}
				SET_MEMORY_VALUE_OM(tmp16_val3.uval, tmp16_val5.uval);
				if (gbl_verbose_debug == debugging_on)
#if DEBUG_SBX >= 1
					fprintf(stderr, " Set byte pc: 04%04x  inst: 04%04x - in mem addr 0x%04x, valu 0x%04x, rx 0x%04x, rx+1 0x%04x\n",
							program_counter, instruction.all, tmp16_val3.uval, tmp16_val2.uval, tmp16_val1.uval, tmp16_val5.uval);
#endif
					fprintf(stderr, " SBX pc: 0x%04x  inst: 0x%04x, calc addr: 0x%04x, value: 0x%04x, base addr: 0x%04x, off: 0x%04x\n",
						program_counter, instruction.all, tmp16_val3.uval, tmp16_val5.uval, tmp16_val1.uval, tmp16_val2.uval);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;


		case  OP_MPS:			// 	        0xb0  --  MPS  --  Multiply Register by Memory (Short-Displaced)       
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest | 0x01);
			}
			else {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
			}
			tmp32_val1.sval = tmp16_val1.sval;
			tmp32_val2.sval = tmp16_val2.sval;
			tmp32_val3.sval = tmp32_val1.sval * tmp32_val2.sval;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
			}
			else {
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			}
			SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
			SET_CC_N(ISVAL32_NEG(tmp32_val3));
			SET_CC_O(false);
			SET_CC_C(((tmp32_val3.uval & 0xffff8000) == 0xffff8000) || ((tmp32_val3.uval & 0xffff8000) == 0x00000000));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_DVS:			// 	        0xb1  --  DVS  --  Divide Register by Memory (Short-Displaced)       
			if ( ISREGNUM_DOUBLE(GET_DESTINATION_REGISTER_NUMB) ) {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp32_val2.sval = tmp16_val2.sval;
				if (tmp32_val2.sval != 0) {
					tmp32_val3.sval = tmp32_val1.sval / tmp32_val2.sval;	// quotient
					tmp32_val6.sval = tmp32_val1.sval % tmp32_val2.sval;	// remainder
				}
				else {
					tmp32_val3.uval = 0;
					tmp32_val6.uval = 0;
				}
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);		// low 16 bits of quotient.
				tmp16_val6.uval = (SIMJ_U16)(tmp32_val6.uval & 0x0000ffff);		// low 16 bits of remainder.
				tmp_instr_dest = (GET_DESTINATION_REGISTER_NUMB & 0x00e);		// must be even.
				SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val6.uval);		// remainder
				SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);		// quotient
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				// TODO: DVS OR IN O if divide by 0
				SET_CC_O(!(((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000)));	// not a 16 bit result
				SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				ILLEGAL_INSTRUCTION;
			}
			break;

		case  OP_SCCC:			// 	        0xb2  - SCCC  --  Select Current Condition Codes in PSD      
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_CC_N( (tmp16_val1.uval & 0x0008) != 0);
			SET_CC_Z( (tmp16_val1.uval & 0x0004) != 0);
			SET_CC_O( (tmp16_val1.uval & 0x0002) != 0);
			SET_CC_C( (tmp16_val1.uval & 0x0001) != 0)
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		// -------- the only way to get here is if the result of an EXI or EXR instruction is an EXI
		// -------- instruction.
		case  OP_EXR:			// 	        0xb3
			EXI_EXR_INSTRUCTION;
			break;

		case  OP_LFS:			// 	        0xb4
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_DISPLACED;
				tmp16_val5.uval = 0;	// temp register to check for all zero.
				if (tmp_instr_dest > 7) {
					for (j = tmp_instr_dest; j < 16; j++) {
						GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
						tmp16_val1.uval++;
						if (j == tmp_instr_dest) {
							SET_CC_N(ISVAL16_NEG(tmp16_val2));
						}
						tmp16_val5.uval |= tmp16_val2.uval;
						SET_REGISTER_VALUE(j, tmp16_val2.uval);
					}
				}
				else { // if (tmp_instr_dest >= 1) {
					for (j = tmp_instr_dest; j < 8; j++) {
						GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
						tmp16_val1.uval++;
						if (j == tmp_instr_dest) {
							SET_CC_N(ISVAL16_NEG(tmp16_val2));
						}
						tmp16_val5.uval |= tmp16_val2.uval;
						SET_REGISTER_VALUE(j, tmp16_val2.uval);
					}
				}
				SET_CC_Z(ISVAL16_ZERO(tmp16_val5));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_SFS:			// 	        0xb5
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_DISPLACED;
				if (tmp_instr_dest > 7) {
					for (j = tmp_instr_dest; j < 16; j++) {
						SET_MEMORY_VALUE_OM(tmp16_val1.uval, GET_REGISTER_VALUE(j));
						tmp16_val1.uval++;
					}
				}
				else { 
					for (j = tmp_instr_dest; j < 8; j++) {
						SET_MEMORY_VALUE_OM(tmp16_val1.uval, GET_REGISTER_VALUE(j));
						tmp16_val1.uval++;
					}
				}
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_IRM:			// 	        0xb6  --  Interchange Register and Memory        
			GET_MEMORY_DIRECT_ADDR(tmp_addr_direct);
			GET_MEMORY_VALUE_OM(tmp16_val1.uval, tmp_addr_direct);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_MEMORY_VALUE_OM(tmp_addr_direct, tmp16_val2.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_IRR:			// 	        0xb7  --  Interchange Register and Register    -    
			tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
			SET_SOURCE_REGISTER_VALUE(GET_DESTINATION_REGISTER_VALUE);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_MPX:			// 	        0xb8 --  MPX  --  Multiply Register by Memory (Short-Indexed)       
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest | 0x01);
			}
			else {
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
			}
			tmp32_val1.sval = tmp16_val1.sval;
			tmp32_val2.sval = tmp16_val2.sval;
			tmp32_val3.sval = tmp32_val1.sval * tmp32_val2.sval;
			if ISREGNUM_DOUBLE(tmp_instr_dest) {
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
			}
			else {
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			}
			SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
			SET_CC_N(ISVAL32_NEG(tmp32_val3));
			SET_CC_O(false);
			SET_CC_C(((tmp32_val3.uval & 0xffff8000) == 0xffff8000) || ((tmp32_val3.uval & 0xffff8000) == 0x00000000));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_DVX:			// 	        0xb9  --  DVX  --  Divide Register by Memory (Short-Indexed)       
			if ( ISREGNUM_DOUBLE(GET_DESTINATION_REGISTER_NUMB) ) {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
				tmp32_val2.sval = tmp16_val2.sval;
				if (tmp32_val2.sval != 0) {
					tmp32_val3.sval = tmp32_val1.sval / tmp32_val2.sval;	// quotient
					tmp32_val6.sval = tmp32_val1.sval % tmp32_val2.sval;	// remainder
				}
				else {
					tmp32_val3.uval = 0;
					tmp32_val6.uval = 0;
				}
				tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);		// low 16 bits of quotient.
				tmp16_val6.uval = (SIMJ_U16)(tmp32_val6.uval & 0x0000ffff);		// low 16 bits of remainder.
				tmp_instr_dest = (GET_DESTINATION_REGISTER_NUMB & 0x00e);		// must be even.
				SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val6.uval);		// remainder
				SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);		// quotient
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				// TODO: DVX OR IN O if divide by 0
				SET_CC_O(!(((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000)));	// not a 16 bit result
				SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				ILLEGAL_INSTRUCTION;
			}
			break;

#define STACK_OFF_LSA	-1
#define STACK_OFF_CSP	0
#define STACK_OFF_HSA	1
#define STACK_OFF_OVL_JUMP 2
#define STACK_OFF_SAV_R1 3


		case  OP_PLM:			// 	        0xba
			GET_MEMORY_DIRECT_ADDR(tmp16_STK_ADDR);								// address of stack definition + 1
			GET_MEMORY_VALUE_OM(tmp16_STK_LSA, tmp16_STK_ADDR + STACK_OFF_LSA);	// (LSA) lowest address of stack
			GET_MEMORY_VALUE_OM(tmp16_STK_CSP, tmp16_STK_ADDR + STACK_OFF_CSP);	// (CSP) current stack pointer.
			GET_MEMORY_VALUE_OM(tmp16_STK_HSA, tmp16_STK_ADDR + STACK_OFF_HSA);	// (HSA) highest address of stack
			GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp16_val4.uval );			// contains NV, NR-1
			tmp16_STK_NW = tmp16_val4.uval & 0x00ff;					// NV also called NW
			tmp16_STK_NR = ((tmp16_val4.uval & 0xf00) >> 8) + 1;		// NR

			tmp16_val7.uval = tmp16_STK_HSA - tmp16_STK_CSP;	// number of words stored on stack.

#if DEBUG_PLM >= 1
			util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
			fprintf(stderr, "\n %s inst: 0x%04x, stack addr: 0x%04x  \n", junkxx, instruction.all, tmp16_STK_ADDR);
			fprintf(stderr, "        NV numb vals:       0x%04x  \n", tmp16_STK_NW);
			fprintf(stderr, "        NR numb reg:        0x%04x  \n", tmp16_STK_NR);
			fprintf(stderr, "        LSA:     0x%04x\n", tmp16_STK_LSA);
			fprintf(stderr, "        CSP:     0x%04x\n", tmp16_STK_CSP);
			fprintf(stderr, "        HSA:     0x%04x\n", tmp16_STK_HSA);
			fprintf(stderr, "        OV ADDR: 0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_OVL_JUMP));
			fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_SAV_R1));
			fprintf(stderr, "        stored:  0x%04x\n", tmp16_val7.uval);
			disp_cur_reg(stderr);
#endif


			// -------- stack underflow
			//           NV > words on stack                     CSP < LSP
			if ( (tmp16_STK_NW > tmp16_val7.uval ) || (tmp16_STK_CSP < tmp16_STK_LSA) ) {
				SET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_SAV_R1, GET_REGISTER_VALUE(1));		// save r1
				SET_REGISTER_VALUE(1, program_counter);								// set r1 to pc
				// (CSP+NV) - HSA
				tmp16_val1.uval = (tmp16_STK_CSP + tmp16_STK_NW);
				tmp16_val2.uval = tmp16_STK_HSA;
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;	// (CSP+NV) - HSA for cc determination.
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				skip_interrupt_determination = true;		// next instruction not interruptable.
#if DEBUG_PLM >= 1
				fprintf(stderr, "        *** UNDERFLOW ***  \n");
				fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_SAV_R1));
				disp_cur_reg(stderr);
#endif

				GET_MEMORY_VALUE_OM(tmp_new_prog_count, tmp16_STK_ADDR + STACK_OFF_OVL_JUMP);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);		// jump to over/under flow routine
			}
			// -------all is good, process stack pull
			else {
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				tmp16_val8.uval = 0;	// for determining if all are zero.
				// --------if the number of registers is greater that the number of values
				// --------set the register value to 0.
				// --------NO -- PULLING BEYOND THE HIGH STACK ADDRESS IS EXPECTED.!!!
				for (j = 0; j < tmp16_STK_NR; j++) {
					GET_MEMORY_VALUE_OM(tmp16_val9.uval, tmp16_STK_CSP + j);
					if (j == 0) {
						SET_CC_N(ISVAL16_NEG(tmp16_val9));
					}
					tmp16_val8.uval |= tmp16_val9.uval;
					SET_REGISTER_VALUE( (tmp_instr_dest + j) & 0x000f, tmp16_val9.uval);
				}
				tmp16_STK_CSP += tmp16_STK_NW;		// new bottom of stack pointer
				SET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_CSP, tmp16_STK_CSP);	// store new stack pointer
				SET_CC_Z(ISVAL16_ZERO(tmp16_val8));
				SET_CC_O(false);
				SET_CC_C(false);
#if DEBUG_PLM >= 1
				fprintf(stderr, "    NEW CSP:     0x%04x\n", tmp16_STK_CSP);
				disp_cur_reg(stderr);
#endif
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;



			// SIMJ_U16		tmp16_STK_ADDR;
			// SIMJ_U16		tmp16_STK_HSA;
			// SIMJ_U16		tmp16_STK_CSP;
			// SIMJ_U16		tmp16_STK_LSA;
			// SIMJ_U16		tmp16_STK_NW;		// numb of words to allocate, same as NV
			// SIMJ_U16		tmp16_STK_NR;		// numb of registers to copy.

		case  OP_PSM:			// 	        0xbb
			GET_MEMORY_DIRECT_ADDR(tmp16_val1.uval);					// address of stack definition + 1
			GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval - 1);	// (LSA) lowest address of stack
			GET_MEMORY_VALUE_OM(tmp16_val3.uval, tmp16_val1.uval );	// (CSP) current stack pointer.
			GET_MEMORY_VALUE_OM(tmp16_val9.uval, tmp16_val1.uval + 1);	// (HSA) highest address of stack
			GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp16_val4.uval );			// contains NV, NR-1
			tmp16_val5.uval = tmp16_val4.uval & 0x00ff;					// NV  (oddly this field is 9 bits???)
			tmp16_val6.uval = ((tmp16_val4.uval & 0xf00) >> 8) + 1;		// NR
			//if (tmp16_val6.uval > tmp16_val5.uval) {
			//	tmp16_val6.uval = tmp16_val5.uval;
			//}
			//                  CSP - LSA
			tmp16_val7.uval = tmp16_val3.uval - tmp16_val2.uval;		// number of words left

#if DEBUG_PSM >= 1
			util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
			fprintf(stderr, "\n %s inst: 0x%04x, stack addr: 0x%04x  \n", junkxx, instruction.all, tmp16_val1.uval );
			fprintf(stderr, "        NW numb words:      0x%04x  \n", tmp16_val5.uval);
			fprintf(stderr, "        NR numb reg:        0x%04x  \n", tmp16_val6.uval);
			fprintf(stderr, "        LSA:     0x%04x\n", tmp16_val2.uval);
			fprintf(stderr, "        CSP:     0x%04x\n", tmp16_val3.uval);
			fprintf(stderr, "        HSA:     0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval+1));
			fprintf(stderr, "        OV ADDR: 0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval+2));
			fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval+3));
			fprintf(stderr, "        left:    0x%04x\n", tmp16_val7.uval);
			disp_cur_reg(stderr);
#endif


			// -------- stack overflow
			//                 NV  >   words left                      CSP > HSA 
			if ( ( tmp16_val5.uval > tmp16_val7.uval ) || (tmp16_val3.uval > tmp16_val9.uval) ) {
				SET_MEMORY_VALUE_OM(tmp16_val1.uval + 3, GET_REGISTER_VALUE(1));		// save r1
				SET_REGISTER_VALUE(1, program_counter );								// set r1 to pc
				// (CSP-NV) - LSA
				tmp16_val8.uval = (tmp16_val3.uval - tmp16_val5.uval);
				tmp16_val7.uval = tmp16_val8.uval - tmp16_val2.uval;	// CSP-NW for cc determination.
				SET_CC_N(ISVAL16_NEG(tmp16_val7));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val7));
				SET_CC_O_SUB(tmp16_val8, tmp16_val2, tmp16_val7);
				SET_CC_C_SUB(tmp16_val8, tmp16_val2, tmp16_val7);
				skip_interrupt_determination = true;		// next instruction not interruptable.
#if DEBUG_PSM >= 1
				fprintf(stderr, "        *** OVERFLOW ***  \n");
				fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval + 3));
				disp_cur_reg(stderr);
#endif
				GET_MEMORY_VALUE_OM(tmp_new_prog_count, tmp16_val1.uval + 2)
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);		// jump to over/under flow routine
			}
			 // -------all is good, process stack push
			else {
				tmp16_val3.uval -= tmp16_val5.uval;		// new bottom of stack pointer
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				// ------- This doesn't make sense but, if the number of registers is greater
				// ------- than the number of words allocted and this violates the high stack
				// ------- address, pushing the registers to memory CONTINUES.  WHY???
				// ------- The diagnostic expects this.
				for (j = 0; j < tmp16_val6.uval; j++) {
					tmp16_val8.uval = GET_REGISTER_VALUE((tmp_instr_dest + j) & 0x000f);
					//if (tmp16_val3.uval + j <= tmp16_val9.uval) {
					SET_MEMORY_VALUE_OM(tmp16_val3.uval + j, tmp16_val8.uval);
#if DEBUG_PSM >= 1
					fprintf(stderr, "    pushed mem addr: 0x%04x, val:  0x%04x\n", tmp16_val3.uval + j, tmp16_val8.uval);
#endif
					//}
				}
				SET_MEMORY_VALUE_OM(tmp16_val1.uval, tmp16_val3.uval);	// store new stack pointer
#if DEBUG_PSM >= 1
				fprintf(stderr, "    NEW CSP:     0x%04x\n", tmp16_val3.uval);
#endif
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;

		case  OP_LFX:			// 	        0xbc
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
			tmp16_val5.uval = 0;
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
					tmp16_val1.uval++;
					if (j == tmp_instr_dest) {
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
					}
					tmp16_val5.uval |= tmp16_val2.uval;
					SET_REGISTER_VALUE( j, tmp16_val2.uval);
				}
			}
			else { // if (tmp_instr_dest >= 1) {
				for (j = tmp_instr_dest; j < 8; j++) {
					GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val1.uval);
					tmp16_val1.uval++;
					if (j == tmp_instr_dest) {
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
					}
					tmp16_val5.uval |= tmp16_val2.uval;
					SET_REGISTER_VALUE(j, tmp16_val2.uval);
				}
			}
			SET_CC_Z(ISVAL16_ZERO(tmp16_val5));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_SFX:			// 0xbd  --  SFX  --  Store File in Memory (Short-Indexed)       
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					SET_MEMORY_VALUE_OM(tmp16_val1.uval, GET_REGISTER_VALUE(j));
					tmp16_val1.uval++;
				}
			}
			else { 
				for (j = tmp_instr_dest; j < 8; j++) {
					SET_MEMORY_VALUE_OM(tmp16_val1.uval, GET_REGISTER_VALUE(j));
					tmp16_val1.uval++;
				}
			}
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_LDAM_LDVM:		//        0xbe
			switch (instruction.parts.src_reg & 0x0001) {
			case 0:			// LDAM
				if (IS_PRIV_MODE) {
					tmp16_val1.uval = GET_REGISTER_VALUE(instruction.parts.src_reg);
					tmp16_val2.uval = GET_REGISTER_VALUE(instruction.parts.src_reg + 1);
					tmp32_val1.sval = tmp16_val1.sval;				// get Rx displacement and sign extend.
					tmp32_val2.uval = (tmp16_val2.uval & 0x1fff);	// base address page
					tmp32_val2.uval <<= 8;
					tmp32_val3.sval = tmp32_val1.sval + tmp32_val2.sval;	// calc address
					// TODO: LDAM update address calc.  Do wrap around? or what.
#ifdef SIMJ_SIM_CPU_1MEGMAX 
					// kludge to force max found memory to be 1 meg word
					if (tmp32_val3.uval >= 0x100000) {
						tmp16_val3.uval = 0;
						// fprintf(stderr, " LDAM above 0x10 0000\n");
					}
					else {
						tmp16_val3.uval = GET_MEMORY_VALUE_ABS(tmp32_val3.uval);
					}
#else
					tmp16_val3.uval = GET_MEMORY_VALUE_ABS(tmp32_val3.uval);
#endif
#if DEBUG_LDAM >= 1
					fprintf(stderr, " LDAM 0x%04x, R: 0x%04x, Rv1: 0x%04x, addr: 0x%08x, value: : 0x%04x\n",
							instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp32_val3.uval, tmp16_val3.uval);
#endif
					SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
					SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
					SET_CC_N(ISVAL16_NEG(tmp16_val3));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				}
				else {
					PRIV_INSTR_TRAP;
				}
			case 1:			// LDVM
				if (IS_PRIV_MODE) {
					// -------- get double source register value.
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					// -------- break apart into MIAP - map memory page, VP - word in map, PW - word in page
					// -------- MIAP absolute address
					tmp32_val2.uval = (SIMJ_U32)((tmp32_val1.uval & 0x00001fff) << 8);
					// -------- VP
					tmp32_val3.uval = (SIMJ_U32)((tmp32_val1.uval >> 24) & 0x000000ff);
					// -------- PW
					tmp32_val4.uval = (SIMJ_U32)((tmp32_val1.uval >> 16) & 0x000000ff);
					// -------- Read page map entry.
					// TODO: LDVM make macro GET_MEMORY_VALUE_ABS deal with absolute wrap around.
					tmp16_val5.uval = GET_MEMORY_VALUE_ABS( (tmp32_val2.uval + tmp32_val3.uval) & 0x001fffff);
					// -------- check access rights.
					tmp16_val8.uval = (tmp16_val5.uval >> 14) & 0x0003;
#if DEBUG_LDVM >= 1
					fprintf(stderr, " LDVM 0x%04x, R: 0x%08x, miap: 0x%08x, vp: 0x%08x, pw: 0x%08x, page entry: 0x%04x \n",
							instruction.all, tmp32_val1.uval, tmp32_val2.uval, tmp32_val3.uval, tmp32_val4.uval, tmp16_val5.uval);
#endif
					//if (tmp16_val8.uval == 0) {			// no access.
					//	SET_CC_N(false);
					//	SET_CC_Z(false);
					//	SET_CC_O(true);
					//	SET_CC_C(true);
					//	// --------DEBUG
					//	fprintf(stderr, "          no access \n");
					//	// --------END DEUBG
					//}
					//else {								// any read access.
					// -------- create absolute memory address.
					// -------- ABS Address
					tmp32_val6.uval = (SIMJ_U32)( ((SIMJ_U32)(tmp16_val5.uval & 0x1fff) << 8) | tmp32_val4.uval);
					// -------- read memory value
					tmp16_val7.uval = GET_MEMORY_VALUE_ABS(tmp32_val6.uval);
					// -------- destination register to memory
					// TODO: LDVM should value be set regardless of access
					SET_DESTINATION_REGISTER_VALUE(tmp16_val7.uval);
					SET_CC_N(ISVAL16_NEG(tmp16_val7));
					SET_CC_Z(ISVAL16_ZERO(tmp16_val7));
					if (tmp16_val8.uval == 0) {			// no access.
						SET_CC_O(true);
						SET_CC_C(true);
					}
					else if (tmp16_val8.uval != 3) {
						SET_CC_O(false);
						SET_CC_C(true);
					}
					else {
						SET_CC_O(false);
						SET_CC_C(false);
					}
#if DEBUG_LDVM >= 1
					fprintf(stderr, "          abs addr: 0x%08x, value: 0x%04x \n", tmp32_val6.uval, tmp16_val7.uval);
#endif
					//}
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;
			}
			break;

		case  OP_STAM_STVM:		//        0xbf
			switch (instruction.parts.src_reg & 0x0001) {
				case 0:			// STAM  --  Store Register into (Actual) Memory
					if (IS_PRIV_MODE) {
						tmp16_val1.uval = GET_REGISTER_VALUE(instruction.parts.src_reg);
						tmp16_val2.uval = GET_REGISTER_VALUE(instruction.parts.src_reg + 1);
						tmp32_val1.sval = tmp16_val1.sval;		// get Rx displacement and sign extend.
						tmp32_val2.uval = (tmp16_val2.uval & 0x1fff);
						tmp32_val2.uval <<= 8;
						tmp32_val3.sval = tmp32_val1.sval + tmp32_val2.sval;	// calc address
						tmp16_val3.uval = GET_DESTINATION_REGISTER_VALUE;
						SET_MEMORY_VALUE_ABS(tmp32_val3.uval, tmp16_val3.uval);
#if DEBUG_STAM >= 1						// --------DEBUG
						fprintf(stderr, " STAM 0x%04x, R: 0x%04x, Rv1: 0x%04x, addr: 0x%08x, value: : 0x%04x\n",
								instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp32_val3.uval, tmp16_val3.uval);
#endif
						// TODO: STAM Check to see if CC should be set.  NO
						// SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
						// SET_CC_N(ISVAL16_NEG(tmp16_val3));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						PRIV_INSTR_TRAP;
					}
					break;
				case 1:		// STVM  --  Store Register into Memory (Via Map Image)     
					if (IS_PRIV_MODE) {
						// -------- get double source register value.
						tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
						// -------- break apart into MIAP - map memory page, VP - word in map, PW - word in page
						// -------- MIAP absolute address
						tmp32_val2.uval = (SIMJ_U32)((tmp32_val1.uval & 0x00001fff) << 8);
						// -------- VP
						tmp32_val3.uval = (SIMJ_U32)((tmp32_val1.uval >> 24) & 0x000000ff);
						// -------- PW
						tmp32_val4.uval = (SIMJ_U32)((tmp32_val1.uval >> 16) & 0x000000ff);
						// -------- Read page map entry.
						// TODO: STVM make macro GET_MEMORY_VALUE_ABS deal with absolute wrap around.
						tmp16_val5.uval = GET_MEMORY_VALUE_ABS((tmp32_val2.uval + tmp32_val3.uval) & 0x001fffff);
#if DEBUG_STVM >= 1
						fprintf(stderr, " STVM 0x%04x, R: 0x%08x, miap: 0x%08x, vp: 0x%08x, pw: 0x%08x, page entry: 0x%04x \n",
								instruction.all, tmp32_val1.uval, tmp32_val2.uval, tmp32_val3.uval, tmp32_val4.uval, tmp16_val5.uval);
#endif
							// -------- check access rights.
						tmp16_val8.uval = (tmp16_val5.uval >> 14) & 0x0003;
						if (tmp16_val8.uval == 0) {
							SET_CC_N(false);
							SET_CC_Z(false);
							SET_CC_O(true);
							SET_CC_C(true);
							// --------DEBUG
#if DEBUG_STVM >= 1
							fprintf(stderr, "          STVM no access \n");
							// --------END DEUBG
#endif
						}
						else if (tmp16_val8.uval != 3) {
							SET_CC_N(false);
							SET_CC_Z(false);
							SET_CC_O(false);
							SET_CC_C(true);
							// --------DEBUG
#if DEBUG_STVM >= 1
							fprintf(stderr, "          STVM no write access \n");
							// --------END DEUBG
#endif
						}
						else {
							// -------- create absolute memory address.
							// -------- ABS Address
							tmp32_val6.uval = (SIMJ_U32)(((SIMJ_U32)(tmp16_val5.uval & 0x1fff) << 8) | tmp32_val4.uval);
							// -------- read destination register
							tmp16_val7.uval = GET_DESTINATION_REGISTER_VALUE;
							// -------- destination register to memory
							SET_MEMORY_VALUE_ABS(tmp32_val6.uval, tmp16_val7.uval);
							SET_CC_O(false);
							SET_CC_C(false);
#if DEBUG_STVM >= 1
							fprintf(stderr, "          abs addr: 0x%08x, value: 0x%04x \n", tmp32_val6.uval, tmp16_val7.uval);
#endif
						}
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						PRIV_INSTR_TRAP;
					}
					break;
			}
			break;


		case  OP_ADMM:			// 	        0xc0  --  Add Register to Memory        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			// fprintf(stderr, "\n ADMM instr: 0x%04x, dest: 0x%04x,  src: 0x%04x,  imm mem adr: 0x%04x, indirect: %s, mem addr 0x%04x, mem val: 0x%04x, result val: 0x%04x  \n",
			// 	instruction.all,
			// 	tmp16_val1.uval,
			// 	GET_REGISTER_VALUE(0x7 & instruction.parts.src_reg),
			// 	GET_MEMORY_VALUE_IMMEDIATE,
			// 	((0x8 & instruction.parts.src_reg) != 0 ? "indirect" : "direct"),
			// 	GET_MEMORY_DIRECT_ADDR,
			// 	tmp16_val2.uval,
			// 	tmp16_val3.uval);
			// disp_cur_reg(stderr);
			SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ETMM:			// 	        0xc1  --  Extract Register from Memory        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = ~tmp16_val1.uval & tmp16_val2.uval;
			SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ORMM:			// 	        0xc2  --  OR Register to Memory        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
			SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_CRM:			// 	        0xc3  --
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ADMB:			// 	        0xc4  --  Add Register to Memory and Branch if Nonzero      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			// fprintf(stderr, "\n ADMB instr: 0x%04x, dest: 0x%04x,  src: 0x%04x,  imm mem adr: 0x%04x, indirect: %s, mem addr 0x%04x, mem val: 0x%04x, result val: 0x%04x  \n",
			// 	instruction.all,
			// 	tmp16_val1.uval,
			// 	GET_REGISTER_VALUE(0x7 & instruction.parts.src_reg),
			// 	GET_MEMORY_VALUE_IMMEDIATE,
			// 	((0x8 & instruction.parts.src_reg) != 0 ? "indirect" : "direct"),
			// 	GET_MEMORY_DIRECT_ADDR,
			// 	tmp16_val2.uval,
			// 	tmp16_val3.uval);
			// disp_cur_reg(stderr);
			SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			break;

		case  OP_ETMB:			// 	        0xc5  --  Extract Register from Memory and Branch if Nonzero    
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = ~tmp16_val1.uval & tmp16_val2.uval;
			SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			break;

		case  OP_TRMB:			// 	        0xc6  --  Test Register and Memory and Branch if any Ones Compare  
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval & tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			GET_MEMORY_VALUE_IMMEDIATE_2ND( tmp_new_prog_count )
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			break;

		case  OP_CRMB:			// 	        0xc7  --  CRMB  --  Compare Register with Memory and Branch Equal or Less   
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else if (TEST_CC_LT) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
			}
			break;

		case  OP_ADRD_ADMD:		//        0xc8
			//  --  ADRD(DAR)  --  Add Double-Register to Double-Register        
			if ( (instruction.parts.dest_reg & 1) == 0 ) {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
				tmp32_val3.uval = tmp32_val1.uval + tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			//  --  ADMD  --  Add Memory Doubleword to Double- Register      
			else {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val2.uval);
				tmp32_val3.uval = tmp32_val1.uval + tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			break;

		case  OP_SURD_SUMD:		//        0xc9
			//  --  SURD  --  Subtract Double-Register from Double-Register        
			if ((instruction.parts.dest_reg & 1) == 0) {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
				tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			//  --  SUMD  --  Subtract Memory Doubleword from Double-Register       
			else {
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val2.uval);
				tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			break;


		case  OP_AUGCA:			//         0xca
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;

			// -------- NOTE TRUE = 0, FALSE = FFFF
			switch (tmp_instr_dest) {
				case 0:		// --  SRNS	Set Register if Condition Code N Set
					tmp16_val1.uval = (TEST_CC_N ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 1:		// --  SRZS	Set Register if Condition Code Z Set
					tmp16_val1.uval = (TEST_CC_Z ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 2:		// --  SROS	Set Register if Condition Code O Set
					tmp16_val1.uval = (TEST_CC_O ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 3:		// --  SRCS	Set Register if Code C Set
					tmp16_val1.uval = (TEST_CC_C ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 4:		// --  SRLS	Set Register on Less than Condition
					tmp16_val1.uval = (TEST_CC_LT ? 0 : 0xffff);		// CCN XOR CCO
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 5:		// --  SRLE	Set Register on Less than or Equal Condition
					tmp16_val1.uval = (TEST_CC_LE ? 0 : 0xffff);	// CCZ .OR. (CCN .XOR. CCO)
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 6:		// --  SRHI	Set Register on Magnitude Higher Condition
					tmp16_val1.uval = (TEST_CC_HI ? 0 : 0xffff);	// !CCC .or. CCZ
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 8:		// --  SRNR	Set Register if Condition Code N Reset
					tmp16_val1.uval = (TEST_CC_NOT_N ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 9:		// --  SRZR	Set Register if Condition Code Z Reset
					tmp16_val1.uval = (TEST_CC_NOT_Z ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 10:		// --  SROR	Set Register if Condition Code O Reset
					tmp16_val1.uval = (TEST_CC_NOT_O ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 11:		// --  SRCR	Set Register if Condition Code C Reset 
					tmp16_val1.uval = (TEST_CC_NOT_C ? 0 : 0xffff);
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 12:		// --  SRGE	Set Register on Greater than or Equal Condition
					tmp16_val1.uval = (TEST_CC_GE ? 0 : 0xffff);  //  CCZ .or. (!CCZ .AND. (!CCN .XOR. CCO))
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 13:		// --  SRGT	Set Register on Greater than Condition
					tmp16_val1.uval = (TEST_CC_GT ? 0 : 0xffff);  //  
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 14:		// --  SRNH	Set Register on Magnitude not Higher Condition
					tmp16_val1.uval = (TEST_CC_NH ? 0 : 0xffff);  // !CCC .or CCZ
					SET_SOURCE_REGISTER_VALUE(tmp16_val1.uval);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				case 15:		// -- UNDOCUMENTED -- Don't know what this really does, set reg to -1
					fprintf(stderr, " Undocumented pc: 0x%04x instruction: 0x%04x\n", program_counter, instruction.all);
					SET_SOURCE_REGISTER_VALUE(0xffff);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;
				default:
					ILLEGAL_INSTRUCTION;
					break;
			}
			break;

		case  OP_UIT:			// 	        0xcb
			UNIMPLEMENTED_INSTRUCTION;
			break;

		case  OP_TSBM:			// 	        0xcc  --  TSBM  --  Test and Set Bit Memory       
			// -------TODO: Is a resource needed here for TSBM ?? -- be careful about virtual protect traps!
			GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
			SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0); // test it
			tmp16_val1.uval |= bit[GET_DESTINATION_REGISTER_NUMB];	// now set it.
			SET_CC_N(ISVAL16_NEG(tmp16_val1));		
			SET_CC_Z(false);
			SET_CC_O(false);
			SET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);		// write new memory value..
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;


		case  OP_TRRD_LDMD:		//        0xcd
			switch ( instruction.parts.dest_reg & 0x0001 ) {

				case 0:				//  --  TRRD -- Transfer Double-Register to Double- Register
					tmp32_val1.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val1.uval);
					SET_CC_Z(ISVAL32_ZERO(tmp32_val1));
					SET_CC_N(ISVAL32_NEG(tmp32_val1));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				case 1:				//  --  LDMD  -- Load Double-Register from Memory Doubleword 
					GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val1.uval );
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val1.uval);
					SET_CC_Z(ISVAL32_ZERO(tmp32_val1));
					SET_CC_N(ISVAL32_NEG(tmp32_val1));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
			}
			break;

		case  OP_CLM_STMD_CLMD:	//        0xce
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			if (tmp_instr_dest == 0) {
				//--------CLM  --  Clear Memory          
				SET_MEMORY_VALUE_DIRECT(0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			else if ((tmp_instr_dest & 0x1) == 0) {
				//--------CLMD
				SET_MEMORY_VALUE_DIRECT_DOUBLE(0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			else {
				//--------STMD          
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				SET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val1.uval);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			break;

		case  OP_CRRD_CRMD:		//        0xcf
			switch (instruction.parts.dest_reg & 0x0001) {

				case 0:				//  --  CRRD  --  Compare Double-Register with Double Register       
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					tmp32_val2.uval = GET_SOURCE_REGISTER_VALUE_DOUBLE;
					tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					break;

				case 1:				//  --  CRMD  --  Compare Double-Register with Memory Doubleword       
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_DIRECT_DOUBLE(tmp32_val2.uval);
					tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					break;
			}
			break;


		case  OP_ADSM:			// 	        0xd0  --  Add Register to Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ETSM:			// 	        0xd1  --  Extract Register from Memory (Short- Displaced)      
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = ~tmp16_val1.uval & tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ORSM:			// 	        0xd2  --  OR Register to Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_CRS:			// 	        0xd3  -- CRS  --  Compare Register with Memory  (Short-Displaced)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ADSB:			// 	        0xd4  --  Add Register to Memory (Short-Displaced) and Branch if Nonzero   
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_ETSB:			// 	        0xd5  --  Extract Register from Memory (Short- Displaced) and Branch if Nonzero  
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = ~tmp16_val1.uval & tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_TRSB:			// 	        0xd6  --  Test Register and Memory (Short- Displaced)  and Branch if any ones Compare        
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval & tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_CRSB:			// 	        0xd7  --  CRSB  --  Compare Register with Memory (Short- Displaced) and Branch if Equal or Less
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
#if DEBUG_CRSB >= 1
				util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, Sd: 0x%04x, Rd: 0x%04x,  Res: 0x%04x\n",
					junkxx, instruction.all, program_counter, tmp16_val2.uval, tmp16_val1.uval, tmp16_val3.uval);
				disp_psw(stderr, cpu_get_current_PSW());
				disp_cur_reg(stderr);
#endif
				if (TEST_CC_Z) {
					GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
				}
				else if (TEST_CC_LT) {
					GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
					SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
				}
				else {
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
				}
			// }
			break;

		case  OP_ADXM:			// 	        0xd8  --  Add Register to Memory (Short-Indexed)       
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ETXM:			// 	        0xd9  --  Extract Register from Memory (Short- Indexed)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = ~tmp16_val1.uval & tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ORXM:			// 	        0xda  --  OR Register to Memory (Short-Indexed)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_CRX:			// 	        0xdb  -  CRX  --  Compare Register with Memory (Short-Indexed)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ADXB:			// 	        0xdc  --  Add Register to Memory (Short-Indexed) and Branch if Nonzero   
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ETXB:			// 	        0xdd  --  Extract Register from Memory Short- Indexed and Branch if Nonzero  
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = ~tmp16_val1.uval & tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_TRXB:			// 	        0xde  --  Test Register and Memory (Short-Indexed) and Branch if any Ones Compare 
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval & tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			GET_MEMORY_VALUE_IMMEDIATE(tmp_new_prog_count);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_CRXB:			// 	        0xdf  --  CRXB  --  Compare Register with Memory (Short- Indexed) and Branch if Equal or Less
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else if (TEST_CC_LT) {
				GET_MEMORY_VALUE_IM(tmp_new_prog_count, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;


		case  OP_ADM:			// 	        0xe0  --  Add Memory to Register        
			GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			// fprintf(stderr, "\n ADM instr: 0x%04x, dest: 0x%04x,  src: 0x%04x,  imm mem adr: 0x%04x, indirect: %s, mem addr 0x%04x, mem val: 0x%04x, result val: 0x%04x  \n", 
			// 	instruction.all, 
			// 	tmp16_val2.uval, 
			// 	GET_REGISTER_VALUE( 0x7 & instruction.parts.src_reg ),
			// 	GET_MEMORY_VALUE_IMMEDIATE,
			// 	((0x8 & instruction.parts.src_reg) != 0 ? "indirect" : "direct"),
			// 	GET_MEMORY_DIRECT_ADDR,
			// 	tmp16_val1.uval, 
			// 	tmp16_val3.uval );
			// disp_cur_reg(stderr);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_SUM:			// 	        0xe1  --  Subtract Memory from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ETM:			// 	        0xe2  --  Extract Memory from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval & ~tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ORM:			// 	        0xe3  --  OR Memory to Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_XOM:			// 	        0xe4  --  Exclusive OR Memory to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval ^ tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_LDM:			// 	        0xe5  --   Load Register from Memory        
			GET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_STM:			// 	        0xe6  --  Store Register in Memory        
			SET_MEMORY_VALUE_DIRECT( GET_DESTINATION_REGISTER_VALUE );
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_BRU_BLM:			//         0xe7
			// BLM  --  Branch and Link 
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			if (tmp_instr_dest != 0) {
				SET_DESTINATION_REGISTER_VALUE(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			}
			// BRU  --  Branch Unconditionally          
			else {
				// nothing to do here.
			}
			GET_MEMORY_DIRECT_ADDR(tmp_new_prog_count);
			SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			break;


		case  OP_AUGE8:			//         0xe8
			switch (instruction.parts.src_reg) {

				// --  0	ADI  --  Add Memory(Immediate) to Register	
			case 0:
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  1	LDES  --  Load Immediate and Extend Sign	
			case 1:
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
				tmp32_val3.sval = tmp16_val1.sval;	// sign extend to 32 bits.
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  2	ADES  --  Add Immediate with Extended Sign	
			case 2:
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp32_val2.sval = tmp16_val2.sval;	// sign extend to 32 bits.
				tmp32_val3.uval = tmp32_val1.uval + tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_ADD_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  3	SUES  --  Subtract Immediate with Extended Sign	
			case 3:
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp32_val2.sval = tmp16_val2.sval;	// sign extend to 32 bits.
				tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  4	CIES  --  Compare Immediate with Extended Sign	
			case 4:
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp32_val2.sval = tmp16_val2.sval;	// sign extend to 32 bits.
				tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
#if DEBUG_CIES >= 1
				util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, Imm: 0x%04x, Rd: 0x%08x,  Res: 0x%08x\n",
					junkxx, instruction.all, program_counter, tmp32_val2.uval, tmp32_val1.uval, tmp32_val3.uval);
				disp_psw(stderr, cpu_get_current_PSW());
				disp_cur_reg(stderr);
#endif
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  5	EXI  --  Execute Immediate	
				// -------- the only way to get here is if the result of an EXI or EXR instruction is an EXI
				// -------- instruction.
			case 5:
				EXI_EXR_INSTRUCTION;
				break;

				// --  6	MPI  --  Multiply Immediate	
			case 6:
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				if ISREGNUM_DOUBLE(tmp_instr_dest) {
					tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest | 0x01);
				}
				else {
					tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_dest);
				}
				tmp32_val1.sval = tmp16_val1.sval;
				tmp32_val2.sval = tmp16_val2.sval;
				tmp32_val3.sval = tmp32_val1.sval * tmp32_val2.sval;
				if ISREGNUM_DOUBLE(tmp_instr_dest) {
					SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				}
				else {
					tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);
					SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				}
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_O(false);
				SET_CC_C(((tmp32_val3.uval & 0xffff8000) == 0xffff8000) || ((tmp32_val3.uval & 0xffff8000) == 0x00000000));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  7	DVI  --  Divide Immediate	
			case 7:
				if ( ISREGNUM_DOUBLE(GET_DESTINATION_REGISTER_NUMB) ) {
					tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
					GET_MEMORY_VALUE_IMMEDIATE( tmp16_val2.uval );
					tmp32_val2.sval = tmp16_val2.sval;
					if (tmp32_val2.sval != 0) {
						tmp32_val3.sval = tmp32_val1.sval / tmp32_val2.sval;	// quotient
						tmp32_val6.sval = tmp32_val1.sval % tmp32_val2.sval;	// remainder
					}
					else {
						tmp32_val3.uval = 0;
						tmp32_val6.uval = 0;
					}
					tmp16_val3.uval = (SIMJ_U16)(tmp32_val3.uval & 0x0000ffff);		// low 16 bits of quotient.
					tmp16_val6.uval = (SIMJ_U16)(tmp32_val6.uval & 0x0000ffff);		// low 16 bits of remainder.
					tmp_instr_dest = (GET_DESTINATION_REGISTER_NUMB & 0x00e);		// must be even.
					SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val6.uval);			// remainder
					SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);		// quotient
					SET_CC_N(ISVAL32_NEG(tmp32_val3));
					SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
					SET_CC_O(ISVAL32_ZERO(tmp32_val2) || !(((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000)));	// not a 16 bit result
					SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				}
				else {
					ILLEGAL_INSTRUCTION;
				}
				break;

				// --  8	EPMD  --  Enter Pipeline Mode of Execution	
			case 8:
				cpu_pipeline_mode = true;		// not much to really done on simulator.
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// --  9	CRZ  --  Compare Register to Zero	
			case 9:
				tmp16_result_value.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = 0;
				SET_CC_N(ISVAL16_NEG(tmp16_result_value));
				SET_CC_Z(ISVAL16_ZERO(tmp16_result_value));
				SET_CC_O_SUB(tmp16_result_value,tmp16_val2, tmp16_result_value );
				SET_CC_C_SUB(tmp16_result_value, tmp16_val2, tmp16_result_value);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// --  A	CRZD  --  Compare Double Register to Zero	
			case 10:
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp32_val2.uval = 0;
				SET_CC_N(ISVAL32_NEG(tmp32_val1));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val1));
				SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val1);
				SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val1);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// --  B	XPMD  --  Exit Pipeline Mode of Execution	
			case 11:
				cpu_pipeline_mode = false;		// not much to really do
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;

				// --  C	LTIL  --  Loop Termination with Indirectly Addressed Control Variable and Literal Terminal Value	
			case 12:
				// var9 - addr of addr of ctrl var
				// val5 - ctrl var addr
				// val1 - updated ctrl var value
				// val2 - term var value
				// val3 - difference
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp16_val9.uval);		// -- addr of addr of ctrl var
				GET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val9.uval);  // -- control variable address
				GET_MEMORY_VALUE_OM(tmp16_val1.uval , tmp16_val5.uval);	// -- increment control variable
				tmp16_val1.uval += (SIMJ_U16)1;
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);	// -- update control variable
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );			// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE_3RD(tmp16_val9.uval);
				CONDITIONAL_BRANCH(TEST_CC_LE, tmp16_val9.uval, PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
				break;

				// --  D	LTDL  --  Loop Termination with Directly Addressed Control - Variable - and Literal Terminal Value	
			case 13:
				// val5 - ctrl var addr
				// val1 - updated ctrl var value
				// val2 - term var value
				// val3 - difference
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp16_val5.uval );		// -- control variable address
				GET_MEMORY_VALUE_OM(tmp16_val1.uval, tmp16_val5.uval);	// -- get control variable 
				tmp16_val1.uval += (SIMJ_U16)1;
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);  // -- increment and update control variable
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );			// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE_3RD(tmp_new_prog_count);
				CONDITIONAL_BRANCH( TEST_CC_LE, tmp_new_prog_count, PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
				break;

				// --  E	LTID  --  Loop Termination with Indirectly Addressed Control Variable and Directly Addressed Terminal Value	
			case 14:
				// var9 - addr of addr of ctrl var
				// val5 - ctrl var addr
				// val1 - updated ctrl var value
				// val8 - term var addr
				// val2 - term var value
				// val3 - difference
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp16_val9.uval);		// -- addr of addr of control varl
				GET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val9.uval);	// -- control variable address
				GET_MEMORY_VALUE_OM(tmp16_val1.uval , tmp16_val5.uval);	// -- increment control variable
				tmp16_val1.uval += (SIMJ_U16)1;							// -- inc control variable
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);	// -- update control variable
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val8.uval);			// -- terminal var addr
				GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val8.uval);	// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE_3RD(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_LE, tmp_new_prog_count, PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
				break;

				// --  F	LTDD  --  Loop Termination with Directly Addressed - Control Variable and Directly Addressed Terminal Value
			case 15:
				// val5 - ctrl var addr
				// val1 - updated ctrl var value
				// val8 - term var addr
				// val2 - term var value
				// val3 - difference
				GET_MEMORY_VALUE_IMMEDIATE_2ND(tmp16_val5.uval );		// -- control variable address
				GET_MEMORY_VALUE_OM(tmp16_val1.uval , tmp16_val5.uval);	// -- increment control variable
				tmp16_val1.uval += (SIMJ_U16)1;							// -- inc control variable
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);	// -- update control variable
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val8.uval);			// -- address of term value
				GET_MEMORY_VALUE_OM(tmp16_val2.uval, tmp16_val8.uval);	// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				GET_MEMORY_VALUE_IMMEDIATE_3RD(tmp_new_prog_count);
				CONDITIONAL_BRANCH(TEST_CC_LE, tmp_new_prog_count, PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_SUI_CRI:			//         0xe9
			switch (instruction.parts.src_reg) {

			case 0:		// SUI  --  SUI  --  Subtract Memory (Immediate) from Register       
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// CRI  --  CRI  --  Compare Register with Memory (Immediate)       
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
#if DEBUG_CRI >= 1
				util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, Imm: 0x%04x, Rd: 0x%04x,  Res: 0x%04x\n",
					junkxx, instruction.all, program_counter, tmp16_val2.uval, tmp16_val1.uval, tmp16_val3.uval);
				disp_psw(stderr, cpu_get_current_PSW());
				disp_cur_reg(stderr);
#endif
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_ETI_TETI:			//         0xea
			switch (instruction.parts.src_reg) {

			case 0:		// ETI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval & ~tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// TETI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval & ~tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_ORI_TORI:			//         0xeb
			switch (instruction.parts.src_reg) {

			case 0:		// ORI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// TORI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_XOI_TXOI:			//         0xec
			switch (instruction.parts.src_reg) {

			case 0:		// XOI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval ^ tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// TXOI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val2.uval );
				tmp16_val3.uval = tmp16_val1.uval ^ tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_LDI_LDF_LDFD_FDFQ:    //    0xed
			switch (GET_SOURCE_REGISTER_NUMB) {
			case 0:	// -------- LDI   load immediate
				GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1: // -------- LDF  load floating immediate
				if (TEST_VALID_DOUBLE_REGISTER(GET_DESTINATION_REGISTER_NUMB)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000e;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
					SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val1.uval);
					SET_REGISTER_VALUE(tmp_instr_dest+1, 0);
					SET_CC_N(ISVAL16_NEG(tmp16_val1));
					SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				}
				else {
					ILLEGAL_INSTRUCTION;
				}
				break;

			case 2:	// -------- LDFD load floating double immediate 
				if (TEST_VALID_TRIPLE_REGISTER(GET_DESTINATION_REGISTER_NUMB)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000c;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
					// TODO: LDFD Use Triple macros
					SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val1.uval);
					SET_REGISTER_VALUE(tmp_instr_dest + 1, 0);
					SET_REGISTER_VALUE(tmp_instr_dest + 2, 0);
					SET_CC_N(ISVAL16_NEG(tmp16_val1));
					SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				}
				else {
					ILLEGAL_INSTRUCTION;
				}
				break;

			case 3: // -------- LDFQ load floating quad immediate
				if (TEST_VALID_QUAD_REGISTER(GET_DESTINATION_REGISTER_NUMB)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000c;
					GET_MEMORY_VALUE_IMMEDIATE(tmp16_val1.uval );
					SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val1.uval);
					SET_REGISTER_VALUE(tmp_instr_dest + 1, 0);
					SET_REGISTER_VALUE(tmp_instr_dest + 2, 0);
					SET_REGISTER_VALUE(tmp_instr_dest + 3, 0);
					SET_CC_N(ISVAL16_NEG(tmp16_val1));
					SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				}
				else {
					ILLEGAL_INSTRUCTION;
				}
				break;

			default:
				ILLEGAL_INSTRUCTION;
				break;
			}
			break;

		case  OP_STI:			// 	        0xee  --  Store Register in Memory (Immediate)       
			SET_MEMORY_VALUE_IMMEDIATE(GET_DESTINATION_REGISTER_VALUE);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_BLI:			// 	        0xef  --  Branch and Link (Immediate)        
			SET_DESTINATION_REGISTER_VALUE( PROGRAM_COUNTER_TWO_WORD_INSTRUCT)
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;


		case  OP_ADS:			// 	        0xf0  --  Add Memory (Short-Displaced) to Register       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_SUS:			// 	        0xf1  --  Subtract Memory (Short-Displaced) from Register       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ETS:			// 	        0xf2  --  Extract Memory (Short-Displaced) from Register       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval & ~tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ORS:			// 	        0xf3  --  OR Memory (Short-Displaced) to Register       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_XOS:			// 	        0xf4  --  Exclusive OR Memory to Register (Short-Displaced)      
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval ^ tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_LDS:			// 	        0xf5  --  Load Register from Memory Short-displaced       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				GET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_STS:			// 	        0xf6
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
			SET_MEMORY_VALUE_SHORT_DISPLACED(GET_DESTINATION_REGISTER_VALUE);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_HOP_BLT:		//         0xf7
			// BLT  --  Branch and Link (Indexed Through-Table)       
			if (instruction.all & 0x0080) {
				SET_REGISTER_VALUE(8, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				GET_MEMORY_VALUE_OM(tmp_new_prog_count, (SIMJ_U16)(GET_REGISTER_VALUE(2) + GET_HOP_OFFSET));
				SET_NEXT_PROGRAM_COUNTER(tmp_new_prog_count);
			}
			// HOP  --  Hop Unconditionally          
			else {
				SET_NEXT_PROGRAM_COUNTER(GET_NEXT_PROGRAM_COUNTER_HOP);
			}
			break;

		case  OP_ADX:			// 	        0xf8  --  Add Memory (Short-Indexed) to Register       
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_SUX:			// 	        0xf9  --  Subtract Memory (Short-Indexed) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ETX:			// 	        0xfa  --  Extract Memory (Short-Indexed) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval & ~tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ORX:			// 	        0xfb  --  OR Memory (Short-Indexed) to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_XOX:			// 	        0xfc  --  Exclusive OR Memory to Register (Short-Displaced)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			tmp16_val3.uval = tmp16_val1.uval ^ tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_LDX:			// 	        0xfd  --  Load Register from Memory (Short-Indexed)       
			GET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
#if DEBUG_LDX >= 1
			util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
			fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, LDX: 0x%04x, value: 0x%04x \n",
				junkxx, instruction.all, program_counter,
				instruction.parts.src_reg, tmp16_val1.uval);
#endif
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_STX:			// 	        0xfe  --  Store Register in Memory (Short-Indexed)       
			SET_MEMORY_VALUE_SHORT_INDEXED( GET_DESTINATION_REGISTER_VALUE );
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_BRX_BLX:			// 	    0xff
			// TODO: BLX BRX what should be done if source register is 0.  For now illegal inst.
			if (GET_SOURCE_REGISTER_NUMB == 0) {
				printf("\n BRX, BLX -- source register is 0.\n");
				UNIMPLEMENTED_INSTRUCTION;
			}
			else {
				tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;

				// BRX  --  Branch (Short-Indexed) Unconditionally         
				// if ( GET_DESTINATION_REGISTER_NUMB == 0 ) {
					// nothing extra to do here...
				// }
				// BLX  --  Branch and Link (Short-Indexed)
				// -----if the registers are the same need to get the source value first...       
				if (GET_DESTINATION_REGISTER_NUMB != 0) {
					SET_DESTINATION_REGISTER_VALUE(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				// then just do BRX...
				SET_NEXT_PROGRAM_COUNTER(tmp16_val1.uval);
			}
			break;

		// --------NO valid instruction.  Set UIT trap.
		default:
			UNIMPLEMENTED_INSTRUCTION;
			break;

		}

	// -------- label to goto when instruction fault occurs
	end_instruction:

		// --------increment instruction count.
		cpu_instruction_count++;

		// --------throttle cpu execution to try and match 7860  
		// TODO: make end of instruction wait optional.
		// TODO: add wait time for each instruction or each type of instruction (branching).
#if DEBUG_SLOWER_CPU == 1
		util_high_res_spin_wait_finish(4, StartingTime);		// 400 ns
#else
		util_high_res_spin_wait_finish(2, StartingTime);		// 200 ns
#endif

		// --------extended addressing state machine processing
		// TODO: Finish extended addressing for 7860
		switch ( use_extended_addressing_mode ) {
			case 1:
				use_extended_addressing_mode = 2;
				break;
			case 2:
				use_extended_addressing_mode = 0;
		}

		// --------kludge for register 0
		// TODO: Register 0, should it be updated every instruction from front panel or just wait for a nop
		SET_REGISTER_VALUE(0, gbl_fp_switches);

		// -------- reset single step if active
		if (gbl_fp_single_step) {
			gbl_fp_single_step = false;
			WakeByAddressSingle((LPVOID)& gbl_fp_single_step);
		}

	}

	// printf("\n CPU exiting.\n");
	return;
}