// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			cpu_classic_7860.c
//
//	Description:	THis module contains the CPU instruction set processing.  This
//                  can be compiled to simulate Classic 7860, 7830, II/15 cpus, with or
//                  without EAU floating point instructions.
//
//	Externally accessible routines:
//					void cpu_get_virtual_map(SIMJ_U16 map, MEM_MAP* copied_map)
//					void cpu_get_instruction_trace(SIMJ_U16* inx, SIMJ_U32 trace[1024])
//					void cpu_init_data()
//					void cpu_stop_data()
//					void cpu_set_power_on()
//					void cpu_master_clear()
//					bool cpu_get_power_on()
//					void cpu_get_interrupt(SIMJ_U16* act, SIMJ_U16* req, SIMJ_U16* ena,
//									SIMJ_U32* di_req, SIMJ_U32* di_prc, SIMJ_U32* si_req, SIMJ_U32* si_prc )
//					void cpu_request_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr)
//					void cpu_request_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr)
//					SIMJ_U32 cpu_get_instruction_count()
//					SIMJ_U16 cpu_get_clock_trigger_count()
//					void cpu_trigger_console_interrupt()
//					void cpu_trigger_clock_interrupt()
//					void cpu_set_register_value(SIMJ_U16 reg_index, SIMJ_U16 reg_value)
//					SIMJ_U16 cpu_get_register_value(SIMJ_U16 reg_index)
//					void cpu_set_program_counter(SIMJ_U16 pc)
//					SIMJ_U16 cpu_get_program_counter()
//					PSW cpu_get_current_PSW()
//					SIMJ_U16 cpu_read_internal_register(SIMJ_U16 front_panel_address)
//					void cpu_classic_7860()
//                  
// 
//	Internal only routines:
//					inline SIMJ_U16 cpu_get_next_DI_request()
//					inline SIMJ_U16 cpu_get_next_SI_request()
//					inline void cpu_switch_register_blocks(SIMJ_U8 new_reg_block)
//					inline void cpu_set_current_PSW(PSW new_psw)
//					inline SIMJ_U16 cpu_find_bit(SIMJ_U16 bit_value)
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

static REG_BLOCK cpu_register;				// register block in use by CPU.
static REG_BLOCK cpu_register_blocks[16];	// indexed by register block.
static SIMJ_U8 cpu_register_current_block = 0;		// GRB

// -------- interrupts
//		0	0x8000	Power Fail Safe / Auto Start
#define cpu_intr_power_fail (SIMJ_U16)0x8000
//		1	0x4000	Memory Parity
#define cpu_intr_mem_parity (SIMJ_U16)0x4000
//		2	0x2000	System Protect (priv instruction)
#define cpu_intr_sys_protect (SIMJ_U16)0x2000
//		3	0x1000	Multiprocessor Communications
#define cpu_intr_RMI (SIMJ_U16)0x1000
//		4	0x0800	Unimplemented Instruction Trap
#define cpu_intr_UIT (SIMJ_U16)0x0800
//		5	0x0400	Floating Point Overflow
#define cpu_intr_floating_overflow (SIMJ_U16)0x0400
//		6	0x0200	Real Time Clock
#define cpu_intr_clock (SIMJ_U16)0x0200
//		7	0x0100	External
#define cpu_intr_ext_7 (SIMJ_U16)0x0100
//		8	0x0080	External
#define cpu_intr_ext_8 (SIMJ_U16)0x0080
//		9	0x0040	External
#define cpu_intr_ext_9 (SIMJ_U16)0x0040
//		10	0x0020	External
#define cpu_intr_ext_A (SIMJ_U16)0x0020
//		11	0x0010	External
#define cpu_intr_ext_B (SIMJ_U16)0x0010
//		12	0x0008	I/O Data Party Line
#define cpu_intr_DI (SIMJ_U16)0x0008
//		13	0x0004	I/O Service Party Line
#define cpu_intr_SI (SIMJ_U16)0x0004
//		14	0x0002	Console interrupt
#define cpu_intr_console (SIMJ_U16)0x0002
//		15	0x0001	Task Scheduler
#define cpu_intr_task_sch (SIMJ_U16)0x0001
//

#define DEFAULT_INTR_ENABLED (cpu_intr_power_fail | cpu_intr_sys_protect | cpu_intr_UIT | cpu_intr_floating_overflow)
static volatile SIMJ_U16 cpu_interrupt_active = 0;							// interrupt active bits.
static volatile SIMJ_U16 cpu_interrupt_enabled = DEFAULT_INTR_ENABLED;		// interrupt enabled bits.
static volatile SIMJ_U16 cpu_interrupt_request = 0;							// interrupt requested bits.
static volatile SIMJ_U16 cpu_interrupt_request_external = 0;				// external interrupt requested bits.
static volatile SIMJ_U16 cpu_interrupt_active_mask = 0xffff;
static volatile SIMJ_U16 cpu_interrupt_active_current = 16;					// currently active interrupt level, 16 = nothing active.

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


static volatile bool cpu_virtual_mode = false;
static volatile MEM_MAP cpu_virtual_mem_map[8];

static volatile bool cpu_pipeline_mode = false;

static volatile SIMJ_U16 cpu_extended_memory_ctrl_reg = 0;

static volatile SIMJ_U16 cpu_trigger_clock_int = 0;	// updated by external call
static volatile SIMJ_U16 cpu_last_clock_int = 0;		// updated by cpu

static volatile SIMJ_U32 cpu_instruction_count = 0;	// how many instructions executed.

static SIMJ_U16 saved_reg_0 = 0;

#if SIMJ_PLATFORM == MSWINDOWS
CRITICAL_SECTION CritSectInterruptRequest;
#else
#error Other platforms not supported yet.
#endif

static bool skip_interrupt_determination = false;

static int use_extended_addressing_mode = 0;	// state machine 0 = none, 1 = next instr to use, 2 = after next instruction

static bool cpu_power_on = false;


#if SIMJ_PLATFORM == MSWINDOWS
CRITICAL_SECTION CritSectInstructionTrace;
#else
#error Other platforms not supported yet.
#endif

static SIMJ_U32		instruction_trace[1024] = { 0 };	// list of ABS memory addresses containing instructions.
static SIMJ_U16		instruction_trace_index = 0;		// next index into instruction trace list 

#if SIMJ_SIM_CPU == 7830
static bool			cpu_nonvirt_prot_enabled = false;
static SIMJ_U16		cpu_nonvirt_prot_lowprot_reg = 0xffff;
static SIMJ_U16		cpu_nonvirt_prot_upprot_reg = 0xffff;
static SIMJ_U16		cpu_nonvirt_prot_gbl_reg = 0xffff;
#endif

#include "cpu_macro_register_access.h"


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
void cpu_get_instruction_trace( SIMJ_U16 *inx, SIMJ_U32 trace[1024]) {
	
	int j = 0;

	EnterCriticalSection(&CritSectInstructionTrace);

	for (j = 0; j < 1024; j++) {
		trace[j] = instruction_trace[j];
	}
	*inx = instruction_trace_index;
	LeaveCriticalSection(&CritSectInstructionTrace);

	return;
}

// ===========================================================================================================
// --------public
void cpu_init_data() {

	bool status = false;

	// --------initialize the critical section for interrupt requests.
	// Initialize the critical section one time only.
	status = InitializeCriticalSectionAndSpinCount(&CritSectInterruptRequest, 0x00000400);
	if (!status) {
		printf(" *** ERROR *** Cpu could not create locking mechanism for interrupt request.\n");
	}
	status = InitializeCriticalSectionAndSpinCount(&CritSectInstructionTrace, 0x00000400);
	if (!status) {
		printf(" *** ERROR *** Cpu could not create locking mechanism for instruction trace.\n");
	}


}


// ===========================================================================================================
// --------public
void cpu_stop_data() {

	// -------- set virtual cpu power off.
	cpu_power_on = false;

	// --------delete the critical section for interrupt requests.
	DeleteCriticalSection(&CritSectInterruptRequest);
	DeleteCriticalSection(&CritSectInstructionTrace);


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
	cpu_interrupt_active_current = 16;

	// --------reset all interrupt requests
	// -------- Request ownership of the critical section.
	EnterCriticalSection(&CritSectInterruptRequest);
	cpu_interrupt_request = 0;
	cpu_interrupt_request_external = 0;
	// -------- Release ownership of the critical section.
	LeaveCriticalSection(&CritSectInterruptRequest);
	
	// TODO: clear SI, DI queues.

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

	if ( (( cpu_intr_DI & cpu_interrupt_enabled ) != 0) && cpu_power_on ) {

		// -------- Request ownership of the critical section.
		EnterCriticalSection(&CritSectInterruptRequest);
		cpu_interrupt_DI_request_dev_addr[prio][bus] = dev_addr;
		cpu_interrupt_DI_request_count[prio][bus]++;
		cpu_interrupt_DI_total_request_count++;
		cpu_interrupt_request_external |= (cpu_intr_DI); // &cpu_interrupt_enabled);
		// -------- Release ownership of the critical section.
		LeaveCriticalSection(&CritSectInterruptRequest);

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

	if ( ( ( cpu_intr_SI & cpu_interrupt_enabled ) != 0 ) && cpu_power_on ) {
		// -------- Request ownership of the critical section.
		EnterCriticalSection(&CritSectInterruptRequest);
		cpu_interrupt_SI_request_dev_addr[prio][bus] = dev_addr;
		cpu_interrupt_SI_request_count[prio][bus]++;
		cpu_interrupt_SI_total_request_count++;
		cpu_interrupt_request_external |= (cpu_intr_SI); // &cpu_interrupt_enabled);
		// -------- Release ownership of the critical section.
		LeaveCriticalSection(&CritSectInterruptRequest);

		// fprintf(stderr, " SI requested da %d, bus %d, pri %d\n", dev_addr, bus, prio);
	}
	else {
		fprintf(stderr, " SI requested, but NOT enabled da %d, bus %d, pri %d\n", dev_addr, bus, prio);
	}
	return;
}

// ===========================================================================================================
// --------returns with device address or 0xffff
// -------- THIS MUST BE CALLED WHILE IN A CRITICAL SECTION FOR THE INTERRUPT REQUEST.
// --------private
inline SIMJ_U16 cpu_get_next_DI_request() {
	SIMJ_U16 fnd_dev_addr = 0xffff;
	int j = 0;

	// --------loop over all priorities and buses
	// j = priority
	for ( j = 0; j < 16; j++) {
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
// -------- THIS MUST BE CALLED WHILE IN A CRITICAL SECTION FOR THE INTERRUPT REQUEST.
// -------- private
inline SIMJ_U16 cpu_get_next_SI_request() {
	SIMJ_U16 fnd_dev_addr = 0xffff;
	int j = 0;

	// --------loop over all priorities and buses
	for ( j = 0; j < 16; j++) {
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
		// -------- Request ownership of the critical section.
		EnterCriticalSection(&CritSectInterruptRequest);
		cpu_interrupt_request_external |= (cpu_intr_console & cpu_interrupt_enabled);
		// -------- Release ownership of the critical section.
		LeaveCriticalSection(&CritSectInterruptRequest);
	}
}



// ===========================================================================================================
// --------public
void cpu_trigger_clock_interrupt() {

	if (cpu_power_on) {
		cpu_trigger_clock_int++;
		// -------- Request ownership of the critical section.
		EnterCriticalSection(&CritSectInterruptRequest);
		cpu_interrupt_request_external |= (cpu_intr_clock & cpu_interrupt_enabled);
		// -------- Release ownership of the critical section.
		LeaveCriticalSection(&CritSectInterruptRequest);
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
// --------this should be local to this module only!!
// --------private
inline void cpu_switch_register_blocks(SIMJ_U8 new_reg_block) {

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
inline void cpu_set_current_PSW( PSW new_psw ) {
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
inline SIMJ_U16 cpu_find_bit(SIMJ_U16 bit_value) {
	SIMJ_U16 new_bit = 16;
	int j;
	for (j = 0; j < 16; j++) {
		if ((bit[j] & bit_value) != 0) {
			new_bit = j;
			break;
		}
	}
	return new_bit;
}

// ===========================================================================================================
// read internal register -- as address by CPU commands, not front panel.
// int_reg_addr
// [ 0 0 0 0 ] [ GRF ]                [IOP, MBC, CTXT, MAP] [ EAU, CPU]
//
// front panel format
// [ 0 0 0 0 ] [ MAP, IOP, MBC, CTXT] [ EAU, CPU]           [ GRF ] 
// --------private
SIMJ_U16 cpu_convert_to_front_panel_address(SIMJ_U16 cpu_internal_reg_addr) {

	return(		((cpu_internal_reg_addr & 0x0010) < 7) |			// map
				((cpu_internal_reg_addr & 0x0080) < 3) |			// iop
				((cpu_internal_reg_addr & 0x0040) < 3) |			// mbc
				((cpu_internal_reg_addr & 0x0020) < 3) |			// ctx
				((cpu_internal_reg_addr & 0x000f) < 4) |			// eau, cpu
				((cpu_internal_reg_addr & 0x0f00) > 8));			// grf

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
	// 	retval = cpu_extended_memory_ctrl_reg;
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
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x018) {									// -------- OS
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x019) {									// -------- IS
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x01A) {									// -------- IR
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x01B) {									// -------- PTB (PR)
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x01C) {									// -------- LIT / AG
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x01D) {									// -------- TW
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x01E) {									// -------- DSW
	// 	retval = cpu_interrupt_enabled;
	// }
	// else if (front_panel_address == 0x01F) {									// -------- MDB
	// 	retval = cpu_interrupt_enabled;
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
void cpu_classic_7860() {



	// -------- potentially global values  -- for now local, may need to be global later



	// -------- local variables
	INSTRUCTION instruction = { .all = 0 };

	// -------- for temporary use
	static SIMJ_S32     temp32_addr_calc = 0;

	// -------- parsed instruction values
	static SIMJ_U16			tmp_instr_src = 0;
	static SIMJ_U16			tmp_instr_src_dbl = 0;
	static SIMJ_U16			tmp_instr_dest = 0;
	static SIMJ_U16			tmp_instr_dest_dbl = 0;

	static VAL16			tmp16_src_value = { .uval = 0 };
	static VAL16			tmp16_dest_value = { .uval = 0 };
	static VAL16			tmp16_result_value = { .uval = 0 };

	SIMJ_U64				tempu64_val1 = 0;
	SIMJ_U64				tempu64_val2 = 0;
	SIMJ_U64				tempu64_val3 = 0;
	SIMJ_U64				tempu64_val4 = 0;

	register SIMJ_U16		tmp16_STK_ADDR;
	register SIMJ_U16		tmp16_STK_HSA;
	register SIMJ_U16		tmp16_STK_CSP;
	register SIMJ_U16		tmp16_STK_LSA;
	register SIMJ_U16		tmp16_STK_NW;		// numb of words to allocate, same as NV
	register SIMJ_U16		tmp16_STK_NR;		// numb of registers to copy.

	register VAL16			tmp16_val1 = { .uval = 0 };
	register VAL16			tmp16_val2 = { .uval = 0 };
	register VAL16			tmp16_val3 = { .uval = 0 };
	register VAL16			tmp16_val4 = { .uval = 0 };
	register VAL16			tmp16_val5 = { .uval = 0 };
	register VAL16			tmp16_val6 = { .uval = 0 };
	register VAL16			tmp16_val7 = { .uval = 0 };
	register VAL16			tmp16_val8 = { .uval = 0 };
	register VAL16			tmp16_val9 = { .uval = 0 };

	register VAL32			tmp32_val1 = { .uval = 0 };
	register VAL32			tmp32_val2 = { .uval = 0 };
	register VAL32			tmp32_val3 = { .uval = 0 };
	register VAL32			tmp32_val4 = { .uval = 0 };
	register VAL32			tmp32_val5 = { .uval = 0 };
	register VAL32			tmp32_val6 = { .uval = 0 };

	register VAL64			tmp64_val1 = { .uval = 0 };
	register VAL64			tmp64_val2 = { .uval = 0 };
	register VAL64			tmp64_val3 = { .uval = 0 };
	register VAL64			tmp64_val4 = { .uval = 0 };
	register VAL64			tmp64_val5 = { .uval = 0 };
	register VAL64			tmp64_val6 = { .uval = 0 };



#ifdef SHIT
	SIMJ_S16				temp16_val10 = 0;
#endif
	int						temp_bit = 0;

	int						j = 0;
	bool					do_branch = false;

	char op_code_string[20] = "";

	int new_int = 0;

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

	LARGE_INTEGER StartingTime = { .QuadPart = 0 };

// -------- DEBUG
	char junkxx[200] = { 0 };
// -------- END DEUBG

#include "cpu_instruction_memory_macros.h"
#include "cpu_cond_code_macros.h"


	// TODO: Set CC for UIT
#define UNIMPLEMENTED_INSTRUCTION {\
					fprintf(stderr, "\n unimplemented instruction 0x%04x @  0x%04x\n",instruction.all, program_counter);\
					fprintf(stdout, "\n unimplemented instruction 0x%04x @  0x%04x\n",instruction.all, program_counter);\
					SET_CC_Z(false);\
					SET_CC_O(false);\
					if ( cpu_interrupt_active >= cpu_intr_UIT ) {\
						gbl_fp_runlight = false;\
						printf("\nCpu halted.  UIT while interrupt 0-4 active.\n");\
						cmd_process_print_prompt();\
					}\
					EnterCriticalSection(&CritSectInterruptRequest);\
					cpu_interrupt_request |= cpu_intr_UIT;\
					LeaveCriticalSection(&CritSectInterruptRequest);\
					goto end_instruction;\
					}

// TODO: Set CC for UIT
#define ILLEGAL_INSTRUCTION {\
					fprintf(stderr, "\n Illegal instruction 0x%04x @ 0x%04x\n",instruction.all, program_counter);\
					SET_CC_Z(false);\
					SET_CC_O(false);\
					EnterCriticalSection(&CritSectInterruptRequest);\
					cpu_interrupt_request |= cpu_intr_UIT;\
					LeaveCriticalSection(&CritSectInterruptRequest);\
					goto end_instruction;\
					}

#define PRIV_INSTR_TRAP {\
					fprintf(stderr, "\n privileged instruction 0x%04x @ 0x%04x\n",instruction.all, program_counter);\
					EnterCriticalSection(&CritSectInterruptRequest);\
					cpu_interrupt_request |= cpu_intr_sys_protect;\
					LeaveCriticalSection(&CritSectInterruptRequest);\
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
//					EnterCriticalSection(&CritSectInterruptRequest);\
//					cpu_interrupt_request |= cpu_intr_sys_protect;\
//					LeaveCriticalSection(&CritSectInterruptRequest);\
//					goto end_instruction;\
//					}


#define GET_HOP_OFFSET ( ( instruction.all & 0x0040 ) ? (instruction.all & 0x007f) | 0xff80 : instruction.all & 0x007f )


#define GET_NEXT_PROGRAM_COUNTER_HOP  ( (SIMJ_U16)(program_counter + GET_HOP_OFFSET) )

#define SET_NEXT_PROGRAM_COUNTER(A)	{\
					program_counter = (A);\
					}

#define PROGRAM_COUNTER_ONE_WORD_INSTRUCT ( program_counter+1 )
#define PROGRAM_COUNTER_TWO_WORD_INSTRUCT ( program_counter+2 )
#define PROGRAM_COUNTER_THREE_WORD_INSTRUCT ( program_counter+3 )
#define PROGRAM_COUNTER_FOUR_WORD_INSTRUCT ( program_counter+4 )


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
#define IS_PRIV_MODE  (cpu_priv_mode | !cpu_virtual_mode) 

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

		// -------- check for new interrupts
		// TODO: optimize this !!
		if (!skip_interrupt_determination ) {

			// -------- make interrupt request register synchronous.
			EnterCriticalSection(&CritSectInterruptRequest);
			cpu_interrupt_request |= cpu_interrupt_request_external;
			cpu_interrupt_request_external = 0;
			LeaveCriticalSection(&CritSectInterruptRequest);

			if (((tmp16_val1.uval = (cpu_interrupt_request & cpu_interrupt_enabled & cpu_interrupt_active_mask)) > (cpu_interrupt_active & cpu_interrupt_active_mask))) {

				// --------find out which interrupt we are requesting
				new_int = cpu_find_bit(tmp16_val1.uval);

				// --------save current process_status_double_word in dedicated memory location
				if (new_int < 16) {

					// --------store the current program counter and status double word to dedicated memory location
					SET_MEMORY_VALUE_ABS(0x0020 + (new_int * 2), program_counter);
					SET_MEMORY_VALUE_ABS(0x0041 + (new_int * 2), cpu_get_current_PSW().all);

					// --------set new PSW and new PC -- to go to the interrupt processing routine.
					// TODO: check to implement SI and DI routines.

					// -------- DI
					if (new_int == 12) {
						print_delayed_message = false;
						// -------- turn off DI request and report error.
						// -------- Request ownership of the critical section.
						EnterCriticalSection(&CritSectInterruptRequest);
						tmp_dev_addr = cpu_get_next_DI_request();
						// -------- if nothing found, report error  (try device address 0 )
						if (tmp_dev_addr == 0xffff) {
							tmp_dev_addr = 0;
						}
						//	//  cpu_interrupt_request &= ~cpu_intr_DI;
						//	print_delayed_message = true;
						//	// -------- set the normal address for DI if no devices requested...
						//	program_counter = GET_MEMORY_VALUE_ABS(0x0021 + (new_int * 2));
						//	tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0040 + (new_int * 2));
						//	cpu_set_current_PSW(tmp_PSW);

						//	// --------set that the interrupt is active!
						//	cpu_interrupt_active |= bit[new_int];
						//	cpu_interrupt_active_mask = mask[new_int];
						//  cpu_interrupt_active_current = new_int;
						//}
						//else {
						program_counter = GET_MEMORY_VALUE_ABS(0x0080 + tmp_dev_addr);
						tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0040 + (new_int * 2));
						cpu_set_current_PSW(tmp_PSW);

						// --------set that the interrupt is active!
						cpu_interrupt_active |= bit[new_int];
						cpu_interrupt_active_mask = mask[new_int];
						cpu_interrupt_active_current = new_int;
						//}
						// -------- Release ownership of the critical section.
						LeaveCriticalSection(&CritSectInterruptRequest);
						if (print_delayed_message) {
							fprintf(stderr, "\n cpu - Erroneous DI interrupt request, ignored.\n");
						}
					}

					// -------- SI
					else if (new_int == 13) {
						print_delayed_message = false;
						// -------- Request ownership of the critical section.
						EnterCriticalSection(&CritSectInterruptRequest);
						tmp_dev_addr = cpu_get_next_SI_request();
						// -------- if nothing found, report error
						if (tmp_dev_addr == 0xffff) {
							tmp_dev_addr = 0;
						}
						// 	// -------- turn off SI request and report error.
						// 	//  cpu_interrupt_request &= ~cpu_intr_SI;
						// 	print_delayed_message = true;
						// 	// -------- set the normal address for DI if no devices requested...
						// 	program_counter = GET_MEMORY_VALUE_ABS(0x0021 + (new_int * 2));
						// 	tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0040 + (new_int * 2));
						// 	cpu_set_current_PSW(tmp_PSW);

						// 	// --------set that the interrupt is active!
						// 	cpu_interrupt_active |= bit[new_int];
						// 	cpu_interrupt_active_mask = mask[new_int];
						//	cpu_interrupt_active_current = new_int;
						// }
						// else {
						program_counter = GET_MEMORY_VALUE_ABS(0x00C0 + tmp_dev_addr);
						tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0040 + (new_int * 2));
						cpu_set_current_PSW(tmp_PSW);

						// --------set that the interrupt is active!
						cpu_interrupt_active |= bit[new_int];
						cpu_interrupt_active_mask = mask[new_int];
						cpu_interrupt_active_current = new_int;
						// }
					// -------- Release ownership of the critical section.
						LeaveCriticalSection(&CritSectInterruptRequest);
						if (print_delayed_message) {
							fprintf(stderr, "\n cpu - Erroneous SI interrupt request, ignored.\n");
						}
					}

					// -------- All other interrupts
					else {
						// --------get instruction that caused issue..
						tmp16_val1.uval = GET_MEMORY_VALUE_IM(program_counter);
						program_counter = GET_MEMORY_VALUE_ABS(0x0021 + (new_int * 2));
						tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x0040 + (new_int * 2));
						cpu_set_current_PSW(tmp_PSW);
						if (new_int == 1) {
							SET_CC_N(true);		// global
						}
						else if (new_int == 2) {
							SET_CC_N(false);
							SET_CC_Z(true);
						}
						else if (new_int == 4) {
							SET_CC_Z(((tmp16_val1.uval & 0xff00) == 0x2300));  // rex
							SET_CC_O(((tmp16_val1.uval & 0xff00) == 0xb300) || ((tmp16_val1.uval & 0xff0f) == 0xe805)); // exi, exr
						}
						// --------set that the interrupt is active!
						cpu_interrupt_active |= bit[new_int];
						cpu_interrupt_active_mask = mask[new_int];
						cpu_interrupt_active_current = new_int;
					}

					// fprintf(stderr, "\n cpu - new interrupt level %d, new pc 0x%04x, new psw 0x%04x\n", new_int, program_counter, cpu_get_current_PSW().all);

				}
			}
		}

		// -------- reset skip interrupt determination (this is used so we get one instruction executed before the next interrupt)
		skip_interrupt_determination = false;

		// -------- fetch the next instruction
		instruction.all = GET_MEMORY_VALUE_IM(program_counter);

		// -------- add to instruction trace.
		EnterCriticalSection(&CritSectInstructionTrace);
		instruction_trace[instruction_trace_index++] = GET_ABS_MEMORY_ADDR_IM(program_counter);
		instruction_trace_index &= 0x03ff;		// 0 -  1023
		LeaveCriticalSection(&CritSectInstructionTrace);

		// -------- check for EXI and EXR instructions.  These are special.  Replace the value
		// -------- of instruction with the results of that instruction.
		if (instruction.parts.op_code == OP_EXR) {
			cpu_inst_used[instruction.parts.op_code]++;	// debug
			instruction.all = GET_SOURCE_REGISTER_VALUE | GET_DESTINATION_REGISTER_VALUE;
		}
		else if ((instruction.all & 0xff0f) == 0xe805) {
			cpu_inst_used[instruction.parts.op_code]++;	// debug
			instruction.all = GET_MEMORY_VALUE_IMMEDIATE | GET_DESTINATION_REGISTER_VALUE;
			program_counter++;		// it only makes sense to do this.  document doesn't mention this.
		}


		// -------- debug -- fill array of used instrutions
		cpu_inst_used[instruction.parts.op_code]++;

		// -------- if verbose display the instruction.
		if (gbl_verbose_debug) {

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
				gbl_fp_runlight = false;
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
					cpu_operand_map = (tmp16_val1.uval >> 13) & 0x0007;
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
					cpu_operand_map = (tmp16_val1.uval >> 5) & 0x0007;
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
					tmpu8 = (SIMJ_U8)(tmp16_val1.uval >> 8) & 0x000f;
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
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> 13) & 0x0007;		// contains map number. (IMAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;			// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val5.sval *= -1;											// positive length
					// TODO: Make instruction abortable and restartable
					for (j = 0; j < tmp32_val5.sval; j++) {
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val4.uval + j].all = 0;
					}
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
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> 5) & 0x0007;		// contains map number. (OMAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;			// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val5.sval *= -1;											// positive length
					// TODO: Make instruction abortable and restartable
					for (j = 0; j < tmp32_val5.sval; j++) {
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val4.uval + j].all = 0;
					}
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
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> 13) & 0x0007;		// contains map number. (IMAP)
					tmp32_val3.uval = (tmp32_val1.uval & 0x00001fff) << 8;			// abs page address (MIAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;			// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val5.sval *= -1;											// positive length
					// --------DEBUG
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, imap: 0x%04x \n", 
							junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start page:   0x%08x \n", tmp32_val3.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val5.uval);
					// --------END DEBUG
					// TODO: Make instruction abortable and restartable
					for (j = 0; j < tmp32_val5.sval; j++) {
						tmp32_val6.uval = tmp32_val4.uval + j;
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val6.uval].all = GET_MEMORY_VALUE_ABS(tmp32_val3.uval + tmp32_val6.uval);
					}
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
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> 5) & 0x0007;		// contains map number. (OMAP)
					tmp32_val3.uval = (tmp32_val1.uval & 0x00001fff) << 8;			// abs page address (MIAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;			// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val5.sval *= -1;											// positive length
					// --------DEBUG
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%08x, imap: 0x%04x \n",
						junkxx, instruction.all, tmp32_val1.uval, tmp16_val2.uval);
					fprintf(stderr, "       start page:   0x%08x \n", tmp32_val3.uval);
					fprintf(stderr, "       start offset: 0x%08x \n", tmp32_val4.uval);
					fprintf(stderr, "       length:       0x%08x \n", tmp32_val5.uval);
					// --------END DEBUG
					// TODO: Make instruction abortable and restartable
					for (j = 0; j < tmp32_val5.sval; j++) {
						tmp32_val6.uval = tmp32_val4.uval + j;
						cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val6.uval].all = GET_MEMORY_VALUE_ABS(tmp32_val3.uval + tmp32_val6.uval);
					}
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
					tmp16_val2.uval = (GET_REGISTER_VALUE(15) >> 5) & 0x0007;		// contains map number. (OMAP)
					tmp32_val3.uval = (tmp32_val1.uval & 0x00001fff) << 8;			// abs page address (MIAP)
					tmp32_val4.uval = (tmp32_val1.uval >> 24) & 0x000000ff;			// VP (starting offset in map
					tmp32_val5.uval = 0xffffff00 | ((tmp32_val1.uval >> 16) & 0x000000ff);	// neg length
					tmp32_val5.sval *= -1;											// positive length
					for (j = 0; j < tmp32_val5.sval; j++) {
						tmp32_val6.uval = tmp32_val4.uval + j;
						SET_MEMORY_VALUE_ABS(tmp32_val3.uval + tmp32_val6.uval, cpu_virtual_mem_map[tmp16_val2.uval].entry[tmp32_val6.uval].all );
					}
					SET_CC_N(false);
					SET_CC_Z(false);
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

				// --  A	UIT
			case 10:
				// --  C	UIT
			case 12:
			default:
				UNIMPLEMENTED_INSTRUCTION;
				break;
			}
			break;

		case  OP_LXR:			// 0x02
#if SIMJ_SIM_CPU == 7830
			switch (instruction.parts.dest_reg & 0x9) {

				case 0:		// LXR  -- Load Extended Memory Control Register
#endif
					if (IS_PRIV_MODE) {
						cpu_extended_memory_ctrl_reg = GET_SOURCE_REGISTER_VALUE;		// doesn't really do anything on 7830
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
					}
					else {
						PRIV_INSTR_TRAP;
					}
#if SIMJ_SIM_CPU == 7830
					break;

				case 1:		// SPR - Set Protect (IF)  (disable non-virtual protection emulation)  7830 only
					if (IS_PRIV_MODE) {
						cpu_nonvirt_prot_enabled = false;
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
#endif
			break;

			


		case  OP_RMPS_RMWS:	    // 0x03 -         
#if SIMJ_SIM_CPU == 7830
			if (instruction.parts.dest_reg == 0) {			// SLP -- Set Lower Protect Value
				cpu_nonvirt_prot_enabled = true;
				cpu_nonvirt_prot_lowprot_reg = GET_SOURCE_REGISTER_VALUE & 0xff80;
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else if (instruction.parts.src_reg & 0x0001) {	//  RMWS -- Read Memory Word Status
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
					// --------DEBUG
					util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					fprintf(stderr, " % s inst : 0x%04x  reg: 0x%04x, reg or 1: 0x%04x , mem status: 0x%04x \n",
						junkxx, instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp16_val3.uval);
					// --------END DEBUG
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					PRIV_INSTR_TRAP;
				}
			}
			break;

		case  OP_WMS:			// 0x04 --  Write Memory Status
#if SIMJ_SIM_CPU == 7830
			if (instruction.parts.dest_reg == 0) {			// SUP -- Set Upper Protect Value
				cpu_nonvirt_prot_enabled = true;
				cpu_nonvirt_prot_upprot_reg = GET_SOURCE_REGISTER_VALUE & 0xff80;
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {											// WMS -- Write Memory Status
#endif
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;				// TODO: implement this.
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
				// TODO: This doesn't do anything it just prints some information and keeps going.
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				// --------DEBUG
				fprintf(stderr, " pc: 0x%04x  DMPI: 0x%04x  Reg Value: 0x%08x\n", program_counter, instruction.all, tmp32_val1.uval);
				// --------END DEBUG
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				PRIV_INSTR_TRAP;
			}
			break;

		case  OP_MMRB:			// 0x06  --  Move Memory File to RegisterBlock Section of Context
			if (IS_PRIV_MODE) {
				// -------- get dest register value - holding register block value.
				tmp16_val1.uval = (GET_DESTINATION_REGISTER_VALUE >> 8) & 0x000f;
				// -------- get memory address in src register value
				tmp16_val2.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
				// -------- copy mem (x) style to reg 1-15 in particular block
				// TODO: Make register block copy more efficient.
				for (j = 1; j < 16; j++) {
					cpu_register_blocks[tmp16_val1.uval].reg16[j] = GET_MEMORY_VALUE_OM(tmp16_val2.uval + j - 1);
				}
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				PRIV_INSTR_TRAP;
			}
			break;

		case  OP_MRBM:			// 0x07  --  Move Register-Block Section of Context File to Memory
			if (IS_PRIV_MODE) {
				// TODO: Current registers may not match current register block.
				// -------- get dest register value - holding register block value.
				tmp16_val1.uval = (GET_DESTINATION_REGISTER_VALUE >> 8) & 0x000f;
				// -------- get memory address in src register value
				tmp16_val2.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
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
				SET_DESTINATION_REGISTER_VALUE(cpu_get_current_PSW().all & 0x000f);
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
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
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
				tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					// TODO: should sign of truncated value forced to be correct?
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

				// -------- E	BRM  -- Branch to Microroutine Immediate        
			case 14:
				if (IS_PRIV_MODE) {
					UNIMPLEMENTED_INSTRUCTION;
				}
				else {
					PRIV_INSTR_TRAP;
				}
				break;

				// -------- F	BRMI  -- Branch to Microroutine Immediate        
			case 15:
				if (IS_PRIV_MODE) {
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
					// TODO: OR IN O if divide by 0
					SET_CC_O(   !( ((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000) ) );	// not a 16 bit result
					SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				}
				else {
					ILLEGAL_INSTRUCTION;
				}
				break;

			// TODO: This is the same as ADRD ????
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
				// SET_CC_Z(true);		// It is a REX
				// SET_CC_O(false);	// It is not a EXR or EXI
				// -------- Request ownership of the critical section.
				EnterCriticalSection(&CritSectInterruptRequest);
				cpu_interrupt_request |= cpu_intr_UIT;
				// -------- Release ownership of the critical section.
				LeaveCriticalSection(&CritSectInterruptRequest);
				break;

			case  OP_CAR:			// 0x24  --  CAR  --  Clear Active and Return        
				if (IS_PRIV_MODE) {
					// --------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, " pc: 0x%04x, %s inst: 0x%04x, active: 0x%04x, request: 0x%04x\n", 
					// 	program_counter, junkxx, instruction.all, 
					// 	cpu_interrupt_active, cpu_interrupt_request);
					// --------END DEBUG
					// --------find highest active interrupt
					// TODO: Use cpu_interrupt_active_current instead of search.
					new_int = 16;
					for (j = 0; j < 16; j++) {
						if ((bit[j] & cpu_interrupt_active) != 0) {
							new_int = j;
							break;
						}
					}
					// --------get return process_status_double_word from dedicated memory location or register
					if (new_int < 16) {
						// --------set return new PSW and PC
						// --------get return process pc and status double word from dedicated memory location
						if ((tmp16_val1.uval = (instruction.parts.src_reg & 0x000e)) == 0) {

							program_counter = GET_MEMORY_VALUE_ABS(0x20 + (new_int * 2));
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x41 + (new_int * 2));
							cpu_set_current_PSW(tmp_PSW);
						}
						// -------- get return pc and psw from register.
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval + 1);
							cpu_set_current_PSW(tmp_PSW);
						}
						// --------clear active for that interrupt
						cpu_interrupt_active &= bitnot[new_int];

						// --------nothing active, reset masks.
						if (cpu_interrupt_active == 0) {
							cpu_interrupt_active_mask = mask[15];
							cpu_interrupt_active_current = 16;
						}
						// --------set information for newest high active interrupt.
						else {
							new_int = cpu_find_bit(cpu_interrupt_active);
							cpu_interrupt_active_mask = mask[new_int];
							cpu_interrupt_active_current = new_int;
						}
					}
					// -------- If no interrupt is active when the CIR instruction is executed, the dedicated locations O and 1 are used to restore the PSD if register Rs = 0.
					// -------- Assume this applies to CAR too.
					// -------- TODO: check to make certain that 0 - PC and 1 - PSW (it only makes sense)
					else {
						// --------set return new PSW and PC
						if ((tmp16_val1.uval = (instruction.parts.src_reg & 0x000d)) == 0) {
							// --------get current process pc and status double word from dedicated memory location
							program_counter = GET_MEMORY_VALUE_ABS(0);
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(1);
							cpu_set_current_PSW(tmp_PSW);
						}
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval + 1);
							cpu_set_current_PSW(tmp_PSW);
						}
						// --------DEBUG
						// fprintf(stderr, "      nothing was active\n" );
						// --------END DEBUG
					}
					// -------- allow one instruction to execute before the next interrupt.
					skip_interrupt_determination = true;
					// --------DEBUG
					// fprintf(stderr, "     new pc 0x%04x, new active: 0x%04x, request: 0x%04x\n",
					// 	program_counter, cpu_interrupt_active, cpu_interrupt_request);
					// disp_psw(stderr, cpu_get_current_PSW());
					// --------END DEBUG
				}
			
				// --------not priviledged.
				else {
					PRIV_INSTR_TRAP;
				}
				break;

			case  OP_CIR:			// 0x25  --  CIR  --  Clear interrupt and Return        
				if (IS_PRIV_MODE) {
					// --------DEBUG
					// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
					// fprintf(stderr, " pc: 0x%04x, %s inst: 0x%04x, active: 0x%04x, request: 0x%04x\n",
					// 	program_counter, junkxx, instruction.all,
					// 	cpu_interrupt_active, cpu_interrupt_request);
					// --------END DEBUG
					// TODO: Use cpu_interrupt_active_current variable.
					// --------find highest active interrupt
					new_int = 16;
					for (j = 0; j < 16; j++) {
						if ((bit[j] & cpu_interrupt_active) != 0) {
							new_int = j;
							break;
						}
					}
					// --------get return process_status_double_word from dedicated memory location or register
					if (new_int < 16) {
						// --------set return new PSW and PC
						// --------get return process pc and status double word from dedicated memory location
						if ((tmp16_val1.uval = (instruction.parts.src_reg & 0x000e)) == 0) {
							program_counter = GET_MEMORY_VALUE_ABS(0x20 + (new_int * 2));
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(0x41 + (new_int * 2));
							cpu_set_current_PSW(tmp_PSW);
						}
						// -------- get return pc and psw from register.
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval + 1);
							cpu_set_current_PSW(tmp_PSW);
						}
						// --------clear active for that interrupt
						cpu_interrupt_active &= bitnot[new_int];

						// --------clear request
						// -------- Request ownership of the critical section.
						EnterCriticalSection(&CritSectInterruptRequest);
						if ((new_int == 12) && (cpu_interrupt_DI_total_request_count == cpu_interrupt_DI_total_proc_count)) {
							cpu_interrupt_request &= bitnot[new_int];
						}
						else if ( (new_int == 13) && (cpu_interrupt_SI_total_request_count == cpu_interrupt_SI_total_proc_count)) {
							cpu_interrupt_request &= bitnot[new_int];
						}
						else {
							cpu_interrupt_request &= bitnot[new_int];
						}
						// -------- Release ownership of the critical section.
						LeaveCriticalSection(&CritSectInterruptRequest);

						// --------nothing active, reset masks.
						if (cpu_interrupt_active == 0) {
							cpu_interrupt_active_mask = mask[15];
						}
						// --------set information for newest high active interrupt.
						else {
							new_int = cpu_find_bit(cpu_interrupt_active);
							cpu_interrupt_active_mask = mask[new_int];
							cpu_interrupt_active_current = new_int;
						}
					}
					// -------- If no interrupt is active when the CIR instruction is executed, the dedicated locations O and 1 are used to restore the PSD if register Rs = 0.
					// -------- TODO: check to make certain that 0 - PC and 1 - PSW (it only makes sense)
					else {
						// --------set return new PSW and PC
						if ((tmp16_val1.uval = (instruction.parts.src_reg & 0x000d)) == 0) {
							// --------get current process pc and status double word from dedicated memory location
							program_counter = GET_MEMORY_VALUE_ABS(0);
							tmp_PSW.all = GET_MEMORY_VALUE_ABS(1);
							cpu_set_current_PSW(tmp_PSW);
						}
						else {
							program_counter = GET_REGISTER_VALUE(tmp16_val1.uval);
							tmp_PSW.all = GET_REGISTER_VALUE(tmp16_val1.uval + 1);
							cpu_set_current_PSW(tmp_PSW);
						}
						// --------DEBUG
						// fprintf(stderr, "      nothing was active\n");
						// --------END DEBUG
					}
					// -------- allow one instruction to execute before the next interrupt.
					skip_interrupt_determination = true;
					// --------DEBUG
					// fprintf(stderr, "     new pc 0x%04x, new active: 0x%04x, request: 0x%04x\n",
					// 	program_counter, cpu_interrupt_active, cpu_interrupt_request);
					// disp_psw(stderr, cpu_get_current_PSW());
					// --------END DEBUG
				}
				// --------not priviledged.
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
							if (cpu_interrupt_active == 0) {
								cpu_interrupt_active_mask = mask[15];
							}
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
							if ((tmp16_val1.uval & SIE_ALLOWED) == cpu_intr_DI) {
								// -------- Request ownership of the critical section.
								EnterCriticalSection(&CritSectInterruptRequest);
								if (cpu_interrupt_DI_total_proc_count != cpu_interrupt_DI_total_request_count) {
									cpu_interrupt_request |= cpu_intr_DI;
									fprintf(stderr, " SIE needed to re-request outstanding DI.\n");
								}
								// -------- Release ownership of the critical section.
								LeaveCriticalSection(&CritSectInterruptRequest);
							}
							else if ((tmp16_val1.uval & SIE_ALLOWED) == cpu_intr_SI) {
								// -------- Request ownership of the critical section.
								EnterCriticalSection(&CritSectInterruptRequest);
								if (cpu_interrupt_SI_total_proc_count != cpu_interrupt_SI_total_request_count) {
									cpu_interrupt_request |= cpu_intr_SI;
									fprintf(stderr, " SIE needed to re-request outstanding SI.\n");
								}
								// -------- Release ownership of the critical section.
								LeaveCriticalSection(&CritSectInterruptRequest);
							}
							// TOD: FIX THIS - should not need to poke clock int when enabled.  Likely throttling issue.
							// -------- Poke clock interrupt if enabled.
							// else if ((tmp16_val1.uval & SIE_ALLOWED) == cpu_intr_clock) {
							// 	EnterCriticalSection(&CritSectInterruptRequest);
							// 	// -------- Release ownership of the critical section.
							// 	cpu_interrupt_request |= cpu_intr_clock;
							// 	LeaveCriticalSection(&CritSectInterruptRequest);
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
							// -------- Request ownership of the critical section.
							EnterCriticalSection(&CritSectInterruptRequest);
							cpu_interrupt_request |= ( tmp16_val1.uval & SIR_ALLOWED );
							// -------- Release ownership of the critical section.
							LeaveCriticalSection(&CritSectInterruptRequest);
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
							cpu_interrupt_active &= ( tmp16_val1.uval | RIA_ALLOWED_NOT);
							if (cpu_interrupt_active == 0) {
								cpu_interrupt_active_mask = mask[15];
							}
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

					// RIE  --  Reset interrupt Enable         
					case 4:
						if (IS_PRIV_MODE) {
							cpu_interrupt_enabled &= ( tmp16_val1.uval | RIE_ALLOWED_NOT);
							// --------Undocumented, it looks like requests may also be reset...if not active...
							// if ((cpu_interrupt_active & bit[GET_SOURCE_REGISTER_NUMB]) == 0) {
							// 	// -------- Request ownership of the critical section.
							// 	EnterCriticalSection(&CritSectInterruptRequest);
							// 	cpu_interrupt_request &= (tmp16_val1.uval | RIR_ALLOWED_NOT);
							// 	// TODO: make the si/di stuf more efficient.
							// 	if ((cpu_interrupt_DI_total_proc_count != cpu_interrupt_DI_total_request_count) && ((cpu_interrupt_enabled & cpu_intr_DI) != 0)) {
							// 		cpu_interrupt_request |= cpu_intr_DI;
							// 		//fprintf(stderr, " RIR needed to re-request outstanding DI.\n");
							// 	}
							// 	if ((cpu_interrupt_SI_total_proc_count != cpu_interrupt_SI_total_request_count) && ((cpu_interrupt_enabled & cpu_intr_SI) != 0)) {
							// 		cpu_interrupt_request |= cpu_intr_SI;
							// 		//fprintf(stderr, " RIR needed to re-request outstanding SI.\n");
							// 	}
							// 	// -------- Release ownership of the critical section.
							// 	LeaveCriticalSection(&CritSectInterruptRequest);
							// }
							// --------DEBUG
							util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
							fprintf(stderr, " % s inst : 0x%04x  level: %02d enabled: 0x%04x\n", junkxx, instruction.all, GET_SOURCE_REGISTER_NUMB, cpu_interrupt_enabled);
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
							// -------- Request ownership of the critical section.
							EnterCriticalSection(&CritSectInterruptRequest);
							cpu_interrupt_request &= ( tmp16_val1.uval | RIR_ALLOWED_NOT);
							// TODO: make the si/di stuf more efficient.
							if ( (cpu_interrupt_DI_total_proc_count != cpu_interrupt_DI_total_request_count) && ( ( cpu_interrupt_enabled & cpu_intr_DI ) != 0 ) ) {
								cpu_interrupt_request |= cpu_intr_DI;
								//fprintf(stderr, " RIR needed to re-request outstanding DI.\n");
							}
							if ((cpu_interrupt_SI_total_proc_count != cpu_interrupt_SI_total_request_count) && ( ( cpu_interrupt_enabled & cpu_intr_SI ) != 0 ) ) {
								cpu_interrupt_request |= cpu_intr_SI;
								//fprintf(stderr, " RIR needed to re-request outstanding SI.\n");
							}
							// -------- Release ownership of the critical section.
							LeaveCriticalSection(&CritSectInterruptRequest);
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
						tmp32_val1.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
						tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp32_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp32_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
						tmp64_val1.uval = GET_MEMORY_VALUE_DIRECT_TRIPLE;
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
						tmp64_val1.uval = GET_MEMORY_VALUE_DIRECT_QUAD;
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
						tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
						tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp64_val2.uval = GET_MEMORY_VALUE_DIRECT_TRIPLE;
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
					tmp64_val2.uval = GET_MEMORY_VALUE_DIRECT_QUAD;
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
					// TODO: Implement get triple register
					tmp64_val1.uval = GET_DESTINATION_REGISTER_VALUE_TRIPLE;
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp64_val2.uval = GET_MEMORY_VALUE_DIRECT_TRIPLE;
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
					tmp64_val2.uval = GET_MEMORY_VALUE_DIRECT_QUAD;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp64_val2.uval = GET_MEMORY_VALUE_DIRECT_TRIPLE;
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
					tmp64_val2.uval = GET_MEMORY_VALUE_DIRECT_QUAD;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					if (tmp_instr_src == 0) {
#if SIMJ_SIM_CPU == II15
						tmp16_val1.uval = 0x3200;		// classic cpu. II/15
#elif SIMJ_SIM_CPU == 7860
						tmp16_val1.uval = 0x3000;		// classic cpu. 7860
#elif SIMJ_SIM_CPU == 7830
						tmp16_val1.uval = 0x2000;		// classic cpu. 7830
#elif SIMJ_SIM_CPU == 3285
						tmp16_val1.uval = 0x3100;		// classic cpu. 32/85 (maybe others as well.
#else
#error BAD CPU type specifiied
#endif
						if (cpu_virtual_mode)
							tmp16_val1.uval |= 0x4000;		// relocatable mode.
						SET_CC_N(false);
						SET_CC_Z(true);
						SET_CC_O(false);
						SET_CC_C(false);
					}
					// -------- ISA - Input Status from 1/0 Group A
					else {
						if (iop_input_status_proc[tmp_instr_src] != NULL) {
							tmp16_val1.uval = (*iop_input_status_proc[tmp_instr_src])(tmp_instr_src);
							// printf(" status 0x%04x\n", tmp16_val1.uval);
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
				SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
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
				tmp16_src_value.uval = GET_SOURCE_REGISTER_VALUE;
				tmp16_dest_value.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_result_value.uval =  tmp16_dest_value.uval - tmp16_src_value.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_result_value));
				SET_CC_N(ISVAL16_NEG(tmp16_result_value));
				SET_CC_O_SUB(tmp16_dest_value, tmp16_src_value, tmp16_result_value);
				SET_CC_C_SUB(tmp16_dest_value, tmp16_src_value, tmp16_result_value);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
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
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
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
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ZBRB:			// 0x72  --  Zero Bit in Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & bitnot[GET_SOURCE_REGISTER_NUMB];
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C(false);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_OBRB:			// 0x73  --  OR Bit in Register and Branch Unconditionally     
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | bit[GET_SOURCE_REGISTER_NUMB];
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(false);
				SET_CC_O(false);
				SET_CC_C(true);
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IMMEDIATE);
				break;

			case  OP_XBRB:			// 0x74  --  XBRB  --  Exclusive OR Bit in Register and Branch if Nonzero        
				tmp16_val1.uval = bit[GET_SOURCE_REGISTER_NUMB];
				tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE ^ tmp16_val1.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val2.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & tmp16_val2.uval) != 0);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_LBRB:			// 0x75 -- Load Bit in Register and Branch Unconditionally
				tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
				SET_DESTINATION_REGISTER_VALUE(bit[tmp_instr_src]);
				SET_CC_Z(false);
				SET_CC_N(tmp_instr_src == 0);
				SET_NEXT_PROGRAM_COUNTER( GET_MEMORY_VALUE_IMMEDIATE );
				break;

			case  OP_TBRB:			// 0x76  --  Test Bit in Register and Branch if One    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_SOURCE_REGISTER_NUMB]) != 0);
				CONDITIONAL_BRANCH( TEST_CC_C, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_GMRB:			// 0x77  --  Generate Mask in Register and Branch Unconditionally     
				tmp_instr_src = GET_SOURCE_REGISTER_NUMB;
				SET_DESTINATION_REGISTER_VALUE(mask[tmp_instr_src]);
				SET_CC_Z(false);
				SET_CC_N(true);		// TODO: manual doesn't say it is aways set, but it is -- check
				SET_CC_O(tmp_instr_src == 0);
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IMMEDIATE);
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
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
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
				CONDITIONAL_BRANCH( TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ETRB:			// 0x7a  --  Extract Register from Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				CONDITIONAL_BRANCH(tmp16_val1.uval != 0, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;
	
			case  OP_ORRB:			// 0x7b  --  OR Register to Register and Branch if Nonzero    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_XORB:			// 0x7c  --  Exclusive OR Register to Register and Branch if Nonzero   
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TRRB:			// 0x7d --  Transfer Register to Register and Branch if Nonzero    
				tmp16_val1.uval = GET_SOURCE_REGISTER_VALUE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				CONDITIONAL_BRANCH( TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TERB:			// 0x7e  --  Test Register and Branch if any Ones Compare    
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_SOURCE_REGISTER_VALUE;
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				CONDITIONAL_BRANCH(tmp16_val1.uval != 0, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
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
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;


			case  OP_ABMM:			// 	        0x80  --  Add Bit in Memory            
				tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT & tmp16_val1.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C(false);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_OBMM:			//  0x82  --  OR Bit in Memory·        
				tmp16_val1.uval = bit[instruction.parts.dest_reg];
				tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT | tmp16_val1.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
				SET_CC_Z(false);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C(true);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_TBMM:			// x83  --  Test Bit(s) in Memory        
				tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case  OP_ABMB:			// 		    0x84  --  Add Bit in Memory and Branch if Nonzero    
				tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
				tmp16_val2.uval = bit[instruction.parts.dest_reg];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE_2ND, program_counter + 3);
				break;

			case  OP_ZBMB:			// 		    0x85  --  Zero Bit in Memory and Branch if Nonzero    
				tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
				tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT & tmp16_val1.uval;
				SET_MEMORY_VALUE_DIRECT(tmp16_val2.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C(false);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE_2ND, program_counter + 3);
				break;

			case  OP_TBMB:			// 		    0x86  --  Test Bit(s) in Memory and Branch if One    
				tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				CONDITIONAL_BRANCH(TEST_CC_C, GET_MEMORY_VALUE_IMMEDIATE_2ND, program_counter + 3);
				break;

			case  OP_CBMB:			// 		    0x87  --  CBMB  --  Compare Bit and Memory and Branch if Equal or Less  
				tmp16_val1.uval = bit[instruction.parts.dest_reg];
				tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				if (TEST_CC_Z) {
					SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_TWO_WORD_INSTRUCT));
				}
				else if (TEST_CC_LT) {
					SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_THREE_WORD_INSTRUCT));
				}
				else {
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_FOUR_WORD_INSTRUCT);
				}
				break;

			case  OP_LDXT_STXT_LDMT_STMT:	//	0x88
				switch (instruction.parts.dest_reg & 0x0003) {

					case 0:				//  STXT  --  Store Triple-Register into Memory Tripleword (Short-Indexed)      
						// TODO: Use Triple macros
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
						// TODO: Use triple macros.
						tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
						tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000c;
						tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
						tmp16_val3.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval + 1);
						tmp16_val4.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval + 2);
						SET_REGISTER_VALUE(tmp_instr_dest, tmp16_val2.uval);
						SET_REGISTER_VALUE(tmp_instr_dest + 1, tmp16_val3.uval);
						SET_REGISTER_VALUE(tmp_instr_dest + 2, tmp16_val4.uval);
						SET_CC_Z((tmp16_val2.uval == 0) && (tmp16_val3.uval == 0) && (tmp16_val4.uval == 0));
						SET_CC_N(ISVAL16_NEG(tmp16_val2));
						SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
						break;

					case 2:				//  STMT  --  Store Triple-Register into Memory Triple-Word       
						// TODO: Use triple macros.
						tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
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
						// TODO: Triple Macros
						tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
						tmp_instr_dest =  GET_DESTINATION_REGISTER_NUMB & 0x000c;
						tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
						tmp16_val3.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval + 1);
						tmp16_val4.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval + 2);
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
						// TODO: Check method for triple compare.
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
						tmp32_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE;
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
					tmp32_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE;
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

			case  OP_AUG8F:			//         0x8f
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
						break;
					default:
						ILLEGAL_INSTRUCTION;		// TODO: The PC will be wrong when this is taken!
						program_counter--;			// KLUDGE In prep for the increment below....
						do_branch = false;
						break;
				}
				// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
				// fprintf(stderr, " %s inst: 0x%04x, pc: 0x%04x, Rx: 0x%04x, branch addr: 0x%04x, dobranch: %s\n",
				// 	junkxx, instruction.all, program_counter, GET_SOURCE_REGISTER_VALUE, GET_MEMORY_VALUE_SHORT_INDEXED, (do_branch ? "true" : "false"));
				// disp_psw(stderr, cpu_get_current_PSW());
				// disp_cur_reg(stderr);
				CONDITIONAL_BRANCH(do_branch, GET_SOURCE_REGISTER_VALUE, PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
				break;


		case  OP_ABSM:			// 	        0x90  --  Add Bit in Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED & tmp16_val1.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED | tmp16_val1.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				SET_CC_Z(false);
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
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
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
				tmp16_val2.uval = bit[instruction.parts.dest_reg];
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_ZBSB:			// 	        0x95  --  Zero Bit in Memory (Short-Displaced} and Branch     
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED & tmp16_val1.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val2.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
				SET_CC_N(ISVAL16_NEG(tmp16_val2));
				SET_CC_O(false);
				SET_CC_C(false);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_TBSB:			// 	        0x96  --  Test Bit(s) in Memory (Short-Displaced) and Branch if One   
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_CC_O(false);
				SET_CC_C((tmp16_val1.uval& bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
				CONDITIONAL_BRANCH(TEST_CC_C, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_CBSB:			// 	        0x97  --  Compare Bit and Memory (Short-Displaced) and Branch if Equal or Less 
			tmp16_val1.uval = bit[instruction.parts.dest_reg];
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_ONE_WORD_INSTRUCT));
			}
			else if (TEST_CC_LT) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_TWO_WORD_INSTRUCT));
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(program_counter + 3);
			}
			break;

		case  OP_ABXM:			// 	        0x98  --  Add Bit in Memory (Short-Indexed)       
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
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
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED & tmp16_val1.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
			SET_CC_N(ISVAL16_NEG(tmp16_val2));
			SET_CC_O(false);
			SET_CC_C(false);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_OBXM:			// 	        0x9a  --  OR Bit in Memory (Short-Indexed)       
			tmp16_val1.uval = bit[instruction.parts.dest_reg];
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED | tmp16_val1.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(false);
			SET_CC_N(ISVAL16_NEG(tmp16_val2));
			SET_CC_O(false);
			SET_CC_C(true);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_TBXM:			// 	        0x9b  --  Test Bit(s) in Memory (Short-Indexed)       
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_O(false);
			SET_CC_C((tmp16_val1.uval& bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ABXB:			// 	        0x9c  --  Add Bit in Memory (Short-Indexed) and Branch if Nonzero   
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			tmp16_val2.uval = bit[instruction.parts.dest_reg];
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ZBXB:			// 	        0x9d  --  Zero Bit in Memory (Short-Indexed} and Branch if Nonzero   
			tmp16_val1.uval = bitnot[instruction.parts.dest_reg];
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED & tmp16_val1.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val2.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val2));
			SET_CC_N(ISVAL16_NEG(tmp16_val2));
			SET_CC_O(false);
			SET_CC_C(false);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_TBXB:			// 	        0x9e  --  Test Bit in Memory (Short-Indexed) and Branch if One   
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_O(false);
			SET_CC_C((tmp16_val1.uval & bit[GET_DESTINATION_REGISTER_NUMB]) != 0);
			CONDITIONAL_BRANCH(TEST_CC_C, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_CBXB:			// 	        0x9f  --  CBXB  --  Compare Bit and Memory (Short-Indexed) and Branch     
			tmp16_val1.uval = bit[instruction.parts.dest_reg];
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_ONE_WORD_INSTRUCT));
			}
			else if (TEST_CC_LT) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_TWO_WORD_INSTRUCT));
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(program_counter + 3);
			}
			break;


		case  OP_MPM:			// 	        0xa0  --  MPM  --  Multiply Register by Memory        
			tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
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
				// TODO: OR IN O if divide by 0
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
					tmp32_val1.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
						// TODO: OR IN O if divide by 0
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
						tmp32_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
						// TODO: OR IN O if divide by 0
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
			tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
			tmp16_val5.uval = 0;	// temp register to check for all zero.
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
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
					tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
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
			tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;
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
				SET_NEXT_PROGRAM_COUNTER( GET_MEMORY_VALUE_IMMEDIATE );
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
				// TODO: make this a macro...
				tmp16_val1.uval = GET_REGISTER_VALUE(tmp_instr_src);		//  base address
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_src + 1);	//  word offset and byte numb
				temp_bit = tmp16_val2.uval & 0x0001;						// byte numb 0=high/first or 1=low/second
				tmp16_val2.sval >>= 1;										// get word offset
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;		// complete word address
#ifdef SHIT
				memcpy(&temp16_val10, &tmp16_val5.uval, sizeof(temp16_val10));
				// printf("\n after memcpy dest 0x%04x, src 0x%04x\n", temp16_val10, tempu16_val5);
				temp16_val10 = temp16_val10 >> 1;
				tmp32_val1.uval = tmp16_val1.uval;
				temp32_addr_calc = tmp32_val1.sval + (SIMJ_S32)temp16_val10;
				// printf("\n addr calc base 0x%08x, offset 0x%04x, calc 0x%08x\n", tempu32_val1, temp16_val10, temp32_addr_calc);
				tmp16_val3.uval = (SIMJ_U16)(temp32_addr_calc & 0x0000ffff);
				// printf("\n to 16 bit 0x%04x, calc 0x%08x\n", tmp16_val3.uval, temp32_addr_calc);
#endif
				// --------read memory value
				tmp16_val4.uval = GET_MEMORY_VALUE_OM(tmp16_val3.uval);
				if (temp_bit == 0) {
					tmp16_val5.uval = (tmp16_val4.uval >> 8 ) & 0x00ff;
				}
				else {
					tmp16_val5.uval = (tmp16_val4.uval & 0x00ff);
				}
				// -- STORE BYTE IN RIGHT SIDE OF REG (0 in left half )...
				SET_DESTINATION_REGISTER_VALUE(tmp16_val5.uval);
				SET_CC_CHAR(tmp16_val5.uval);
				if (gbl_verbose_debug) {
#ifdef SHIT
					fprintf(stderr, " Load byte pc: 0x%04x  inst: 0x%04x - from mem addr 0x%04x, valu 0x%04x, rx 0x%04x, rx+1 0x%04x\n",
						program_counter, instruction.all, tmp16_val3.uval, tmp16_val2.uval, tmp16_val1.uval, tmp16_val5.uval);
#else
					fprintf(stderr, " LBX pc: 0x%04x  inst: 0x%04x, calc addr: 0x%04x, value: 0x%04x, base addr: 0x%04x, off: 0x%04x\n",
						program_counter, instruction.all, tmp16_val3.uval, tmp16_val4.uval, tmp16_val1.uval, tmp16_val2.uval);
#endif
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
				// TODO: Fix all this !
				// TODO: make this a macro...
				tmp16_val1.uval = GET_REGISTER_VALUE(tmp_instr_src);		//  base address
				tmp16_val2.uval = GET_REGISTER_VALUE(tmp_instr_src + 1);	//  word offset and byte numb
				temp_bit = tmp16_val2.uval & 0x0001;						// byte numb 0=high/first or 1=low/second
				tmp16_val2.sval >>= 1;										// get word offset
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;		// complete word address
#ifdef SHIT
				memcpy(&temp16_val10, &tmp16_val5.uval, sizeof(temp16_val10));
				// printf("\n after memcpy dest 0x%04x, src 0x%04x\n", temp16_val10, tempu16_val5);
				temp_bit = temp16_val10 & 0x0001;
				temp16_val10 = temp16_val10 >> 1;
				tmp32_val1.uval = tmp16_val1.uval;
				temp32_addr_calc = tmp32_val1.sval + (SIMJ_S32)temp16_val10;
				// printf("\n addr calc base 0x%08x, offset 0x%04x, calc 0x%08x\n", tempu32_val1, temp16_val10, temp32_addr_calc);
				tmp16_val3.uval = (SIMJ_U16)(temp32_addr_calc & 0x0000ffff);
				// printf("\n to 16 bit 0x%04x, calc 0x%08x\n", tmp16_val3.uval, temp32_addr_calc);
#endif
				tmp16_val4.uval = GET_DESTINATION_REGISTER_VALUE & 0x00ff;
				tmp16_val5.uval = GET_MEMORY_VALUE_OM(tmp16_val3.uval);
				if (temp_bit != 0) {
					tmp16_val5.uval = (tmp16_val5.uval & 0xff00) | (tmp16_val4.uval);
				}
				else {
					tmp16_val5.uval = (tmp16_val5.uval & 0x00ff) | (tmp16_val4.uval<<8);
				}
				SET_MEMORY_VALUE_OM(tmp16_val3.uval, tmp16_val5.uval);
				if (gbl_verbose_debug)
#ifdef SHIT
					fprintf(stderr, " Set byte pc: 04%04x  inst: 04%04x - in mem addr 0x%04x, valu 0x%04x, rx 0x%04x, rx+1 0x%04x\n",
						program_counter, instruction.all, tmp16_val3.uval, tmp16_val2.uval, tmp16_val1.uval, tmp16_val5.uval);
#else
					fprintf(stderr, " SBX pc: 0x%04x  inst: 0x%04x, calc addr: 0x%04x, value: 0x%04x, base addr: 0x%04x, off: 0x%04x\n",
						program_counter, instruction.all, tmp16_val3.uval, tmp16_val5.uval, tmp16_val1.uval, tmp16_val2.uval);
#endif
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			break;


		case  OP_MPS:			// 	        0xb0  --  MPS  --  Multiply Register by Memory (Short-Displaced)       
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				// TODO: OR IN O if divide by 0
				SET_CC_O(!(((tmp32_val3.uval & 0xffff8000) == 0) || ((tmp32_val3.uval & 0xffff8000) == 0xffff8000)));	// not a 16 bit result
				SET_CC_C(ISVAL32_ZERO(tmp32_val2));		// divide by zero
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			else {
				ILLEGAL_INSTRUCTION;
			}
			break;

		case  OP_SCCC:			// 	        0xb2  - SCCC  --  Sefect Current Condition Codes in PSD      
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
			ILLEGAL_INSTRUCTION;
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
						tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
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
						tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
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
			temp32_addr_calc = GET_MEMORY_DIRECT_ADDR;
			tmp16_val1.uval = GET_MEMORY_VALUE_OM(temp32_addr_calc);
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_MEMORY_VALUE_OM(temp32_addr_calc, tmp16_val2.uval);
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
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
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
				// TODO: OR IN O if divide by 0
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
			tmp16_STK_ADDR = GET_MEMORY_DIRECT_ADDR;								// address of stack definition + 1
			tmp16_STK_LSA = GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_LSA);	// (LSA) lowest address of stack
			tmp16_STK_CSP = GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_CSP);	// (CSP) current stack pointer.
			tmp16_STK_HSA = GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_HSA);	// (HSA) highest address of stack
			tmp16_val4.uval = GET_MEMORY_VALUE_IMMEDIATE_2ND;			// contains NV, NR-1
			tmp16_STK_NW = tmp16_val4.uval & 0x00ff;					// NV also called NW
			tmp16_STK_NR = ((tmp16_val4.uval & 0xf00) >> 8) + 1;		// NR

			tmp16_val7.uval = tmp16_STK_HSA - tmp16_STK_CSP;	// number of words stored on stack.

			// --------DEBUG
			// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
			// fprintf(stderr, "\n %s inst: 0x%04x, stack addr: 0x%04x  \n", junkxx, instruction.all, tmp16_STK_ADDR);
			// fprintf(stderr, "        NV numb vals:       0x%04x  \n", tmp16_STK_NW);
			// fprintf(stderr, "        NR numb reg:        0x%04x  \n", tmp16_STK_NR);
			// fprintf(stderr, "        LSA:     0x%04x\n", tmp16_STK_LSA);
			// fprintf(stderr, "        CSP:     0x%04x\n", tmp16_STK_CSP);
			// fprintf(stderr, "        HSA:     0x%04x\n", tmp16_STK_HSA);
			// fprintf(stderr, "        OV ADDR: 0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_OVL_JUMP));
			// fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_SAV_R1));
			// fprintf(stderr, "        stored:  0x%04x\n", tmp16_val7.uval);
			// disp_cur_reg(stderr);
			// --------END DEBUG


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
				// --------DEBUG
				// fprintf(stderr, "        *** UNDERFLOW ***  \n");
				// fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_SAV_R1));
				// disp_cur_reg(stderr);
				// --------END DEBUG
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_OM(tmp16_STK_ADDR + STACK_OFF_OVL_JUMP));		// jump to over/under flow routine
			}
			// -------all is good, process stack pull
			else {
				tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
				tmp16_val8.uval = 0;	// for determining if all are zero.
				// --------if the number of registers is greater that the number of values
				// --------set the register value to 0.
				// --------NO -- PULLING BEYOND THE HIGH STACK ADDRESS IS EXPECTED.!!!
				for (j = 0; j < tmp16_STK_NR; j++) {
					tmp16_val9.uval = GET_MEMORY_VALUE_OM(tmp16_STK_CSP + j);
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
				// --------DEBUG
				// fprintf(stderr, "    NEW CSP:     0x%04x\n", tmp16_STK_CSP);
				// disp_cur_reg(stderr);
				// --------END DEBUG
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
			tmp16_val1.uval = GET_MEMORY_DIRECT_ADDR;					// address of stack definition + 1
			tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval - 1);	// (LSA) lowest address of stack
			tmp16_val3.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval );	// (CSP) current stack pointer.
			tmp16_val9.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval + 1);	// (HSA) highest address of stack
			tmp16_val4.uval = GET_MEMORY_VALUE_IMMEDIATE_2ND;			// contains NV, NR-1
			tmp16_val5.uval = tmp16_val4.uval & 0x00ff;					// NV  (oddly this field is 9 bits???)
			tmp16_val6.uval = ((tmp16_val4.uval & 0xf00) >> 8) + 1;		// NR
			//if (tmp16_val6.uval > tmp16_val5.uval) {
			//	tmp16_val6.uval = tmp16_val5.uval;
			//}
			//                  CSP - LSA
			tmp16_val7.uval = tmp16_val3.uval - tmp16_val2.uval;		// number of words left

			// --------DEBUG
			// util_get_opcode_disp(instruction.all, &junkxx[0], (size_t)200);
			// fprintf(stderr, "\n %s inst: 0x%04x, stack addr: 0x%04x  \n", junkxx, instruction.all, tmp16_val1.uval );
			// fprintf(stderr, "        NW numb words:      0x%04x  \n", tmp16_val5.uval);
			// fprintf(stderr, "        NR numb reg:        0x%04x  \n", tmp16_val6.uval);
			// fprintf(stderr, "        LSA:     0x%04x\n", tmp16_val2.uval);
			// fprintf(stderr, "        CSP:     0x%04x\n", tmp16_val3.uval);
			// fprintf(stderr, "        HSA:     0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval+1));
			// fprintf(stderr, "        OV ADDR: 0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval+2));
			// fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval+3));
			// fprintf(stderr, "        left:    0x%04x\n", tmp16_val7.uval);
			// disp_cur_reg(stderr);
			// --------END DEBUG


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
				// --------DEBUG
				// fprintf(stderr, "        *** OVERFLOW ***  \n");
				// fprintf(stderr, "        SAV R1:  0x%04x\n", GET_MEMORY_VALUE_OM(tmp16_val1.uval + 3));
				// disp_cur_reg(stderr);
				// --------END DEBUG
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_OM(tmp16_val1.uval + 2));		// jump to over/under flow routine
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
						// --------DEBUG
						// fprintf(stderr, "    pushed mem addr: 0x%04x, val:  0x%04x\n", tmp16_val3.uval + j, tmp16_val8.uval);
						// --------END DEBUG
					//}
				}
				SET_MEMORY_VALUE_OM(tmp16_val1.uval, tmp16_val3.uval);	// store new stack pointer
				// --------DEBUG
				// fprintf(stderr, "    NEW CSP:     0x%04x\n", tmp16_val3.uval);
				// --------END DEBUG
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;

		case  OP_LFX:			// 	        0xbc
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;
			tmp16_val1.uval = GET_MEMORY_ADDR_SHORT_INDEXED;
			tmp16_val5.uval = 0;
			if (tmp_instr_dest > 7) {
				for (j = tmp_instr_dest; j < 16; j++) {
					tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
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
					tmp16_val2.uval = GET_MEMORY_VALUE_OM(tmp16_val1.uval);
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
					// TODO: update
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
					// --------DEBUG
					fprintf(stderr, " LDAM 0x%04x, R: 0x%04x, Rv1: 0x%04x, addr: 0x%08x, value: : 0x%04x\n",
						instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp32_val3.uval, tmp16_val3.uval);
					// --------END DEUBG
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
					// TODO: make macro deal with absolute wrap around.
					tmp16_val5.uval = GET_MEMORY_VALUE_ABS( (tmp32_val2.uval + tmp32_val3.uval) & 0x001fffff);
					// -------- check access rights.
					tmp16_val8.uval = (tmp16_val5.uval >> 14) & 0x0003;
					// --------DEBUG
					fprintf(stderr, " LDVM 0x%04x, R: 0x%08x, miap: 0x%08x, vp: 0x%08x, pw: 0x%08x, page entry: 0x%04x \n",
						instruction.all, tmp32_val1.uval, tmp32_val2.uval, tmp32_val3.uval, tmp32_val4.uval, tmp16_val5.uval);
					// --------END DEUBG
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
						// --------DEBUG
						fprintf(stderr, "          abs addr: 0x%08x, value: 0x%04x \n", tmp32_val6.uval, tmp16_val7.uval);
						// --------END DEUBG
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
						// --------DEBUG
						fprintf(stderr, " STAM 0x%04x, R: 0x%04x, Rv1: 0x%04x, addr: 0x%08x, value: : 0x%04x\n",
							instruction.all, tmp16_val1.uval, tmp16_val2.uval, tmp32_val3.uval, tmp16_val3.uval);
						// --------END DEUBG
						// TODO: Check to see if CC should be set.  NO
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
						// TODO: make macro deal with absolute wrap around.
						tmp16_val5.uval = GET_MEMORY_VALUE_ABS((tmp32_val2.uval + tmp32_val3.uval) & 0x001fffff);
						// --------DEBUG
						fprintf(stderr, " STVM 0x%04x, R: 0x%08x, miap: 0x%08x, vp: 0x%08x, pw: 0x%08x, page entry: 0x%04x \n",
							instruction.all, tmp32_val1.uval, tmp32_val2.uval, tmp32_val3.uval, tmp32_val4.uval, tmp16_val5.uval);
						// --------END DEUBG
							// -------- check access rights.
						tmp16_val8.uval = (tmp16_val5.uval >> 14) & 0x0003;
						if (tmp16_val8.uval == 0) {
							SET_CC_N(false);
							SET_CC_Z(false);
							SET_CC_O(true);
							SET_CC_C(true);
							// --------DEBUG
							fprintf(stderr, "          no access \n");
							// --------END DEUBG
						}
						else if (tmp16_val8.uval != 3) {
							SET_CC_N(false);
							SET_CC_Z(false);
							SET_CC_O(false);
							SET_CC_C(true);
							// --------DEBUG
							fprintf(stderr, "          no write access \n");
							// --------END DEUBG
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
							// --------DEBUG
							fprintf(stderr, "          abs addr: 0x%08x, value: 0x%04x \n", tmp32_val6.uval, tmp16_val7.uval);
							// --------END DEUBG
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
			tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
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
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_DIRECT;
			SET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ORMM:			// 	        0xc2  --  OR Register to Memory        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_VALUE_DIRECT;
			SET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_CRM:			// 	        0xc3  --
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ADMB:			// 	        0xc4  --  Add Register to Memory and Branch if Nonzero      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
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
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_ETMB:			// 	        0xc5  --  Extract Register from Memory and Branch if Nonzero    
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_DIRECT;
			SET_MEMORY_VALUE_DIRECT(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_TRMB:			// 	        0xc6  --  Test Register and Memory and Branch if any Ones Compare  
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_DIRECT;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE_2ND, program_counter + 3);
			break;

		case  OP_CRMB:			// 	        0xc7  --  CRMB  --  Compare Register with Memory and Branch Equal or Less   
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_TWO_WORD_INSTRUCT));
			}
			else if (TEST_CC_LT) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(program_counter + 3));
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(program_counter + 4);
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
				tmp32_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
				tmp32_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
			// -------TODO: Is a critical section needed here for TSBM ??
			tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
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
					tmp32_val1.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
					tmp32_val2.uval = GET_MEMORY_VALUE_DIRECT_DOUBLE;
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
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_ORSM:			// 	        0xd2  --  OR Register to Memory (Short-Displaced)       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_CRS:			// 	        0xd3  -- CRS  --  Compare Register with Memory  (Short-Displaced)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
				tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val3.uval);
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_ETSB:			// 	        0xd5  --  Extract Register from Memory (Short- Displaced) and Branch if Nonzero  
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_MEMORY_VALUE_SHORT_DISPLACED(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_TRSB:			// 	        0xd6  --  Test Register and Memory (Short- Displaced)  and Branch if any ones Compare        
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			// }
			break;

		case  OP_CRSB:			// 	        0xd7  --  CRSB  --  Compare Register with Memory (Short- Displaced) and Branch if Equal or Less
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				if (TEST_CC_Z) {
					SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_ONE_WORD_INSTRUCT));
				}
				else if (TEST_CC_LT) {
					SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_TWO_WORD_INSTRUCT));
				}
				else {
					SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
				}
			// }
			break;

		case  OP_ADXM:			// 	        0xd8  --  Add Register to Memory (Short-Indexed)       
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
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
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ORXM:			// 	        0xda  --  OR Register to Memory (Short-Indexed)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_CRX:			// 	        0xdb  -  CRX  --  Compare Register with Memory (Short-Indexed)       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ADXB:			// 	        0xdc  --  Add Register to Memory (Short-Indexed) and Branch if Nonzero   
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			tmp16_val2.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val3.uval = tmp16_val1.uval + tmp16_val2.uval;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val3.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_ADD(tmp16_val1, tmp16_val2, tmp16_val3);
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ETXB:			// 	        0xdd  --  Extract Register from Memory Short- Indexed and Branch if Nonzero  
			tmp16_val1.uval = ~GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_MEMORY_VALUE_SHORT_INDEXED(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_TRXB:			// 	        0xde  --  Test Register and Memory (Short-Indexed) and Branch if any Ones Compare 
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			CONDITIONAL_BRANCH(TEST_CC_NOT_Z, GET_MEMORY_VALUE_IMMEDIATE, PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_CRXB:			// 	        0xdf  --  CRXB  --  Compare Register with Memory (Short- Indexed) and Branch if Equal or Less
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			if (TEST_CC_Z) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_ONE_WORD_INSTRUCT));
			}
			else if (TEST_CC_LT) {
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_IM(PROGRAM_COUNTER_TWO_WORD_INSTRUCT));
			}
			else {
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_THREE_WORD_INSTRUCT);
			}
			break;


		case  OP_ADM:			// 	        0xe0  --  Add Memory to Register        
			tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
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
			tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ETM:			// 	        0xe2  --  Extract Memory from Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_MEMORY_VALUE_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_ORM:			// 	        0xe3  --  OR Memory to Register        
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
			tmp16_val2.uval = GET_MEMORY_VALUE_DIRECT;
			tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_XOM:			// 	        0xe4  --  Exclusive OR Memory to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_MEMORY_VALUE_DIRECT;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
			break;

		case  OP_LDM:			// 	        0xe5  --   Load Register from Memory        
			tmp16_val1.uval = GET_MEMORY_VALUE_DIRECT;
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
			SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_DIRECT_ADDR);
			break;


		case  OP_AUGE8:			//         0xe8
			switch (instruction.parts.src_reg) {

				// --  0	ADI  --  Add Memory(Immediate) to Register	
			case 0:
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
				tmp32_val3.sval = tmp16_val1.sval;	// sign extend to 32 bits.
				SET_DESTINATION_REGISTER_VALUE_DOUBLE(tmp32_val3.uval);
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  2	ADES  --  Add Immediate with Extended Sign	
			case 2:
				tmp32_val1.uval = GET_DESTINATION_REGISTER_VALUE_DOUBLE;
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
				tmp32_val2.sval = tmp16_val2.sval;	// sign extend to 32 bits.
				tmp32_val3.uval = tmp32_val1.uval - tmp32_val2.uval;
				SET_CC_N(ISVAL32_NEG(tmp32_val3));
				SET_CC_Z(ISVAL32_ZERO(tmp32_val3));
				SET_CC_O_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_CC_C_SUB_DOUBLE(tmp32_val1, tmp32_val2, tmp32_val3);
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

				// --  5	EXI  --  Execute Immediate	
				// -------- the only way to get here is if the result of an EXI or EXR instruction is an EXI
				// -------- instruction.
			case 5:
				ILLEGAL_INSTRUCTION;
				break;

				// --  6	MPI  --  Multiply Immediate	
			case 6:
				tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val5.uval = GET_MEMORY_VALUE_OM(GET_MEMORY_VALUE_IMMEDIATE_2ND);  // -- control variable address
				tmp16_val1.uval = GET_MEMORY_VALUE_OM(tmp16_val5.uval) + 1;	// -- increment control variable
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);		// -- update control variable
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;		// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH(cpu_cond_code_n, GET_MEMORY_VALUE_IMMEDIATE_3RD, program_counter + 4);
				break;

				// --  D	LTDL  --  Loop Terminationwit h Directly Addressed Control - Variable - and Literal Terminal Value	
			case 13:
				tmp16_val5.uval = GET_MEMORY_VALUE_IMMEDIATE_2ND;	// -- control variable address
				tmp16_val1.uval = GET_MEMORY_VALUE_OM(tmp16_val5.uval) + 1;	// -- increment control variable
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);		// -- update control variable
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;		// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH( cpu_cond_code_n, GET_MEMORY_VALUE_IMMEDIATE_3RD, program_counter+4 );
				break;

				// --  E	LTlD  --  Lo0p Termination with Indirectly Addressed Control Variable and Directly Addressed Terminal Value	
			case 14:
				tmp16_val5.uval = GET_MEMORY_VALUE_OM( GET_MEMORY_VALUE_IMMEDIATE_2ND );	// -- control variable address
				tmp16_val1.uval = GET_MEMORY_VALUE_OM(tmp16_val5.uval) + 1;	// -- increment control variable
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);		// -- update control variable
				tmp16_val2.uval = GET_MEMORY_VALUE_OM(GET_MEMORY_VALUE_IMMEDIATE);		// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH(cpu_cond_code_n, GET_MEMORY_VALUE_IMMEDIATE_3RD, program_counter + 4);
				break;

				// --  F	LTDD  --  Loop Termination with Directly Addressed - Control Variable and Directly Addressed Terminal Value
			case 15:
				tmp16_val5.uval = GET_MEMORY_VALUE_IMMEDIATE_2ND;	// -- control variable address
				tmp16_val1.uval = GET_MEMORY_VALUE_OM(tmp16_val5.uval) + 1;	// -- increment control variable
				SET_MEMORY_VALUE_OM(tmp16_val5.uval, tmp16_val1.uval);		// -- update control variable
				tmp16_val2.uval = GET_MEMORY_VALUE_OM( GET_MEMORY_VALUE_IMMEDIATE );		// -- terminal value
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				CONDITIONAL_BRANCH(cpu_cond_code_n, GET_MEMORY_VALUE_IMMEDIATE_3RD, program_counter + 4);
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
				tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
				SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
				tmp16_val3.uval = tmp16_val1.uval & ~tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// TETI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
				tmp16_val3.uval = tmp16_val1.uval | tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// TORI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
				tmp16_val3.uval = tmp16_val1.uval ^ tmp16_val2.uval;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
				SET_CC_N(ISVAL16_NEG(tmp16_val3));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1:		// TXOI
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE;
				tmp16_val2.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_TWO_WORD_INSTRUCT);
				break;

			case 1: // -------- LDF  load floating immediate
				if (TEST_VALID_DOUBLE_REGISTER(GET_DESTINATION_REGISTER_NUMB)) {
					tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB & 0x000e;
					tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
					tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
					// TODO: Use Triple macros
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
					tmp16_val1.uval = GET_MEMORY_VALUE_IMMEDIATE;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
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
				tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_MEMORY_VALUE_SHORT_DISPLACED;
				SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
				SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
				SET_CC_N(ISVAL16_NEG(tmp16_val1));
				SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			// }
			break;

		case  OP_LDS:			// 	        0xf5  --  Load Register from Memory Short-displaced       
			// -------- These are all short displaced.   check for address fault first.
			// if (SHORT_DISPLACED_ADDR_FAULT) {
			// 	BAD_SHORT_DISPLACED_ADDR_TRAP;
			// }
			// else {
				tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_DISPLACED;
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
				SET_NEXT_PROGRAM_COUNTER(GET_MEMORY_VALUE_OM((SIMJ_U16)(GET_REGISTER_VALUE(2)+GET_HOP_OFFSET)));
			}
			// HOP  --  Hop Unconditionally          
			else {
				SET_NEXT_PROGRAM_COUNTER(GET_NEXT_PROGRAM_COUNTER_HOP);
			}
			break;

		case  OP_ADX:			// 	        0xf8  --  Add Memory (Short-Indexed) to Register       
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
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
			tmp16_val2.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			tmp16_val3.uval = tmp16_val1.uval - tmp16_val2.uval;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val3.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val3));
			SET_CC_N(ISVAL16_NEG(tmp16_val3));
			SET_CC_O_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_CC_C_SUB(tmp16_val1, tmp16_val2, tmp16_val3);
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ETX:			// 	        0xfa  --  Extract Memory (Short-Indexed) from Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE & ~GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_ORX:			// 	        0xfb  --  OR Memory (Short-Indexed) to Register       
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE | GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_XOX:			// 	        0xfc  --  Exclusive OR Memory to Register (Short-Displaced)      
			tmp16_val1.uval = GET_DESTINATION_REGISTER_VALUE ^ GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_LDX:			// 	        0xfd  --  Load Register from Memory (Short-Indexed)       
			tmp16_val1.uval = GET_MEMORY_VALUE_SHORT_INDEXED;
			SET_DESTINATION_REGISTER_VALUE(tmp16_val1.uval);
			SET_CC_N(ISVAL16_NEG(tmp16_val1));
			SET_CC_Z(ISVAL16_ZERO(tmp16_val1));
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_STX:			// 	        0xfe  --  Store Register in Memory (Short-Indexed)       
			SET_MEMORY_VALUE_SHORT_INDEXED( GET_DESTINATION_REGISTER_VALUE );
			SET_NEXT_PROGRAM_COUNTER(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			break;

		case  OP_BRX_BLX:			// 	    0xff
			tmp_instr_dest = GET_DESTINATION_REGISTER_NUMB;

			// BRX  --  Branch (Short-Indexed) Unconditionally         
			if (tmp_instr_dest == 0) {
				// nothing extra to do here...
			}
			// BLX  --  Branch and Link (Short-Indexed)        
			else {
				SET_DESTINATION_REGISTER_VALUE(PROGRAM_COUNTER_ONE_WORD_INSTRUCT);
			}
			SET_NEXT_PROGRAM_COUNTER(GET_SOURCE_REGISTER_VALUE);
			break;

		default:
			UNIMPLEMENTED_INSTRUCTION;
			break;

		}

	// -------- label to goto when instruction fault occurs
	end_instruction:

		// --------increment instruction count.
		cpu_instruction_count++;

		// --------throttle cpu execution to try and match 7860  
		// TODO: make wait optional.
		// TODO: add wait time for each instruction.
		util_high_res_spin_wait_finish(2, StartingTime);		// 200 ns

		// --------extended addressing state machine processing
		switch ( use_extended_addressing_mode ) {
			case 1:
				use_extended_addressing_mode = 2;
				break;
			case 2:
				use_extended_addressing_mode = 0;
		}

		// --------kludge for register 0
		// TODO: Should this really just wait for a nop
		SET_REGISTER_VALUE(0, gbl_fp_switches);

		// -------- reset single step if active
		if (gbl_fp_single_step) {
			gbl_fp_single_step = false;
			WakeByAddressSingle((LPVOID)& gbl_fp_single_step);
		}

		// --------update some front panel values
		// TODO: make these getters so not to slow down CPU.
		// gbl_fp_cc_n_light = cpu_cond_code_n;
		// gbl_fp_cc_z_light = cpu_cond_code_z;
		// gbl_fp_cc_o_light = cpu_cond_code_o;
		// gbl_fp_cc_c_light = cpu_cond_code_c;

	}

	// printf("\n CPU exiting.\n");
	return;
}