// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			user_cmd_execute_functions.c
//
//	Description:	Routines to execute command line functions.
//
//	Externally accessible routines:
//		int cmd_execute_attach_device_a4808(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_a4811(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_console(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_consoletcp(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_disc_ips(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_disc_lx(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_disc_mh(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_modacs1600(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_modacsIII(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_null(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_attach_device_tape(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_cc(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_ci(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_config_file(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_dismount_device(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_exit(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_fill(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_halt(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_help(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_key_lock(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_key_unlock(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_master_clear(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_mount_device(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_power_off(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_power_on(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_rewind_device(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_run(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_badword1(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_badword2(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_mem(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_mem_clear(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_mem_restore(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_mem_save(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_pc(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_reg(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_set_switches(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_badword1(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_badword2(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_clock(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_devices(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_inst_count(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_inst_use(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_interrupts(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_key_lock(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_magic(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_mem(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_mmem(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_pc(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_power(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_psw(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_reg(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_run(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_switches(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_trace(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_verbose(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_virt_mode(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_vmap(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_show_vmem(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_step(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_verbose_auto(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_verbose_off(int cmd_cnt_found, char** cmd_line_parsed);
//		int cmd_execute_verbose_on(int cmd_cnt_found, char** cmd_line_parsed);
// 
// Internal only routines:
//		<none>
//
// Notes:
//		These routines must always have the same calling parameters.
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>

static bool cmd_exec_input_from_file = false;
static SIMJ_U32 cmd_exec_last_starting_mem_address = 0;
static SIMJ_U32 cmd_exec_last_ending_mem_address = 1;
static SIMJ_U16 cmd_exec_last_mem_map_number = 0;

// ================================================================================================
//--------config file <filename>
int cmd_execute_config_file(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------config file <filenname>
	if (cmd_exec_input_from_file) {
		printf(" *** ERROR *** Configuration files can not be executed from within a configuratio file.\n");
	}
	// -------- all is good execute the file.
	else {
		user_cmd_config_execute(cmd_line_parsed[2]);
	}
	return 0;
}


// ================================================================================================
//--------attach device 4808.
int cmd_execute_attach_device_a4808(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device a4811.
int cmd_execute_attach_device_a4811(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device console.
int cmd_execute_attach_device_console(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device consoletcp.
int cmd_execute_attach_device_consoletcp(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device disc_ips.
int cmd_execute_attach_device_disc_ips(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device disc lx.
int cmd_execute_attach_device_disc_lx(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device disc mh.
int cmd_execute_attach_device_disc_mh(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device modacs1600
int cmd_execute_attach_device_modacs1600(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device modacs III.
int cmd_execute_attach_device_modacsIII(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device null
int cmd_execute_attach_device_null(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}

// ================================================================================================
//--------attach device tape
int cmd_execute_attach_device_tape(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
	if (cmd_cnt_found < 7) {
		printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	// TODO: add bus number
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = false;
		bool good4 = false;
		bool good5 = false;
		SIMJ_U16 device_type;
		SIMJ_U16 dev_addr;
		SIMJ_U16 bus;
		SIMJ_U16 prio;
		SIMJ_U16 dmp;
		// --------string of the device type
		good1 = user_cmd_parse_device_type(cmd_line_parsed[2], &device_type);
		// --------device address 0-#3f (probably should be 1-3f)
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &dev_addr, 0, 0x3f);
		if (!good2) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[3]);
		}
		// --------IO bus 0-3
		good3 = user_cmd_parse_u16(cmd_line_parsed[4], &bus, 0, 3);
		if (!good3) {
			printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_line_parsed[4]);
		}
		// --------priority 0-#f
		good4 = user_cmd_parse_u16(cmd_line_parsed[5], &prio, 0, 15);
		if (!good4) {
			printf(" *** ERROR *** Not a valid priority: %s\n", cmd_line_parsed[5]);
		}
		// --------dmp 0-#3f
		good5 = user_cmd_parse_u16(cmd_line_parsed[6], &dmp, 0, 0x3f);
		if (!good5) {
			printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_line_parsed[6]);
		}
		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good3 && good4 && good5) {
			user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_cnt_found - 7,
				cmd_line_parsed[7], cmd_line_parsed[8], cmd_line_parsed[9]);
		}
	}
	return 0;
}


// ================================================================================================
//--------mount device.
int cmd_execute_mount_device(int cmd_cnt_found, char** cmd_line_parsed) {

	// -------- check to ensure there are at least 5 parmeters mount device <dev addr> <unit> <filename> readonly
	if (cmd_cnt_found < 5) {
		printf(" *** ERROR *** Mount device command does not have enough parameters.\n");
	}

	// --------so far looks good, call a routine to process the specifics.
	else {
		bool good1 = false;
		bool good2 = false;
		bool good3 = true;
		bool good4 = false;
		SIMJ_U16 dev_addr;
		SIMJ_U16 unit;
		bool readonly = false;

		// --------device address 0-#3f (probably should be 1-3f)
		good1 = user_cmd_parse_u16(cmd_line_parsed[2], &dev_addr, 0, 0x3f);
		if (!good1) {
			printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[2]);
		}
		// --------unit number 0-3
		good2 = user_cmd_parse_u16(cmd_line_parsed[3], &unit, 0, 3);
		if (!good2) {
			printf(" *** ERROR *** Not a unit: %s\n", cmd_line_parsed[3]);
		}
		// -------- if there is a 6th parameter it must be rw, ro, or readonly
		good4 = true;
		if (cmd_cnt_found >= 6) {
			if ((strcmp(cmd_line_parsed[5], "ro") == 0) ||
				(strcmp(cmd_line_parsed[5], "readonly") == 0)) {
				readonly = true;
			}
			else if ((strcmp(cmd_line_parsed[5], "rw") != 0)) {
				printf(" *** ERROR *** Parameter invalid.  It must be: rw, ro, readonly: %s\n", cmd_line_parsed[5]);
				good4 = false;
			}
		}

		// -------- if all good try and initialize this device.
		// -------- individual devices will parse extra parameters as needed.
		if (good1 && good2 && good4) {
			user_cmd_mount_device(dev_addr, unit, cmd_line_parsed[4], readonly);
		}
	}
	return 0;
}


// ================================================================================================
//--------dismount device.
// int cmd_execute_dismount_device(int cmd_cnt_found, char** cmd_line_parsed) {
int cmd_execute_dismount_device(int cmd_cnt_found, char** cmd_line_parsed) {

	bool good1 = false;
	bool good2 = false;
	SIMJ_U16 dev_addr;
	SIMJ_U16 unit;

	// --------device address 0-#3f (probably should be 1-3f)
	good1 = user_cmd_parse_u16(cmd_line_parsed[2], &dev_addr, 0, 0x3f);
	if (!good1) {
		printf(" *** ERROR *** Not a valid device address: %s\n", cmd_line_parsed[2]);
	}
	// --------unit number 0-3
	good2 = user_cmd_parse_u16(cmd_line_parsed[3], &unit, 0, 3);
	if (!good2) {
		printf(" *** ERROR *** Not a unit: %s\n", cmd_line_parsed[3]);
	}

	// -------- if all good try and initialize this device.
	// -------- individual devices will parse extra parameters as needed.
	if (good1 && good2) {
		user_cmd_dismount_device(dev_addr, unit);
	}
	return 0;
}


// ================================================================================================
// --------rewind device
int cmd_execute_rewind_device(int cmd_cnt_found, char** cmd_line_parsed) {
	// TODO: implement rewind device..
	printf("\n rewind device command not implemented yet.\n");
	return 0;
}


// ================================================================================================
//--------show processor status word 
int cmd_execute_show_psw(int cmd_cnt_found, char** cmd_line_parsed) {
	disp_psw(stdout, cpu_get_current_PSW());
	return 0;
}

// ================================================================================================
//--------show clock trigger counts
int cmd_execute_show_clock(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" Clock trigger count: %d\n", cpu_get_clock_trigger_count());
	return 0;
}

// ================================================================================================
//--------show pc (program counter)
int cmd_execute_show_pc(int cmd_cnt_found, char** cmd_line_parsed) {
	disp_pc(stdout, cpu_get_program_counter());
	return 0;
}


// ================================================================================================
//--------show interrupts
int cmd_execute_show_interrupts(int cmd_cnt_found, char** cmd_line_parsed) {
	disp_interrupts(stdout);
	return 0;
}


// ================================================================================================
//--------show devices
int cmd_execute_show_devices(int cmd_cnt_found, char** cmd_line_parsed) {
	disp_devices(stdout);
	return 0;
}


// ================================================================================================
//--------show trace
int cmd_execute_show_trace(int cmd_cnt_found, char** cmd_line_parsed) {
	disp_instruction_trace(stdout);
	return 0;
}


// ================================================================================================
//--------show key
int cmd_execute_show_key_lock(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" Front panel key is : %s\n", (gbl_fp_keylocked ? "Locked" : "Unlocked"));
	return 0;
}


// ================================================================================================
//--------show switches
int cmd_execute_show_switches(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
	return 0;
}


// ================================================================================================
//--------show power
int cmd_execute_show_power(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" CPU Power on state : %s\n", (cpu_get_power_on() ? "On" : "Off"));
	return 0;
}


// ================================================================================================
// --------show run
int cmd_execute_show_run(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" Run / Halt mode :%s\n", (gbl_fp_runlight ? "Run" : "Halt"));
	return 0;
}


// ================================================================================================
// --------show verbose
int cmd_execute_show_verbose(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" Verbose debug mode : %s\n", cpu_get_debug_string());
	return 0;
}


// ================================================================================================
// --------show virtual mode
int cmd_execute_show_virt_mode(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" CPU Real/Virtual mode is: %s\n", (cpu_get_virtual_mode() ? "Virtual" : "Real"));
	return 0;
}


// ================================================================================================
// --------show reg
int cmd_execute_show_reg(int cmd_cnt_found, char** cmd_line_parsed) {
	// -------- a particular block...
	if (cmd_cnt_found >= 3) {
		SIMJ_U32 parm_parse = 0;
		SIMJ_U16 reg_block = 0;
		if (sscanf_s(cmd_line_parsed[2], "%li", &parm_parse) == 1) {
			reg_block = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
		disp_reg_block(stdout, reg_block);
	}
	else {
		disp_cur_reg(stdout);
	}
	return 0;
}


// ================================================================================================
//--------show virtual map
int cmd_execute_show_vmap(int cmd_cnt_found, char** cmd_line_parsed) {
	SIMJ_U32 parm_parse = 0;
	SIMJ_U16 map = 0;
	if (cmd_cnt_found >= 3) {
		if (sscanf_s(cmd_line_parsed[2], "%li", &parm_parse) == 1) {
			map = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	disp_virtual_map(stdout, map);
	return 0;
}



// ================================================================================================
//--------show mem
int cmd_execute_show_mem(int cmd_cnt_found, char** cmd_line_parsed) {
	
	SIMJ_U32 parm_parse = 0;
	SIMJ_U32 j = 0;

	if (cmd_cnt_found >= 3) {
		if (sscanf_s(cmd_line_parsed[2], "%li", &parm_parse) == 1) {
			cmd_exec_last_starting_mem_address = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	if (cmd_cnt_found >= 4) {
		if (sscanf_s(cmd_line_parsed[3], "%li", &parm_parse) == 1) {
			cmd_exec_last_ending_mem_address = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
		}
	}

	printf(" Memory\n");
	for (j = cmd_exec_last_starting_mem_address; j < (cmd_exec_last_ending_mem_address + 1); j += 8) {
		printf("  0x%04x  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n", j,
			gbl_mem[j], gbl_mem[j + 1], gbl_mem[j + 2], gbl_mem[j + 3],
			gbl_mem[j + 4], gbl_mem[j + 5], gbl_mem[j + 6], gbl_mem[j + 7]
		);
	}
	return 0;
}


// ================================================================================================
// --------show mmem virtual memory via map page image
// --------show mmem <abs page numb> <virt addr start> <virt addr end>
int cmd_execute_show_mmem(int cmd_cnt_found, char** cmd_line_parsed) {

	SIMJ_U32 parm_parse = 0;
	SIMJ_U32 uj = 0;

	printf("\n show mmem command not implemented yet.\n");

	// if (cmd_cnt_found >= 3) {
	// 	if (sscanf_s(cmd_line_parsed[2], "%li", &parm_parse) == 1) {
	// 		cmd_exec_last_mem_map_number = parm_parse;
	// 	}
	// 	else {
	// 		printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
	// 	}
	// }
	// if (cmd_cnt_found >= 4) {
	// 	if (sscanf_s(cmd_line_parsed[3], "%li", &parm_parse) == 1) {
	// 		cmd_exec_last_starting_mem_address = parm_parse;
	// 	}
	// 	else {
	// 		printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
	// 	}
	// }
	// if (cmd_cnt_found >= 5) {
	// 	if (sscanf_s(cmd_line_parsed[4], "%li", &parm_parse) == 1) {
	// 		cmd_exec_last_ending_mem_address = parm_parse;
	// 	}
	// 	else {
	// 		printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
	// 	}
	// }

	// printf(" Virtual memory via hardware map %d\n", cmd_exec_last_mem_map_number);
	// for (uj = cmd_exec_last_starting_mem_address; uj < (cmd_exec_last_ending_mem_address + 8); uj += 8) {
	// 	printf("  0x%04x  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n", uj,
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 1),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 2),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 3),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 4),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 5),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 6),
	// 		cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 7)
	// 	);
	// }
	return 0;
}


// ================================================================================================
// --------show vmem virtual memory via hardware map
// --------show vmem <map num> <virt addr start> <virt addr end>
int cmd_execute_show_vmem(int cmd_cnt_found, char** cmd_line_parsed) {

	SIMJ_U32 parm_parse = 0;
	SIMJ_U32 uj = 0;

	if (cmd_cnt_found >= 3) {
		if (sscanf_s(cmd_line_parsed[2], "%li", &parm_parse) == 1) {
			cmd_exec_last_mem_map_number = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	if (cmd_cnt_found >= 4) {
		if (sscanf_s(cmd_line_parsed[3], "%li", &parm_parse) == 1) {
			cmd_exec_last_starting_mem_address = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
		}
	}
	if (cmd_cnt_found >= 5) {
		if (sscanf_s(cmd_line_parsed[4], "%li", &parm_parse) == 1) {
			cmd_exec_last_ending_mem_address = parm_parse;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
		}
	}

	printf(" Virtual memory via hardware map %d\n", cmd_exec_last_mem_map_number);
	for (uj = cmd_exec_last_starting_mem_address; uj < (cmd_exec_last_ending_mem_address + 8); uj += 8) {
		printf("  0x%04x  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n", uj,
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 1),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 2),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 3),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 4),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 5),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 6),
			cpu_get_virtual_mem(cmd_exec_last_mem_map_number, uj + 7)
		);
	}
	return 0;
}

// ================================================================================================
//--------show instruction use (deug)
int cmd_execute_show_inst_use(int cmd_cnt_found, char** cmd_line_parsed) {
	disp_instruction_use(stdout);
	return 0;
}

// ================================================================================================
//--------show instruction execution count
int cmd_execute_show_inst_count(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" Instruction execution count %d\n", cpu_get_instruction_count());
	return 0;
}


// ================================================================================================
// --------show badword1
int cmd_execute_show_badword1(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" It is brown gross and smelly.\n");
	return 0;
}


// ================================================================================================
// --------show badword2
int cmd_execute_show_badword2(int cmd_cnt_found, char** cmd_line_parsed) {
	printf(" No.  Simulators cant show that kind of thing.\n");
	return 0;
}


// ================================================================================================
// --------
//--------set switches
int cmd_execute_set_switches(int cmd_cnt_found, char** cmd_line_parsed) {
	SIMJ_U16 new_switch_value = 0;
	SIMJ_U16 parm_parse = 0;
	if (cmd_cnt_found >= 3) {
		if (sscanf_s(cmd_line_parsed[2], "%hi", &parm_parse) == 1) {
			new_switch_value = parm_parse;
			cpu_set_switches(new_switch_value);
			printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	else {
		printf(" *** ERROR *** Command requires another parameter. \n");
	}
	return 0;
}


// ================================================================================================
// --------set mem clear
int cmd_execute_set_mem_clear(int cmd_cnt_found, char** cmd_line_parsed) {
	// -------- do memory clear....
	int j = 0;
	for (j = 0; j < 2097152; j++) {
		gbl_mem[j] = 0;
	}
	return 0;
}

// ================================================================================================
// --------set mem save
int cmd_execute_set_mem_save(int cmd_cnt_found, char** cmd_line_parsed) {
			// --------cpu must be halted....
	// --------get file name.
	// --------process the memory save command.
	user_cmd_mem_save(cmd_line_parsed[3]);
	return 0;
}

// ================================================================================================
// --------set mem restore
int cmd_execute_set_mem_restore(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------cpu must be halted....
	// --------get file name.
	// --------process the memory restore command.
	user_cmd_mem_restore(cmd_line_parsed[3]);
	return 0;
}

// ================================================================================================
// --------set mem <addr> <addr>
int cmd_execute_set_mem(int cmd_cnt_found, char** cmd_line_parsed) {
	SIMJ_U32 parm_parse = 0;
	SIMJ_U32 set_addr = 0;
	SIMJ_U16 set_value = 0;
	if (sscanf_s(cmd_line_parsed[2], "%i", &parm_parse) == 1) {
		set_addr = parm_parse;
		if (sscanf_s(cmd_line_parsed[3], "%i", &parm_parse) == 1) {
			set_value = parm_parse;
			// TODO: check bounds of address value !!
			gbl_mem[set_addr] = set_value;
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
		}
	}
	else {
		printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
	}
	return 0;
}


// ================================================================================================
// --------set register
int cmd_execute_set_reg(int cmd_cnt_found, char** cmd_line_parsed) {
	if (cmd_cnt_found >= 4) {
		SIMJ_U32 parm_parse = 0;
		SIMJ_U16 set_reg = 0;
		SIMJ_U16 set_value = 0;
		if (sscanf_s(cmd_line_parsed[2], "%i", &parm_parse) == 1) {
			set_reg = parm_parse;
			if (sscanf_s(cmd_line_parsed[3], "%i", &parm_parse) == 1) {
				set_value = parm_parse;
				if (set_reg >= 1 && set_reg <= 15) {
					cpu_set_register_value(set_reg, set_value);
				}
				else {
					printf(" *** ERROR *** Register number must be within 1 to 15 : %d\n", set_reg);
				}
			}
			else {
				printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
			}
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	else {
		printf(" *** ERROR *** Expecting two numeric values following set mem\n");
	}
	return 0;
}


// ================================================================================================
// --------set verbose on
int cmd_execute_verbose_on(int cmd_cnt_found, char** cmd_line_parsed) {
	gbl_verbose_debug = debugging_on;
	return 0;
}
// ================================================================================================
// --------set verbose off
int cmd_execute_verbose_off(int cmd_cnt_found, char** cmd_line_parsed) {
	gbl_verbose_debug = debugging_off;
	return 0;
}

// ================================================================================================
// --------set verbose auto
int cmd_execute_verbose_auto(int cmd_cnt_found, char** cmd_line_parsed) {
	gbl_verbose_debug = debugging_automatic;
	return 0;
}

// ================================================================================================
// --------set pc
int cmd_execute_set_pc(int cmd_cnt_found, char** cmd_line_parsed) {
	if (cmd_cnt_found >= 3) {
		SIMJ_U32 parm_parse = 0;
		SIMJ_U32 set_pc = 0;
		if (sscanf_s(cmd_line_parsed[2], "%i", &parm_parse) == 1) {
			set_pc = parm_parse;
			cpu_set_program_counter(set_pc);
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	return 0;
}

// ================================================================================================
// --------set key lock
int cmd_execute_key_lock(int cmd_cnt_found, char** cmd_line_parsed) {
	//--------set key lock
	gbl_fp_keylocked = true;
	printf(" Front panel key is now locked.\n");
	return 0;
}

// ================================================================================================
// --------set key_unlock
int cmd_execute_key_unlock(int cmd_cnt_found, char** cmd_line_parsed) {

	gbl_fp_keylocked = true;
	printf(" Front panel key is now unlocked.\n");
	return 0;
}

// ================================================================================================
// --------ci
int cmd_execute_ci(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------ci
	cpu_trigger_console_interrupt();
	return 0;
}

// ================================================================================================
// --------cc
int cmd_execute_cc(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------cc
	gbl_capture_console = true;
	return 0;
}

// ================================================================================================
// --------halt
int cmd_execute_halt(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------halt
	// TODO: Make a separate procedure.
	gbl_fp_runlight = false;
	return 0;
}

// ================================================================================================
// --------power on
int cmd_execute_power_on(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------power
	cpu_set_power_on();
	return 0;
}

// ================================================================================================
// --------power off
int cmd_execute_power_off(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------power off
	// TODO: implement power off..
	printf("\n power off command not implemented yet.\n");
	return 0;
}

// ================================================================================================
// --------run
int cmd_execute_run(int cmd_cnt_found, char** cmd_line_parsed) {
	cpu_do_run();
	return 0;
}

// ================================================================================================
// --------step
int cmd_execute_step(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------step
	SIMJ_U16 step_count = 1;
	int k;

	if (cmd_cnt_found >= 2) {
		if (sscanf_s(cmd_line_parsed[1], "%i", &k) == 1) {
			step_count = k;
			cpu_do_step(step_count);
		}
		else {
			printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
		}
	}
	else {
		cpu_do_step(step_count);
	}
	return 0;
}

// ================================================================================================
// --------fill
int cmd_execute_fill(int cmd_cnt_found, char** cmd_line_parsed) {
	// --------fill
	SIMJ_U16 new_switch_value = 10;
	if (!cpu_get_power_on()) {
		printf(" *** ERROR ***  Cant perform fill.  CPU is not powered on.\n");
	}
	else if (gbl_fp_runlight) {
		printf(" *** ERROR ***  Cant perform fill.  CPU is not halted.\n");
	}
	else if (gbl_fp_single_step) {
		printf(" *** ERROR ***  Cant perform fill.  CPU is being single stepped.\n");
	}
	else {
		if (cmd_cnt_found >= 2) {
			SIMJ_U16 parm_parse = 0;
			if (sscanf_s(cmd_line_parsed[1], "%hi", &parm_parse) == 1) {
				new_switch_value = parm_parse;
			}
			else {
				printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
			}
		}
		// -------- perform the fill command
		cpu_do_fill(new_switch_value);
	}
	return 0;
}

// ================================================================================================
// --------master clear
int cmd_execute_master_clear(int cmd_cnt_found, char** cmd_line_parsed ) {

	// -------- perform the master clear.
	cpu_master_clear();
	return 0;
}

// ================================================================================================
// --------help
int cmd_execute_help(int cmd_cnt_found, char** cmd_line_parsed) {

	// --------help
	user_cmd_print_help();
	return 0;
}

// ================================================================================================
// --------bad word 1
int cmd_execute_set_badword1(int cmd_cnt_found, char** cmd_line_parsed) {

	// --------just in case 1
	printf(" Please dont.  The smell would be unbearable.\n");
	return 0;
}

// ================================================================================================
// --------bad word 2
int cmd_execute_set_badword2(int cmd_cnt_found, char** cmd_line_parsed) {

	// --------just in case 2
	printf(" No thanks.  Im not that kind of simulator.\n");
	return 0;
}


// ================================================================================================
// --------magic
int cmd_execute_show_magic(int cmd_cnt_found, char** cmd_line_parsed) {

	// --------just in case 2
	printf(" Sorry, no magic today.\n");
	return 0;
}


// ================================================================================================
// --------exit
int cmd_execute_exit(int cmd_cnt_found, char** cmd_line_parsed) {

	// ------- exit
	// exit_request = true;
	printf(" Exit requested.\n");
	return 1;	// request exit..
}
