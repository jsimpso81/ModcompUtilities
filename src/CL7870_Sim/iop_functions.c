// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			iop_functions.c
//
//	Description:	Routines to xxxxxxx.
// 
//  Routines:
//			void iop_init_data()    -- init iop data structures.
//			int iop_finish_dmp_read(int device_address, SIMJ_U16 dmp, SIMJ_U16* databuffer, int words_in_buffer)
//			int iop_get_dmp_parameters(int device_address, SIMJ_U16 dmp, SIMJ_S16* raw_tc, SIMJ_U16* raw_ta, bool* virt )
//			int iop_load_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U16* word_loaded)
//			int iop_store_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U16 word_to_store)
// 
//  Internal routines:
//			void iop_calc_absaddr_from_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U32* abs_addr, SIMJ_U16* page_access ) {
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		XXXXXX
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "modcomp_opcodes.h"


// ================================================================================================
// --------calculate absolute memory address from miap information
void iop_calc_absaddr_from_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U32* abs_addr, SIMJ_U16* page_access ) {

	SIMJ_U32 miap_start_addr;
	SIMJ_U32 miap_offset;
	SIMJ_U32 miap_addr;
	SIMJ_U32 abs_addr_page;
	SIMJ_U32 abs_offset;

	// -------- MIAP absolute address
	miap_start_addr = (SIMJ_U32)((SIMJ_U32)(miap) << 8);
	// -------- MIAP page offset.
	miap_offset = (SIMJ_U32)((virt_addr >> 8) & 0x00ff);
	abs_offset = (SIMJ_U32)(virt_addr & 0x00ff);

	miap_addr = miap_start_addr | miap_offset;
	abs_addr_page = ((SIMJ_U32)(gbl_mem[miap_addr] & 0x1fff) << 8);

	*abs_addr = abs_addr_page | abs_offset;
	*page_access = (gbl_mem[miap_addr] >> 14) & 0x0003;

	return;
}


// ================================================================================================
// --------initialize iop data structures
void iop_init_data() {

	int j;

	// --------indexed by device address..
	for (j = 0; j < 64; j++) {

		// iop_last_dev_status[j] = 0;

		iop_output_data_proc[j] = NULL;
		iop_output_cmd_proc[j] = NULL;
		iop_input_data_proc[j] = NULL;
		iop_input_status_proc[j] = NULL;
		iop_mount_unit_proc[j] = NULL;
		iop_dismount_unit_proc[j] = NULL;

		iop_device_buffer[j] = NULL;
		iop_thread_stop_request[j] = 0;
		iop_device_thread_handle[j] = 0;
		iop_device_thread_id[j] = 0;

	}


	// -------- indexed by DMP number.
	for (j = 0; j < 64; j++) {
		iop_vdmp_miap_page[j] = 0;
		iop_vdmp_miap_length[j] = 0;
	}
}


// ================================================================================================
// --------store word via map image.
int iop_store_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U16 word_to_store) {

	SIMJ_U16 page_access;
	SIMJ_U32 loc_abs_addr;

	// --------calculate absolute address and get page access..
	iop_calc_absaddr_from_miap(miap, miap_len, virt_addr, &loc_abs_addr, &page_access);


	// --------check access and store value.
	switch (page_access) {
		case 0:	// no access
		case 1:	// read access
		case 2: // read and execute access
			printf(" *** ERROR *** iop_store_via_map  no access %d\n", page_access);
			return 1;
			break;

		case 3:	// read, execute, and write access
			gbl_mem[loc_abs_addr] = word_to_store;
			break;
	}

	return 0;
}



// ================================================================================================
// --------load word via map image.
int iop_load_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U16* word_loaded, SIMJ_U32* abs_addr) {

	SIMJ_U16 page_access;
	SIMJ_U32 loc_abs_addr;

	// --------calculate absolute address and get page access..
	iop_calc_absaddr_from_miap(miap, miap_len, virt_addr, &loc_abs_addr, &page_access);
	*abs_addr = loc_abs_addr;

	// --------check access and store value.
	switch (page_access) {
		case 0:	// no access
			printf(" *** ERROR *** iop_load_via_map  no access %d\n", page_access);
			return 1;
			break;

		case 1:	// read access
		case 2: // read and execute access
		case 3:	// read, execute, and write access
			*word_loaded = gbl_mem[loc_abs_addr];
			break;
	}

	return 0;
}

#define IOP_REAL_MODE_TC_BASE	0x0060
#define IOP_REAL_MODE_TA_BASE	0x0070


// ================================================================================================
// --------get DMP information -- called by process_cmd functions..
int iop_get_dmp_parameters(int device_address, SIMJ_U16 dmp, SIMJ_S16* raw_tc, SIMJ_U16* raw_ta, bool* virt, SIMJ_U32* abs_tc_addr ) {

	SIMJ_S16 loc_tc = 0;
	SIMJ_U16 loc_ta = 0;
	SIMJ_U32 loc_dmp_offset = 0;
	SIMJ_U16 loc_dmp_virt_ptr = 0;
	bool	 loc_virtual = false;
	int load_via_map_stat = 0;
	SIMJ_U32  loc_abs_tc_addr = 0;
	SIMJ_U32  loc_abs_ta_addr = 0;

	// --------is this real or virtual
	loc_virtual = cpu_get_virtual_mode();
	*virt = loc_virtual;

	// --------virtual mode
	if (loc_virtual) {

		// --------get pointer to map image 1 ta/tc pointers..
		loc_dmp_virt_ptr = gbl_mem[IOP_REAL_MODE_TC_BASE];

		load_via_map_stat = iop_load_via_miap(1, 256, loc_dmp_virt_ptr, &loc_tc, &loc_abs_tc_addr);
		if (load_via_map_stat != 0) {
			*raw_tc = 0;
			*raw_ta = 0;
			*abs_tc_addr = 0;
			return 1;
		}

		load_via_map_stat = iop_load_via_miap(1, 256, loc_dmp_virt_ptr+1, &loc_ta, &loc_abs_ta_addr);
		if (load_via_map_stat != 0) {
			*raw_tc = 0;
			*raw_ta = 0;
			*abs_tc_addr = 0;
			return 1;
		}

		// --------all good
		*raw_tc = loc_tc;
		*raw_ta = loc_ta;
		*abs_tc_addr = loc_abs_tc_addr;
	}

	// --------real mode
	else {

		// --------get offset based on dmp address
		// --------only 16 dmp channels are supported.
		loc_dmp_offset = 0x0000000f & dmp;

		// --------real mode dmp
		*raw_tc = (SIMJ_S16)(gbl_mem[IOP_REAL_MODE_TC_BASE + loc_dmp_offset]);
		*raw_ta = gbl_mem[IOP_REAL_MODE_TA_BASE + loc_dmp_offset];
		*abs_tc_addr = IOP_REAL_MODE_TC_BASE + loc_dmp_offset;
	}

	// -------- debug
	printf(" Get DMP parameters. tc 0x%04x, ta 0x%04x, virt mode %d, abs addr: 0x%08x\n", *raw_tc, *raw_ta, *virt, *abs_tc_addr);
	return 0;
}

// ================================================================================================
// --------status 0=good, others=error
// TODO: DMP has to update the tc and ta!!!
int iop_finish_dmp_read( bool virt, SIMJ_S16 tc, SIMJ_U16 ta, SIMJ_U32 tc_abs_addr, 
		SIMJ_U16 vdmp_miap_page, SIMJ_U16 vdmp_miap_length,
		SIMJ_U16* databuffer, int words_in_buffer) {

	int j = 0;
	SIMJ_U16 loc_miap = 0;
	SIMJ_U16 loc_miap_len = 0;
	SIMJ_S16 use_tc = 0;
	SIMJ_S16 count_tc = 0;
	SIMJ_U16 use_ta = 0;
	SIMJ_U16 string_addr = 0;
	int load_via_map_stat = 0;
	SIMJ_U32  loc_abs_tc_addr = 0;
	SIMJ_U32  loc_abs_ta_addr = 0;		// not used....

	printf(" iop_finish_dmp_read virt %d, tc 0x%04x, ta 0x%04x, miap page 0x%04x, miap len 0x%04x, buf size %d \n",
		virt, tc, ta, vdmp_miap_page, vdmp_miap_length, words_in_buffer);

	// --------first is it real or virtual
	if ( virt ) {

		// --------TC is 2s complement
		// SIMJ_U16 type = tc & 0xc000;
		SIMJ_U16 string_type = tc & 0xe000;
		bool single_block = tc & 0x8000;

		// --------single block -- 14 bit transfer count
		if (single_block) {
			use_tc = tc;
			count_tc = (SIMJ_S16)(use_tc | 0xc000) * -1;
			loc_miap_len = vdmp_miap_length;
			loc_miap = vdmp_miap_page;
			loc_abs_tc_addr = tc_abs_addr;
			use_ta = ta;
		}
	
		// --------chaining -- 13 bit transfer count
		else {
			
			// --------string chain via task map image
			if (string_type == 0x2000) {
				loc_miap_len = vdmp_miap_length;
				loc_miap = vdmp_miap_page;
			}
			// --------string chain via map 0
			else {
				loc_miap_len = 256;
				loc_miap = 1;
			}
			string_addr = ta;
			// --------for debug
			use_ta = ta;
			use_tc = tc;
		}

		// --------can only transfer the amount in the buffer....
		if (words_in_buffer < count_tc)
			count_tc = words_in_buffer;


		// --------for now only support single transfer....
		// --------single transfer
		if (single_block) {
			// --------store via map...
			for (j = 0; j < count_tc; j++) {
				//printf(" inx %d, set memory loc 0x%04x = 0x%04x \n", j, (SIMJ_U16)(j + use_ta), databuffer[j]);
				iop_store_via_miap(loc_miap, loc_miap_len, use_ta + j, databuffer[j]);
				(SIMJ_S16)(gbl_mem[tc_abs_addr])++;
			}
		}
		// --------string chaining..
		else {
			printf("\n *** ERROR ***  Virtual DMP string chaining not yet supported. use_tc 0x%04x, use_ta: 0x%04x\n\n",
				use_tc, use_ta);

			bool not_done = true;
			SIMJ_S16 remain_count = words_in_buffer;
			SIMJ_S16 buffer_offset = 0;
	
			while (not_done) {

				load_via_map_stat = iop_load_via_miap(loc_miap, loc_miap_len, string_addr, &use_tc, &loc_abs_tc_addr);
				if (load_via_map_stat != 0) {
					printf(" *** ERROR ***  iop_finish_dmp_read - cant load via map %d, stat %d\n", loc_miap, load_via_map_stat);
					return 1;
				}
				string_addr++;
				load_via_map_stat = iop_load_via_miap(loc_miap, loc_miap_len, string_addr, &use_ta, &loc_abs_ta_addr);
				if (load_via_map_stat != 0) {
					printf(" *** ERROR ***  iop_finish_dmp_read - cant load via map %d, stat %d\n", loc_miap, load_via_map_stat);
					return 1;
				}
				string_addr++;

				count_tc = (SIMJ_S16)(use_tc | 0xe000) * -1;
				SIMJ_U16 string_type = use_tc & 0xe000;

				// --------now we have the real tc and ta, process it again to get info we need to do transfer..
				printf("   string chain use_tc, use_ta count_tc remain 0x%04hx, 0x%04hx, %d %d\n", 
					use_tc, use_ta, count_tc, remain_count);

				if (count_tc > remain_count)
					count_tc = remain_count;

				// --------store via map...
				for (j = 0; j < count_tc; j++) {
					//printf(" inx %d, set memory loc 0x%04x = 0x%04x \n", j, (SIMJ_U16)(j + use_ta), databuffer[j+buffer+offset]);
					iop_store_via_miap(vdmp_miap_page, vdmp_miap_length, use_ta + j, databuffer[j+buffer_offset]);
					(SIMJ_S16)(gbl_mem[loc_abs_tc_addr])++;
				}
				buffer_offset += count_tc;
				remain_count -= count_tc;
				single_block = use_tc & 0x8000;
				if (single_block)
					not_done = false;
			}
			return 1;
		}

	}
	// --------real mode
	else {

		count_tc = (SIMJ_S16)(tc | 0x8000) * -1;
		bool single_block = tc & 0x8000;
		//printf(" iop_finish_dmp_read real mode count_tc 0x%04x, single_block %d, ta 0x%04x\n", count_tc, single_block, ta);

		// --------single block...
		if (single_block) {
			if (words_in_buffer < count_tc)
				count_tc = words_in_buffer;
		
			for (j = 0; j < count_tc; j++) {
				gbl_mem[j + ta] = databuffer[j];
				(SIMJ_S16)(gbl_mem[tc_abs_addr])++;
				//printf(" inx %d, set memory loc 0x%04x = 0x%04x \n", j, (SIMJ_U16)(j + ta), databuffer[j]);
			}
			// printf(" checksum  0x%04hx \n", chk);
			// printf("\n");
		}
		// --------string chaining -- NOT DONE YET
		else {
			printf("\n *** ERROR ***  Real DMP string chaining not yet supported.\n\n");
			return 1;
		}

	}

	printf(" iop_finish_dmp_read - last tc value 0x%04x \n", gbl_mem[tc_abs_addr] );

	return 0;
}



// ================================================================================================
// --------status 0=good, others=error
// TODO: DMP has to update the tc and ta!!!
int iop_get_dmp_word_count(bool virt, SIMJ_S16 tc, SIMJ_U16 ta, SIMJ_U32 tc_abs_addr,
	SIMJ_U16 vdmp_miap_page, SIMJ_U16 vdmp_miap_length,
	SIMJ_U16* dmp_words_requested) {

	SIMJ_U16 loc_miap = 0;
	SIMJ_U16 loc_miap_len = 0;
	//SIMJ_U16 use_ta = 0;
	SIMJ_S16 use_tc = 0;
	SIMJ_U16 string_addr = 0;
	int load_via_map_stat = 0;
	SIMJ_U16  loc_word_count = 0;
	SIMJ_U16 string_type = 0;
	bool single_block = true;
	SIMJ_U32 loc_abs_tc_addr = 0;

	printf(" iop_get_dmp_word_count virt %d, tc 0x%04x, ta 0x%04x, miap page 0x%04x, miap len 0x%04x \n",
		virt, tc, ta, vdmp_miap_page, vdmp_miap_length);

	// --------first is it real or virtual
	if (virt) {

		// -------- get string chaining type and single block...
		string_type = tc & 0xe000;
		single_block = tc & 0x8000;

		// --------single block -- 14 bit transfer count
		if (single_block) {
			loc_word_count = (SIMJ_S16)( tc | 0xc000) * -1;
		}

		// --------chaining -- 13 bit transfer count
		else {

			// --------ta points to actual tc/ta pair...
			// --------find out where then loop of chain..
			// --------string chain via task map image
			if (string_type == 0x2000) {
				loc_miap_len = vdmp_miap_length;
				loc_miap = vdmp_miap_page;
			}
			// --------string chain via map 0
			else {
				loc_miap_len = 256;
				loc_miap = 1;
			}

			string_addr = ta;

			bool not_done = true;

			// TODO: set limits on this loop.   Think max chain list is 128 entries (256 words) ???
			while (not_done) {

				load_via_map_stat = iop_load_via_miap(loc_miap, loc_miap_len, string_addr, &use_tc, &loc_abs_tc_addr);
				if (load_via_map_stat != 0) {
					printf(" *** ERROR ***  iop_finish_dmp_read - cant load via map %d, stat %d\n", loc_miap, load_via_map_stat);
					return 1;
				}
				string_addr+=2;

				loc_word_count += (SIMJ_S16)(use_tc | 0xe000) * -1;

				SIMJ_U16 string_type = use_tc & 0xe000;
				single_block = use_tc & 0x8000;
				if (single_block)
					not_done = false;

				printf("   string chain use_tc, loc_word_count 0x%04hx, %d not done %d\n",
					use_tc, loc_word_count, not_done);
			}
		}
	}

	// --------real mode
	else {

		single_block = tc & 0x8000;

		// --------single block...
		if (single_block) {
			loc_word_count = (SIMJ_S16)(tc | 0x8000) * -1;
		}
		// --------string chaining -- NOT DONE YET
		else {
			loc_word_count = 0;
			printf("\n *** ERROR ***  iop_get_dmp_word_count - Real DMP string chaining not yet supported.\n\n");
			*dmp_words_requested = loc_word_count;
			return 1;
		}

	}

	printf(" iop_get_dmp_word_count - word count %d \n", loc_word_count);
	*dmp_words_requested = loc_word_count;

	return 0;
}
