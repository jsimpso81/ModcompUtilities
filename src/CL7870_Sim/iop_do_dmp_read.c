#include "simj_base.h"



// --------status 0=good, others=error
int iop_do_dmp_read( int device_address, int dmp, SIMJ_U16* databuffer, int words_in_buffer ) {

	int j = 0;
	SIMJ_U16 loc_miap = 0;
	SIMJ_U16 loc_miap_len = 0;
	SIMJ_S16 tc = 0;

	// --------first is it real or virtual
	if (cpu_get_virtual_mode()) {

		// -------- get TC / TA pair from DMPI instruction data.
		// -------- DMP addresses are only 1-63
		int loc_dmp = dmp & 0x003f;

		// --------TC is 2s complement
		SIMJ_U16 type = gbl_mem[0x60 + loc_dmp] & 0xc000;
		SIMJ_U16 string_type = gbl_mem[0x60 + loc_dmp] & 0xc000;
		// --------single block -- 14 bit transfer count
		if (type == 0x8000) {
			tc = (SIMJ_S16)(gbl_mem[0x60 + loc_dmp] | 0xc000) * -1;
		}
		// --------chaining -- 13 bit transfer count
		else {
			tc = (SIMJ_S16)(gbl_mem[0x60 + loc_dmp] | 0xe000) * -1;
		}
		SIMJ_U16 ta = gbl_mem[0x70 + loc_dmp];
		SIMJ_S16 use_tc = tc;
		bool single_block = gbl_mem[0x60 + loc_dmp] * 0x8000;

		// --------can only transfer the amount in the buffer....
		if (words_in_buffer < use_tc)
			use_tc = words_in_buffer;

		// --------single block 
		if (type == 0x8000) {
			loc_miap_len = iop_vdmp_miap_length[loc_dmp];
			loc_miap = iop_vdmp_miap_page[loc_dmp];
		}
		// --------string chain via task map image
		else if (string_type == 0x2000) {
			loc_miap_len = iop_vdmp_miap_length[loc_dmp];
			loc_miap = iop_vdmp_miap_page[loc_dmp];
		}
		// --------string chain via map 0
		else {
			loc_miap_len = 256;
			loc_miap = 1;
		}

		// --------for now only support single transfer....
		if (type == 0x8000) {
			// --------store via map...
			for (j = 0; j < tc; j++) {
				// printf(" inx %d, set memory loc 0x%04x = 0x%04x \n", j, (SIMJ_U16)(j + ta), ball);
				iop_store_via_miap(loc_miap, loc_miap_len, ta + j, databuffer[j]);
			}
		}
		else {
			printf("\n *** ERROR ***  Virtual DMP string chaining not yet supported. dmp %d, tc 0x%04x, ta: 0x%04x\n", 
					loc_dmp, gbl_mem[0x60 + loc_dmp], gbl_mem[0x70 + loc_dmp]);
			return 1;

		}

	}
	// --------real mode
	else {

		// -------- get TC / TA pair from absolute memory.
		// -------- DMP addresses are only 1-15
		int loc_dmp = dmp & 0x000f;
		// --------TC is 2s complement
		SIMJ_S16 tc = (SIMJ_S16)( gbl_mem[0x60+loc_dmp] | 0x8000 ) * -1;
		SIMJ_U16 ta = gbl_mem[0x70 + loc_dmp];
		SIMJ_S16 use_tc = tc;
		bool single_block = gbl_mem[0x60 + loc_dmp] * 0x8000;

		// --------single block...
		if (single_block) {
			if (words_in_buffer < use_tc)
				use_tc = words_in_buffer;
			for (j = 0; j < tc; j++) {
				// TODO: rectify this...
				gbl_mem[j + ta] = databuffer[j];
				(SIMJ_S16)gbl_mem[0x60 + loc_dmp]++;
				// printf(" inx %d, set memory loc 0x%04x = 0x%04x \n", j, (SIMJ_U16)(j + ta), ball);
			}
			// printf(" checksum  0x%04hx \n", chk);
			// printf("\n");
		}
		// --------string chaining -- NOT DONE YET
		else {
			printf("\n *** ERROR ***  Real DMP string chaining not yet supported.\n");
			return 1;
		}

	}

	return 0;
}