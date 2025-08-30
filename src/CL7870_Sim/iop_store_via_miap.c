#include "simj_base.h"


int iop_store_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 ta, SIMJ_U16 word_to_store ) {

	SIMJ_U32 miap_start_addr;
	SIMJ_U32 miap_offset;
	SIMJ_U32 miap_addr;
	SIMJ_U32 abs_addr_page;
	SIMJ_U16 page_access;
	SIMJ_U32 abs_offset;
	SIMJ_U32 abs_addr;

	// -------- MIAP absolute address
	miap_start_addr = (SIMJ_U32)((SIMJ_U32)(miap) << 8);
	// -------- MIAP page offset.
	miap_offset = (SIMJ_U32)((ta >> 8) & 0x00ff);
	abs_offset = (SIMJ_U32)(ta & 0x00ff);
	miap_addr = miap_start_addr | miap_offset;
	
	page_access = (gbl_mem[miap_addr] >> 14 ) & 0x0003;
	abs_addr_page = ((SIMJ_U32)(gbl_mem[miap_addr] & 0x1fff) << 8);
	abs_addr = abs_addr_page | abs_offset;
	
	//TODO: check access
	switch (page_access) {
	case 0:	// no access
	case 1:
	case 2:
		printf(" *** ERROR *** iop_store_via_map  no access %d\n", page_access);
		return 1;
		break;

	case 3:	// write access
		gbl_mem[abs_addr] = word_to_store;
	}


	return 0;
}