// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
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

#include <stdbool.h>
#include <string.h>


void disp_devices(FILE* io_unit) {

	volatile DEVICE_GENERIC_DATA* ptr_generic_data =(volatile DEVICE_GENERIC_DATA*) NULL;
	int j = 0;
	unsigned int rd_buf_count = 0;
	unsigned int wr_buf_count = 0;
	unsigned int tmp_rd = 0;
	unsigned int tmp_wr = 0;

	fprintf(io_unit, "\nDevice Information\n");
	fprintf(io_unit, " DevAddr  Status  Reading  Writing  Bus  Pri  Dmp  SI Ena DI Ena Rd buf  Wr buf  Type\n");

	// --------loop over all devices...
	for (j = 0; j < 64; j++) {

		if (iop_device_buffer[j] != NULL) {
			ptr_generic_data = iop_device_buffer[j];

			// rd buffer
			tmp_wr = ptr_generic_data->in_buff.last_byte_writen_index;
			tmp_rd = ptr_generic_data->in_buff.last_byte_read_index;
			if (tmp_wr < tmp_rd) {
				tmp_wr += DEVICE_BUFFER_MAX_LEN;
			}
			rd_buf_count = tmp_wr - tmp_rd;

			// wr buffer
			tmp_wr = ptr_generic_data->out_buff.last_byte_writen_index;
			tmp_rd = ptr_generic_data->out_buff.last_byte_read_index;
			if (tmp_wr < tmp_rd) {
				tmp_wr += DEVICE_BUFFER_MAX_LEN;
			}
			wr_buf_count = tmp_wr - tmp_rd;


			fprintf(io_unit, "    %2d    0x%04x    %s   %s    %2d   %2d   %2d    %s      %s    %6d  %6d  %2s\n",
				ptr_generic_data->device_address, ptr_generic_data->ctrl_status,
				( ptr_generic_data->device_address, ptr_generic_data->read_in_progress ? "Read " : "---- "),
				(ptr_generic_data->device_address, ptr_generic_data->write_in_progress ? "Write" : "-----"),
				ptr_generic_data->bus, ptr_generic_data->pri, ptr_generic_data->dmp, 
				( ptr_generic_data->SI_enabled ? "X" : "-" ), (ptr_generic_data->DI_enabled ? "X" : "-"),
				rd_buf_count, wr_buf_count,
				ptr_generic_data->info);
		}

	}
}
