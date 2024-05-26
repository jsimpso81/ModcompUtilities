#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "modcomp_sim_types.h"
#include "modcomp_opcode_string.h"
#include "modcomp_opcodes.h"


void util_get_opcode_disp(unsigned __int16 instruction, char* op_buffer, size_t buf_size) {

	INSTRUCTION loc_inst = { .all = 0 };

	loc_inst.all = instruction;

	switch (loc_inst.parts.op_code) {

		// --------special cases.
		//	OP_AUG01
			
		//	OP_RMPS_RMWS
		//	OP_AUG0E
		//	OP_SIA_SIE_SIR
		//	OP_RIA_RIE_RIR
		//	OP_RLD_RLQ

		//	OP_RAD_RAQ

		//	OP_LLD_LLQ

		//	OP_LAD_LAQ

		//	OP_FAR_CDIF

		//	OP_FARD_FARQ_CFDI
		//	OP_FSRD_FSRQ_CQFF
		//	OP_FMRD_FMRQ_CDFI
		//	OP_FDRD_FDRQ_CQFI

		//	OP_FAMD_FAMQ_FAID_FAIQ
		//	OP_FSMD_FSMQ_FSID_FSIQ
		//	OP_FMMD_FMMQ_FMID_FMIQ
		//	OP_FDMD_FDMQ_FDID_FDIQ

		//	OP_LDXT_STXT_DMT_STMT

		//	OP_IRRD_TTRD
		//	OP_CRRT_CRRQ_TTRQ
		//	OP_ESD_ESS
		//	OP_TRRQ_LDXD
		//	OP_CRXD_STXD
		//	OP_AUG8F

		//	OP_MPRD_MPMD
		//	OP_DVRD_DVMD

		//	OP_HHI_HNH
		//	OP_AUGA7
		//	OP_HNS_HNR
		//	OP_HZS_HZR
		//	OP_HOS_HOR
		//	OP_HCS_HCR
		//	OP_HLS_HGE
		//	OP_HLE_HGT
		//	OP_LDAM_LDVM
		//	OP_STAM_STVM

		//	OP_ADRD_ADMD
		//	OP_SURD_SUMD
		//	OP_AUGCA

		//	OP_TRRD_LDMD
		//	OP_CLM_STMD_CLMD
		//	OP_CRRD_CRMD

		case  OP_BRU_BLM:			
			// BLM  --  Branch and Link 
			if (loc_inst.parts.dest_reg != 0) {
				strcpy_s(op_buffer, buf_size, "BLM");
			}
			// BRU  --  Branch Unconditionally          
			else {
				strcpy_s(op_buffer, buf_size, "BRU");
			}
			break;

		//	OP_AUGE8
		// 
		//	OP_SUI_CRI
		//	OP_ETI_TETI
		//	OP_ORI_TORI
		//	OP_XOI_TXOI
		//	OP_LDI_LDF_LDFD_FDFQ

		//	OP_HOP_BLT

		case OP_BRX_BLX:
			// BRX  --  Branch (Short-Indexed) Unconditionally         
			if (loc_inst.parts.dest_reg == 0) {
				strcpy_s(op_buffer, buf_size, "BRX");
			}
			// BLX  --  Branch and Link (Short-Indexed)        
			else {
				strcpy_s(op_buffer, buf_size, "BLX");
			}
			break;

		default:
			strcpy_s(op_buffer, buf_size, opcode_string[loc_inst.parts.op_code]);
			break;
	}
	return;

}