#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "modcomp_opcode_string.h"
#include "modcomp_opcodes.h"


void util_get_opcode_disp(SIMJ_U16 instruction, char* op_buffer, size_t buf_size) {

	INSTRUCTION loc_inst = { .all = 0 };

	loc_inst.all = instruction;

	switch (loc_inst.parts.op_code) {

		// --------special cases.
		//	OP_AUG01
			
		//	OP_RMPS_RMWS
		//	OP_AUG0E
		case  OP_AUG0E:			// 0x0e
			switch (loc_inst.parts.src_reg) {

			// -------- 0	TRO  -- Transfer and Reset Overflow Status History      
			case 0:
				strcpy_s(op_buffer, buf_size, "TRO");
				break;

			// -------- 1	LCPS  -- Load Register with Current Program Status Register of PSD   
			case 1:
				strcpy_s(op_buffer, buf_size, "LCPS");
				break;

			// -------- 2	LCPR  -- Load Register with Current Program Register of PSD   
			case 2:
				strcpy_s(op_buffer, buf_size, "LCPR");
				break;

			// -------- 3	LCCC  -- Load Register with Current Condition Code of PSD   
			case 3:
				strcpy_s(op_buffer, buf_size, "LCCC");
				break;

			// -------- 4	LCIA  -- Load Register with Current interrupt Active Latches    
			case 4:
				strcpy_s(op_buffer, buf_size, "LCIA");
				break;

			// -------- 5	LCIE  -- Load Register with Current interrupt Enable Latches     
			case 5:
				strcpy_s(op_buffer, buf_size, "LCIE");
				break;

			// -------- 6	LCIR  -- Load Register with Current interrupt Request Latches     
			case 6:
				strcpy_s(op_buffer, buf_size, "LCIR");
				break;

			// -------- 7	MBVV  -- Move Virtual Block to Virtual Block       
			case 7:
				strcpy_s(op_buffer, buf_size, "MBVV");
				break;

			// -------- 8	MBVE  -- Move Block from Virtual to Extended Memory     
			case 8:
				strcpy_s(op_buffer, buf_size, "MBVE");
				break;

			// -------- 9	MBEV  -- Move Block from Extended to Virtual Memory     
			case 9:
				strcpy_s(op_buffer, buf_size, "MBEV");
				break;

			// -------- A	MPES  -- Multiply Immediate with Extended Sign       
			case 10:
				strcpy_s(op_buffer, buf_size, "MPES");
				break;

			// -------- B	DVES  -- Divide lmmediate with Extended Sign       
			case 11:
				strcpy_s(op_buffer, buf_size, "DVES");
				break;

			// -------- C	RDI  -- Read Internal Registers         
			case 12:
				strcpy_s(op_buffer, buf_size, "RDI");
				break;

			// -------- D	WIR  -- Write Internal Register         
			case 13:
				strcpy_s(op_buffer, buf_size, "WIR");
				break;

			// -------- E	BRM  -- Branch to Microroutine Immediate        
			case 14:
				strcpy_s(op_buffer, buf_size, "BRM");
				break;

			// -------- F	BRMI  -- Branch to Microroutine Immediate        
			case 15:
				strcpy_s(op_buffer, buf_size, "BRMI");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "----");
				break;
		}
		break;

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