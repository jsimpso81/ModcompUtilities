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

#include "modcomp_opcode_string.h"
#include "modcomp_opcodes.h"


void util_get_opcode_disp(SIMJ_U16 instruction, char* op_buffer, size_t buf_size) {

	INSTRUCTION loc_inst = { .all = 0 };

	loc_inst.all = instruction;

	switch (loc_inst.parts.op_code) {

		// --------special cases.

		// -------- OP_AUG01
		case  OP_AUG01:			// 0x01	
			switch (loc_inst.parts.dest_reg) {

				// --  0	RMI -- Request Multi·processor interrupt
				case 0:
					strcpy_s(op_buffer, buf_size, "RMI");
					break;

				// --  1	EVMO -- Enter Virtual Mode of CPU Execution
				case 1:
					strcpy_s(op_buffer, buf_size, "EVMO");
					break;

				// --  2	SIOM -- Select Another Program's IM as Current OM
				case 2:
					strcpy_s(op_buffer, buf_size, "SIOM");
					break;

				// --  3	SOOM -- Select Another Program's OM as Current OM
				case 3:
					strcpy_s(op_buffer, buf_size, "SOOM");
					break;

				// --  4	SZOM -- Select Map Zero as Current OM
				case 4:
					strcpy_s(op_buffer, buf_size, "SZOM");
					break;

				// --  5	SCRB -- Select Current Register Block in PSD
				case 5:
					strcpy_s(op_buffer, buf_size, "SCRB");
					break;

				// --  6	EXMA -- Enter Extended Memory Addressing Mode
				// --  7	EXMA -- Enter Extended Memory Addressing Mode
				case 6:
				case 7:
					strcpy_s(op_buffer, buf_size, "EXMA");
					break;

				// --  8	XVMO -- Exit Virtual Mode of CPU Execution
				case 8:
					strcpy_s(op_buffer, buf_size, "XVMO");
					break;

				// --  9	ZIMP -- Zero Section of Instruction Map
				case 9:
					strcpy_s(op_buffer, buf_size, "ZIMP");
					break;

				// --  A	UIT
				case 10:
					strcpy_s(op_buffer, buf_size, "----");
					break;

				// --  B	ZOMP -- Zero Section of Operand Map
				case 11:
					strcpy_s(op_buffer, buf_size, "ZOMP");
					break;

				// --  C	UIT
				case 12:
					strcpy_s(op_buffer, buf_size, "----");
					break;

				// --  D	LIMP -- Load Instruction Map Image into Hardware Map
				case 13:
					strcpy_s(op_buffer, buf_size, "LIMP");
					break;

				// --  E	LOMP -- Load Operand Map Image Into Hardware Map
				case 14:
					strcpy_s(op_buffer, buf_size, "LOMP");
					break;

				// --  F	SOMP -- Store Operand Map into Map Image
				case 15:
					strcpy_s(op_buffer, buf_size, "SOMP");
					break;

				default:
					strcpy_s(op_buffer, buf_size, "----");
					break;
			}
			break;


		case  OP_LXR:			// 0x02
#if SIMJ_SIM_CPU == 7830
			switch (loc_inst.parts.dest_reg & 0x9) {

			case 0:		// LXR  -- Load Extended Memory Control Register
#endif
				strcpy_s(op_buffer, buf_size, "LXR");
#if SIMJ_SIM_CPU == 7830
				break;

			case 1:		// SPR - Set Protect (IF)  (disable non-virtual protection emulation)  7830 only
				strcpy_s(op_buffer, buf_size, "SPR");
				break;

			case 8:		// SGP -- Set Global Protect (Set global protect boundary)  7830 only
				strcpy_s(op_buffer, buf_size, "SGP");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "----");
				break;
			}
#endif
			break;


			// -------- OP_RMPS_RMWS
		case  OP_RMPS_RMWS:	    // 0x03 -         
#if SIMJ_SIM_CPU == 7830
			if (loc_inst.parts.dest_reg == 0) {			// SLP -- Set Lower Protect Value
				strcpy_s(op_buffer, buf_size, "SLP");
			}
			else if (loc_inst.parts.src_reg & 0x0001) {	//  RMWS -- Read Memory Word Status
#else
			if (loc_inst.parts.src_reg & 0x0001) {		//  RMWS -- Read Memory Word Status
#endif
				strcpy_s(op_buffer, buf_size, "RMWS");
			}
			else {											// RMPS -- Read Memory Plane Status
				strcpy_s(op_buffer, buf_size, "RMPS");
			}
			break;

		case  OP_WMS:			// 0x04 --  Write Memory Status
#if SIMJ_SIM_CPU == 7830
			if (loc_inst.parts.dest_reg == 0) {			// SUP -- Set Upper Protect Value
				strcpy_s(op_buffer, buf_size, "SUP");
			}
			else {											// WMS -- Write Memory Status
#endif
				strcpy_s(op_buffer, buf_size, "WMS");
#if SIMJ_SIM_CPU == 7830
			}
#endif
			break;

		// -------- OP_AUG0E
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

		// -------- OP_SIA_SIE_SIR
		case  OP_SIA_SIE_SIR:		// 0x26
			switch (loc_inst.parts.dest_reg) {

				// SIA  --  Set interrupt Active         
			case 0:
				strcpy_s(op_buffer, buf_size, "SIA");
				break;

				// SIE  --  Set interrupt Enable         
			case 4:
				strcpy_s(op_buffer, buf_size, "SIE");
				break;

				// SIR  --  Set interrupt Request         
			case 8:
				strcpy_s(op_buffer, buf_size, "SIR");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "---");
				break;
			}
			break;


		// -------- OP_RIA_RIE_RIR
		case  OP_RIA_RIE_RIR:		//  0x27
			switch (loc_inst.parts.dest_reg) {

				// RIA  --  Reset interrupt Active         
			case 0:
				strcpy_s(op_buffer, buf_size, "RIA");
				break;

				// RIE  --  Reset interrupt Enable         
			case 4:
				strcpy_s(op_buffer, buf_size, "RIE");
				break;

				// RIR  --  Reset interrupt Request         
			case 8:
				strcpy_s(op_buffer, buf_size, "RIR");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "---");
				break;
			}
			break;



		// -------- OP_RLD_RLQ
		case  OP_RLD_RLQ:			// 0x28
			switch (loc_inst.parts.dest_reg & 0x0001) {

			case 0:		// RLD  --  Shift Right Logical Double-Register
				strcpy_s(op_buffer, buf_size, "RLD");
				break;

			case 1:		// RLQ  --  Shift Right Logical Quadruple-Register        
				strcpy_s(op_buffer, buf_size, "RLQ");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "---");
				break;
			}
			break;

		// -------- OP_RAD_RAQ
		case  OP_RAD_RAQ:		// 0x2a 
			switch (loc_inst.parts.dest_reg & 0x0001) {

			case 0:		// RAD -- right arithmetic shift double
				strcpy_s(op_buffer, buf_size, "RAD");
				break;

			case 1:		// RAQ  --  Shift Right Arithmetic Quadruple-Register             
				strcpy_s(op_buffer, buf_size, "RAQ");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "---");
				break;
			}
			break;

		// -------- OP_LLD_LLQ
		case  OP_LLD_LLQ:		//            0x2c
			switch (loc_inst.parts.dest_reg & 0x0001) {

			case 0:		// LLD  --  Shift Left Logical Double-Register        
				strcpy_s(op_buffer, buf_size, "LLD");
				break;

			case 1:		// LLQ  --  Shift Left Logical Quadruple-Register        
				strcpy_s(op_buffer, buf_size, "LLQ");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "---");
				break;
			}
			break;

		// -------- OP_LAD_LAQ
		case  OP_LAD_LAQ:		//            0x2e
			switch (loc_inst.parts.dest_reg & 0x0001) {

			case 0:		// LAD - left arithmetic shift double
				strcpy_s(op_buffer, buf_size, "LAD");
				break;

			case 1:		// LAQ  --  Shift Left Arithmetic Quadruple-Register        
				strcpy_s(op_buffer, buf_size, "LAQ");
				break;

			default:
				strcpy_s(op_buffer, buf_size, "---");
				break;
			}
			break;

		// -------- OP_FAR_CDIF
		case  OP_FAR_CDIF:		// 0x30
			switch (loc_inst.parts.src_reg & 0x1) {
				case 0:		// FAR  --  Floating Point Add Double-Register to Double Register     
					strcpy_s(op_buffer, buf_size, "FAR");
					break;
				case 1:		// CDIF  --  Convert Double-Register Integer to Floating Point   
					strcpy_s(op_buffer, buf_size, "CDIF");
					break;
			}
			break;

		// -------- OP_FARD_FARQ_CFDI
		case  OP_FARD_FARQ_CFDI:	//          0x34
			switch (loc_inst.parts.src_reg & 0x01) {
			case 0:				// fard, farq
				switch (loc_inst.parts.dest_reg & 0x1) {
				case 0:			// FARD  --  Floating Point Add Triple Register to Triple Register    
					strcpy_s(op_buffer, buf_size, "FARD");
					break;
				case 1:			// FARQ  --  Floating Point Add Quad Register to Quad Register    
					strcpy_s(op_buffer, buf_size, "FARQ");
					break;
				}
				break;
			case 1:				// CFDI  --  Convert Floating Point to Double-Register Integer      
				strcpy_s(op_buffer, buf_size, "CFDI");
				break;
			}
			break;


		// -------- OP_FSRD_FSRQ_CQFF
		case  OP_FSRD_FSRQ_CQFF:	//           0x35
			switch (loc_inst.parts.src_reg & 0x01) {
			case 0:
				switch (loc_inst.parts.dest_reg & 0x1) {
				case 0:			// FSRD
					strcpy_s(op_buffer, buf_size, "FSRD");
					break;
				case 1:			// FSRQ
					strcpy_s(op_buffer, buf_size, "FSRQ");
					break;
				}
				break;
			case 1:				//  CQFF  --  Convert Quad-Register Floating Point to Floating Point     
				strcpy_s(op_buffer, buf_size, "CQFF");
				break;
			}
			break;


		// -------- OP_FMRD_FMRQ_CDFI
		case  OP_FMRD_FMRQ_CDFI:	//           0x36
			switch (loc_inst.parts.src_reg & 0x01) {
			case 0:				// 
				switch (loc_inst.parts.dest_reg & 0x1) {
				case 0:			// FMRD
					strcpy_s(op_buffer, buf_size, "FMRD");
					break;
				case 1:			// FMRQ
					strcpy_s(op_buffer, buf_size, "FMRQ");
					break;
				}
				break;
			case 1:				// CDFI  --  Convert Double Precision Floating Point Operand to Double Integer   
				strcpy_s(op_buffer, buf_size, "CDFI");
				break;
			}
			break;


		// -------- OP_FDRD_FDRQ_CQFI
		case  OP_FDRD_FDRQ_CQFI:	//           0x37
			switch (loc_inst.parts.src_reg & 0x01) {
			case 0:				// 
				switch (loc_inst.parts.dest_reg & 0x1) {
				case 0:			// FDRD
					strcpy_s(op_buffer, buf_size, "FDRD");
					break;
				case 1:			// FDRQ
					strcpy_s(op_buffer, buf_size, "FDRQ");
					break;
				}
				break;
			case 1:				// CQFI  --  Convert Quad Precision Floating Point Operand to Double Integer   
				strcpy_s(op_buffer, buf_size, "CQFI");
				break;
			}
			break;


		// -------- OP_FAM_FAI
		case  OP_FAM_FAI:	//		            0x38
			switch (loc_inst.parts.dest_reg & 0x1) {
			case 0:		// fam
				strcpy_s(op_buffer, buf_size, "FAM");
				break;
			case 1:		// fai
				strcpy_s(op_buffer, buf_size, "FAI");
				break;
			}
			break;


		// -------- OP_FSM_FSI
		case  OP_FSM_FSI:	//		            0x39
			switch (loc_inst.parts.dest_reg & 0x1) {
			case 0:		// FSM
				strcpy_s(op_buffer, buf_size, "FSM");
				break;
			case 1:		// FSI
				strcpy_s(op_buffer, buf_size, "FSI");
				break;
			}
			break;


		// -------- OP_FMM_FMI
		case  OP_FMM_FMI:	//		            0x3a
			switch (loc_inst.parts.dest_reg & 0x1) {
			case 0:		// fmm
				strcpy_s(op_buffer, buf_size, "FMM");
				break;
			case 1:		// fmi
				strcpy_s(op_buffer, buf_size, "FMI");
				break;
			}
			break;

		// -------- OP_FDM_FDI
		case  OP_FDM_FDI:	//		0x3b
			switch (loc_inst.parts.dest_reg & 0x1) {
			case 0:		// fmm
				strcpy_s(op_buffer, buf_size, "FDM");
				break;
			case 1:		// fdi
				strcpy_s(op_buffer, buf_size, "FDI");
				break;
			}
			break;


		// -------- OP_FAMD_FAMQ_FAID_FAIQ
		case  OP_FAMD_FAMQ_FAID_FAIQ:	//      0x3c
			switch (loc_inst.parts.dest_reg & 0x3) {
			case 0:		// famd
				strcpy_s(op_buffer, buf_size, "FAMD");
				break;
			case 1:		// famq
				strcpy_s(op_buffer, buf_size, "FAMQ");
				break;
			case 2:		// faid
				strcpy_s(op_buffer, buf_size, "FAID");
				break;
			case 3:		// faiq
				strcpy_s(op_buffer, buf_size, "FAIQ");
				break;
			}
			break;


		// -------- OP_FSMD_FSMQ_FSID_FSIQ
		case  OP_FSMD_FSMQ_FSID_FSIQ:	//		0x3d
			switch (loc_inst.parts.dest_reg & 0x3) {
			case 0:		// fsmd
				strcpy_s(op_buffer, buf_size, "FSMD");
				break;
			case 1:		// fsmq
				strcpy_s(op_buffer, buf_size, "FSMQ");
				break;
			case 2:		// fsid
				strcpy_s(op_buffer, buf_size, "FSID");
				break;
			case 3:		// fsiq
				strcpy_s(op_buffer, buf_size, "FSIQ");
				break;
			}
			break;


		// -------- OP_FMMD_FMMQ_FMID_FMIQ
		case  OP_FMMD_FMMQ_FMID_FMIQ:	//      0x3e
			switch (loc_inst.parts.dest_reg & 0x3) {
			case 0:		// fmmd
				strcpy_s(op_buffer, buf_size, "FMMD");
				break;
			case 1:		// fmmq
				strcpy_s(op_buffer, buf_size, "FMMQ");
				break;
			case 2:		// fmid
				strcpy_s(op_buffer, buf_size, "FMID");
				break;
			case 3:		// fsiq
				strcpy_s(op_buffer, buf_size, "FMIQ");
				break;
			}
			break;

		// -------- OP_FDMD_FDMQ_FDID_FDIQ
		case  OP_FDMD_FDMQ_FDID_FDIQ:	//      0x3f
			switch (loc_inst.parts.dest_reg & 0x3) {
			case 0:		// fdmd
				strcpy_s(op_buffer, buf_size, "FDMD");
				break;
			case 1:		// fdmq
				strcpy_s(op_buffer, buf_size, "FDMQ");
				break;
			case 2:		// fdid
				strcpy_s(op_buffer, buf_size, "FDID");
				break;
			case 3:		// fdiq
				strcpy_s(op_buffer, buf_size, "FDIQ");
				break;
			}
			break;


		// -------- OP_LDXT_STXT_DMT_STMT

		// -------- OP_IRRD_TTRD

		// -------- OP_CRRT_CRRQ_TTRQ

		// -------- OP_ESD_ESS

		// -------- OP_TRRQ_LDXD

		// -------- OP_CRXD_STXD
 
		// -------- OP_AUG8F
		case  OP_AUG8F:			//         0x8f
			switch (loc_inst.parts.dest_reg) {
				case 0:		// --  BXNS	Branch (Short-Indexed) on Condition Code N Set     
					strcpy_s(op_buffer, buf_size, "BXNS");
					break;
				case 1:		// --  BXZS	Branch (Short-Indexed) on Condition Code Z Set     
					strcpy_s(op_buffer, buf_size, "BXZS");
					break;
				case 2:		// --  BXOS	Branch (Short-Indexed) On Condition Code O Set     
					strcpy_s(op_buffer, buf_size, "BXOS");
					break;
				case 3:		// --  BXCS	Branch (Short-Indexed) on Condition Code C Set     
					strcpy_s(op_buffer, buf_size, "BXCS");
					break;
				case 4:		// --  BXLS	Branch (Short-Indexed) on Less Than Condition      
					strcpy_s(op_buffer, buf_size, "BXLS");
					break;
				case 5:		// --  BXLE	Branch (Short-Indexed) on Less Than or Equal Condition    
					strcpy_s(op_buffer, buf_size, "BXLE");
					break;
				case 6:		// --  BXHI	Branch (Short-Indexed) on Magnitude Higher Condition      
					strcpy_s(op_buffer, buf_size, "BXHI");
					break;
				case 8:		// --  BXNR	Branch (Short-Indexed) on Condition Code N Reset     
					strcpy_s(op_buffer, buf_size, "BXNR");
					break;
				case 9:		// --  BXZR	Branch (Short-Indexed) on Condition Code Z Reset     
					strcpy_s(op_buffer, buf_size, "BXZR");
					break;
				case 10:		// --  BXOR	Branch (Short-Indexed) On Condition Code O Reset     
					strcpy_s(op_buffer, buf_size, "BXOR");
					break;
				case 11:		// --  BXCR	Branch (Short-Indexed) on Condition Code C Reset     
					strcpy_s(op_buffer, buf_size, "BXCR");
					break;
				case 12:		// --  BXGE	Branch (Short-Indexed) on Greater Than or Equal Condition    
					strcpy_s(op_buffer, buf_size, "BXGE");
					break;
				case 13:		// --  BXGT	Branch (Short-Indexed) on Greater Than Condition      
					strcpy_s(op_buffer, buf_size, "BXGT");
					break;
				case 14:		// --  BXNH	Branch (Short-Indexed) on Magnitude Not Higher Condition     
					strcpy_s(op_buffer, buf_size, "BXNH");
					break;
				default:
					strcpy_s(op_buffer, buf_size, "----");
					break;
				}
				break;

		// -------- OP_MPRD_MPMD
		case  OP_MPRD_MPMD:		//        0xa2 --
			switch (loc_inst.parts.dest_reg & 0x1) {
			case 0:				//  --  MPRD  --  Multiply Double-Register by Double- Register
				strcpy_s(op_buffer, buf_size, "MPMR");
				break;

			case 1:				//  --  MPMD  --  Multiply Double-Register by Memory Double word      
				strcpy_s(op_buffer, buf_size, "MPMD");
				break;
			}
			break;


		// -------- OP_DVRD_DVMD

		// -------- OP_HHI_HNH

		// -------- OP_AUGA7
		case  OP_AUGA7:			//         0xa7
			switch (loc_inst.parts.dest_reg) {
			case 0:		// --  BLNS	Branch and Link on Condition Code N Set
				strcpy_s(op_buffer, buf_size, "BLNS");
				break;
			case 1:		// --  BLZS	Branch and Link on Condition Code Z Set    
				strcpy_s(op_buffer, buf_size, "BLZS");
				break;
			case 2:		// --  BLOS	Branch and Link on Condition Code O Set    
				strcpy_s(op_buffer, buf_size, "BLOS");
				break;
			case 3:		// --  BLCS	Branch and Link on Condition Code C Set    
				strcpy_s(op_buffer, buf_size, "BLCS");
				break;
			case 4:		// --  BLLS	Branch and Link on Less Than Condition     
				strcpy_s(op_buffer, buf_size, "BLLS");
				break;
			case 5:		// --  BLLE	Branch and Link on Less Than or Equal Condition   
				strcpy_s(op_buffer, buf_size, "BLLE");
				break;
			case 6:		// --  BLHI	Branch and Link on Magnitude Higher Condition     
				strcpy_s(op_buffer, buf_size, "BLHI");
				break;
			case 8:		// --  BLNR	Branch and Link on Condition Code N Reset    
				strcpy_s(op_buffer, buf_size, "BLNR");
				break;
			case 9:		// --  BLZR	Branch and Link on Condition Code Z Reset    
				strcpy_s(op_buffer, buf_size, "BLZR");
				break;
			case 10:		// --  BLOR	Branch and Link on Condition Code O Reset    
				strcpy_s(op_buffer, buf_size, "BLOR");
				break;
			case 11:		// --  BLCR	Branch and Link on Condition Code C Reset    
				strcpy_s(op_buffer, buf_size, "BLCR");
				break;
			case 12:		// --  BLGE	Branch and Link on Greater Than or Equal Condition   
				strcpy_s(op_buffer, buf_size, "BLGE");
				break;
			case 13:		// --  BLGT	Branch and Link on Greater Than Condition     
				strcpy_s(op_buffer, buf_size, "BLGT");
				break;
			case 14:		// --  BLNH	Branch and Link on Magnitude Not Higher Condition    
				strcpy_s(op_buffer, buf_size, "BLNH");
				break;
			default:
				strcpy_s(op_buffer, buf_size, "----");
				break;
			}
			break;


		// -------- OP_HNS_HNR
		case  OP_HNS_HNR:			//         0xa8
			// HNR  --  Hop on Condition Code N Reset      
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "HNR");
			}
			// HNS  --  Hop on Condition Code N Set      
			else {
				strcpy_s(op_buffer, buf_size, "HNS");
			}
			break;

		// -------- OP_HZS_HZR
		case  OP_HZS_HZR:			//         0xa9
			// HZR  --  Hop on Condition Code Z Reset      
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "HZR");
			}
			// HZS  --  Hop on Condition Code Z Set      
			else {
				strcpy_s(op_buffer, buf_size, "HZS");
			}
			break;

		// -------- OP_HOS_HOR
		case  OP_HOS_HOR:			//         0xaa
			// HOR  --  Hop on Condition Code O Reset      
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "HOR");
			}
			// HOS  --  Hop on Condition Code O Set      
			else {
				strcpy_s(op_buffer, buf_size, "HOS");
			}
			break;

		// -------- OP_HCS_HCR
		case  OP_HCS_HCR:			//         0xab
			// HCR  --  Hop on Condition Code C Reset      
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "HCR");
			}
			// HCS  --  Hop on Condition Code C Set      
			else {
				strcpy_s(op_buffer, buf_size, "HCS");
			}
			break;

		// -------- OP_HLS_HGE
		case  OP_HLS_HGE:			//         0xac
			//       HGE	Hop oh Greater than or Equal Condition     
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "HGE");
			}
			//  HLS	Hop on Less Than Condition            
			else {
				strcpy_s(op_buffer, buf_size, "HLS");
			}
			break;

		// -------- OP_HLE_HGT
		case  OP_HLE_HGT:			//         0xad
			//       HGT	Hop on Greater Than Condition       
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "HGT");
			}
			//       HLE	Hop on Less Than or Equal Condition     
			else {
				strcpy_s(op_buffer, buf_size, "HLE");
			}
			break;


		// -------- OP_LDAM_LDVM
		case  OP_LDAM_LDVM:		//        0xbe
			switch (loc_inst.parts.src_reg & 0x0001) {
				case 0:			// LDAM
					strcpy_s(op_buffer, buf_size, "LDAM");
					break;
				case 1:			// LDVM
					strcpy_s(op_buffer, buf_size, "LDVM");
			}
			break;

		// -------- OP_STAM_STVM
		case  OP_STAM_STVM:		//        0xbf
			switch (loc_inst.parts.src_reg & 0x0001) {
			case 0:			// STAM
				strcpy_s(op_buffer, buf_size, "STAM");
				break;
			case 1:		// STVM
				strcpy_s(op_buffer, buf_size, "STVM");
				break;
			}
			break;

		// -------- OP_ADRD_ADMD

		// -------- OP_SURD_SUMD

		// -------- OP_AUGCA

		// -------- OP_TRRD_LDMD

		// -------- OP_CLM_STMD_CLMD

		// -------- OP_CRRD_CRMD

		// -------- OP_BRU_BLM
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

		// -------- OP_AUGE8
		case  OP_AUGE8:			//         0xe8
			switch (loc_inst.parts.src_reg) {

				// --  0	ADI  --  Add Memory(Immediate) to Register	
				case 0:
					strcpy_s(op_buffer, buf_size, "ADI");
					break;

				// --  1	LDES  --  Load Immediate and Extend Sign	
				case 1:
					strcpy_s(op_buffer, buf_size, "LDES");
					break;

				// --  2	ADES  --  Add Immediate with Extended Sign	
				case 2:
					strcpy_s(op_buffer, buf_size, "ADES");
					break;

				// --  3	SUES  --  Subtract Immediate with Extended Sign	
				case 3:
					strcpy_s(op_buffer, buf_size, "SUES");
					break;

				// --  4	CIES  --  Compare Immediate with Extended Sign	
				case 4:
					strcpy_s(op_buffer, buf_size, "CIES");
					break;

				// --  5	EXI  --  Execute Immediate	
				case 5:
					strcpy_s(op_buffer, buf_size, "EXI");
					break;

				// --  6	MPI  --  Multiply Immediate	
				case 6:
					strcpy_s(op_buffer, buf_size, "MPI");
					break;

				// --  7	DVI  --  Divide Immediate	
				case 7:
					strcpy_s(op_buffer, buf_size, "DVI");
					break;

				// --  8	EPMD  --  Enter Pipeline Mode of Execution	
				case 8:
					strcpy_s(op_buffer, buf_size, "EPMD");
					break;

				// --  9	CRZ  --  Compare Register to Zero	
				case 9:
					strcpy_s(op_buffer, buf_size, "CRZ");
					break;

				// --  A	CRZD  --  Compare Double Register to Zero	
				case 10:
					strcpy_s(op_buffer, buf_size, "CRZD");
					break;

				// --  B	XPMD  --  Exit Pipeline Mode of Execution	
				case 11:
					strcpy_s(op_buffer, buf_size, "XPMD");
					break;

				// --  C	LTIL  --  Loop Termination with Indirectly Addressed Control Variable and Literal Terminal Value	
				case 12:
					strcpy_s(op_buffer, buf_size, "LTIL");
					break;

				// --  D	LTDL  --  Loop Terminationwit h Directly Addressed Control - Variable - and Literal Terminal Value	
				case 13:
					strcpy_s(op_buffer, buf_size, "LTDL");
					break;

				// --  E	LTID  --  Lo0p Termination with Indirectly Addressed Control Variable and Directly Addressed Terminal Value	
				case 14:
					strcpy_s(op_buffer, buf_size, "LTID");
					break;

				// --  F	LTDD  --  Loop Termination with Directly Addressed - Control Variable and Directly Addressed Terminal Value
				case 15:
					strcpy_s(op_buffer, buf_size, "LTDD");
					break;

				default:
					strcpy_s(op_buffer, buf_size, "----");
					break;
			}
			break;

		// -------- OP_SUI_CRI

		// -------- OP_ETI_TETI

		// -------- OP_ORI_TORI

		// -------- OP_XOI_TXOI

		// -------- OP_LDI_LDF_LDFD_FDFQ
		case  OP_LDI_LDF_LDFD_FDFQ:    //    0xed
			switch (loc_inst.parts.src_reg) {
				case 0:	// -------- LDI   load immediate
					strcpy_s(op_buffer, buf_size, "LDI");
					break;

				case 1: // -------- LDF  load floating immediate
					strcpy_s(op_buffer, buf_size, "LDF");
					break;

				case 2:	// -------- LDFD load floating double immediate 
					strcpy_s(op_buffer, buf_size, "LDFD");
					break;

				case 3: // -------- LDFQ load floating quad immediate
					strcpy_s(op_buffer, buf_size, "LDFQ");
					break;

				default:
					strcpy_s(op_buffer, buf_size, "----");
					break;
			}
			break;

		// -------- OP_HOP_BLT
		case  OP_HOP_BLT:		//         0xf7
			// BLT  --  Branch and Link (Indexed Through-Table)       
			if (loc_inst.all & 0x0080) {
				strcpy_s(op_buffer, buf_size, "BLT");
			}
			// HOP  --  Hop Unconditionally          
			else {
				strcpy_s(op_buffer, buf_size, "HOP");
			}
			break;

		// -------- OP_BRX_BLX
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