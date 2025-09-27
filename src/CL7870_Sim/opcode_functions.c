// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			opcode_functions.c
//
//	Description:	Routines to xxxxxxx.
// 
//  Routines:
// 
//  Internal routines:
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
#include "modcomp_opcodes.h"


SIMJ_U16 opcode_get_opcode_index(SIMJ_U16 instruction) {

	SIMJ_U16 loc_switch = instruction >> 8;
	SIMJ_U16 loc_index = instruction >> 8;
	SIMJ_U16 loc_sub_index = 0;
	SIMJ_U16 loc_ret_opcode = loc_index;

	// --------process all the special cases.  There are a lot!
	switch (loc_switch) {

		// --------augment 01 --- used dest register.
	case OP_AUG01:
		loc_sub_index = (instruction >> 4) & 0x000f;
		loc_ret_opcode = OPSI_AUG01 + loc_sub_index;
		break;

	case OP_LXR_SPR_SGP:
		loc_sub_index = (instruction >> 4) & 0x0009;

		switch ( loc_sub_index) {

		case 0:		// LXR  -- Load Extended Memory Control Register
			loc_ret_opcode = OPSI_LXR;
			break;
		case 1:		// SPR - Set Protect (IF)  (disable non-virtual protection emulation)  7830 only
			loc_ret_opcode = OPSI_SPR;
			break;
		case 8:		// SGP -- Set Global Protect (Set global protect boundary)  7830 only
			loc_ret_opcode = OPSI_SGP;
			break;
		default:
			loc_ret_opcode = OPSI_NOINST;
			break;
		}
		break;

		// -------- 
	case OP_RMPS_RMWS_SLP:
		if (((instruction >> 4) & 0x000f) == 0) {		// SLP -- Set Lower Protect Value
			loc_ret_opcode = OPSI_SLP;
		}
		else if (instruction & 0x0001) {		//  RMWS -- Read Memory Word Status
			loc_ret_opcode = OPSI_RMWS;
		}
		else {										// RMPS -- Read Memory Plane Status
			loc_ret_opcode = OPSI_RMPS;
		}
		break;

	case OP_WMS_SUP:
		if ((( instruction >> 4) & 0x000f) == 0) {		// SUP -- Set Upper Protect Value
			loc_ret_opcode = OPSI_SUP;
		}
		else {											// WMS -- Write Memory Status
			loc_ret_opcode = OPSI_WMS;
		}
		break;

		// --------uses source register
	case OP_AUG0E:
		loc_sub_index = instruction & 0x000f;
		loc_ret_opcode = OPSI_AUG0E + loc_sub_index;
		break;

	case OP_SIA_SIE_SIR:
		loc_sub_index = (instruction >> 4) & 0x000f;
		switch (loc_sub_index) {
			case 0:	// SIA  --  Set interrupt Active         
				loc_ret_opcode = OPSI_SIA;
				break;
			case 4:	// SIE  --  Set interrupt Enable         
				loc_ret_opcode = OPSI_SIE;
				break;
			case 8:	// SIR  --  Set interrupt Request         
				loc_ret_opcode = OPSI_SIR;
				break;
			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case OP_RIA_RIE_RIR:
		loc_sub_index = (instruction >> 4) & 0x000f;
		switch (loc_sub_index) {
			case 0:	// RIA  --  Reset interrupt Active         
				loc_ret_opcode = OPSI_RIA;
				break;
			case 4:	// RIE  --  Reset interrupt Enable         
				loc_ret_opcode = OPSI_RIE;
				break;
			case 8:	// RIR  --  Reset interrupt Request         
				loc_ret_opcode = OPSI_RIR;
				break;
			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case OP_RLD_RLQ:
		switch ((instruction >> 4) & 0x0001) {
			case 0:		// RLD  --  Shift Right Logical Double-Register
				loc_ret_opcode = OPSI_RLD;
				break;
			case 1:		// RLQ  --  Shift Right Logical Quadruple-Register        
				loc_ret_opcode = OPSI_RLQ;
				break;
		}
		break;

	case OP_RAD_RAQ:
		switch ((instruction >> 4) & 0x0001) {
		case 0:		// RAD -- right arithmetic shift double
			loc_ret_opcode = OPSI_RAD;
			break;
		case 1:		// RAQ  --  Shift Right Arithmetic Quadruple-Register             
			loc_ret_opcode = OPSI_RAQ;
			break;
		}
		break;

	case OP_LLD_LLQ:
		switch ((instruction >> 4) & 0x0001) {
		case 0:		// LLD  --  Shift Left Logical Double-Register        
			loc_ret_opcode = OPSI_LLD;
			break;
		case 1:		// LLQ  --  Shift Left Logical Quadruple-Register            
			loc_ret_opcode = OPSI_LLQ;
			break;
		}
		break;

	case OP_LAD_LAQ:
		switch ((instruction >> 4) & 0x0001) {
		case 0:		// LAD - left arithmetic shift double
			loc_ret_opcode = OPSI_LAD;
			break;
		case 1:		// LAQ  --  Shift Left Arithmetic Quadruple-Register        
			loc_ret_opcode = OPSI_LAQ;
			break;
		}
		break;

	case OP_FAR_CDIF:
		switch ( instruction & 0x0001) {
			case 0:		// FAR  --  Floating Point Add Double-Register to Double Register     
				loc_ret_opcode = OPSI_FAR;
				break;
			case 1:		// CDIF  --  Convert Double-Register Integer to Floating Point   
				loc_ret_opcode = OPSI_CDIF;
				break;
		}
		break;

	case OP_FARD_FARQ_CFDI:
		switch (instruction & 0x01) {
			case 0:				// fard, farq
				switch (((instruction >> 4 ) & 0x000f ) & 0x1) {
					case 0:			// FARD  --  Floating Point Add Triple Register to Triple Register    
						loc_ret_opcode = OPSI_FARD;
						break;
					case 1:			// FARQ  --  Floating Point Add Quad Register to Quad Register    
						loc_ret_opcode = OPSI_FARQ;
						break;
				}
				break;
			case 1:				// CFDI  --  Convert Floating Point to Double-Register Integer      
				loc_ret_opcode = OPSI_CFDI;
				break;
		}
		break;

	case OP_FSRD_FSRQ_CQFF:
		switch (instruction & 0x01) {
		case 0:				
			switch (((instruction >> 4) & 0x000f) & 0x1) {
				case 0:			// FSRD
					loc_ret_opcode = OPSI_FSRD;
					break;
				case 1:			// FSRQ
					loc_ret_opcode = OPSI_FSRQ;
					break;
			}
			break;
		case 1:				//  CQFF  --  Convert Quad-Register Floating Point to Floating Point     		     
			loc_ret_opcode = OPSI_CQFF;
			break;
		}
		break;

	case OP_FMRD_FMRQ_CDFI:
		switch (instruction & 0x01) {
		case 0:
			switch (((instruction >> 4) & 0x000f) & 0x1) {
			case 0:			// FMRD
				loc_ret_opcode = OPSI_FMRD;
				break;
			case 1:			// FMRQ
				loc_ret_opcode = OPSI_FMRQ;
				break;
			}
			break;
		case 1:				// CDFI  --  Convert Double Precision Floating Point Operand to Double Integer   
			loc_ret_opcode = OPSI_CDFI;
			break;
		}
		break;

	case OP_FDRD_FDRQ_CQFI:
		switch (instruction & 0x01) {
		case 0:
			switch (((instruction >> 4) & 0x000f) & 0x1) {
				case 0:			// FDRD
					loc_ret_opcode = OPSI_FDRD;
					break;
				case 1:			// FDRQ
					loc_ret_opcode = OPSI_FDRQ;
					break;
			}
			break;
		case 1:				// CQFI  --  Convert Quad Precision Floating Point Operand to Double Integer   
			loc_ret_opcode = OPSI_CQFI;
			break;
		}
		break;

	case OP_FAM_FAI:
		switch ( (instruction >> 4) & 0x01) {
			case 0:			// FAM
				loc_ret_opcode = OPSI_FAM;
				break;
			case 1:			// FAI
				loc_ret_opcode = OPSI_FAI;
				break;
		}
		break;

	case OP_FSM_FSI:
		switch ((instruction >> 4) & 0x01) {
			case 0:			// FSM
				loc_ret_opcode = OPSI_FSM;
				break;
			case 1:			// FSI
				loc_ret_opcode = OPSI_FSI;
				break;
		}
		break;

	case OP_FMM_FMI:
		switch ((instruction >> 4) & 0x01) {
			case 0:			// FMM
				loc_ret_opcode = OPSI_FMM;
				break;
			case 1:			// FMI
				loc_ret_opcode = OPSI_FMI;
				break;
		}
		break;

	case OP_FDM_FDI:
		switch ((instruction >> 4) & 0x01) {
			case 0:			// FDM
				loc_ret_opcode = OPSI_FDM;
				break;
			case 1:			// FDI
				loc_ret_opcode = OPSI_FDI;
				break;
		}
		break;

	case OP_FAMD_FAMQ_FAID_FAIQ:
		switch ((instruction >> 4) & 0x03) {
			case 0:			// FAMD
				loc_ret_opcode = OPSI_FAMD;
				break;
			case 1:			// FAMQ
				loc_ret_opcode = OPSI_FAMQ;
				break;
			case 2:			// FAID
				loc_ret_opcode = OPSI_FAID;
				break;
			case 3:			// FAIQ
				loc_ret_opcode = OPSI_FAIQ;
				break;
		}
		break;

	case OP_FSMD_FSMQ_FSID_FSIQ:
		switch ((instruction >> 4) & 0x03) {
			case 0:			// FSMD
				loc_ret_opcode = OPSI_FSMD;
				break;
			case 1:			// FSMQ
				loc_ret_opcode = OPSI_FSMQ;
				break;
			case 2:			// FSID
				loc_ret_opcode = OPSI_FSID;
				break;
			case 3:			// FSIQ
				loc_ret_opcode = OPSI_FSIQ;
				break;
		}
		break;

	case OP_FMMD_FMMQ_FMID_FMIQ:
		switch ((instruction >> 4) & 0x03) {
			case 0:			// FMMD
				loc_ret_opcode = OPSI_FMMD;
				break;
			case 1:			// FMMQ
				loc_ret_opcode = OPSI_FMMQ;
				break;
			case 2:			// FMID
				loc_ret_opcode = OPSI_FMID;
				break;
			case 3:			// FMIQ
				loc_ret_opcode = OPSI_FMIQ;
				break;
		}
		break;

	case OP_FDMD_FDMQ_FDID_FDIQ:
		switch ((instruction >> 4) & 0x03) {
			case 0:			// FDMD
				loc_ret_opcode = OPSI_FDMD;
				break;
			case 1:			// FDMQ
				loc_ret_opcode = OPSI_FDMQ;
				break;
			case 2:			// FDID
				loc_ret_opcode = OPSI_FDID;
				break;
			case 3:			// FDIQ
				loc_ret_opcode = OPSI_FDIQ;
				break;
		}
		break;

	case OP_LDXT_STXT_LDMT_STMT:
		switch ((instruction >> 4) & 0x03) {
			case 0:			//  STXT  --  Store Triple-Register into Memory Tripleword (Short-Indexed)      
				loc_ret_opcode = OPSI_STXT;
				break;
			case 1:			//  LDXT  --  Load Triple-Register from Memory Triple- word (Short-Indexed)
				loc_ret_opcode = OPSI_LDXT;
				break;
			case 2:			//  STMT  --  Store Triple-Register into Memory Triple-Word       
				loc_ret_opcode = OPSI_STMT;
				break;
			case 3:			//  LDMT  --  Load Triple-Register from Memory Tripleword       
				loc_ret_opcode = OPSI_LDMT;
				break;
		}
		break;

	case OP_IRRD_TTRD:
		// -------- IRRD  --  Interchange Double Register and Double Register      
		if ((instruction & 0x0010) != 0) {
			loc_ret_opcode = OPSI_IRRD;
		}
		// -------- TTRD  --  Transfer Two's Complement of Double- Register to Double-Register    
		else {
			loc_ret_opcode = OPSI_TTRD;
		}
		break;

	case OP_CRRT_CRRQ_TTRQ:
		switch ( ( instruction >> 4 ) & 0x0003) {
			case 0:			//  --  TTRQ  --  Transfer Two's Complement of Quadruple- Register to Quadruple-Register    
				loc_ret_opcode = OPSI_TTRQ;
				break;

			case 2:			// --  CRRT  --  Compare Triple Register to Triple Register      
				loc_ret_opcode = OPSI_CRRT;
				break;

			case 3:			// --  CRRQ  --  Compare Quad Register to Quad Register      
				loc_ret_opcode = OPSI_CRRQ;
				break;

			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case OP_ESD_ESS:
		switch ( ( instruction >> 4 ) & 0x01) {

		case 0:				//  --  ESD  --  Extend Sign Double         
			loc_ret_opcode = OPSI_ESD;
			break;

		case 1:				//  --  ESS  --  Extend Sign Single         
			loc_ret_opcode = OPSI_ESS;
			break;
		}
		break;

	case OP_TRRQ_LDXD:
		switch ( ( instruction >> 4 ) & 0x0001) {

			case 0:			// --  TRRQ  --  Transfer Quadruple-Register to Quadruple Register       
				loc_ret_opcode = OPSI_TRRQ;
				break;

			case 1:			//  --  LDXD  --  Load Double-Register from Memoty Doubleword (Short-Indexed)      
				loc_ret_opcode = OPSI_LDXD;
				break;
		}
		break;

	case OP_CRXD_STXD:
		switch ( ( instruction >> 4 ) & 0x0001) {

			case 0:				//  --  CRXD  --  Compare Double Register to Short-Indexed Memory Doubleword     
				loc_ret_opcode = OPSI_CRXD;
				break;

			case 1:				//  --  STXD  --  Store Double-Register into Memory Doubleword (Short-Indexed)      
				loc_ret_opcode = OPSI_STXD;
				break;
		}
		break;

		// --------uses destination reg
	case OP_AUG8F:
		loc_sub_index = ( instruction >> 4 ) & 0x000f;
		loc_ret_opcode = OPSI_AUG8F + loc_sub_index;
		break;

	case OP_MPRD_MPMD:
		switch ( ( instruction >> 4 ) & 0x1) {
			case 0:				//  --  MPRD  --  Multiply Double-Register by Double- Register
				loc_ret_opcode = OPSI_MPRD;
				break;

			case 1:				//  --  MPMD  --  Multiply Double-Register by Memory Double word      
				loc_ret_opcode = OPSI_MPMD;
				break;
		}
		break;

	case OP_DVRD_DVMD:
		switch ((instruction >> 4) & 0x1) {
			case 0:				//  --  DVRD  --  Divide Quad-Register by Double-Register        
				loc_ret_opcode = OPSI_DVRD;
				break;

			case 1:				//  --  DVMD  --  Divide Quad-Register by Memory Doubleword       
				loc_ret_opcode = OPSI_DVMD;
				break;
		}
		break;


	case OP_HHI_HNH:
		// HNH  --  Hop on Magnitude Not Higher Condition      
		if ( instruction & 0x0080) {
			loc_ret_opcode = OPSI_HNH;
		}
		// HHI  --  Hop on Magnitude Higher Condition        
		else {
			loc_ret_opcode = OPSI_HHI;
		}
		break;

		// --------uses destination register.
	case OP_AUGA7:
		loc_sub_index = (instruction >> 4) & 0x000f;
		loc_ret_opcode = OPSI_AUGA7 + loc_sub_index;
		break;

	case OP_HNS_HNR:
		// HNR  --  Hop on Condition Code N Reset      
		if (instruction & 0x0080) {
			loc_ret_opcode = OPSI_HNR;
		}
		// HNS  --  Hop on Condition Code N Set      
		else {
			loc_ret_opcode = OPSI_HNS;
		}
		break;

	case OP_HZS_HZR:
		// HZR  --  Hop on Condition Code Z Reset      
		if (instruction & 0x0080) {
			loc_ret_opcode = OPSI_HZR;
		}
		// HZS  --  Hop on Condition Code Z Set      
		else {
			loc_ret_opcode = OPSI_HZS;
		}
		break;

	case OP_HOS_HOR:
		// HOR  --  Hop on Condition Code O Reset      
		if (instruction & 0x0080) {
			loc_ret_opcode = OPSI_HOR;
		}
		// HOS  --  Hop on Condition Code O Set      
		else {
			loc_ret_opcode = OPSI_HOS;
		}
		break;

	case OP_HCS_HCR:
		// HCR  --  Hop on Condition Code C Reset      
		if (instruction & 0x0080) {
			loc_ret_opcode = OPSI_HCR;
		}
		// HCS  --  Hop on Condition Code C Set      
		else {
			loc_ret_opcode = OPSI_HCS;
		}
		break;

	case OP_HLS_HGE:
		//       HGE	Hop oh Greater than or Equal Condition     
		if (instruction & 0x0080) {
			loc_ret_opcode = OPSI_HGE;
		}
		//  HLS	Hop on Less Than Condition            
		else {
			loc_ret_opcode = OPSI_HLS;
		}
		break;

	case OP_HLE_HGT:
		//       HGT	Hop on Greater Than Condition       
		if (instruction & 0x0080) {
			loc_ret_opcode = OPSI_HLE;
		}
		//       HLE	Hop on Less Than or Equal Condition     
		else {
			loc_ret_opcode = OPSI_HGT;
		}
		break;

	case OP_LDAM_LDVM:				//be
		switch ( instruction & 0x0001 ) {
			case 0:			// LDAM
				loc_ret_opcode = OPSI_LDAM;
				break;
			case 1:			// LDVM
				loc_ret_opcode = OPSI_LDVM;
				break;
		}
		break;

	case OP_STAM_STVM:				//bf
		switch (instruction & 0x0001) {
		case 0:			// STAM
			loc_ret_opcode = OPSI_STAM;
			break;
		case 1:			// STVM
			loc_ret_opcode = OPSI_STVM;
			break;
		}
		break;

	case  OP_ADRD_ADMD:				//c8
		//  --  ADRD(DAR)  --  Add Double-Register to Double-Register        
		if (( ( instruction >> 4 ) & 1) == 0) {
			loc_ret_opcode = OPSI_ADRD;
		}
		//  --  ADMD  --  Add Memory Doubleword to Double- Register      
		else {
			loc_ret_opcode = OPSI_ADMD;
		}
		break;

	case  OP_SURD_SUMD:				//c9
		//  --  SURD  --  Subtract Double-Register from Double-Register        
		if (( ( instruction >> 4 ) & 1) == 0) {
			loc_ret_opcode = OPSI_SURD;
		}
		//  --  SUMD  --  Subtract Memory Doubleword from Double-Register       
		else {
			loc_ret_opcode = OPSI_SUMD;
		}
		break;

		// -------- uses destination register.
	case  OP_AUGCA:					//ca
		loc_sub_index = (instruction >> 4) & 0x000f;
		loc_ret_opcode = OPSI_AUGCA + loc_sub_index;
		break;

	case  OP_TRRD_LDMD:				//cd
		switch ( ( instruction >> 4 ) & 0x0001) {
			case 0:				//  --  TRRD -- Transfer Double-Register to Double- Register
				loc_ret_opcode = OPSI_TRRD;
				break;

			case 1:				//  --  LDMD  -- Load Double-Register from Memory Doubleword 
				loc_ret_opcode = OPSI_LDMD;
				break;
		}
		break;

	case  OP_CLM_STMD_CLMD:			//ce
		if ( ( ( instruction >> 4 ) & 0x000f ) == 0) {
			//--------CLM  --  Clear Memory          
			loc_ret_opcode = OPSI_CLM;
		}
		else if ((((instruction >> 4) & 0x000f) & 0x1) == 0) {
			//--------CLMD
			loc_ret_opcode = OPSI_CLMD;
		}
		else {
			//--------STMD          
			loc_ret_opcode = OPSI_STMD;
		}
		break;

	case  OP_CRRD_CRMD:				//cf
		switch ( ( instruction >> 4 ) & 0x0001) {

			case 0:				//  --  CRRD  --  Compare Double-Register with Double Register       
				loc_ret_opcode = OPSI_CRRD;
				break;

			case 1:				//  --  CRMD  --  Compare Double-Register with Memory Doubleword       
				loc_ret_opcode = OPSI_CRMD;
				break;
		}
		break;

	case  OP_BRU_BLM:				//e7
		// BLM  --  Branch and Link 
		if ( ((instruction>>4) & 0x000f) != 0) {
			loc_ret_opcode = OPSI_BLM;
		}
		// BRU  --  Branch Unconditionally          
		else {
			loc_ret_opcode = OPSI_BRU;
		}
		break;

		// --------uses source register.
	case  OP_AUGE8:					//e8
		loc_sub_index = instruction & 0x000f;
		loc_ret_opcode = OPSI_AUGE8 + loc_sub_index;
		break;

	case  OP_SUI_CRI:				//e9
		switch (instruction & 0x000f) {

			case 0:		// SUI  --  SUI  --  Subtract Memory (Immediate) from Register       
				loc_ret_opcode = OPSI_SUI;
				break;

			case 1:		// CRI  --  CRI  --  Compare Register with Memory (Immediate)       
				loc_ret_opcode = OPSI_CRI;
				break;

			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case  OP_ETI_TETI:				//ea
		switch (instruction & 0x000f ) {

			case 0:		// ETI
				loc_ret_opcode = OPSI_ETI;
				break;

			case 1:		// TETI
				loc_ret_opcode = OPSI_TETI;
				break;

			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case  OP_ORI_TORI:				//eb
		switch ( instruction & 0x000f ) {

			case 0:		// ORI
				loc_ret_opcode = OPSI_ORI;
				break;

			case 1:		// TORI
				loc_ret_opcode = OPSI_TORI;
				break;

			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case  OP_XOI_TXOI:				//ec
		switch ( instruction & 0x000f ) {

			case 0:		// XOI
				loc_ret_opcode = OPSI_XOI;
				break;

			case 1:		// TXOI
				loc_ret_opcode = OPSI_TXOI;
				break;

			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case  OP_LDI_LDF_LDFD_FDFQ:     //ed
		switch ( instruction & 0x000f ) {
			case 0:	// -------- LDI   load immediate
				loc_ret_opcode = OPSI_LDI;
				break;
			case 1: // -------- LDF  load floating immediate
				loc_ret_opcode = OPSI_LDF;
				break;
			case 2:	// -------- LDFD load floating double immediate 
				loc_ret_opcode = OPSI_LDFD;
				break;
			case 3: // -------- LDFQ load floating quad immediate
				loc_ret_opcode = OPSI_LDFQ;
				break;
			default:
				loc_ret_opcode = OPSI_NOINST;
				break;
		}
		break;

	case OP_HOP_BLT:				//f7
		// BLT  --  Branch and Link (Indexed Through-Table)       
		if ( instruction & 0x0080) {
			loc_ret_opcode = OPSI_BLT;
		}
		// HOP  --  Hop Unconditionally          
		else {
			loc_ret_opcode = OPSI_HOP;
		}
		break;

	case OP_BRX_BLX:				//ff
		// BRX  --  Branch (Short-Indexed) Unconditionally         
		if ( ( ( instruction >> 4 ) & 0x000f ) == 0 ) {
			loc_ret_opcode = OPSI_BRX;
		}
		// BLX  --  Branch and Link (Short-Indexed)
		else {
			loc_ret_opcode = OPSI_BLX;
		}
		break;

	}
	return loc_ret_opcode;
}