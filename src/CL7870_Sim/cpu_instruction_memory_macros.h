// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			cpu_register_memory_macros.h
//
//	Description:	Contains macros used by the CPU module for accessing registers and memory.
//
//	Externally accessible routines:
// 
// Notes:			
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================
#pragma once

// ================================================================================================
// 
// --------macros for register address, and values

// -------- source register 
#define GET_SOURCE_REGISTER_NUMB (instruction.parts.src_reg )
#define GET_SOURCE_REGISTER_NUMB_DOUBLE ( (instruction.parts.src_reg >> 1 ) )
#define GET_SOURCE_REGISTER_NUMB_TRIPLE ( (instruction.parts.src_reg >> 2 ) )
#define GET_SOURCE_REGISTER_NUMB_QUAD ( (instruction.parts.src_reg >> 2 ) )

#define GET_SOURCE_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_SOURCE_REGISTER_NUMB )) 
#define GET_SOURCE_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_SOURCE_REGISTER_NUMB_DOUBLE )) 
#define GET_SOURCE_REGISTER_VALUE_TRIPLE ( GET_REGISTER_VALUE_TRIPLE( GET_SOURCE_REGISTER_NUMB_TRIPLE )) 
#define GET_SOURCE_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_SOURCE_REGISTER_NUMB_QUAD )) 

#define SET_SOURCE_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_SOURCE_REGISTER_NUMB, V1 ); \
					}
#define SET_SOURCE_REGISTER_VALUE_DOUBLE( DV1 ) {\
					SET_REGISTER_VALUE_DOUBLE(  GET_SOURCE_REGISTER_NUMB_DOUBLE, DV1 ); \
					}

// -------- destination register 
#define GET_DESTINATION_REGISTER_NUMB ( instruction.parts.dest_reg  )
#define GET_DESTINATION_REGISTER_NUMB_DOUBLE (( instruction.parts.dest_reg >> 1)  )
#define GET_DESTINATION_REGISTER_NUMB_TRIPLE (( instruction.parts.dest_reg >> 2) )  
#define GET_DESTINATION_REGISTER_NUMB_QUAD (( instruction.parts.dest_reg >> 2) )  

#define GET_DESTINATION_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_DESTINATION_REGISTER_NUMB ))
#define GET_DESTINATION_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_DESTINATION_REGISTER_NUMB_DOUBLE ))
#define GET_DESTINATION_REGISTER_VALUE_TRIPLE ( GET_REGISTER_VALUE_TRIPLE( GET_DESTINATION_REGISTER_NUMB_TRIPLE )) 
#define GET_DESTINATION_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_DESTINATION_REGISTER_NUMB_QUAD ))

#define SET_DESTINATION_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_DESTINATION_REGISTER_NUMB, V1 ); \
					}

#define SET_DESTINATION_REGISTER_VALUE_DOUBLE( DV1 ) {\
					SET_REGISTER_VALUE_DOUBLE(  GET_DESTINATION_REGISTER_NUMB_DOUBLE, DV1); \
					}

#define SET_DESTINATION_REGISTER_VALUE_TRIPLE( TV1 ) {\
					SET_REGISTER_VALUE_TRIPLE(GET_DESTINATION_REGISTER_NUMB_TRIPLE, TV1); \
					}

#define SET_DESTINATION_REGISTER_VALUE_QUAD( QV1 ) {\
					SET_REGISTER_VALUE_QUAD( GET_DESTINATION_REGISTER_NUMB_QUAD, QV1); \
					}


// ================================================================================================
// 
// --------macros for memory address, access and values
//
// -------- absolute direct memory access
#define GET_MEMORY_VALUE_ABS( A ) (gbl_mem[(A)])


#define SET_MEMORY_VALUE_ABS( A, VAL ) (gbl_mem[(A)] = (VAL))

//
// -------- VIRTUAL ACCESS MACROS.
// 00	0	no access
// 01	1	read only
// 10	2	read and execute
// 11   3	read write execute

#define VIRT_MEM_CHECK_ACCESS_READ( VIRT_ADDR, MAP ) {\
					if ( (SIMJ_U16)( cpu_mem_last_access_rights = cpu_virtual_mem_map[MAP].entry[ ( (VIRT_ADDR) >> 8 ) & 0x00ff ].all & MEM_MAP_WORD_ACC_MASK ) == MEM_MAP_WORD_ACC_NONE ) {\
						MEM_ACCESS_TRAP_NO_READ; \
					}\
					}
#define VIRT_MEM_CHECK_ACCESS_EXEC( VIRT_ADDR, MAP ) {\
					if ( (SIMJ_U16)( cpu_mem_last_access_rights = cpu_virtual_mem_map[MAP].entry[ ( (VIRT_ADDR) >> 8 ) & 0x00ff ].all & MEM_MAP_WORD_ACC_MASK  ) < MEM_MAP_WORD_ACC_EXEC) {\
						MEM_ACCESS_TRAP_NO_EXEC; \
					}\
					}
#define VIRT_MEM_CHECK_ACCESS_WRITE( VIRT_ADDR, MAP ) {\
					if ( (SIMJ_U16)( cpu_mem_last_access_rights = cpu_virtual_mem_map[MAP].entry[ ( (VIRT_ADDR) >> 8 ) & 0x00ff ].all & MEM_MAP_WORD_ACC_MASK ) != MEM_MAP_WORD_ACC_WRITE) {\
						MEM_ACCESS_TRAP_NO_WRITE; \
					}\
					}

#define GET_ABS_MEMORY_ADDR_IM( A ) ( cpu_virtual_mode ? (SIMJ_U32)(  (SIMJ_U32)cpu_virtual_mem_map[cpu_instruction_map].entry[(A >> 8) & 0x00ff].parts.mem_page << 8 | (SIMJ_U32)(A & 0x00ff)) : A )
#define GET_ABS_MEMORY_ADDR_OM( A ) ( cpu_virtual_mode ? (SIMJ_U32)(  (SIMJ_U32)cpu_virtual_mem_map[cpu_operand_map].entry[(A >> 8) & 0x00ff].parts.mem_page << 8 | (SIMJ_U32)(A & 0x00ff)) : A )


#define GET_MEMORY_VALUE_IM_VIRT( A ) ( gbl_mem[ cpu_virtual_mem_map[cpu_instruction_map].entry[ ( A >> 8 ) & 0x00ff ].parts.mem_page << 8 | ( A & 0x00ff ) ] )
#define GET_MEMORY_VALUE_OM_VIRT( A ) ( gbl_mem[ cpu_virtual_mem_map[cpu_operand_map].entry[ ( A >> 8 ) & 0x00ff ].parts.mem_page << 8 | ( A & 0x00ff ) ] )

#define GET_MEMORY_VALUE_IM( OUTVAL, A ) {\
					if ( cpu_virtual_mode ) {\
						VIRT_MEM_CHECK_ACCESS_EXEC( A, cpu_instruction_map );\
						OUTVAL = GET_MEMORY_VALUE_IM_VIRT( (SIMJ_U16)(A));\
					}\
					else {\
						OUTVAL = GET_MEMORY_VALUE_ABS( (SIMJ_U16)(A) );\
					}\
					}
#define GET_MEMORY_VALUE_OM( OUTVAL, A ) {\
					if (  cpu_virtual_mode ) {\
						VIRT_MEM_CHECK_ACCESS_READ( A, cpu_operand_map );\
						OUTVAL = GET_MEMORY_VALUE_OM_VIRT( (SIMJ_U16)(A));\
					}\
					else {\
						OUTVAL = GET_MEMORY_VALUE_ABS((SIMJ_U16)(A) );\
					}\
					}


#define SET_MEMORY_VALUE_IM_VIRT( A, VAL ) {\
			gbl_mem[ cpu_virtual_mem_map[cpu_instruction_map].entry[ ( A >> 8 ) & 0x00ff ].parts.mem_page << 8 | ( A & 0x00ff ) ] = (VAL);\
			}

#define SET_MEMORY_VALUE_OM_VIRT( A, VAL ) {\
			 gbl_mem[ cpu_virtual_mem_map[cpu_operand_map].entry[ ( A >> 8 ) & 0x00ff ].parts.mem_page << 8 | ( A & 0x00ff ) ] = (VAL);\
			}

//#define SET_MEMORY_VALUE_IM( A, VAL ) ( cpu_virtual_mode ? SET_MEMORY_VALUE_IM_VIRT( (SIMJ_U16)(A), VAL ) : SET_MEMORY_VALUE_ABS( (SIMJ_U16)(A), VAL ))

//#define SET_MEMORY_VALUE_OM( A, VAL ) ( cpu_virtual_mode ? SET_MEMORY_VALUE_OM_VIRT( (SIMJ_U16)(A), VAL ) : SET_MEMORY_VALUE_ABS( (SIMJ_U16)(A), VAL ))

#define SET_MEMORY_VALUE_IM( A, VAL ) {\
			if ( cpu_virtual_mode ) {\
				VIRT_MEM_CHECK_ACCESS_WRITE( A, cpu_instruction_map );\
				SET_MEMORY_VALUE_IM_VIRT( (SIMJ_U16)(A), VAL );\
			}\
			else {\
				SET_MEMORY_VALUE_ABS( (SIMJ_U16)(A), VAL );\
			}\
			}

#define SET_MEMORY_VALUE_OM( A, VAL ) {\
			if ( cpu_virtual_mode ) {\
				VIRT_MEM_CHECK_ACCESS_WRITE( A, cpu_operand_map );\
				SET_MEMORY_VALUE_OM_VIRT( (SIMJ_U16)(A), VAL );\
			}\
			else {\
				SET_MEMORY_VALUE_ABS( (SIMJ_U16)(A), VAL );\
			}\
			}


// -------- IMMEDIATE MODE (I)
#define GET_MEMORY_VALUE_IMMEDIATE( OUTVAL ) GET_MEMORY_VALUE_IM( OUTVAL, (SIMJ_U16)(program_counter + 1) )

#define SET_MEMORY_VALUE_IMMEDIATE( VAL ) {\
				SET_MEMORY_VALUE_IM( (SIMJ_U16)(program_counter + 1), VAL);\
				}
#define GET_MEMORY_VALUE_IMMEDIATE_2ND( OUTVAL ) GET_MEMORY_VALUE_IM( OUTVAL, (SIMJ_U16)(program_counter + 2))
#define GET_MEMORY_VALUE_IMMEDIATE_3RD( OUTVAL ) GET_MEMORY_VALUE_IM( OUTVAL, (SIMJ_U16)(program_counter + 3))


// -------- SHORT DISPLACED (S)
#define GET_MEMORY_ADDR_SHORT_DISPLACED  ( (SIMJ_U16)(GET_REGISTER_VALUE(1) + instruction.parts.src_reg) )
//#define SHORT_DISPLACED_ADDR_FAULT (  ( ( GET_REGISTER_VALUE(1) & 0x8000 ) != ( GET_MEMORY_ADDR_SHORT_DISPLACED & 0x8000 ) ? true : false ) )
//#define SHORT_DISPLACED_ADDR_FAULT (  false )
#define GET_MEMORY_VALUE_SHORT_DISPLACED( OUT_VAL ) {\
				GET_MEMORY_VALUE_OM( OUT_VAL, GET_MEMORY_ADDR_SHORT_DISPLACED );\
				}

#define SET_MEMORY_VALUE_SHORT_DISPLACED( VAL ) {\
				SET_MEMORY_VALUE_OM( GET_MEMORY_ADDR_SHORT_DISPLACED , VAL);\
				}

// -------- SHORT INDEXED (X)
// TODO: fix when Rs = 0 
#define GET_MEMORY_ADDR_SHORT_INDEXED ( GET_SOURCE_REGISTER_VALUE )
#define GET_MEMORY_VALUE_SHORT_INDEXED( OUT_VAL ) {\
					 GET_MEMORY_VALUE_OM( OUT_VAL, GET_SOURCE_REGISTER_VALUE );\
					}
#define SET_MEMORY_VALUE_SHORT_INDEXED(VAL) {\
					SET_MEMORY_VALUE_OM( GET_SOURCE_REGISTER_VALUE, VAL );\
					}

#define GET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE( OUT_VAL_DBLE ) {\
				GET_MEMORY_VALUE_OM( tmp_val_direct_1, GET_MEMORY_ADDR_SHORT_INDEXED );\
				GET_MEMORY_VALUE_OM( tmp_val_direct_2, GET_MEMORY_ADDR_SHORT_INDEXED+1 );\
				OUT_VAL_DBLE = ( (tmp_val_direct_1 << 16) | tmp_val_direct_2 );\
				}

#define SET_MEMORY_VALUE_SHORT_INDEXED_DOUBLE( VAL ) {\
					SET_MEMORY_VALUE_OM( GET_MEMORY_ADDR_SHORT_INDEXED, (SIMJ_U16)((VAL>>16) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( GET_MEMORY_ADDR_SHORT_INDEXED+1, (SIMJ_U16)(VAL & 0x0000ffff ));\
					}


// -------- DIRECT
// --------note that the first two are for only for use by GET_MEMORY_DIRECT !!!
// TODO: Look at signed vs unsigned...
#define GET_MEMORY_DIRECT_ADDR_PARTIAL( OUT_PART_DIRECT_ADDR ) {\
					if ( (instruction.all & 0x0007) == 0 ) {\
						GET_MEMORY_VALUE_IMMEDIATE( OUT_PART_DIRECT_ADDR );\
					}\
					else {\
						GET_MEMORY_VALUE_IMMEDIATE( OUT_PART_DIRECT_ADDR );\
						OUT_PART_DIRECT_ADDR = (SIMJ_U16)((SIMJ_U16)OUT_PART_DIRECT_ADDR + (SIMJ_U16)(GET_REGISTER_VALUE(instruction.all & 0x0007))); \
					}\
					}

// TODO: Ensure that GET_MEMORY_VALUE_OM works with the putput and input parameters being the same.
#define GET_MEMORY_DIRECT_ADDR( OUT_DIRECT_ADDR ) {\
					GET_MEMORY_DIRECT_ADDR_PARTIAL(OUT_DIRECT_ADDR);\
					if( (instruction.all & 0x0008) != 0 ) {\
						GET_MEMORY_VALUE_OM(OUT_DIRECT_ADDR, OUT_DIRECT_ADDR);\
					}\
					}

#define GET_MEMORY_VALUE_DIRECT( OUT_VAL ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( OUT_VAL, tmp_addr_direct );\
					}

#define SET_MEMORY_VALUE_DIRECT( VAL ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					SET_MEMORY_VALUE_OM( tmp_addr_direct, VAL );\
					}

#define GET_MEMORY_VALUE_DIRECT_DOUBLE( OUT_VAL_DBLE ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( tmp_val_direct_1, tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( tmp_val_direct_2, (SIMJ_U16)(tmp_addr_direct + 1));\
					OUT_VAL_DBLE =  (SIMJ_U32)( ( (SIMJ_U32)tmp_val_direct_1 << 16) | (SIMJ_U32)tmp_val_direct_2 );\
					}

#define SET_MEMORY_VALUE_DIRECT_DOUBLE( VAL ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					SET_MEMORY_VALUE_OM( tmp_addr_direct, (SIMJ_U16)((VAL>>16) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( tmp_addr_direct+1, (SIMJ_U16)(VAL & 0x0000ffff ));\
					}

// TODO: Should this occupy the top or bottom 48 bits?? (for now bottom -- think it should be top)
#define GET_MEMORY_VALUE_DIRECT_TRIPLE( OUT_VAL_TRPL ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( tmp_val_direct_1, tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( tmp_val_direct_2, (SIMJ_U16)(tmp_addr_direct + 1));\
					GET_MEMORY_VALUE_OM( tmp_val_direct_3, (SIMJ_U16)(tmp_addr_direct + 2));\
					OUT_VAL_TRPL =  (SIMJ_U64)( ((SIMJ_U64)tmp_val_direct_1 << 48) | ((SIMJ_U64)tmp_val_direct_2 << 32) | ((SIMJ_U64)tmp_val_direct_3 << 16) );\
					}

//					 ( ((SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR ) << 32) | ((SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR+1 ) <<16) | (SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR+2 ) )

#define SET_MEMORY_VALUE_DIRECT_TRIPLE( VAL ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					SET_MEMORY_VALUE_OM( tmp_addr_direct, (SIMJ_U16)((VAL>>48) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( tmp_addr_direct+1, (SIMJ_U16)((VAL>>32) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( tmp_addr_direct+2, (SIMJ_U16)((VAL>>16) & 0x0000ffff ));\
					}


#define GET_MEMORY_VALUE_DIRECT_QUAD( OUT_VAL_QUAD ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( tmp_val_direct_1, tmp_addr_direct );\
					GET_MEMORY_VALUE_OM( tmp_val_direct_2, (SIMJ_U16)(tmp_addr_direct + 1));\
					GET_MEMORY_VALUE_OM( tmp_val_direct_3, (SIMJ_U16)(tmp_addr_direct + 2));\
					GET_MEMORY_VALUE_OM( tmp_val_direct_4, (SIMJ_U16)(tmp_addr_direct + 3));\
					OUT_VAL_QUAD =  (SIMJ_U64)( ((SIMJ_U64)tmp_val_direct_1 << 48) | ((SIMJ_U64)tmp_val_direct_2 << 32) | ((SIMJ_U64)tmp_val_direct_3 << 16) | (SIMJ_U64)tmp_val_direct_4 );\
					}
//					 ( ((SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR ) << 48) | ((SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR+1 ) << 32) | ((SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR+2 ) <<16) | (SIMJ_U64)GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR+3 ) )

#define SET_MEMORY_VALUE_DIRECT_QUAD( VAL ) {\
					GET_MEMORY_DIRECT_ADDR( tmp_addr_direct );\
					SET_MEMORY_VALUE_OM( tmp_addr_direct, (SIMJ_U16)((VAL>>48) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( tmp_addr_direct+1, (SIMJ_U16)((VAL>>32) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( tmp_addr_direct+2, (SIMJ_U16)((VAL>>16) & 0x0000ffff ));\
					SET_MEMORY_VALUE_OM( tmp_addr_direct+3, (SIMJ_U16)(VAL & 0x0000ffff ));\
					}

// ------- dont need the order is done by the access macros.
// -------WORD ORDER TRANSLATION MACROS
// #define GET_NUMERIC_DOUBLE_FROM_RAW( IN_RAW, OUT_NUM ) {\
//		OUT_NUM.uval = IN_RAW.uval;\
//		}

// #define GET_RAW_DOUBLE_FROM_NUMERIC( IN_NUM, OUT_RAW ) {\
// 		OUT_RAW.uval = IN_NUM.uval;\
//		}


// -------WORD ORDER TRANSLATION MACROS
// #define GET_NUMERIC_QUAD_FROM_RAW( IN_RAW, OUT_NUM ) {\
// 		OUT_NUM.zval[0] = IN_RAW.zval[0];\
// 		OUT_NUM.zval[1] = IN_RAW.zval[1];\
// 		OUT_NUM.zval[2] = IN_RAW.zval[2];\
// 		OUT_NUM.zval[3] = IN_RAW.zval[3];\
// 		}
// #define GET_RAW_QUAD_FROM_NUMERIC( IN_NUM, OUT_RAW ) {\
// 		OUT_RAW.zval[0] = IN_NUM.zval[0];\
// 		OUT_RAW.zval[1] = IN_NUM.zval[1];\
// 		OUT_RAW.zval[2] = IN_NUM.zval[2];\
// 		OUT_RAW.zval[3] = IN_NUM.zval[3];\
// 		}

// ================================================================================================
