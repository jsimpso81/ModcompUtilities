#pragma once

// ===============================================================================================================
// 
// --------macros for register and memory access and address calculations

// -------- source register 
#define GET_SOURCE_REGISTER_NUMB (instruction.parts.src_reg )
#define GET_SOURCE_REGISTER_NUMB_DOUBLE ( (instruction.parts.src_reg >> 1 ) )
#define GET_SOURCE_REGISTER_NUMB_QUAD ( (instruction.parts.src_reg >> 2 ) )

#define GET_SOURCE_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_SOURCE_REGISTER_NUMB )) 
#define GET_SOURCE_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_SOURCE_REGISTER_NUMB_DOUBLE )) 
#define GET_SOURCE_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_SOURCE_REGISTER_NUMB_QUAD )) 

#define SET_SOURCE_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_SOURCE_REGISTER_NUMB, V1 ); \
					}

// -------- destination register 
#define GET_DESTINATION_REGISTER_NUMB ( instruction.parts.dest_reg  )
#define GET_DESTINATION_REGISTER_NUMB_DOUBLE (( instruction.parts.dest_reg >> 1)  )
#define GET_DESTINATION_REGISTER_NUMB_QUAD (( instruction.parts.dest_reg >> 2) )  

#define GET_DESTINATION_REGISTER_VALUE ( GET_REGISTER_VALUE( GET_DESTINATION_REGISTER_NUMB ))
#define GET_DESTINATION_REGISTER_VALUE_DOUBLE ( GET_REGISTER_VALUE_DOUBLE( GET_DESTINATION_REGISTER_NUMB_DOUBLE ))
#define GET_DESTINATION_REGISTER_VALUE_QUAD ( GET_REGISTER_VALUE_QUAD( GET_DESTINATION_REGISTER_NUMB_QUAD ))

#define SET_DESTINATION_REGISTER_VALUE( V1 ) {\
					SET_REGISTER_VALUE( GET_DESTINATION_REGISTER_NUMB, V1 ); \
					}

#define SET_DESTINATION_REGISTER_VALUE_DOUBLE( DV1 ) {\
					SET_REGISTER_VALUE_DOUBLE(  GET_DESTINATION_REGISTER_NUMB_DOUBLE, DV1); \
					}

// TODO: finish set_destination_register_value_triple
#define SET_DESTINATION_REGISTER_VALUE_TRIPLE( V1, V2, V3 ) {}

// TODO: finish set_destination_register_value_quad
#define SET_DESTINATION_REGISTER_VALUE_QUAD( QV1 ) {\
					SET_REGISTER_VALUE_QUAD(GET_DESTINATION_REGISTER_QUAD, QV1); \
					}

#define GET_MEMORY_VALUE_IM( A ) (gbl_mem[(A)])
#define GET_MEMORY_VALUE_OM( A ) (gbl_mem[(A)])
#define GET_MEMORY_VALUE_ABS( A ) (gbl_mem[(A)])
#define SET_MEMORY_VALUE_IM( A, VAL ) {\
				gbl_mem[(A)] = VAL;\
				}
#define SET_MEMORY_VALUE_OM( A, VAL ) {\
				gbl_mem[(A)] = VAL;\
				}
#define SET_MEMORY_VALUE_ABS( A, VAL ) {\
				gbl_mem[(A)] = VAL;\
				}


#define GET_MEMORY_VALUE_IMMEDIATE (GET_MEMORY_VALUE_IM( program_counter + 1))
#define SET_MEMORY_VALUE_IMMEDIATE( VAL ) {\
				SET_MEMORY_VALUE_IM( program_counter + 1, VAL);\
				}
#define GET_MEMORY_VALUE_IMMEDIATE_2ND (GET_MEMORY_VALUE_IM( program_counter + 2))

#define GET_MEMORY_ADDR_SHORT_DISPLACED ( GET_REGISTER_VALUE(1) + ( instruction.parts.src_reg) )
#define GET_MEMORY_VALUE_SHORT_DISPLACED 	(GET_MEMORY_VALUE_OM( GET_MEMORY_ADDR_SHORT_DISPLACED ))
#define SET_MEMORY_VALUE_SHORT_DISPLACED( VAL ) {\
				SET_MEMORY_VALUE_OM( GET_MEMORY_ADDR_SHORT_DISPLACED , VAL);\
				}

// TODO: fix when Rs = 0 
#define GET_MEMORY_ADDR_SHORT_INDEXED ( GET_SOURCE_REGISTER_VALUE )
#define GET_MEMORY_VALUE_SHORT_INDEXED (GET_MEMORY_VALUE_OM( GET_SOURCE_REGISTER_VALUE ))
#define SET_MEMORY_VALUE_SHORT_INDEXED(VAL) {\
					SET_MEMORY_VALUE_OM( GET_SOURCE_REGISTER_VALUE, VAL );\
					}

// --------note that the first two are for only for use by GET_MEMORY_DIRECT !!!
#define GET_MEMORY_DIRECT_ADDR_PARITAL 	((instruction.all & 0x0007) == 0 ? GET_MEMORY_VALUE_IMMEDIATE : ( (__int32)GET_MEMORY_VALUE_IMMEDIATE + (__int32)GET_REGISTER_VALUE(instruction.all & 0x0007)) & 0x0000ffff )
#define GET_MEMORY_DIRECT_ADDR ((instruction.all & 0x0008) == 0 ? GET_MEMORY_DIRECT_ADDR_PARITAL : GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR_PARITAL ))
#define GET_MEMORY_VALUE_DIRECT (GET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR ))
#define SET_MEMORY_VALUE_DIRECT( VAL ) {\
					SET_MEMORY_VALUE_OM( GET_MEMORY_DIRECT_ADDR, VAL );\
					}

// ===============================================================================================================