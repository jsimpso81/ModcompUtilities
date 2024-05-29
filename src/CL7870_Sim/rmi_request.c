#include <stdio.h>


// -------- this is a template for outputting RMI.
// -------- Change this routine to connection to an actual I/O
// -------- low 4 bits indicate which of the 4 ourputs should be generated 
//			L1 = 0x8
//			L2 = 0x4
//			L3 = 0x2
//			L4 = 0x1
void rmi_request( unsigned __int16 rmi_request) {

	printf("\n ==== RMI OUTPUT ==== Requested.  0x%04x\n",rmi_request);
}