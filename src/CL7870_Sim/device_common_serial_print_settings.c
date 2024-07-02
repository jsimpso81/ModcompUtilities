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

void device_common_serial_print_settings(DCB this_dcb) {

    // -------- Print some of the DCB structure values
    fprintf(stderr, "BaudRate = %d, ByteSize = %d, Parity = %d, StopBits = %d ",
        this_dcb.BaudRate,
        this_dcb.ByteSize,
        this_dcb.Parity,
        this_dcb.StopBits);

    // --------this must be true -- windows only supports binery
    fprintf(stderr, "  fBinary = %d\n", this_dcb.fBinary);

    // --------Set DTR to always be on.(input hardware handshaking)
    fprintf(stderr, "  fRtsControl = %d ", this_dcb.fRtsControl);
    fprintf(stderr, "  fDtrControl = %d ", this_dcb.fDtrControl);

    // --------it sounds like this is input
    fprintf(stderr, "  fDsrSensitivity = %d\n", this_dcb.fDsrSensitivity);

    // --------turn off any input xon/xoff handshaking
    fprintf(stderr, "  fInX = %d ", this_dcb.fInX);

    // --------Turn off any output Xon/Xoff handshaking
    fprintf(stderr, "  fOutX = %d ", this_dcb.fOutX);

    // --------hardware output handshaking
    fprintf(stderr, "  fOutxCtsFlow = %d ", this_dcb.fOutxCtsFlow);
    fprintf(stderr, "  fOutxDsrFlow = %d\n", this_dcb.fOutxDsrFlow);

}
