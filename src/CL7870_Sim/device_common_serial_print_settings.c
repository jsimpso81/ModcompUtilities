#include <windows.h>
#include <stdio.h>
#include <stdbool.h>

void device_common_serial_print_settings(DCB this_dcb) {

    // -------- Print some of the DCB structure values
    printf("\nBaudRate = %d, ByteSize = %d, Parity = %d, StopBits = %d\n",
        this_dcb.BaudRate,
        this_dcb.ByteSize,
        this_dcb.Parity,
        this_dcb.StopBits);

    // --------this must be true -- windows only supports binery
    printf("  fBinary = %d\n", this_dcb.fBinary);

    // --------Set DTR to always be on.(input hardware handshaking)
    printf("  fRtsControl = %d\n", this_dcb.fRtsControl);
    printf("  fDtrControl = %d\n", this_dcb.fDtrControl);

    // --------it sounds like this is input
    printf("  fDsrSensitivity = %d\n", this_dcb.fDsrSensitivity);

    // --------turn off any input xon/xoff handshaking
    printf("  fInX = %d\n", this_dcb.fInX);

    // --------Turn off any output Xon/Xoff handshaking
    printf("  fOutX = %d\n", this_dcb.fOutX);

    // --------hardware output handshaking
    printf("  fOutxCtsFlow = %d\n", this_dcb.fOutxCtsFlow);
    printf("  fOutxDsrFlow = %d\n", this_dcb.fOutxDsrFlow);

}
