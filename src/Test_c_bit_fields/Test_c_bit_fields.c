// Test_c_bit_fields.c : This file contains the 'main' function. Program execution begins and ends there.
//

#include <windows.h>
//#include <tchar.h>
#include <stdio.h>
#include <stdbool.h>


// --------processor status word.
typedef struct {
    bool cc_c : 1;
    bool cc_o : 1;
    bool cc_z : 1;
    bool cc_n : 1;
    bool oh : 1;
    unsigned __int8 om : 3;
    unsigned _int8  grb : 4;
    bool prv : 1;
    unsigned __int8 im : 3;
}  PSD_BITS;

typedef union {
    unsigned __int16 all;
    PSD_BITS sep;
} PSD;



// --------------------------------------------------------------------------------
void printPSW( PSD psd ) {

    // --------raw value
    printf("  raw value = 0x%04x\n", psd.all);

    //--------individual values
    printf("  im   = 0x%04x\n", psd.sep.im);
    printf("  prv  = 0x%04x\n", psd.sep.prv);
    printf("  grb  = 0x%04x\n", psd.sep.grb);
    printf("  om   = 0x%04x\n", psd.sep.om);
    printf("  oh   = 0x%04x\n", psd.sep.oh);
    printf("  cc n = 0x%04x\n", psd.sep.cc_n);
    printf("  cc z = 0x%04x\n", psd.sep.cc_z);
    printf("  cc o = 0x%04x\n", psd.sep.cc_o);
    printf("  cc c = 0x%04x\n", psd.sep.cc_c);
}



// --------------------------------------------------------------------------------
int main(int argc, char* argv[])
{

    int junk;


    PSD my_ps = { .all = 0 };

    printf("should be 0\n");
    my_ps.all = 0;
    printPSW(my_ps);


    printf("should be 1\n");
    my_ps.all = 0;
    my_ps.sep.cc_c = 1;
    printPSW(my_ps);

    printf("should be 0x10\n");
    my_ps.all = 0;
    my_ps.sep.oh = 1;
    printPSW(my_ps);

    printf("should be 0x20\n");
    my_ps.all = 0;
    my_ps.sep.om = 1;
    printPSW(my_ps);

    printf("should be 0x100\n");
    my_ps.all = 0;
    my_ps.sep.grb = 1;
    printPSW(my_ps);

    printf("should be 0x1000\n");
    my_ps.all = 0;
    my_ps.sep.prv = 1;
    printPSW(my_ps);

    printf("should be 0x2000\n");
    my_ps.all = 0;
    my_ps.sep.im = 1;
    printPSW(my_ps);



    junk = getchar();

    return (0);
}