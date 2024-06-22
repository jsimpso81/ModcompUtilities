
#include "simj_base.h"

// #include <cstdio>
#include <stdio.h>

static SIMJ_F64 ieee64_vals1[] = {
            16.0,
            15.0,
            14.0,
            13.0,
            12.0,
            11.0,
            10.0,
            9.0,
            8.0,
            7.0,
            6.0,
            5.0,
            4.0,
            3.0,
            2.0,
            1.0,
            0.0,
            -1.0,
            -2.0,
            -3.0,
            -4.0,
            -5.0,
            -6.0,
            -7.0,
            -8.0,
            -9.0,
            -10.0,
            -11.0,
            -12.0,
            -13.0,
            -14.0,
            -15.0 };

static SIMJ_M32 mod32_vals1[] = {
            0x41600000,
            0x413C0000,
            0x41380000,
            0x41340000,
            0x41300000,
            0x412C0000,
            0x41280000,
            0x41240000,
            0x41200000,
            0x40F80000,
            0x40F00000,
            0x40E80000,
            0x40E00000,
            0x40B00000,
            0x40A00000,
            0x40600000,
            0x00000000,
            0xBFA00000,
            0xBF600000,
            0xBF500000,
            0xBF200000,
            0xBF180000,
            0xBF100000,
            0xBF080000,
            0xBEE00000,
            0xBEDC0000,
            0xBED80000,
            0xBED40000,
            0xBED00000,
            0xBECC0000,
            0xBEC80000,
            0xBEC40000 };

static SIMJ_F64 ieee64_vals2[] = {
            1.0,
            0.938,              // this one doesn't match...
            0.875,
            0.813,              // this one doesn't match...
            0.75,
            0.688,              // this one doesn't match...
            0.625,
            0.563,              // this one doesn't match...
            0.5,
            0.438,              // this one doesn't match...
            0.375,
            0.313,              // this one doesn't match...
            0.25,
            0.188,              // this one doesn't match...
            0.125,
            0.063,
            0.0,
            -0.063,             // this one doesn't match...
            -0.125,
            -0.188,             // this one doesn't match
            -0.25,
            -0.313,
            -0.375,
            -0.488,
            -0.5,
            -0.563,
            -0.625,
            -0.688,
            -0.75,
            -0.813,
            -0.875,
            -0.938 };

static SIMJ_M32 mod32_vals2[] = {
            0x40600000,
            0x403C0000,
            0x40380000,
            0x40340000,
            0x40300000,
            0x402C0000,
            0x40280000,
            0x40240000,
            0x40200000,
            0x3FF80000,
            0x3FF00000,
            0x3FE80000,
            0x3FE00000,
            0x3FB00000,
            0x3FA00000,
            0x3F600000,
            0x00000000,
            0xC0A00000,
            0xC0600000,
            0xC0500000,
            0xC0200000,
            0xC0180000,
            0xC0100000,
            0xC0080000,
            0xBFB00000,
            0xBFDC0000,
            0xBFD80000,
            0xBFD40000,
            0xBFD00000,
            0xBFCC0000,
            0xBFC80000,
            0xBFC40000 };


int main()
{


    SIMJ_M32 m32_out = 0;
    SIMJ_M64 m64_out = 0;
    SIMJ_U32 status1 = 0;
    SIMJ_U32 status2 = 0;
    SIMJ_F64 f64_out = 0.0;
    int j = 0;

    union {
        SIMJ_F64 native;
        SIMJ_U64 toprint;
    } injunk = { .toprint = 0 };

    printf("\n\n\n hello from ************************************************************* \n");

    int size1 = sizeof(ieee64_vals1) / sizeof(ieee64_vals1[0]);
    int size2 = sizeof(ieee64_vals2) / sizeof(ieee64_vals2[0]);

    for (j = 0; j < size1; j++) {
        injunk.native = ieee64_vals1[j];
        status1 = util_cvt_IEEE64_MCS32(ieee64_vals1[j], &m32_out);
        printf(" inval 0x%016llx, outval 0x%08x, expecting  0x%08x  \n", injunk.toprint, m32_out, mod32_vals1[j]);
        // status2 = util_cvt_MCS32_IEEE64(mod32_vals1[j], &f64_out);
        status2 = util_cvt_MCS32_IEEE64(m32_out, &f64_out);
        printf(" ============  %30.20e, %30.20e \n", ieee64_vals1[j], f64_out );
    }

    for (j = 0; j < size2; j++) {
        injunk.native = ieee64_vals2[j];
        status1 = util_cvt_IEEE64_MCS32(ieee64_vals2[j], &m32_out);
        printf(" inval 0x%016llx, outval 0x%08x, expecting  0x%08x  \n", injunk.toprint, m32_out, mod32_vals2[j]);
        // status2 = util_cvt_MCS32_IEEE64(mod32_vals2[j], &f64_out);
        status2 = util_cvt_MCS32_IEEE64(m32_out, &f64_out);
        printf(" ============  %30.20e, %30.20e \n", ieee64_vals2[j], f64_out );
    }

    printf("\n\n\n\nhello from %s!\n", "Test_Float_Conv");

    for (j = 0; j < size1; j++) {
        injunk.native = ieee64_vals1[j];
        status1 = util_cvt_IEEE64_MCS64(ieee64_vals1[j], &m64_out);
        // printf(" inval 0x%016llx, outval 0x%08x, expecting  0x%08x  \n", injunk.toprint, m32_out, mod32_vals1[j]);
        // status2 = util_cvt_MCS64_IEEE64(mod32_vals1[j], &f64_out);
        status2 = util_cvt_MCS64_IEEE64(m64_out, &f64_out);
        printf(" ============  %30.20e, %30.20e \n", ieee64_vals1[j], f64_out);
    }

    for (j = 0; j < size2; j++) {
        injunk.native = ieee64_vals2[j];
        status1 = util_cvt_IEEE64_MCS64(ieee64_vals2[j], &m64_out);
        // printf(" inval 0x%016llx, outval 0x%08x, expecting  0x%08x  \n", injunk.toprint, m32_out, mod32_vals2[j]);
        // status2 = util_cvt_MCS64_IEEE64(mod32_vals2[j], &f64_out);
        status2 = util_cvt_MCS64_IEEE64(m64_out, &f64_out);
        printf(" ============  %30.20e, %30.20e \n", ieee64_vals2[j], f64_out);
    }





    return 0;
}