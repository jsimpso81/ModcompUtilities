// Math_Test.c : This file contains the 'main' function. Program execution begins and ends there.
//

//
// #include <windows.h>
#include <stdio.h>
// #include <stdbool.h>


// --------------------------------------------------------------------------------
int main(int argc, char* argv[])
{

    typedef union {
        unsigned __int16 u16;
        signed __int16 s16;
    } Z16;

    Z16 a = { .u16 = 0 };
    Z16 b = { .u16 = 0 };
    Z16 c = { .u16 = 0 };
    Z16 d = { .u16 = 0 };


    a.u16 = 40;
    b.s16 = -2;

    c.u16 = a.u16 + b.u16;
    d.s16 = a.s16 + b.s16;

    printf("\n u %d  s %d\n", c.s16, d.s16);




    return (0);
}