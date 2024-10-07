// DiskFile_Dump.c : Dump disk file
//

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    bool swap_bytes = false;

    /* -------- annouce our program  */
    printf("\nDiskFile_Dump - Dump disk file in readable format.\n");

    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nDiskFile_Dump - Dump disk file in readable format.\n\n");
                printf("        -h       print help message and exit\n");
                printf("        -?       print help message and exit\n");
                printf("        -f file  name of file to dump\n");
                printf("        -s       swap  bytes\n");
                printf("        -n       dont swap bytes\n");
                exit(0);
            }

            /* -------- set swap bytes */
            else if (strcmp(argv[j], "-s") == 0) {
                swap_bytes = true;
            }

            /* -------- unset swap bytes */
            else if (strcmp(argv[j], "-n") == 0) {
                swap_bytes = false;
            }

            /* -------- set file name to dump */
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc)
                    dump_raw_disk_file(argv[j], swap_bytes);
            }

            /* --------unrecognized parameter */
            else {
                if ( j != 0) 
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }
    exit(0);
}
