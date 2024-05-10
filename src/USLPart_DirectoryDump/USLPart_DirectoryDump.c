// USLPart_DirectoryDump.c : Dump directory entries of a USL formatted disk partition
//

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"



/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;

    /* -------- annouce our program  */
    printf("\nUSLPart_DirectoryDump - List all directory entries for a USL formatted disk partition\n");


    /* -------- parse command line */
    /*      -f file                */
    /*      -h                     */
    /*      -?                     */
    /*                             */


    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {
            /* printf("     %s\n", argv[j]); */

            if ( strcmp( argv[j], "-h" ) ==0  || strcmp(argv[j], "-?" ) ==0 ) {
                printf("\n\nUSLPart_DirectoryDump - List all directory entries for a USL formatted disk partition\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -f file  dump the directory of this USL partition file\n");
                exit(0);
            }
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc)
                    USL_dump_directory(argv[j]);
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }


    exit(0);

}


// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
