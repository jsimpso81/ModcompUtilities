// USLPart_ExtractFiles.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"


/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    char  partition_name[1000];
    char  extract_directory[1000];
    char  recovery_directory[1000];
    bool  have_partition = false;
    bool  have_extract_dir = false;
    bool  have_recovery_dir = false;
    int   max_line_bytes = 80;
    bool  tape_flag = false;


    /* -------- annouce our program  */
    printf("\nUSLPart_DirectoryDump - List all directory entries for a USL formatted disk partition\n");


    /* -------- parse command line */
    /*      -f partition file      */
    /*      -d extract directory   */
    /*      -r recovered file directory */
    /*      -m max line byte len */    
    /*      -h                     */
    /*      -?                     */
    /*                             */


    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {
            /* printf("     %s\n", argv[j]); */

            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nUSLPart_DirectoryDump - List all directory entries for a USL formatted disk partition\n\n");
                printf("        -h            print help message nad exit\n");
                printf("        -?            print help message nad exit\n");
                printf("        -f file       extract individual files from this USL partition file\n");
                printf("        -d directory  extract files to this directory\n");
                printf("        -r directory  extract recoverd (deleted) files to this directory\n");
                printf("        -m rec_bytes  max line length in bytes, default = 80\n");
                printf("        -t            file is tape. No EOF after each entry.\n");
                exit(0);
            }

            /* -------- USL partition file */
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(partition_name, 1000, argv[j], strlen(argv[j]));
                    have_partition = true;
                }
            }

            /* -------- extract directory */
            else if (strcmp(argv[j], "-d") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(extract_directory, 1000, argv[j], strlen(argv[j]));
                    have_extract_dir = true;
            }
        }

            /* -------- recovery directory */
            else if (strcmp(argv[j], "-r") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(recovery_directory, 1000, argv[j], strlen(argv[j]));
                    have_recovery_dir = true;
                }
            }

            /* -------- max line byte length */
            else if (strcmp(argv[j], "-m") == 0) {
                j++;
                if (j < argc) {
                    max_line_bytes = 80;
                    sscanf_s(argv[j], "%d", &max_line_bytes);
                    if (max_line_bytes < 0 || max_line_bytes > 1500) {
                        printf("\n *** ERROR **** Max line bytes parameter: %d invalid, using default of 80\n", max_line_bytes);
                        max_line_bytes = 80;
                    }
                }
            }

            /* -------- tape file */
            else if (strcmp(argv[j], "-t") == 0) {
                tape_flag = true;
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }

    if (have_partition && have_extract_dir && have_recovery_dir) {
        USL_extract_all_files( partition_name, extract_directory, recovery_directory, max_line_bytes, tape_flag );
    }
    else {
        printf("\n *** ERROR ***  not all required parameters specified.\n");
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
