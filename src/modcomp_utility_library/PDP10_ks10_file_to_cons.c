#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

// #define MAX(i, j) (((i) > (j)) ? (i) : (j))
// #define MIN(i, j) (((i) < (j)) ? (i) : (j))



/* ========================================================================================================================*/
void PDP10_ks10_file_to_cons(char* filename, char* consport) {

    FILE* cmdfile = NULL;
    errno_t status;
    bool have_ks10_prompt = true;
    unsigned int ks10_prompt_state = 0;
    int open_status;
    bool not_done = true;
    bool write_init_crlf = true;
    unsigned __int8 loc_write_data[1000] = { 0 };
    unsigned __int8* ptr_loc_write_data = NULL;
    unsigned __int8 loc_read_data[1000] = { 0 };
    unsigned __int8 write_crlf[4] = { 13, 10, 0, 0 };
    bool have_good_line = false;
    HANDLE cons_port = 0;
    DWORD last_error = 0;
    DWORD desired_read_bytes = 0;
    DWORD actual_read_bytes = 0;
    size_t bytes_to_write_t = 0;
    DWORD bytes_to_write = 0;
    DWORD bytes_written = 0;
    BOOL read_status = false;
    BOOL write_status = false;

    /* -------- open input disk image */
    status = fopen_s(&cmdfile, filename, "r");

    if (status != 0) {
        fprintf(stderr, "\n *** ERROR *** - could not open file -- %s\n\n", filename);
        return;
    }

    // --------Open com port
    open_status = util_serial_open( consport, &cons_port, &last_error);
    if (open_status != 0) {
        fprintf(stderr, "\n *** ERROR *** - could not open console -- %s, 0x%08lx\n\n", consport,last_error);
        return;
    }

    // -------- set com port parameters
    util_serial_set_params(cons_port, &last_error, false);


    // --------process state machine....

    not_done = true;
    have_ks10_prompt = true;
    write_init_crlf = true;
    ks10_prompt_state = 0;
    unsigned int j;

    // --------do until done....
    while (not_done) {

        // --------if ready for write then do it...
        // --------if ready for a write, then write next line from file....
        if (have_ks10_prompt) {
            if (write_init_crlf) {
                bytes_to_write = 2;
                bytes_written = 0;
                write_status = WriteFile(cons_port, &write_crlf[0], bytes_to_write,
                    &bytes_written, NULL);
                write_init_crlf = false;
                fprintf(stdout, "%s", &write_crlf[0]);
            }
            else {
                have_good_line = false;
                bytes_to_write = 0;
                while (!have_good_line) {
                    loc_write_data[0] = ';';
                    ptr_loc_write_data = fgets( &loc_write_data[0], 997, cmdfile);
                    if (ptr_loc_write_data == NULL) {
                        have_good_line = true;
                        not_done = false;
                    }
                    else {
                        if (loc_write_data[0] != ';') {
                            have_good_line = true;
                            bytes_to_write_t = strlen(loc_write_data);  // does this have trailing CR LF... NO only LF
                            bytes_to_write = bytes_to_write_t;
                            loc_write_data[bytes_to_write - 1] = 13;
                            loc_write_data[bytes_to_write] = 10;
                            loc_write_data[bytes_to_write+1] = 0;
                            bytes_to_write++;
                        }
                    }
                 }
                if (bytes_to_write > 0) {
                    bytes_written = 0;
                    write_status = WriteFile(cons_port, loc_write_data, bytes_to_write,
                        &bytes_written, NULL);
                    fprintf(stdout, "%s", loc_write_data);
                }
            }
            have_ks10_prompt = false;
            ks10_prompt_state = 0;
            fprintf(stderr, "\n bytes writen: %d, state: %d", bytes_written, ks10_prompt_state);
        }

        // --------do a read.
        desired_read_bytes = 1;			// 50;
        actual_read_bytes = 0;
        read_status = ReadFile(cons_port, &loc_read_data,
            desired_read_bytes, &actual_read_bytes, NULL);


        // --------got a byte
        if (actual_read_bytes > 0) {
            // fprintf(stderr, " Console bytes read %d.  Device Addr %d\n", actual_read_bytes, loc_device_addr);
            for (j = 0; j < actual_read_bytes; j++) {
                //  loc_read_data[j];
                switch ( ks10_prompt_state ) {

                case 0: // wait for K
                    if (loc_read_data[j] == 'K') {
                        ks10_prompt_state = 1;
                    }
                    else {
                        ks10_prompt_state = 0;
                    }
                break;

                case 1: // wait for S
                    if (loc_read_data[j] == 'S') {
                        ks10_prompt_state = 2;
                    }
                    else {
                        ks10_prompt_state = 0;
                    }
                    break;

                case 2: // wait for 1
                    if (loc_read_data[j] == '1') {
                        ks10_prompt_state = 3;
                    }
                    else {
                        ks10_prompt_state = 0;
                    }
                    break;

                case 3: // wait for 0
                    if (loc_read_data[j] == '0') {
                        ks10_prompt_state = 4;
                    }
                    else {
                        ks10_prompt_state = 0;
                    }
                    break;

                case 4: // wait for >
                    if (loc_read_data[j] == '>') {
                        ks10_prompt_state = 0;
                        have_ks10_prompt = true;
                    }
                    else {
                        ks10_prompt_state = 0;
                    }
                    break;


                default:
                    ks10_prompt_state = 0;
                    break;

                }
                fprintf(stdout, "%c", loc_read_data[j]);
            }
        }

        // -------- no bytes read....
        else {

            // -------- do a small wait and do it all over again...
            Sleep(1);   // milliseconds
        }
    }

    // -------- close console port
    util_serial_close(cons_port, &last_error);

    // --------  close input file
    fclose(cmdfile);

    return;

}
