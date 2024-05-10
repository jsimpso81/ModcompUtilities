#include <stdio.h>
#include <string.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"


void USL_Extract_File(FILE* inpart, unsigned __int16 USL_log_file, USL_FILE_ENTRY* parsed_file_entry, char* directory, int max_line_bytes) {

	/* -------- see if this is from an attached file. */
	if (USL_log_file != 0) {
		char cancode[4] = { 0, 0, 0, 0 };
		char temp_string[10] = { 0 };
		strcpy_s( cancode, 4, from_can_code(USL_log_file, temp_string));
		printf("\n *** ERROR *** Attached files not supported. File extract not processed. Attached file: %s\n", cancode );
	}

	/* -------- extract the file */
	else {

		char base_file_name[2000];
		char final_file_name[2000];
		char tmp_string[2000];
		size_t dir_len;
		int j;
		FILE* tempfile;
		int status;
		bool done = false;

		/* --------make base file name */
		strcpy_s(base_file_name, 2000, directory);
		dir_len = strlen(directory );
		if (directory[dir_len - 1] != '/ ' && directory[dir_len - 1] != '\\')
			strcat_s(base_file_name, 2000, "/");
		strcat_s(base_file_name, 2000, parsed_file_entry->file_name);

		/* -------- ensure that file does not exist */
		for (j = 0; j < 1000; j++) {
			strcpy_s(final_file_name, 2000, base_file_name);
			if (j > 0) {
				sprintf_s(tmp_string, 2000, "(%d)", j);
				strcat_s(final_file_name, 2000, tmp_string);
			}
			status = fopen_s(&tempfile, final_file_name, "r");
			if (status != 0) 
				done = true;
			if ( tempfile != NULL )
				fclose(tempfile);
			if (done) 
				break;
		}
		if (!done) {
			printf("\n *** ERROR *** Trouble with filename %s.  File not extracted\n", base_file_name);
		}
		else {
			status = fopen_s(&tempfile, final_file_name, "w");
			if (status == 0) {

				/* --------do the work of extracton here */
				unsigned __int16 current_sector = parsed_file_entry->starting_sector;
				unsigned __int16 last_sector = parsed_file_entry->starting_sector + parsed_file_entry->sector_count -2; /* ----- the last sector contains $$ (end of file), not program*/
				struct {
					unsigned __int8 checksum;
					unsigned __int8 header;
					unsigned __int16 byte_count;
					union {
						unsigned __int16 raw_sector_words[126];
						signed __int8  raw_sector_bytes[252];
					} data;
				} sector_data;
				int current_byte_pointer = -1;
				int number_of_data_bytes = 0;
				int state = 0;
				int output_line_index = 0;
				bool not_done = true;
				size_t return_count;
				int end_of_file;
				int stat;
				char byte_to_process = 0;
				char previous_byte = 0;
				char next_byte = 0;
				int  remaining_negative_count_to_process = 0;
				char current_output_line[4000] = { 0 };
				int  current_output_line_index = 0;


				while (not_done) {

					/* -------- see if we need to read a new sector */
					if (current_byte_pointer < 0 || current_byte_pointer >= number_of_data_bytes) {
						if (current_sector > last_sector) {
							not_done = false;
							break;
							printf("\n ---- file break -----\n");
						}
						else {
							/* -------- read next data sector */
							printf("\n==========reading sector %d ==============\n", current_sector);
							stat = read_sector_lba(inpart, current_sector, 1, &sector_data, &return_count, &end_of_file);
							if (stat != 0 || return_count <= 0 || end_of_file != 0) {
								printf("\n *** ERROR *** trouble reading data sector of USL file. status = %d, count = %zd, eof = %d\n", stat, return_count, end_of_file);
								not_done = false;
								break;
								printf("\n ---- file break -----\n");
							}
							/* --------verify first byte has 0x03 */
							if (sector_data.header != 3) {
								printf("\n *** ERROR *** data sector does not start with 0x03  value = %d\n", sector_data.header);
							}
							/* --------verify checksum -- FUTURE */


							number_of_data_bytes = sector_data.byte_count - 4; /* remove count for header */
							//if (current_sector >= last_sector)
							//	number_of_data_bytes = parsed_file_entry->last_sector_words_used * 2;

							current_byte_pointer = 0;
							current_sector++;
						}
					}

					/* --------get next byte -- for use later */
					if (current_byte_pointer % 2 == 0) {
						next_byte = sector_data.data.raw_sector_bytes[current_byte_pointer + 1];
					}
					else {
						next_byte = sector_data.data.raw_sector_bytes[current_byte_pointer - 1];
					}

					/* --------if repeating - countine to repeat until done */
					if (remaining_negative_count_to_process < 0) {
						byte_to_process = previous_byte;
						remaining_negative_count_to_process++;
					}
					/* -------- check for a repeat count */
					else if (next_byte < 0) {
						remaining_negative_count_to_process = next_byte;
						byte_to_process = previous_byte;
						remaining_negative_count_to_process++;
						current_byte_pointer++;
					}

					/* --------get a new byte */
					else {
						remaining_negative_count_to_process = 0;
						byte_to_process = next_byte;
						previous_byte = byte_to_process;
						current_byte_pointer++;
					}

					/* -------- add byte to output string */

					/* --------see if line is full (>80 chars) and the byte isn't line terminator, output line */
					/* --------use a parameter for the max line bytes to accomodate JAS extension from the 90s...*/
					if (byte_to_process != 0 && current_output_line_index >= max_line_bytes) {
						current_output_line[current_output_line_index] = 0;
						// printf("line:%s", current_output_line);
						fprintf(tempfile, "%s\n", current_output_line);
						current_output_line_index = 0;
					}

					current_output_line[current_output_line_index] = byte_to_process;

					if (byte_to_process == 0) {
						// printf("line:%s", current_output_line);
						fprintf(tempfile, "%s\n", current_output_line);
						current_output_line_index = 0;
					}
					else {
						current_output_line_index++;
					}


				}

				/* -------- flush last line */
				if (current_output_line_index > 0) {
					current_output_line[current_output_line_index] = 0;
					// printf("last:%s", current_output_line);
					fprintf(tempfile, "%s\n", current_output_line);
				}

				/* ---------verify that the last sector has $$ */

				if ( tempfile != NULL )
					fclose(tempfile);
			}

		}

	}

}