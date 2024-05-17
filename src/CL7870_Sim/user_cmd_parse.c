#include <stdio.h>
#include <stdbool.h>
#include <string.h>


void cmd_process_parse( char* cmd_line, int max_len, char* cmd_line_parse[], int max_items, int* count_found) {

	const char delim[] = "\t ,.=-;:\n\0";
	// int cmd_line_len = 0;
	char* context = NULL;
	char* token = NULL;
	int j;

	// cmd_line_len = (int)strlen(cmd_line);

	j = 0;
	token = strtok_s(cmd_line, delim, &context);
	if (token != NULL) {
		// printf(" token - %s\n", token);
		strcpy_s(cmd_line_parse[j], 1023, token);
		j++;
		while ((token = strtok_s(NULL, delim, &context)) != NULL) {
			// printf(" token - %s\n", token);
			strcpy_s(cmd_line_parse[j], 1023, token);
			j++;
		}
	}
	*count_found = j;

}
