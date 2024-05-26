#include <stdio.h>

void user_cmd_print_help() {

	printf("\n\nCL7870_sim - Simulate a MODCOMP classic 7870 CPU \n\n");
	printf("\n   Command line parameters:\n");
	printf("        -h       print help message and exit\n");
	printf("        -?       print help message and exit\n");
	printf("        -v       verbose debug messages.\n");
	printf("\n   Commands:\n");

//	printf("      device console <comx> - define console device and assign to com port\n");
//	printf("      device console tcp <port> - define console device and assign to tcp port\n");

	printf("      fill <device addr> - fill\n");
	printf("      halt - halt cpu\n");
	printf("      help - print this list of commands\n");
	printf("      mc - master clear\n");
	printf("      run - run cpu\n");

	printf("      set mem <addr> <value> - set memory value\n");
	printf("      set pc <value> - set program counter value\n");
	printf("      set reg <reg> <value> - set register value\n");
	printf("      set switches <value> - set console switches\n");
	printf("      set verbose <on/off> - set verbose debug mode\n");

	printf("      show devices - show all configured devices\n");
	printf("      show mem <start> <end> - show memory\n");
	printf("      show pc - show program counter\n");
	printf("      show psw - show processor status word\n");
	printf("      show reg - show current register value\n");
	printf("      show run - show current cpu run state\n");
	printf("      show switches - show console switches\n");
	printf("      show verbose - show verbose debug mode\n");

	printf("      step <instruction count> - execute a single instruction\n");

	printf("\n");


}