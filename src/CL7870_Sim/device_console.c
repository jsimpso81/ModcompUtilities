// -------- DEVICE CONSOLE
#include <windows.h>
#include <stdbool.h>

//  only one instance of this is allowed, at device address 0x0A.  Therefore the data here is global.

static unsigned __int16 console_status = 0x0180;
static unsigned __int16 console_command = 0;

static unsigned __int16 console_input_buffer_count = 0;
static unsigned int console_input_buffer_index = 0;
static unsigned __int8 console_input_buffer[8000];

static unsigned __int16 console_output_buffer_count = 0;
static unsigned __int16 console_output_buffer_index = 0;
static unsigned __int8 console_output_buffer[8000];

static int console_device_stop = 0;

void  device_console_output_data(unsigned __int16 data_value) {
	//printf("\n device_null output data -- called - %04x\n", data_value);
}

void  device_console_output_cmd(unsigned __int16 cmd_value) {
	//printf("\n device_null output cmd -- called - %04x\n", cmd_value);
	console_command = cmd_value;
}

unsigned __int16  device_console_input_data() {
	//printf("\n device_null input data -- called - 0x0000\n");
	return 0;
}

unsigned __int16  device_console_input_status() {
	//printf("\n device_null input status -- called - 0x0180\n");
	return 0x0180;
}

DWORD WINAPI device_console_worker_thread(LPVOID lpParam) {

	static dev_connected = false;
	static bool dev_reading = false;
	static bool dev_writing = false;
	static unsigned __int16 cmd_type = 0;

	console_status |= 0x8000; //--------indicate we are on.

	while (console_device_stop == 0) {

		// --------if a new command process it
		if (console_command != 0) {

			cmd_type = (console_command >> 14) & 0x0003;

			switch (cmd_type) {

			// --------command
			case 1:

				// --------terminate
				if ( (console_command & 0x0400) != 0) {
					dev_reading = false;
					dev_writing = false;
					console_input_buffer_count = 0;
					console_input_buffer_index = 0;
					console_status |= 0x0080;
				}
				break;

			// --------transfer initiate
			case 2:
				if (console_command & 0x0800) {
					dev_reading = true;
					dev_writing = false;	// not full duplex?? so can't do both.
				}
				else {
					dev_reading = false;
					dev_writing = true;	// not full duplex?? so can't do both.
				}
				break;

			}
			console_command = 0;
		}

		//--------if not connected, get connected.
		if (!dev_connected) {
			// --------set status

			//---------do a listen -- 2 second timeout
		}
	}

	console_device_stop = 0;

	ExitThread(0);

	return 0;

}