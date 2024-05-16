// -------- DEVICE NULL

void  device_null_output_data(unsigned __int16 data_value) {
	//printf("\n device_null output data -- called - %04x\n", data_value);
}

void  device_null_output_cmd(unsigned __int16 cmd_value) {
	//printf("\n device_null output cmd -- called - %04x\n", cmd_value);
}

unsigned __int16  device_null_input_data() {
	//printf("\n device_null input data -- called - 0x0000\n");
	return 0;
}

unsigned __int16  device_null_input_status() {
	//printf("\n device_null input status -- called - 0x0180\n");
	return 0x0180;

}

