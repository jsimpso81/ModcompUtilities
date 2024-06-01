unsigned __int16 device_address;
volatile unsigned __int16 ctrl_wake;
volatile unsigned __int16 ctrl_status;
volatile unsigned __int16 bus;	// IO bus 0-3
volatile unsigned __int16 dmp;	// for non dmp devices, specify 0
volatile unsigned __int16 pri;	// priority 0 - 15 (or is it 1 - 15)
volatile bool SI_enabled;
volatile bool DI_enabled;
volatile bool write_in_progress;
volatile bool read_in_progress;
char info[40];	// short description
volatile DEVICE_BUFFER in_buff;
volatile DEVICE_BUFFER out_buff;
volatile QUEUE_UWORD ctrl_command_que;		// may be obsolete
