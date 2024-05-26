unsigned __int16 device_address;
volatile unsigned __int16 ctrl_wake;
volatile unsigned __int16 ctrl_status;
QUEUE_UWORD ctrl_command_que;
volatile unsigned _int16 bus;	// IO bus 0-3
volatile unsigned _int16 dmp;	// for non dmp specify 0
volatile unsigned _int16 pri;
char info[40];	// short description
