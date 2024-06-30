
#include "simj_base.h"

#include <profileapi.h>

// -------- ticks per second.  Need this to be at least 10,000,000
static LARGE_INTEGER HIRES_CLK_frequency = { .QuadPart = 0 };

// ================================================================================================
void util_high_res_spin_wait(SIMJ_U16 wait_time_100ns) {

	LARGE_INTEGER StartingTime = { .QuadPart = 0 };
	LARGE_INTEGER EndingTime = { .QuadPart = 0 };
	LARGE_INTEGER CurrentTime = { .QuadPart = 0 };

	if (HIRES_CLK_frequency.QuadPart == 0) {
		QueryPerformanceFrequency(&HIRES_CLK_frequency);
		if (HIRES_CLK_frequency.QuadPart < 10000000) {
			printf(" *** ERROR *** Hi res time resolution is too small %lld\n", HIRES_CLK_frequency.QuadPart);
		}
		else {
			fprintf(stderr, " Hi res timer resolution is %lld\n", HIRES_CLK_frequency.QuadPart);
		}
	}

	// --------get starting time.
	QueryPerformanceCounter(&StartingTime);
	// --------convert to 100 nanoseconds
	StartingTime.QuadPart *= 10000000;
	StartingTime.QuadPart /= HIRES_CLK_frequency.QuadPart;
	// --------get ending time.
	EndingTime.QuadPart = StartingTime.QuadPart;
	EndingTime.QuadPart += (LONGLONG)wait_time_100ns;
	// --------DEBUG
	//fprintf(stderr, "Hi Res Spin Wait ending time %lld\n", EndingTime.QuadPart);
	// --------END DEBUG

	// --------do the wait
	while (true) {
		// --------get starting time.
		QueryPerformanceCounter(&CurrentTime);
		// --------convert to 100 nanoseconds
		CurrentTime.QuadPart *= 10000000;
		CurrentTime.QuadPart /= HIRES_CLK_frequency.QuadPart;
		if (CurrentTime.QuadPart >= EndingTime.QuadPart) {
			break;
		}
 	}
	return;

}

// ================================================================================================
LARGE_INTEGER util_high_res_spin_wait_get_start() {

	LARGE_INTEGER StartingTime = { .QuadPart = 0 };

	if (HIRES_CLK_frequency.QuadPart == 0) {
		QueryPerformanceFrequency(&HIRES_CLK_frequency);
		if (HIRES_CLK_frequency.QuadPart < 10000000) {
			printf(" *** ERROR *** Hi res time resolution is too small %lld\n", HIRES_CLK_frequency.QuadPart);
		}
		else {
			fprintf(stderr, " Hi res timer resolution is %lld\n", HIRES_CLK_frequency.QuadPart);
		}
	}

	// --------get starting time.
	QueryPerformanceCounter(&StartingTime);
	// --------convert to 100 nanoseconds
	StartingTime.QuadPart *= 10000000;
	StartingTime.QuadPart /= HIRES_CLK_frequency.QuadPart;

	return StartingTime;

}


// ================================================================================================
void util_high_res_spin_wait_finish(SIMJ_U16 wait_time_100ns, LARGE_INTEGER StartingTime ) {

	LARGE_INTEGER EndingTime = { .QuadPart = 0 };
	LARGE_INTEGER CurrentTime = { .QuadPart = 0 };

	if (HIRES_CLK_frequency.QuadPart == 0) {
		QueryPerformanceFrequency(&HIRES_CLK_frequency);
		if (HIRES_CLK_frequency.QuadPart < 10000000) {
			printf(" *** ERROR *** Hi res time resolution is too small %lld\n", HIRES_CLK_frequency.QuadPart);
		}
		else {
			fprintf(stderr, " Hi res timer resolution is %lld\n", HIRES_CLK_frequency.QuadPart);
		}
	}

	// --------get ending time.
	EndingTime.QuadPart = StartingTime.QuadPart;
	EndingTime.QuadPart += (LONGLONG)wait_time_100ns;
	// --------DEBUG
	//fprintf(stderr, "Hi Res Spin Wait ending time %lld\n", EndingTime.QuadPart);
	// --------END DEBUG

	// --------do the wait
	while (true) {
		// --------get starting time.
		QueryPerformanceCounter(&CurrentTime);
		// --------convert to 100 nanoseconds
		CurrentTime.QuadPart *= 10000000;
		CurrentTime.QuadPart /= HIRES_CLK_frequency.QuadPart;
		if (CurrentTime.QuadPart >= EndingTime.QuadPart) {
			break;
		}
	}
	return;

}
