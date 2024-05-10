unsigned int get_can_index(unsigned int ascii_code) {

	/* -------- space */
	if (ascii_code == 32) {
		return 0;
	}
	/* -------- numbers - start at 27 */
	else if (ascii_code >= 48 && ascii_code <= 57) {
		return ascii_code - 21;
	}
	/* -------- letters - start at 1 */
	else if (ascii_code >= 65 && ascii_code <= 90) {
		return ascii_code - 64;
	}
	/* -------- colon */
	else if (ascii_code == 58) {
		return 37;
	}
	/* -------- period */
	else if (ascii_code == 46) {
		return 38;
	}
	/* -------- $ */
	else if (ascii_code == 36) {
		return 39;
	}
	return 9999;
}