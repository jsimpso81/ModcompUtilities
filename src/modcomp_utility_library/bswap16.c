unsigned __int16  bswap16(unsigned __int16 a) {
	
		a = ((a & 0x00FF) << 8) | ((a & 0xFF00) >> 8);
		return a;
	
}