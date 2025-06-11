#include "shared.h"

int system_call(int ecall_mode, void* input_pointer, unsigned int input_length) {
  int output_mode;
  asm volatile("addi x5, %[mode], 0\n"
      "addi x6, %[iptr], 0\n"
      "addi x7, %[ilength], 0\n"
      "ecall\n"
      "addi %[output_mode], x5, 0"
      : [output_mode] "=r" (output_mode) 
      : [mode] "r" (ecall_mode),
         [iptr] "r" (input_pointer),
         [ilength] "r" (input_length));
  return output_mode;
}

void send_dma_l(char* msg, int len) {
	while (!system_call(0, msg, len)) {}
}

unsigned int strlen(char* msg) {
	char* origin = msg;
	while (*msg) {
		msg++;
	}
	return msg - origin;
}

void send_dma(char* msg) {
	unsigned int len = strlen(msg);
	send_dma_l(msg, len);
}

char* FRAMEBUFFER_START = (void*) 0x8000;

char* byte_address(unsigned int x, unsigned int y) {

	if (x >= FRAMEBUFFER_WIDTH || y >= FRAMEBUFFER_HEIGHT) {
		return NULL;
	}

	const int byte_index_x = x / 8;
	const int row_offset = (y * (FRAMEBUFFER_WIDTH / 8));
	return FRAMEBUFFER_START + row_offset + byte_index_x;
}

unsigned char which_bit (unsigned int x) {
	return 1 << (x % 8);
}

bool framebuffer_get(unsigned int x, unsigned int y) {
	const char* addr = byte_address(x, y);

	if (addr != NULL) {
		const unsigned char byte = *addr;
		const unsigned char bit = which_bit(x);
		return ((byte & bit) != 0);
	}

	return false;
}

void framebuffer_set( unsigned int x, unsigned int y, bool value) {
	char* addr = byte_address(x, y);

	if (addr != NULL) {
		if (value) {
			*addr = *addr | which_bit(x);
		} else {      
			unsigned char flipped_bit = ~which_bit(x);
			*addr = (*addr) & flipped_bit;
		}
	}
}
