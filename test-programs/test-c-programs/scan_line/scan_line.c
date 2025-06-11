#include <shared.h>

void initialize() {
	for (unsigned int y = 0; y < FRAMEBUFFER_HEIGHT; y++) {
		for (unsigned int x = 0; x < FRAMEBUFFER_WIDTH; x++) {
			framebuffer_set(x, y, false);
		}
	}
}

#define MAX_ELT = (FRAMEBUFFER_WIDTH * FRAMEBUFFER_HEIGHT)

volatile unsigned int data_ptr =  0;

void c_start() {
	send_dma("Start");
	for (;;) {
		for (unsigned int y = 0; y < FRAMEBUFFER_HEIGHT; y++) {
			for (unsigned int x = 0; x < FRAMEBUFFER_WIDTH; x++) {
				framebuffer_set(x, y, !framebuffer_get(x, y));
			}
		}
	}
}
