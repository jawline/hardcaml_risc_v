#include <shared.h>

void initialize() {
	for (unsigned int y = 0; y < FRAMEBUFFER_HEIGHT; y++) {
		for (unsigned int x = 0; x < FRAMEBUFFER_WIDTH; x++) {
			framebuffer_set(x, y, false);
		}
	}
}

#define MAX_ELT = (FRAMEBUFFER_WIDTH * FRAMEBUFFER_HEIGHT)

void c_start() {
	initialize();
	for (;;) {
		for (unsigned int y = 0; y < FRAMEBUFFER_HEIGHT; y++) {
			for (unsigned int x = 0; x < FRAMEBUFFER_WIDTH; x++) {
				framebuffer_set(x, y, !framebuffer_get(x, y));
				// Burn some cycles
				// TODO: Use the processor clock instead
				for (unsigned int i = 0; i < 500000; i++) {
				}
			}
		}
	}
}
