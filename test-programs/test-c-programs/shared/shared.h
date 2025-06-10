#ifndef _SHARED_H
#define _SHARED_H

#include <stdbool.h>

#define NULL 0

// This is the hardware framebuffer size, changing this must also be changed in the hardware RTL.
// Must be a multiple of hardware words * 8
#define FRAMEBUFFER_WIDTH 64
#define FRAMEBUFFER_HEIGHT 32

bool framebuffer_get(unsigned int x, unsigned int y);
void framebuffer_set(unsigned int x, unsigned int y, bool value);

extern int system_call(int ecall_mode, void* input_pointer, unsigned int input_length);
void send_dma_l(char* msg, int len);
void send_dma(char* msg);

#endif
