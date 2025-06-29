#include <shared.h>

char* hello_world = "Hello world!";
char* string_in_the_middle = "In the middle!";
char* goodbye = "Goodbye";

void c_start() {

  send_dma_l((char*) 0 , 32);
  send_dma(hello_world);
  framebuffer_set(0, 0, 1);
  framebuffer_set(1, 0, 1);
  send_dma(string_in_the_middle);
  framebuffer_set(2, 0, 1);
  framebuffer_set(3, 0, 1);
  send_dma(goodbye);
  framebuffer_set(4, 0, 1);
  framebuffer_set(5, 0, 1);
  for (;;) {
  }
}
