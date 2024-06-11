// This function assumes that x5 - x7 are used as the registers
void system_call(int mode, void* ptr, unsigned int length) {
  asm volatile ("ecall");
}

unsigned int strlen(char* msg) {
  char* origin = msg;
  while (*msg) {
    msg++;
  }
  return msg - origin;
}

void send_dma(char* msg) {
  system_call(0, msg, strlen(msg));
}

char* hello_world = "Hello world!";

void c_start() {
  send_dma(hello_world);
  for (;;) {
  }
}
