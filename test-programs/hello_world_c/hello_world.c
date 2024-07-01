// This function assumes that x5 - x7 are used as the registers
int system_call(int imode, void* iptr, unsigned int ilength) {
  register int mode asm("x5") = imode;
  register void* ptr asm("x6") = iptr;
  register unsigned int length asm("x7") = ilength;
  asm volatile ("ecall");
  // x5 will be replaced with true/false if busy
  return mode;
}

unsigned int strlen(char* msg) {
  char* origin = msg;
  while (*msg) {
    msg++;
  }
  return msg - origin;
}

int send_dma(char* msg) {
  return system_call(0, msg, strlen(msg));
}

char* hello_world = "Hello world!";
char* goodbye = "Goodbye";

void c_start() {
  while (!send_dma(hello_world)) {}
  while (!send_dma(goodbye)) {}
  for (;;) {
  }
}
