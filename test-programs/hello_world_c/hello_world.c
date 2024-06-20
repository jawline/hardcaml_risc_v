// This function assumes that x5 - x7 are used as the registers
void system_call(int imode, void* iptr, unsigned int ilength) {
  register int mode asm("x5") = imode;
  register void* ptr asm("x6") = iptr;
  register unsigned int length asm("x7") = ilength;
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
char* goodbye = "Goodbye";

void c_start() {
  send_dma(hello_world);
  send_dma(goodbye);
  for (;;) {
  }
}
