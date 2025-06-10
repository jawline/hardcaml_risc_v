int system_call(int ecall_mode, void* input_pointer, unsigned int input_length) {
  int output_mode;
  asm("addi x5, %[mode], 0\n"
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
char* string_in_the_middle = "In the middle!";
char* goodbye = "Goodbye";

void c_start() {
  while (!send_dma(hello_world)) {}
  while (!send_dma(string_in_the_middle)) {}
  while (!send_dma(goodbye)) {}
  for (;;) {
  }
}
