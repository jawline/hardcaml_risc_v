// Width must be a multiple of 8
#define WIDTH 80
#define HEIGHT 24

// To save memory we use a bitvector
#define BUFFER_SIZE ((WIDTH * HEIGHT) / 8)

// This function assumes that x5 - x7 are used as the registers
void system_call(int mode, void* ptr, unsigned int length) {
  asm volatile ("ecall");
}

void send_dma_l(char* msg, int len) {
  system_call(0, msg, len);
}

char BUFFER1[BUFFER_SIZE];
char BUFFER2[BUFFER_SIZE];

void memset(char* buffer, int size, char value) {
  for (unsigned int i = 0; i < size; i++) {
    buffer[i] = value;
  }
}

char get(char* buffer, unsigned int x, unsigned int y) {
  if (x > WIDTH) { return ' '; }
  if (y > HEIGHT) { return ' '; }
  int byte_index_x = x / 8;
  int bit_x = 1 << (x % 8);
  char* row = buffer + (y * (WIDTH / 8));
  return (row[byte_index_x] & bit_x) != 0;
}

void compute(char* next, char* prev) {
  for (unsigned int i = 0; i < HEIGHT; i++) {
    for (unsigned int i = 0; i < WIDTH; i++) {

    }
  }
}


void expand_row(char* dst, char* src) {
}

void send_rows(char* buffer) {
  for (int i = 0; i < HEIGHT; i++) {
    send_dma_l(buffer + (i * WIDTH), WIDTH);
  }
}

void c_start() {
  char* current = BUFFER1;
  char* next = BUFFER2;

  for (;;) {
    memset(next, BUFFER_SIZE, 0);
    compute(next, current);
    send_rows(next);
    char* tmp = current;
    current = next;
    next = tmp;
  }
}
