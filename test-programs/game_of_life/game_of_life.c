#define NULL 0
#define WIDTH 10
#define HEIGHT 10

// To save memory we use a bitvector
#define BUFFER_SIZE ((WIDTH * HEIGHT) / 8)

int system_call(int ecall_mode, void* input_pointer, unsigned int input_length) {
  int output_mode = 0;
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

// Used when expanding the bitvector into chars 
// for writing to stdout via DMA
char ROW_BUFFER[BUFFER_SIZE];

// Buffer 1 and buffer 2 act as a double buffered bitvector of the current game state.
char BUFFER1[BUFFER_SIZE];
char BUFFER2[BUFFER_SIZE];

void cmemset(char* buffer, int size, char value) {
  for (unsigned int i = 0; i < size; i++) {
    buffer[i] = value;
  }
}

char* byte_address(char* buffer, unsigned int x, unsigned int y) {
  if (x > WIDTH) { return NULL; }
  if (y > HEIGHT) { return NULL; }
  int byte_index_x = x / 8;
  return buffer + (y * (WIDTH / 8)) + byte_index_x;
}

unsigned int which_bit (unsigned int x) {
  return 1 << (x % 8);
}

int get(char* buffer, unsigned int x, unsigned int y) {
  char* addr = byte_address(buffer, x, y);

  if (addr != NULL) {
    return (*addr & which_bit(x));
  }
  
  return 0;
}

void set(char* buffer, unsigned int x, unsigned int y, int value) {
  char* addr = byte_address(buffer, x, y);
  if (addr != NULL) {
    if (value) {
      *addr = *addr | which_bit(x);
    } else {
      *addr = *addr & !which_bit(x);
    }
  }
}

int min(int x1, int x2) {
  if (x1 > x2) {
    return x2;
  } else {
    return x1;
  } 
}

int max(int x1, int x2) {
  if (x1 < x2) {
    return x2;
  } else {
    return x1;
  } 
}

int neighbors(char* buffer, unsigned int x, unsigned int y) {
  // This is a little yucky, but if x or y overflow they will
  // fall out of bounds which will return an empty position upon
  // get.
  int sum = 0;

  for (int yi = min(y - 1, y) ; yi <= min(y + 1, HEIGHT); yi++) {
    for (int xi = min(x - 1, x); xi <= min(x + 1, WIDTH); xi++) {
      if (xi != x && yi != y) {
        sum += get(buffer, xi, yi);
      }
    }
  }

  return sum;
}

void compute(char* next, char* prev) {
  // Compute the next state of the grid
  for (unsigned int y = 0; y < HEIGHT; y++) {
    for (unsigned int x = 0; x < WIDTH; x++) {
      send_dma_l("N", 1);
      int c_neighbors = neighbors(prev, x, y);
      set(next, x, y, c_neighbors > 1 && c_neighbors < 4); 
    }
  }
}

void expand_row(char* dst, char* buffer, int y) {
  for (int i = 0; i < WIDTH; i++) {
   dst[i] = get(buffer, i, y) ? '*' : ' ';
  }
}

void send_rows(char* buffer) {
  for (int i = 0; i < HEIGHT; i++) {
    expand_row(ROW_BUFFER, buffer, i);
    send_dma_l(ROW_BUFFER, WIDTH);
  }
}

void c_start() {
  send_dma_l("Starting up", 11);
  char* current = BUFFER1;
  char* next = BUFFER2;

  for (;;) {
    send_dma_l("Entering loop", 13);
    cmemset(next, BUFFER_SIZE, 0);
    send_dma_l("Cleared",7);
    compute(next, current);
    send_dma_l("Computed", 9); 
    send_rows(next);
    char* tmp = current;
    current = next;
    next = tmp;
  }
}
