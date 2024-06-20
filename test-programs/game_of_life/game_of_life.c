#define NULL 0
#define WIDTH 80
#define HEIGHT 24

// To save memory we use a bitvector
#define BUFFER_SIZE ((WIDTH * HEIGHT) / 8)

// This function assumes that x5 - x7 are used as the registers
void system_call(int imode, void* iptr, unsigned int ilength) {
  register int mode asm("x5") = imode;
  register void* ptr asm("x6") = iptr;
  register unsigned int length asm("x7") = ilength;
  asm volatile ("ecall");
}

void send_dma_l(char* msg, int len) {
  system_call(0, msg, len);
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

int neighbors(char* buffer, unsigned int x, unsigned int y) {
  // This is a little yucky, but if x or y overflow they will
  // fall out of bounds which will return an empty position upon
  // get.
  int sum = 0;

  for (int yi = y - 1 ; yi <= y + 1; yi++) {
    for (int xi = x - y; xi <= x + 1; yi++) {
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
    cmemset(next, BUFFER_SIZE, 0);
    compute(next, current);
    send_rows(next);
    char* tmp = current;
    current = next;
    next = tmp;
  }
}
