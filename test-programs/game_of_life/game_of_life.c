#include <stdbool.h>

extern int system_call(int ecall_mode, void* input_pointer, unsigned int input_length);

#define NULL 0

// Width and height must be divisible by 8
#define WIDTH 8
#define HEIGHT 8

// To save memory we use a bitvector
#define BUFFER_SIZE ((WIDTH * HEIGHT) / 8)

void send_dma_l(char* msg, int len) {
  while (!system_call(0, msg, len)) {}
}

// Used when expanding the bitvector into chars 
// for writing to stdout via DMA
char ROW_BUFFER[WIDTH + 1];

// Buffer 1 and buffer 2 act as a double buffered bitvector of the current game state.
char BUFFER1[BUFFER_SIZE] = { 0 };
char BUFFER2[BUFFER_SIZE] = { 0 };

char* byte_address(char* buffer, unsigned int x, unsigned int y) {
  if (x >= WIDTH) { return NULL; }
  if (y >= HEIGHT) { return NULL; }
  int byte_index_x = x / 8;
  int row_offset = (y * (WIDTH / 8));
  return buffer + row_offset + byte_index_x;
}

unsigned char which_bit (unsigned int x) {
  return 1 << (x % 8);
}

bool get(char* buffer, unsigned int x, unsigned int y) {
  char* addr = byte_address(buffer, x, y);

  if (addr != NULL) {
    unsigned char byte = *addr;
    unsigned char bit = which_bit(x);
    return ((byte & bit) != 0);
  }
  
  return false;
}

void set(char* buffer, unsigned int x, unsigned int y, bool value) {
  char* addr = byte_address(buffer, x, y);

  if (addr != NULL) {
    if (value) {
      *addr = *addr | which_bit(x);
    } else {      
      unsigned char wb = which_bit(x); 
      unsigned char flipped_bit = ~wb;
      *addr = (*addr) & flipped_bit;
    }
  }
}

int neighbors(char* buffer, unsigned int x, unsigned int y) {
  // This is a little yucky, but if x or y overflow they will
  // fall out of bounds which will return an empty position upon
  // get.
  unsigned int sum = 0;

  unsigned int x_min = x - 1;
  unsigned int y_min = y - 1;

  if (x_min > x) {
    x_min = 0;
  }

  if (y_min > y) {
    y_min = 0;
  }

  unsigned int x_max = x + 1;
  unsigned int y_max = y + 1;

  if (x_max >= WIDTH) {
    x_max = WIDTH - 1;
  }

  if (y_max >= HEIGHT) {
    y_max = HEIGHT - 1;
  }

  for (unsigned int yi = y_min ; yi <= y_max; yi++) {
    for (unsigned int xi = x_min; xi <= x_max; xi++) {
      if ((x != xi) || (y != yi)) {
        if (get(buffer, xi, yi)) { 
          sum += 1;
        }
      }
    }
  }

  return sum;
}

void compute(char* next, char* prev) {
  for (unsigned int y = 0; y < HEIGHT; y++) {
    for (unsigned int x = 0; x < WIDTH; x++) {
      int c_neighbors = neighbors(prev, x, y);
      bool alive = get(prev, x, y);
      if (alive) {
        bool stays_alive = c_neighbors >= 2 && c_neighbors <= 3;
        set(next, x, y, stays_alive); 
      } else {
        set(next, x, y, c_neighbors == 3); 
      } 
    }
  }

}

void expand_row(char* dst, char* buffer, int y) {
  for (int x = 0; x < WIDTH; x++) {
    dst[x] = get(buffer, x, y) ? '*' : '-';
  }
}

void send_rows(char* buffer) {
  ROW_BUFFER[WIDTH] = '\n';

  for (int i = 0; i < HEIGHT; i++) {
    expand_row(ROW_BUFFER, buffer, i);
    send_dma_l(ROW_BUFFER, WIDTH + 1);
  }
}

void program_initial_state(char* buffer) {
  set(buffer, 3, 3, true);
  set(buffer, 3, 2, true);
  set(buffer, 2, 3, true);
  set(buffer, 2, 3, true);
  set(buffer, 5, 6, true);
  set(buffer, 6, 6, true);
  set(buffer, 7, 6, true);
  set(buffer, 6, 7, true);
}

void c_start() {
  send_dma_l("Starting up\n", 12);
  char* current = BUFFER1;
  char* next = BUFFER2;

  send_dma_l("Programming initial state\n", 26);
  program_initial_state(current);
  send_dma_l("Done\n", 5);

  send_dma_l("Startup\n", 8);
  send_rows(current);

  send_dma_l("Entering loop\n", 14);
  for (;;) {
    send_dma_l("S\n", 2);
    compute(next, current);
    send_rows(next);
    char* tmp = current;
    current = next;
    next = tmp;
  }
}
