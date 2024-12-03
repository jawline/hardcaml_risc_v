#include <stdbool.h>

#define NULL 0

// Width and height must be divisible by 8
#define WIDTH 16
#define HEIGHT 16

// This is the hardware framebuffer size, changing this must also be changed in the hardware RTL.
// Must be a multiple of hardware words * 8
#define FRAMEBUFFER_WIDTH 32
#define FRAMEBUFFER_HEIGHT 32

// To save memory we use a bitvector
#define BUFFER_SIZE ((WIDTH * HEIGHT) / 8)

// The framebuffer lives at this address and is hardcoded in the hardware.
char* FRAMEBUFFER_START = (void*) 0x8000;

const unsigned int FRAMEBUFFER_ROW_SIZE_IN_WORDS = (FRAMEBUFFER_WIDTH / 32) + ((FRAMEBUFFER_WIDTH % 32 != 0) ? 1 : 0);
const unsigned int FRAMEBUFFER_ROW_SIZE_IN_BYTES = FRAMEBUFFER_ROW_SIZE_IN_WORDS * 4;

// Buffer 1 and buffer 2 act as a double buffered bitvector of the current game state.
char BUFFER1 = (void*) 0x3000;
char BUFFER2 = (void*) 0x4000;

extern int system_call(int ecall_mode, void* input_pointer, unsigned int input_length);

void send_dma_l(char* msg, int len) {
  while (!system_call(0, msg, len)) {}
}

char* byte_address(char* buffer, unsigned int x, unsigned int y) {
  
  if (x >= WIDTH || y >= HEIGHT) {
    return NULL;
  }

  const int byte_index_x = x / 8;
  const int row_offset = (y * (WIDTH / 8));
  return buffer + row_offset + byte_index_x;
}

unsigned char which_bit (unsigned int x) {
  return 1 << (x % 8);
}

bool get(char* buffer, unsigned int x, unsigned int y) {
  const char* addr = byte_address(buffer, x, y);

  if (addr != NULL) {
    const unsigned char byte = *addr;
    const unsigned char bit = which_bit(x);
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
      unsigned char flipped_bit = ~which_bit(x);
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
      if (get(buffer, xi, yi)) { 
        sum += 1;
      }
    }
  }

  // Remove ourselves without branching in the critical path
  sum -= get(buffer, x, y) ? 1 : 0; 
 
  return sum;
}

void compute(char* next, char* prev) {
  for (unsigned int y = 0; y < HEIGHT; y++) {
    for (unsigned int x = 0; x < WIDTH; x++) {
      int c_neighbors = neighbors(prev, x, y);
      if (get(prev, x, y)) {
        set(next, x, y, c_neighbors >= 2 && c_neighbors <= 3); 
      } else {
        set(next, x, y, c_neighbors == 3); 
      } 
    }
  }
}


void expand_row_framebuffer(char* dst, char* buffer) {
  const unsigned int num_bytes_to_copy = WIDTH / 8;

  for (unsigned int x = 0; x < num_bytes_to_copy; x++) {
    dst[x] = buffer[x]; 
  }
}

// The framebuffer format is a row-word aligned bitvector.  To write the
// framebuffer we copy the selected buffer row by row into it, moving the
// framebuffer pointer ahead by a row width and the buffer pointer ahead by a
// game of life width.
void expand_rows_framebuffer(char* buffer) {
  char* row_ptr = FRAMEBUFFER_START;

  const unsigned int bitvector_row_size_in_bytes = WIDTH / 8;

  for (int i = 0; i < HEIGHT; i++) {
    expand_row_framebuffer(row_ptr, buffer);
    row_ptr += FRAMEBUFFER_ROW_SIZE_IN_BYTES;
    buffer += bitvector_row_size_in_bytes;
  }
}

void program_initial_state(char* buffer) {

        for (unsigned int i = 0; i < (WIDTH * HEIGHT) / 8; i++) {
                buffer[i] = 0;
        }

  set(buffer, 3, 3, true);
  set(buffer, 3, 2, true);
  set(buffer, 2, 3, true);
  set(buffer, 2, 3, true);
  set(buffer, 5, 6, true);
  set(buffer, 6, 6, true);
  set(buffer, 7, 6, true);
  set(buffer, 6, 7, true);
}

void program_initial_state_all(char* buffer) {
  for (unsigned int x = 0; x < WIDTH; x++) {
          for (unsigned int y = 0; y < HEIGHT; y++) {
                  set(buffer, x, y, true);
          }
  }
}

void initialize(char* current) {
  send_dma_l("Programming initial state\n", 26);
  program_initial_state(current);
  expand_rows_framebuffer(current);
  send_dma_l("Done\n", 5);
}

void c_start() {
  send_dma_l("Starting up\n", 12);
  char* current = BUFFER1;
  char* next = BUFFER2;

  initialize(current);

 // send_dma_l("Entering loop\n", 14);
 // for (;;) {
 //   send_dma_l("S\n", 2);
 //   compute(next, current);
 //   expand_rows_framebuffer(next);
 //   char* tmp = current;
 //   current = next;
 //   next = tmp;
 //   send_dma_l("Computed row\n", 13);
 // }
}
