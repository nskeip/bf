#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

#define BF_MEMORY_SIZE 256

void bf(const char *const program_start) {
  if (*program_start == '\0' || program_start == NULL) {
    return;
  }
  char memory[BF_MEMORY_SIZE] = {0};
  char *data = memory;
  const char *instruction = program_start;
  do {
    switch (*instruction) {
    case '>':
      data = memory + (data - memory + 1) % BF_MEMORY_SIZE;
      break;
    case '<':
      data = memory + (data - memory - 1 + BF_MEMORY_SIZE) % BF_MEMORY_SIZE;
      break;
    case '+':
      ++(*data);
      break;
    case '-':
      --(*data);
      break;
    case '[':
      if (*data == 0) {
        ++instruction;
        for (int bracket_flag = 1; bracket_flag != 0; ++instruction) {
          bracket_flag += (*instruction == '[') - (*instruction == ']');
        }
      }
      break;
    case ']':
      if (*data != 0) {
        --instruction;
        for (int bracket_flag = 1; bracket_flag != 0; --instruction) {
          bracket_flag -= (*instruction == '[') - (*instruction == ']');
        }
      }
      break;
    case '.':
      printf("%c", *data);
      break;
    case ',':
      *data = getchar();
      break;
    }
  } while (*++instruction != '\0');
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s file\n", argv[0]);
    return 1;
  }
  int fd = open(argv[1], O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "Error: Cannot open file %s\n", argv[1]);
    return 1;
  }

  off_t len = lseek(fd, 0, SEEK_END);
  char *program_text = mmap(0, len, PROT_READ, MAP_PRIVATE, fd, 0);

  bf(program_text);

  munmap(program_text, len);
  return 0;
}
