#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

#define BF_MEMORY_SIZE 256

void bf(const char *const program_start) {
  char memory[BF_MEMORY_SIZE] = {0};
  char *data = memory;

  const char *instruction = program_start;
  // int i = 0;
  while (*instruction) {
    // const size_t instruction_n = instruction - program_start;
    // printf("%c at %zu, (mem: %d,%d,%d,%d,%d)\n", *instruction, instruction_n,
    //        memory[0], memory[1], memory[2], memory[3], memory[4]);
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
        int opening_parenthesis_n = 1;
        while (1) {
          if (*++instruction == ']') {
            --opening_parenthesis_n;
          } else if (*instruction == '[') {
            ++opening_parenthesis_n;
          }
          if (opening_parenthesis_n == 0) {
            break;
          }
        }
      }
      instruction++;
      break;
    case ']':
      if (*data != 0) {
        int closing_parenthesis_n = 1;
        while (1) {
          if (--instruction == program_start) {
            fprintf(stderr, "Error: Unmatched ']'\n");
            return;
          }
          if (*instruction == '[') {
            --closing_parenthesis_n;
          } else if (*instruction == ']') {
            ++closing_parenthesis_n;
          }
          if (closing_parenthesis_n == 0) {
            break;
          }
        }
      }
      instruction++;
      break;
    case '.':
      printf("%c", *data);
      break;
    case ',':
      *data = getchar();
      break;
    }
    instruction++;
  }
  puts("");
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
