#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILE_MAX_COL (2 + 5)
#define FILE_MAX_ROW (2 + 2264)
#define FILE_PATH "../inputs/1"

void readlines(char result[FILE_MAX_ROW][FILE_MAX_COL]) {
  FILE * file;
  file = fopen(FILE_PATH,"r");
  if(file == NULL) { printf("error opening file"); exit(1); }
  while(fgets(*result, FILE_MAX_COL, file) != NULL) { result++; }
  fclose(file);
}

int cmpInts (const void * a, const void * b) { return ( *(int*)a - *(int*)b ); }

int main () {
  char lines[FILE_MAX_ROW][FILE_MAX_COL];
  readlines(lines);

  // [String] -> [Int]
  int summed[FILE_MAX_ROW];
  int current_sum = 0, num_elf = 0;
  for(int i = 0; i < FILE_MAX_ROW; i++){
    if(strcmp(lines[i], "\n") == 0) {
      summed[num_elf] = current_sum;
      num_elf++;
      current_sum = 0;
    } else { current_sum += atoi(lines[i]); }
  }

  qsort(summed, num_elf, sizeof(int), cmpInts);

  printf("part-one: %d\n", summed[num_elf-1]);
  printf("part-two: %d\n", summed[num_elf-1] +
                           summed[num_elf-2] +
                           summed[num_elf-3]);

  return 0;
}
