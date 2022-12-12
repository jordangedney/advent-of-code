#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define FILE_MAX_COL (2 + 32)
#define FILE_MAX_ROW (6)
// #define FILE_MAX_ROW (6)
#define FILE_PATH "../inputs/3.test"

void readlines (char result[FILE_MAX_ROW][FILE_MAX_COL]) {
  FILE * file;
  file = fopen(FILE_PATH,"r");
  if(file == NULL) { printf("error opening file"); exit(1); }
  while(fgets(*result, FILE_MAX_COL, file) != NULL) { result++; }
  fclose(file);
}


  return 0;
}
