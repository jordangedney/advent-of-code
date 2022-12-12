#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define FILE_MAX_COL (2 + 48)
/* #define FILE_MAX_ROW (6) */
#define FILE_MAX_ROW (2 + 300)
/* #define FILE_PATH "../inputs/3.test" */
#define FILE_PATH "../inputs/3"

void readlines (char result[FILE_MAX_ROW][FILE_MAX_COL]) {
  FILE * file;
  file = fopen(FILE_PATH,"r");
  if (file == NULL) { printf("error opening file"); exit(1); }
  while (fgets(*result, FILE_MAX_COL, file) != NULL) { result++; }
  fclose(file);
}

int priority(char c){
  if (isupper((int)c)) { return c - 38; }
  return c - 96;
}

int main () {
  char lines[FILE_MAX_ROW][FILE_MAX_COL];
  readlines(lines);

  // part one
  int total = 0;
  for (int i = 0; i<FILE_MAX_ROW; i++) {
    char * line = lines[i];
    int half = strlen(line) / 2;

    char match;
    for (int j = 0; j<half; j++) { // first half
      for (int k = half; k<FILE_MAX_COL; k++) { // last half
        if (line[j] == line[k]) { match = line[j]; }
      }
    }
    total += priority(match);
  }
  printf("p1: %d\n", total);

  // part two
  total = 0;
  for (int i = 0; i<FILE_MAX_ROW; i += 3) {
    char * line1 = lines[i];
    int line1_len = (int)strlen(line1) - 1;
    char * line2 = lines[i+1];
    int line2_len = strlen(line2) - 1;
    char * line3 = lines[i+2];
    int line3_len = strlen(line3) - 1;

    char match = 'X';
    for (int j = 0; j<line1_len; j++) {
      char c1, c2, c3;
      c1 = line1[j];
      for (int k = 0; k<line2_len; k++) {
        c2 = line2[k];
        for (int l = 0; l<line3_len; l++) {
          c3 = line3[l];
          if (c1 == c2  && c1 == c3) {
            match = line1[j];
          }
        }
      }
    }
    total += priority(match);
  }
  printf("p2: %d\n", total);

  return 0;
}
