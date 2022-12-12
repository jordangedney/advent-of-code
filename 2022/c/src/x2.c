#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define FILE_MAX_COL (2 + 3)
#define FILE_MAX_ROW (2500)
#define FILE_PATH "../inputs/2"

void readlines (char result[FILE_MAX_ROW][FILE_MAX_COL]) {
  FILE * file;
  file = fopen(FILE_PATH,"r");
  if(file == NULL) { printf("error opening file"); exit(1); }
  while(fgets(*result, FILE_MAX_COL, file) != NULL) { result++; }
  fclose(file);
}

int score_shape (char round[3]) {
  switch(round[2]) {
    case 'X': return 1;
    case 'Y': return 2;
    case 'Z': return 3;
  }
  printf("score_shape %s", round); exit(-1);
}

bool won (char round[3]) {
  switch (round[0]) {
    case 'A':
      if (round[2] == 'Y') return true;
      break;
    case 'B':
      if (round[2] == 'Z') return true;
      break;
    case 'C':
      if (round[2] == 'X') return true;
      break;
  }
  return false;
}

bool draw (char round[3]) {
  switch (round[0]) {
    case 'A':
      if (round[2] == 'X') return true;
      break;
    case 'B':
      if (round[2] == 'Y') return true;
      break;
    case 'C':
      if (round[2] == 'Z') return true;
      break;
  }
  return false;
}

int score_round (char round[3]) {
  if (won(round)) { return 6; }
  else if (draw(round)) { return 3; }
  return 0;
}

char get_move (char round[3]) {
  switch (round[2]) {
    case 'X':
      switch (round [0]){
        case 'A': return 'Z';
        case 'B': return 'X';
        case 'C': return 'Y';
      }
    case 'Y':
      switch (round [0]){
        case 'A': return 'X';
        case 'B': return 'Y';
        case 'C': return 'Z';
      }
    case 'Z':
      switch (round [0]){
        case 'A': return 'Y';
        case 'B': return 'Z';
        case 'C': return 'X';
      }
  }
  exit(-1);
}

int main () {
  char lines[FILE_MAX_ROW][FILE_MAX_COL];
  readlines(lines);

  // part one
  int total = 0;
  for(int i = 0; i<FILE_MAX_ROW; i++){
    total += score_shape(lines[i]);
    total += score_round(lines[i]);
  }
  printf("p1: %d\n", total);

  // part two
  total = 0;
  for(int i = 0; i<FILE_MAX_ROW; i++){
    char move = get_move(lines[i]);
    lines[i][2] = move;
    total += score_shape(lines[i]);
    total += score_round(lines[i]);
  }
  printf("p2: %d\n", total);

  return 0;
}
