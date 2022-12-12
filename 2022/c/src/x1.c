#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {

  FILE * file;
  file = fopen("../inputs/1","r");

  if(file == NULL) { printf("error opening file"); exit(1); }

  int cur = 0;
  int max = 0;
  int second_max = 0;
  int third_max = 0;

  char str[10];
  while(fgets(str, 10, file) != NULL) {
    if (strcmp("\n", str) == 0) {

      if (max < cur) {
        third_max = second_max;
        second_max = max;
        max = cur;
      } else if (second_max < cur) {
        third_max = second_max;
        second_max = cur;
      } else if (third_max < cur) {
        third_max = cur;
      }

      cur = 0;

    } else { cur += atoi(str); }
  }

  printf("max: %d\n", max);
  printf("top three: %d + %d + %d = %d\n", max, second_max,  third_max,
                                          (max + second_max + third_max));


  return 0;
}
