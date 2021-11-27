/*
Simple utility to adjust brightness.
Should be owned by root and chmod 4755 so that users can run it.
Usage: bright [+|-|PERCENTAGE]
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage(int ret) {
  fprintf(stderr, "Usage: bright [+|-|PERCENTAGE]\n");
  exit(ret);
}

int readFile(const char *path) {
  FILE *f = fopen(path, "r");
  int n;
  if (!fscanf(f, "%d", &n)) {
    fprintf(stderr, "Unxpected file contents: %s\n", path);
    usage(1);
  }
  fclose(f);
  return n;
}

void writeFile(const char *path, int value) {
  FILE *f = fopen(path, "w");
  fprintf(f, "%d\n", value);
  fclose(f);
}

const char *BRIGHTNESS = "/sys/class/backlight/intel_backlight/brightness";
const char *MAX_BRIGHTNESS = "/sys/class/backlight/intel_backlight/max_brightness";

int main(int argc, char **argv) {
  if (argc == 1) {
    usage(1);
  }
  if (argc != 2) {
    fprintf(stderr, "Expected one argument, got %d\n", argc - 1);
    usage(1);
  }
  int max = readFile(MAX_BRIGHTNESS);
  int cur = readFile(BRIGHTNESS);
  if (!strcmp(argv[1], "+")) {
    cur += (max / 20);
    if (cur > max) cur = max;
  } else if (!strcmp(argv[1], "-")) {
    cur -= (max / 20);
    if (cur < 0) cur = 0;
  } else {
    int value;
    if (!sscanf(argv[1], "%d", &value)) {
      fprintf(stderr, "Bad argument: %s\n", argv[1]);
      usage(1);
    }
    cur = value * max / 100;
  }
  writeFile(BRIGHTNESS, cur);
  return 0;
}
