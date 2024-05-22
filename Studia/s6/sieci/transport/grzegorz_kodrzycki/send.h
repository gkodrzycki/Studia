#ifndef SEND
#define SEND

#include <arpa/inet.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <netinet/ip.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <time.h>
#include <unistd.h>

#define SEGMENT_SIZE 1000
#define SEC 0
#define USEC 400000
#define BUFLEN 50
#define WINDOW_SIZE 2500
#define NO_TRY 4

int send_packet(int fd, int port, char *ip);
void create_window(long long size);

struct Window {
  int start;
  int size;
  char data[SEGMENT_SIZE];
  int ACK;
};

extern struct Window Windows[WINDOW_SIZE];
extern int start, end, finished;
#endif