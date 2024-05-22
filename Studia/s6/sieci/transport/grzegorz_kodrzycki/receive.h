#ifndef RECEIVE
#define RECEIVE
#include <stdio.h>

int recieve_packet(int fd, FILE *file, int *received, char *ip, int port, int size, int no_packets);
void write_file(char *buffor, int size, FILE *file);
#endif