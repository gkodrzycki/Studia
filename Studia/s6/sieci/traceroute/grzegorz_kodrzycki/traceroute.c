// Grzegorz Kodrzycki 332834
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#include "icmp_receive.h"
#include "icmp_send.h"

#define NO_PACKETS 3   // How many packets do we want to send
#define NO_TTL 30      // How many TTL
#define WAITING_TIME 1 // How long do we want to wait [s]

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Wrong number of args. Provide IP and IP only.\n");
    return 1;
  }

  struct sockaddr_in dest_addr;
  memset(&dest_addr, 0, sizeof(dest_addr));
  dest_addr.sin_family = AF_INET;

  if (!inet_pton(AF_INET, argv[1], &dest_addr.sin_addr)) {
    fprintf(stderr, "It doesnt look like correct IP address\n");
    return 1;
  }

  int sock_fd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
  if (sock_fd < 0) {
    fprintf(stderr, "socket error: %s\n", strerror(errno));
    return 1;
  }

  int seq = 1;
  int id = getpid();
  struct timeval send_time;

  for (int TTL = 1; TTL <= NO_TTL; TTL++) {
    for (int PACKET = 1; PACKET <= NO_PACKETS; PACKET++) {
      if (setsockopt(sock_fd, IPPROTO_IP, IP_TTL, &TTL, sizeof(TTL)) < 0)
        return -1;

      gettimeofday(&send_time, NULL);
      if (send_packet(sock_fd, seq, id, &dest_addr) < 0) {
        fprintf(stderr, "Error while sending package\n");
        close(sock_fd);
        return -1;
      }
      seq++;
    }

    int response = receive_packet(sock_fd, NO_PACKETS, WAITING_TIME, &send_time,
                                  TTL, id, seq, argv[1]);

    if (response < 0) {
      fprintf(stderr, "Error while receiving package\n");
      close(sock_fd);
      return -1;
    }

    if (response == 1)
      break;
  }
  close(sock_fd);
  return 0;
}