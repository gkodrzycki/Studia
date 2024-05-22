// 332834 Grzegorz Kodrzycki
#include "send.h"

struct Window Windows[WINDOW_SIZE];
int start = 0, end = 0, finished = 0;

int send_packet(int fd, int port, char *ip) {
  if (fd < 0) {
    fprintf(stderr, "Socket error\n");
    return 1;
  }

  struct sockaddr_in server_address;
  memset(&server_address, 0, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(port);
  inet_pton(AF_INET, ip, &server_address.sin_addr);

  int i = start;
  while (i != end) {
    if (Windows[i].ACK == 0 && Windows[i].size != 0) {
      char message[50];
      sprintf(message, "GET %d %d\n", Windows[i].start, Windows[i].size);
      ssize_t message_size = strlen(message);
      if (sendto(fd, message, message_size, 0,
                (struct sockaddr *)&server_address,
                sizeof(server_address)) != message_size) {
        fprintf(stderr, "Sendto error\n");
        return 1;
      }
    }
    i++;
    i %= WINDOW_SIZE;
  }
  // We need to send extra packet if we are at the end
  if (Windows[i].ACK == 0) {
    char message[50];
    sprintf(message, "GET %d %d\n", Windows[i].start, Windows[i].size);
    ssize_t message_size = strlen(message);
    if (sendto(fd, message, message_size, 0, (struct sockaddr *)&server_address,
               sizeof(server_address)) != message_size) {
      fprintf(stderr, "Sendto error\n");
      return 1;
    }
  }
  return 0;
}

void create_window(long long size) {
  long long s = 0;
  int ile = 0;
  while (s < size && ile < WINDOW_SIZE) {
    ile++;
    int packet_size = (size - s < SEGMENT_SIZE ? size - s : SEGMENT_SIZE);
    struct Window newWindow = {s, packet_size, "", 0};
    Windows[end++ % WINDOW_SIZE] = newWindow;
    s += packet_size;
  }
  end--;
}
