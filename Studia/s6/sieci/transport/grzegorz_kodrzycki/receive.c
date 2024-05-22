// 332834 Grzegorz Kodrzycki
#include "receive.h"
#include "send.h"
#include <stdio.h>
#include <string.h>

int recieve_packet(int fd, FILE *file, int *received, char *ip, int port,
                   int size, int no_packets) {
  fd_set descriptors;
  struct timeval tv;
  tv.tv_sec = SEC;
  tv.tv_usec = USEC;

  struct sockaddr_in sender;
  socklen_t sender_len = sizeof(sender);
  char buffer[IP_MAXPACKET + 1];

  while (1) {
    FD_ZERO(&descriptors);
    FD_SET(fd, &descriptors);
    int ready = select(fd + 1, &descriptors, NULL, NULL, &tv);

    if (ready == 0) {
      return 0;
    } else if (ready < 0) {
      fprintf(stderr, "Select error %s\n", strerror(errno));
      return 1;
    }

    ssize_t datagram_len = recvfrom(fd, buffer, IP_MAXPACKET, 0,
                                    (struct sockaddr *)&sender, &sender_len);
    if (datagram_len < 0) {
      fprintf(stderr, "recvfrom error: %s\n", strerror(errno));
    }
    char sender_ip_str[BUFLEN];
    inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str,
              sizeof(sender_ip_str));

    int my_ip[4], sender_ip[4];
    sscanf(ip, "%d.%d.%d.%d", &my_ip[0], &my_ip[1], &my_ip[2], &my_ip[3]);
    sscanf(sender_ip_str, "%d.%d.%d.%d", &sender_ip[0], &sender_ip[1],
           &sender_ip[2], &sender_ip[3]);

    int same = 1, received_start, received_size;
    char word[5];
    for (int i = 0; i < 4; i++) {
      if (my_ip[i] != sender_ip[i])
        same = 0;
    }

    // Getting start and size from packet
    char *newline_pos = strchr(buffer, '\n');
    size_t length = newline_pos - buffer;
    char line[length + 1];
    strncpy(line, buffer, length);
    line[length] = '\0';

    sscanf(line, "%s %d %d", word, &received_start, &received_size);

    if (same == 1 && ntohs(sender.sin_port) == port) {
      int i = (start + WINDOW_SIZE) % WINDOW_SIZE, cnt = 0;

      while (cnt < WINDOW_SIZE) {
        if (Windows[i].start == received_start &&
            Windows[i].size == received_size && Windows[i].ACK != 1) {
          memcpy(Windows[i].data,
                 buffer + (datagram_len - (ssize_t)received_size),
                 received_size);
          Windows[i].ACK = 1;
          (*received)++;
          printf("PROGRESS %2.2f%% \n", ((float)(*received) * 100 / no_packets));
          break;
        } else if (Windows[i].start == received_start &&
                   Windows[i].size == received_size && Windows[i].ACK != 0) {
          break;
        }
        i = (i + 1 + WINDOW_SIZE) % WINDOW_SIZE;
        cnt++;
      }

      int j, s;
      while (Windows[start].ACK == 1 && start != end) {
        write_file(Windows[start].data, Windows[start].size, file);

        j = (start - 1 + WINDOW_SIZE) % WINDOW_SIZE;
        s = Windows[j].start;
        int packet_size =
            (size - s - SEGMENT_SIZE < SEGMENT_SIZE ? size - s - SEGMENT_SIZE
                                                    : SEGMENT_SIZE);
        if (packet_size > 0 && finished == 0) {
          Windows[start].ACK = 0;
          Windows[start].start = Windows[j].start + Windows[j].size;
          Windows[start].size = packet_size;
          memcpy(Windows[i].data, "a", 1);
          end = (end + 1) % WINDOW_SIZE;
        } else {
          finished = 1;
        }

        start++;
        start %= WINDOW_SIZE;
      }
    }
  }
  return 0;
}

void write_file(char *buffor, int size, FILE *file) {
  fwrite(buffor, sizeof(char), size, file);
}