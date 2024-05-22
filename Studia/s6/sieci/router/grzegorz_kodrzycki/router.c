// Grzegorz Kodrzycki 332834
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/ip.h>
#include <stdbool.h>
#include <sys/select.h>
#include <unistd.h>

#include <time.h>

#include "direct.h"
#include "indirect.h"

#define ROUND 15 // time of round in s

int n, distance;
char ip_str[BUFLEN];

void initialInput() {
  scanf("%d", &n);
  for (int i = 0; i < n; i++) {
    fscanf(stdin, "%s distance %d", ip_str, &distance);
    insertDirectVector(ip_str, distance);
  }
}

void printTable() {
  struct DIRECT *temp = direct_head;
  while (temp != NULL) {
    if (temp->distance < INF) {
      // If we have also indirect connection it may be shorter, need to check
      // for that
      bool indir = findIndirectIp(temp->ip_str);
      if (!indir ||
          (indir && findIndirectDistance(temp->ip_str) >= temp->distance))
        printf("%s distance %d connected directly\n", temp->ip_str,
               temp->distance);
    } else {
      bool indir = findIndirectIp(temp->ip_str);
      if (!indir ||
          (indir && findIndirectDistance(temp->ip_str) >= temp->distance))
        printf("%s unreachable connected directly\n", temp->ip_str);
    }
    temp = temp->next_vector;
  }

  struct INDIRECT *temp2 = indirect_head;
  while (temp2 != NULL) {
    // If we have also direct connection and its shorter/equal we printed it
    // before, so there is no need to print again
    bool dir = findDirectIp(temp2->ip_str);
    if (!dir || (dir && findDirectDistance(temp2->ip_str) > temp2->distance))
      printf("%s distance %d via %s\n", temp2->ip_str, temp2->distance,
             temp2->connection);
    temp2 = temp2->next_vector;
  }

  printf("\n");
  fflush(stdout);
}

char *broadcast(const char *ip) {
  uint32_t ip_address[5], net_address, mask;
  sscanf(ip, "%u.%u.%u.%u/%u", &ip_address[0], &ip_address[1], &ip_address[2],
         &ip_address[3], &ip_address[4]);

  // Creating one 32 bit int without . and creating 32 bit mask
  net_address = (ip_address[0] << 24) | (ip_address[1] << 16) |
                (ip_address[2] << 8) | ip_address[3];
  mask = (0xFFFFFFFF >> ip_address[4]);

  net_address = net_address | mask;

  // Saving fixed address
  uint8_t octets[4];
  octets[0] = (net_address >> 24) & 0xFF;
  octets[1] = (net_address >> 16) & 0xFF;
  octets[2] = (net_address >> 8) & 0xFF;
  octets[3] = net_address & 0xFF;

  char *broadcastNetwork = (char *)malloc(BUFLEN);
  if (broadcastNetwork == NULL) {
    fprintf(stderr, "Memory allocation error\n");
    return NULL;
  }
  sprintf(broadcastNetwork, "%u.%u.%u.%u", octets[0], octets[1], octets[2],
          octets[3]);

  return broadcastNetwork;
}

void sendTable(int fd) {
  if (fd < 0) {
    fprintf(stderr, "socket error: %s\n", strerror(errno));
    return;
  }

  struct sockaddr_in server_address;
  struct DIRECT *iter = direct_head;

  while (iter != NULL) {
    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(54321);

    char *broad = broadcast(iter->ip_str);
    inet_pton(AF_INET, broad, &server_address.sin_addr);
    free(broad);

    struct DIRECT *iter2 = direct_head;
    while (iter2 != NULL) {
      uint32_t ip_address[5];
      sscanf(iter2->ip_str, "%u.%u.%u.%u/%u", &ip_address[0], &ip_address[1],
             &ip_address[2], &ip_address[3], &ip_address[4]);

      char datagram[9];
      datagram[0] = ip_address[0] & 0xFF;
      datagram[1] = ip_address[1] & 0xFF;
      datagram[2] = ip_address[2] & 0xFF;
      datagram[3] = ip_address[3] & 0xFF;
      datagram[4] = ip_address[4] & 0xFF;

      // If we are sending to our direct connection send real distance, not
      // matter what it is now
      if (strcmp(iter->ip_str, iter2->ip_str) == 0) {
        datagram[5] = (iter2->real_dist >> 24) & 0xFF;
        datagram[6] = (iter2->real_dist >> 16) & 0xFF;
        datagram[7] = (iter2->real_dist >> 8) & 0xFF;
        datagram[8] = iter2->real_dist & 0xFF;
      } else {
        datagram[5] = (iter2->distance >> 24) & 0xFF;
        datagram[6] = (iter2->distance >> 16) & 0xFF;
        datagram[7] = (iter2->distance >> 8) & 0xFF;
        datagram[8] = iter2->distance & 0xFF;
      }

      ssize_t bytes_sent =
          sendto(fd, datagram, sizeof(datagram), 0,
                 (struct sockaddr *)&server_address, sizeof(server_address));
      // sendto_failed
      if (bytes_sent != sizeof(datagram)) {
        updateDirect(iter->ip_str, 2 * INF);
        updateIndirectVia(2 * INF, iter->ip_str);

      } else if (bytes_sent == sizeof(datagram) && iter->missed_rounds > 0) {
        updateDirect(iter->ip_str, iter->real_dist);
      }
      iter2 = iter2->next_vector;
    }

    struct INDIRECT *iter3 = indirect_head;
    while (iter3 != NULL) {
      uint32_t ip_address[5];
      sscanf(iter3->ip_str, "%u.%u.%u.%u/%u", &ip_address[0], &ip_address[1],
             &ip_address[2], &ip_address[3], &ip_address[4]);

      char datagram[9];
      datagram[0] = ip_address[0];
      datagram[1] = ip_address[1];
      datagram[2] = ip_address[2];
      datagram[3] = ip_address[3];
      datagram[4] = ip_address[4];

      int ip[5];
      sscanf(iter->ip_str, "%d.%d.%d.%d/%d", &ip[0], &ip[1], &ip[2], &ip[3],
             &ip[4]);
      // If we go via this node we dont want to say we can do better
      if (belongsToNetwork(iter3->connection, iter->ip_str)) {
        datagram[5] = (INF >> 24) & 0xFF;
        datagram[6] = (INF >> 16) & 0xFF;
        datagram[7] = (INF >> 8) & 0xFF;
        datagram[8] = INF & 0xFF;
      } else {
        datagram[5] = (iter3->distance >> 24) & 0xFF;
        datagram[6] = (iter3->distance >> 16) & 0xFF;
        datagram[7] = (iter3->distance >> 8) & 0xFF;
        datagram[8] = iter3->distance & 0xFF;
      }

      ssize_t bytes_sent =
          sendto(fd, datagram, sizeof(datagram), 0,
                 (struct sockaddr *)&server_address, sizeof(server_address));
      // Sendto failed
      if (bytes_sent != sizeof(datagram) && iter->missed_rounds <= 0) {
        updateDirect(iter->ip_str, 2 * INF);
        updateIndirectVia(2 * INF, iter->ip_str);
      } else if (bytes_sent == sizeof(datagram) && iter->missed_rounds > 0) {
        updateDirect(iter->ip_str, iter->real_dist);
      }
      iter3 = iter3->next_vector;
    }
    iter = iter->next_vector;
  }
}

bool myIP(const char *ip) {
  struct MYIPS *temp2 = ips_head;

  while (temp2 != NULL) {
    if (strcmp(temp2->my_ip, ip) == 0) {
      return true;
    }
    temp2 = temp2->next;
  }
  return false;
}

void receiveTable(int fd) {
  fd_set descriptors;
  struct timeval tv;
  tv.tv_sec = ROUND;
  tv.tv_usec = 0;

  struct sockaddr_in sender;
  socklen_t sender_len = sizeof(sender);
  u_int8_t buffer[IP_MAXPACKET + 1];

  uint8_t recv_mask;
  uint32_t recv_dist;
  char recv_ip_str[BUFLEN];

  while (1) {
    FD_ZERO(&descriptors);
    FD_SET(fd, &descriptors);
    int ready = select(fd + 1, &descriptors, NULL, NULL, &tv);
    if (ready < 0) {
      fprintf(stderr, "select error: %s\n", strerror(errno));
      break;
    }
    if (ready == 0) {
      break;
    }

    ssize_t datagram_len = recvfrom(fd, buffer, IP_MAXPACKET, 0,
                                    (struct sockaddr *)&sender, &sender_len);
    if (datagram_len < 0) {
      fprintf(stderr, "recvfrom error: %s\n", strerror(errno));
      break;
    }

    char sender_ip_str[BUFLEN];
    inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str,
              sizeof(sender_ip_str));

    struct DIRECT *temp = direct_head;
    while (temp != NULL) {
      if (belongsToNetwork(sender_ip_str, temp->ip_str) &&
          !myIP(sender_ip_str)) {
        temp->recv = 1;
        char datagram[9];
        memcpy(datagram, buffer, 9);
        recv_mask = datagram[4];

        recv_dist = datagram[8] | ((datagram[7] << 8) & 0xFF00) |
                    ((datagram[6] << 16) & 0xFF0000) |
                    ((datagram[5] << 24) & 0xFF000000);

        sprintf(recv_ip_str, "%u.%u.%u.%u/%u", (u_int32_t)datagram[0] & 0xFF,
                (u_int32_t)datagram[1] & 0xFF, (u_int32_t)datagram[2] & 0xFF,
                (u_int32_t)datagram[3] & 0xFF, recv_mask);

        // If we dont have this network already
        int new_dist = findDirectDistance(temp->ip_str) + recv_dist;
        if (!findIndirectIp(recv_ip_str) && !findDirectIp(recv_ip_str)) {
          insertIndirectVector(recv_ip_str, new_dist, sender_ip_str);
        }
        // If we are connected indirectly but it is direct network and we can
        // better this way
        else if (findIndirectIp(recv_ip_str) && findDirectIp(recv_ip_str) &&
                 findIndirectDistance(recv_ip_str) >
                     findRealDistance(recv_ip_str) + (int)recv_dist) {
          updateDirect(recv_ip_str, recv_dist);
          removeIndirect(recv_ip_str);
        }
        // If already in indirect but we can do better
        else if (findIndirectIp(recv_ip_str) &&
                 (findIndirectDistance(recv_ip_str) > new_dist)) {
          updateIndirect(recv_ip_str, new_dist, sender_ip_str);
        }
        // If already in direct but we can do better by indirect
        else if (findDirectIp(recv_ip_str) &&
                 (findDirectDistance(recv_ip_str) > new_dist)) {
          if (!findIndirectIp(recv_ip_str))
            insertIndirectVector(recv_ip_str, new_dist, sender_ip_str);
          else
            updateIndirect(recv_ip_str, new_dist, sender_ip_str);
        }
        // If we are connected indirectly and our connector says it's longer now
        else if (findIndirectIp(recv_ip_str) &&
                 isIndirectConnector(recv_ip_str, sender_ip_str)) {
          updateIndirect(recv_ip_str, new_dist, sender_ip_str);
        }
      }
      temp = temp->next_vector;
    }
  }
  struct DIRECT *iter = direct_head;
  while (iter != NULL) {
    if (iter->recv != 1) {
      iter->missed_rounds -= 1;
    } else {
      iter->missed_rounds = MAX_MISSED;
    }
    iter->recv = 0;
    iter = iter->next_vector;
  }
}

void cleanTable() {
  struct DIRECT *temp = direct_head;
  while (temp != NULL) {
    // If we waited to long for it but its not sendto problem so we want to
    // remove
    if (temp->missed_rounds <= 0 && temp->distance != 2 * INF) {
      struct INDIRECT *helper = indirect_head;
      while (helper != NULL) {
        if (belongsToNetwork(helper->connection, temp->ip_str)) {
          updateIndirectVia(INF, temp->ip_str);
        }
        helper = helper->next_vector;
      }
    }

    temp = temp->next_vector;
  }

  struct INDIRECT *temp2 = indirect_head;
  while (temp2 != NULL) {
    if (temp2->distance >= INF)
      removeIndirect(temp2->ip_str);

    temp2 = temp2->next_vector;
  }
}

int main() {
  initialInput();
  printTable();

  int send_fd, receive_fd, broadcast_on = 1;
  send_fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (send_fd < 0) {
    fprintf(stderr, "socket error: %s\n", strerror(errno));
  }

  if (setsockopt(send_fd, SOL_SOCKET, SO_BROADCAST, &broadcast_on,
                 sizeof(broadcast_on)) == -1) {
    fprintf(stderr, "broadcast error Error: %s\n", strerror(errno));
  }

  receive_fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (receive_fd < 0) {
    fprintf(stderr, "socket error: %s\n", strerror(errno));
  }

  struct sockaddr_in server_address;
  memset(&server_address, 0, sizeof(server_address));
  server_address.sin_family = AF_INET;
  server_address.sin_port = htons(54321);
  server_address.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(receive_fd, (struct sockaddr *)&server_address,
           sizeof(server_address)) < 0) {
    fprintf(stderr, "bind error: %s\n", strerror(errno));
  }

  while (1) {
    sendTable(send_fd);
    printTable();
    receiveTable(receive_fd);
    sendTable(send_fd);
    printTable();
    cleanTable();
    printTable();
  }

  close(send_fd);
  close(receive_fd);
  return 0;
}
