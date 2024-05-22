// Grzegorz Kodrzycki 332834
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/ip.h>
#include <sys/select.h>
#include <unistd.h>

#include "direct.h"

struct DIRECT *direct_head = NULL;
struct MYIPS *ips_head = NULL;

char *findNetwork(const char *ip) {
  uint32_t ip_address[5], net_address, mask;
  sscanf(ip, "%u.%u.%u.%u/%u", &ip_address[0], &ip_address[1], &ip_address[2],
         &ip_address[3], &ip_address[4]);

  // Creating one 32 bit int without . and creating 32 bit mask
  net_address = (ip_address[0] << 24) | (ip_address[1] << 16) |
                (ip_address[2] << 8) | ip_address[3];
  mask = ~((1 << (32 - ip_address[4])) - 1);

  net_address = net_address & mask;

  // Saving fixed address
  uint8_t octets[4];
  octets[0] = (net_address >> 24) & 0xFF;
  octets[1] = (net_address >> 16) & 0xFF;
  octets[2] = (net_address >> 8) & 0xFF;
  octets[3] = net_address & 0xFF;

  char *network = (char *)malloc(BUFLEN);
  if (network == NULL) {
    fprintf(stderr, "Memory allocation error\n");
    return NULL;
  }
  sprintf(network, "%u.%u.%u.%u/%u", octets[0], octets[1], octets[2], octets[3],
          ip_address[4]);

  return network;
}

struct DIRECT *createDirect(const char *ip, int distance) {
  struct DIRECT *newVector = (struct DIRECT *)malloc(sizeof(struct DIRECT));
  if (newVector == NULL) {
    fprintf(stderr, "Memory allocation failed in DIRECT\n");
    exit(1);
  }

  char *network = findNetwork(ip);

  snprintf(newVector->ip_str, BUFLEN, "%s", network);
  free(network);

  newVector->distance = distance;
  newVector->real_dist = distance;
  newVector->missed_rounds = MAX_MISSED;
  newVector->next_vector = NULL;
  newVector->recv = 0;
  return newVector;
}

struct MYIPS *createIP(const char *ip) {
  struct MYIPS *newVector = (struct MYIPS *)malloc(sizeof(struct MYIPS));
  if (newVector == NULL) {
    fprintf(stderr, "Memory allocation failed in MYIPS\n");
    exit(1);
  }

  uint32_t ip_address[5], net_address;
  sscanf(ip, "%u.%u.%u.%u/%u", &ip_address[0], &ip_address[1], &ip_address[2],
         &ip_address[3], &ip_address[4]);

  net_address = (ip_address[0] << 24) | (ip_address[1] << 16) |
                (ip_address[2] << 8) | ip_address[3];

  uint8_t octets[4];
  octets[0] = (net_address >> 24) & 0xFF;
  octets[1] = (net_address >> 16) & 0xFF;
  octets[2] = (net_address >> 8) & 0xFF;
  octets[3] = net_address & 0xFF;

  sprintf(newVector->my_ip, "%u.%u.%u.%u", octets[0], octets[1], octets[2],
          octets[3]);

  newVector->next = NULL;
  return newVector;
}

void insertDirectVector(const char *ip, int distance) {
  struct DIRECT *newVector = createDirect(ip, distance);
  struct MYIPS *newIP = createIP(ip);
  if (direct_head == NULL) {
    direct_head = newVector;
  }

  if (ips_head == NULL) {
    ips_head = newIP;
    return;
  }

  struct DIRECT *temp = direct_head;
  while (temp->next_vector != NULL) {
    temp = temp->next_vector;
  }

  temp->next_vector = newVector;

  struct MYIPS *temp2 = ips_head;

  while (temp2->next != NULL) {
    temp2 = temp2->next;
  }

  temp2->next = newIP;
}

bool findDirectIp(const char *ip) {
  struct DIRECT *temp = direct_head;

  char *network = findNetwork(ip);

  while (temp != NULL) {
    if (strcmp(temp->ip_str, network) == 0) {
      return true;
    }
    temp = temp->next_vector;
  }

  return false;
}

int findDirectDistance(const char *ip) {
  struct DIRECT *temp = direct_head;

  char *network = findNetwork(ip);

  while (temp != NULL) {
    if (strcmp(temp->ip_str, network) == 0) {
      return temp->distance;
    }
    temp = temp->next_vector;
  }

  return INF;
}

int findRealDistance(const char *ip) {
  struct DIRECT *temp = direct_head;

  char *network = findNetwork(ip);

  while (temp != NULL) {
    if (strcmp(temp->ip_str, network) == 0) {
      return temp->real_dist;
    }
    temp = temp->next_vector;
  }

  return INF;
}

void updateDirect(const char *ip, int new_distance) {
  struct DIRECT *current = direct_head;

  char *network = findNetwork(ip);

  while (current != NULL) {
    if (strcmp(current->ip_str, network) == 0) {
      current->distance = new_distance;
    }
    current = current->next_vector;
  }
  free(network);
}
