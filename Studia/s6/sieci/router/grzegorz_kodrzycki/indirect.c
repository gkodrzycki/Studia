// Grzegorz Kodrzycki 332834
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

struct INDIRECT *indirect_head = NULL;

struct INDIRECT *createIndirect(const char *ip, int distance,
                                const char *conn) {
  struct INDIRECT *newInVector =
      (struct INDIRECT *)malloc(sizeof(struct INDIRECT));
  if (newInVector == NULL) {
    fprintf(stderr, "Memory allocation failed in INDIRECT\n");
    exit(1);
  }

  // Recreate actual address using mask
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

  sprintf(newInVector->ip_str, "%u.%u.%u.%u/%u", octets[0], octets[1],
          octets[2], octets[3], ip_address[4]);

  newInVector->distance = distance;
  strncpy(newInVector->connection, conn, BUFLEN);

  newInVector->next_vector = NULL;
  return newInVector;
}

void insertIndirectVector(const char *ip, int distance, const char *conn) {
  struct INDIRECT *newInVector = createIndirect(ip, distance, conn);
  if (indirect_head == NULL) {
    indirect_head = newInVector;
    return;
  }
  struct INDIRECT *temp2 = indirect_head;

  while (temp2->next_vector != NULL) {
    temp2 = temp2->next_vector;
  }

  temp2->next_vector = newInVector;
}

bool findIndirectIp(const char *ip) {
  struct INDIRECT *temp2 = indirect_head;

  while (temp2 != NULL) {
    if (strcmp(temp2->ip_str, ip) == 0) {
      return true;
    }
    temp2 = temp2->next_vector;
  }

  return false;
}

int findIndirectDistance(const char *ip) {
  struct INDIRECT *temp2 = indirect_head;

  while (temp2 != NULL) {
    if (strcmp(temp2->ip_str, ip) == 0) {
      return temp2->distance;
    }
    temp2 = temp2->next_vector;
  }

  return INF;
}

bool isIndirectConnector(const char *ip, const char *conn) {
  struct INDIRECT *current = indirect_head;

  while (current != NULL) {
    if ((strcmp(current->ip_str, ip) == 0) && (strcmp(current->connection, conn) == 0)) {
      return true;
    }
    current = current->next_vector;
  }
  return false;
}

void removeIndirect(const char *ip) {
  struct INDIRECT *current = indirect_head;
  struct INDIRECT *prev = NULL;

  if (current != NULL && strcmp(current->ip_str, ip) == 0) {
    indirect_head = current->next_vector;
    free(current);
    return;
  }

  while (current != NULL && strcmp(current->ip_str, ip) != 0) {
    prev = current;
    current = current->next_vector;
  }

  if (current == NULL) {
    return;
  }

  prev->next_vector = current->next_vector;
  free(current);
}

void updateIndirect(const char *ip, int new_distance, const char *conn) {
  struct INDIRECT *current = indirect_head;

  while (current != NULL) {
    if (strcmp(current->ip_str, ip) == 0) {
      current->distance = new_distance;
      strncpy(current->connection, conn, BUFLEN);
    }
    current = current->next_vector;
  }
}

int belongsToNetwork(const char *ip_str, const char *network_str) {
  int ip[4], net[5];
  sscanf(ip_str, "%d.%d.%d.%d", &ip[0], &ip[1], &ip[2], &ip[3]);
  sscanf(network_str, "%d.%d.%d.%d/%d", &net[0], &net[1], &net[2], &net[3],
         &net[4]);

  char testing[BUFLEN];
  sprintf(testing, "%d.%d.%d.%d/%d", ip[0], ip[1], ip[2], ip[3], net[4]);

  char *network = findNetwork(testing);
  return (strcmp(network, network_str) == 0);
}

void updateIndirectVia(int new_distance, const char *conn) {
  struct INDIRECT *current = indirect_head;

  while (current != NULL) {
    if (belongsToNetwork(current->connection, conn)) {
      current->distance = new_distance;
    }
    current = current->next_vector;
  }
}
