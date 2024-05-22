// Grzegorz Kodrzycki 332834
#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>

u_int16_t compute_icmp_checksum(const void *buff, int length) {
  const u_int16_t *ptr = buff;
  u_int32_t sum = 0;
  assert(length % 2 == 0);
  for (; length > 0; length -= 2)
    sum += *ptr++;
  sum = (sum >> 16U) + (sum & 0xffffU);
  return (u_int16_t)(~(sum + (sum >> 16U)));
}

int send_packet(int sock_fd, int seq, int id, struct sockaddr_in *dest_addr) {
  struct icmp icmp_packet;
  icmp_packet.icmp_type = ICMP_ECHO;
  icmp_packet.icmp_code = 0;
  icmp_packet.icmp_seq = seq;
  icmp_packet.icmp_id = id;

  icmp_packet.icmp_cksum = 0;
  icmp_packet.icmp_cksum =
      compute_icmp_checksum(&icmp_packet, sizeof(struct icmp));

  if (sendto(sock_fd, &icmp_packet, sizeof(struct icmp), 0,
             (struct sockaddr *)dest_addr, sizeof(*dest_addr)) <= 0)
    return -1;

  return 0;
}