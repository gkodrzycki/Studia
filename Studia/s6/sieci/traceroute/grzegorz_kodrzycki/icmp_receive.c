// Grzegorz Kodrzycki 332834
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

void print_info(struct timeval *times, int num_packets, char senders[][20],
                int recv_packets, int TTL) {
  printf("%d. ", TTL);

  if (recv_packets == 0)
    printf("*\n");
  else {
    int printed_senders[num_packets];
    memset(printed_senders, 0, sizeof(printed_senders));

    for (int i = 0; i < num_packets; i++) {
      int printed = 0;
      for (int j = 0; j < i; j++) {
        if (strcmp(senders[i], senders[printed_senders[j]]) == 0) {
          printed = 1;
          break;
        }
      }

      if (!printed) {
        printf("%s ", senders[i]);
        printed_senders[i] = i;
      }
    }

    double avg = 0.0;

    if (recv_packets != num_packets)
      printf("???\n");
    else {
      for (int i = 0; i < num_packets; i++) {
        avg += (times[i].tv_sec * 1000 + times[i].tv_usec) / 1000;
      }
      printf("%.0fms\n", avg / num_packets);
    }
  }
}

int finished(char senders_ip[][20], char *goal) {
  return !(strcmp(senders_ip[0], senders_ip[1]) ||
           strcmp(senders_ip[1], senders_ip[2]) || strcmp(senders_ip[0], goal));
}

int receive_packet(int sock_fd, int num_packets, int waiting_time,
                   struct timeval *send_time, int TTL, int id, int seq,
                   char *goal) {
  int recv_packets = 0;
  char sender_ip_str[num_packets][20];
  struct timeval package_time[num_packets];

  struct pollfd fds;
  fds.fd = sock_fd;
  fds.events = POLLIN;

  while (recv_packets < num_packets) {
    int poll_retval = poll(&fds, 1, waiting_time * 1000);
    if (poll_retval <= 0) {
      break;
    }

    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);
    u_int8_t buffer[IP_MAXPACKET];

    ssize_t packet_len = recvfrom(sock_fd, buffer, IP_MAXPACKET, 0,
                                  (struct sockaddr *)&sender, &sender_len);
    if (packet_len < 0) {
      return -1;
    }

    if (inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str[recv_packets],
                  sizeof(sender_ip_str[recv_packets])) == NULL)
      return -1;

    int iphdrlen, type;
    struct ip *ip = (struct ip *)buffer;
    struct icmp *icmp;

    iphdrlen = ip->ip_hl << 2;
    icmp = (struct icmp *)(buffer + iphdrlen);

    type = icmp->icmp_type;

    // If packet is ICMP_TIME_EXCEEDED we need to move it because of TE message
    if (type == ICMP_TIME_EXCEEDED) {
      struct ip *ip = (void *)icmp + 8;
      icmp = (void *)ip + (ip->ip_hl << 2);
    }

    if (icmp->icmp_id == id &&
        (icmp->icmp_seq >= seq - 3 && icmp->icmp_seq <= seq - 1)) {
      struct timeval diff;
      gettimeofday(&diff, NULL);
      timersub(&diff, send_time, &package_time[recv_packets]);
      recv_packets++;

      if (recv_packets == num_packets && finished(sender_ip_str, goal)) {
        print_info(package_time, num_packets, sender_ip_str, recv_packets, TTL);
        return 1;
      }
    }
  }

  print_info(package_time, num_packets, sender_ip_str, recv_packets, TTL);

  return 0;
}
