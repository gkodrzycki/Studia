// 332834 Grzegorz Kodrzycki
#include "receive.h"
#include "send.h"

int main(int argc, char *argv[]) {
  if (argc != 5) {
    fprintf(stderr, "INVALID NUMBER OF ARGUMENTS\n Try ./transport IP port "
                    "file_name size\n");
    return 1;
  }

  char *ip = argv[1];
  int port = atoi(argv[2]);
  char *file_name = argv[3];
  long long size = atoll(argv[4]);

  if (size < 0) {
    fprintf(stderr, "Packet size can't be less than 0\n");
    return 1;
  }

  if (port < 0) {
    fprintf(stderr, "It doesnt look like correct port\n");
    return 1;
  }

  struct sockaddr_in dest_addr;
  memset(&dest_addr, 0, sizeof(dest_addr));
  dest_addr.sin_family = AF_INET;

  if (!inet_pton(AF_INET, ip, &dest_addr.sin_addr)) {
    fprintf(stderr, "It doesnt look like correct IP address\n");
    return 1;
  }

  FILE *output_file = fopen(file_name, "w");

  int sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (sock_fd < 0) {
    fprintf(stderr, "Socket error\n");
    return 1;
  }

  int received = 0, no_packets = ceil((double)size / 1000);
  // Waiting for all packets
  create_window(size);
  while (received < no_packets) {
    // We want to send default SEGMENT_SIZE, but if last packet need to be
    // smaller send less
    for (int x = 0; x < NO_TRY; x++)
      send_packet(sock_fd, port, ip);
    recieve_packet(sock_fd, output_file, &received, ip, port, size, no_packets);
  }

  while (Windows[start].ACK == 1 && start != end) {
    write_file(Windows[start].data, Windows[start].size, output_file);
    start++;
    start %= WINDOW_SIZE;
  }

  write_file(Windows[start].data, Windows[start].size, output_file);

  fclose(output_file);
  close(sock_fd);
  return 0;
}
