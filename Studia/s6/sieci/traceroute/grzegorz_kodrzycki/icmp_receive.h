// Grzegorz Kodrzycki 332834
int receive_packet(int sock_fd, int num_packets, int waiting_time,
                   struct timeval *send_time, int TTL, int id, int seq, char* goal);