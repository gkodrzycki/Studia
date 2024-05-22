#ifndef direct_h
#define direct_h
//Grzegorz Kodrzycki 332834
//It handles direct connections
#define BUFLEN 50
#define INF 30
#define MAX_MISSED 4

struct DIRECT {
  char ip_str[BUFLEN];
  int distance;
  int real_dist;
  int missed_rounds;  
  int recv;
  struct DIRECT *next_vector;
};

struct MYIPS {
  char my_ip[BUFLEN];
  struct MYIPS *next;
};

extern struct MYIPS *ips_head;

extern struct DIRECT *direct_head;

struct DIRECT *createDirect(const char *ip, int distance);

void insertDirectVector(const char *ip, int distance);

struct MYIPS *createIP(const char *ip); 

bool findDirectIp(const char *ip);

int findDirectDistance(const char *ip);

int findRealDistance(const char *ip);

char *findNetwork(const char *ip);

void updateDirect(const char *ip, int new_distance);

void updateIndirectVia(int new_distance, const char *conn);

#endif 