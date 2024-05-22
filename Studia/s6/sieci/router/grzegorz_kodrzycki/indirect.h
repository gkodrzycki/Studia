#ifndef indirect_h
#define indirect_h
//Grzegorz Kodrzycki 332834
//It handles indirect connections
#define BUFLEN 50

struct INDIRECT{
  char ip_str[BUFLEN];
  int distance;
  char connection[BUFLEN];
  struct INDIRECT *next_vector;
};

extern struct INDIRECT *indirect_head;

struct INDIRECT *createIndirect(const char *ip, int distance, const char *conn);

void insertIndirectVector(const char *ip, int distance, const char *conn);

bool findIndirectIp(const char *ip);

int findIndirectDistance(const char *ip);

void removeIndirect(const char *ip);

void updateIndirect(const char *ip, int new_distance, const char *conn);

bool isIndirectConnector(const char *ip, const char *conn);

int belongsToNetwork(const char *ip_str, const char *network_str);

#endif 