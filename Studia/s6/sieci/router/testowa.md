# V1
ip link set enp0s3 name enp-rem1
ip link set enp0s8 name enp-rem4

ip link set up dev enp-rem1
ip link set up dev enp-rem4

ip addr add 172.16.1.13/16 dev enp-rem1    //V1 <-> V2
ip addr add 192.168.2.10/24 dev enp-rem4  //V1 <-> V4

2
172.16.1.13/16 distance 4
192.168.2.10/24 distance 2

# V2

ip link set enp0s3 name enp-rem1
ip link set enp0s8 name enp-rem2

ip link set up dev enp-rem1
ip link set up dev enp-rem2

ip addr add 172.16.1.14/24 dev enp-rem1 //V2 <-> V1
ip addr add 10.0.1.2/8 dev enp-rem2 //V2 <-> V3

2
172.16.1.14/24 distance 4
10.0.1.2/8 distance 3

# V3

ip link set enp0s3 name enp-rem2
ip link set enp0s8 name enp-rem3

ip link set up dev enp-rem2
ip link set up dev enp-rem3

ip addr add 10.0.1.1/8 dev enp-rem2 //V3 <-> V2
ip addr add 192.168.5.5/24 dev enp-rem3 //V3 <-> V4

2
10.0.1.1/8 distance 3
192.168.5.5/24 distance 2

# V4

ip link set enp0s3 name enp-rem3
ip link set enp0s8 name enp-rem4

ip link set up dev enp-rem3
ip link set up dev enp-rem4

ip addr add 192.168.5.43/24 dev enp-rem3 //V4 <-> V3
ip addr add 192.168.2.5/24 dev enp-rem4  // V4 <-> V1

2
192.168.5.43/24 distance 2
192.168.2.5/24 distance 2
