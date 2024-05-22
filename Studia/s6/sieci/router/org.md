# V1
ip link set enp0s3 name enp-rem1
ip link set enp0s8 name enp-rem4

ip link set up dev enp-rem1
ip link set up dev enp-rem4

ip addr add 192.168.1.1/24 dev enp-rem1    //V1 <-> V2
ip addr add 192.168.4.1/24 dev enp-rem4  //V1 <-> V4

2
192.168.1.1/24 distance 2
192.168.4.1/24 distance 4

# V2

ip link set enp0s3 name enp-rem1
ip link set enp0s8 name enp-rem2

ip link set up dev enp-rem1
ip link set up dev enp-rem2

ip addr add 192.168.1.2/24 dev enp-rem1 //V2 <-> V1
ip addr add 192.168.2.2/24 dev enp-rem2 //V2 <-> V3

2
192.168.1.2/24 distance 2
192.168.2.2/24 distance 3

# V3

ip link set enp0s3 name enp-rem2
ip link set enp0s8 name enp-rem3

ip link set up dev enp-rem2
ip link set up dev enp-rem3

ip addr add 192.168.2.3/24 dev enp-rem2 //V3 <-> V2
ip addr add 192.168.3.3/24 dev enp-rem3 //V3 <-> V4

2
192.168.2.3/24 distance 3
192.168.3.3/24 distance 2

# V4

ip link set enp0s3 name enp-rem3
ip link set enp0s8 name enp-rem4

ip link set up dev enp-rem3
ip link set up dev enp-rem4

ip addr add 192.168.3.4/24 dev enp-rem3 //V4 <-> V3
ip addr add 192.168.4.4/24 dev enp-rem4  // V4 <-> V1

2
192.168.3.4/24 distance 2
192.168.4.4/24 distance 4


 sudo mount -t vboxsf router folder/


                    192.168.2.0
                        3
                V2 ----------- V3
                |              |
  192.168.1.0  2|              |2  192.168.3.0
                |              X 
                V1 ---------X  V4
                        4
                  192.168.4.0
v1 2 4 5 7
v2 2 3 5 6
v3 2 3 5 9

# V1
2
192.168.1.1/24 distance 2
192.168.4.1/24 distance 4
# V2
2
192.168.1.2/24 distance 2
192.168.2.2/24 distance 3
# V3
2
192.168.2.3/24 distance 3
192.168.3.3/24 distance 2
# V4
2
192.168.3.4/24 distance 2
192.168.4.4/24 distance 4