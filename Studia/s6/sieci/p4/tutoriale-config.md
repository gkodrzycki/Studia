# konfiguracja

## V1: 
ip link set enp0s3 name enp-rem1
ip link set enp0s8 name enp-rem4

ip link set up dev enp-rem1
ip link set up dev enp-rem4

ip addr add 192.168.1.1/24 dev enp-rem1
ip addr add 192.168.4.1/24 dev enp-rem4

## V2: 
ip link set enp0s3 name enp-rem1
ip link set enp0s8 name enp-rem2

ip link set up dev enp-rem1
ip link set up dev enp-rem2

ip addr add 192.168.1.2/24 dev enp-rem1
ip addr add 192.168.2.2/24 dev enp-rem2

## V3: 
ip link set enp0s3 name enp-rem2
ip link set enp0s8 name enp-rem3

ip link set up dev enp-rem2
ip link set up dev enp-rem3

ip addr add 192.168.2.3/24 dev enp-rem2
ip addr add 192.168.3.3/24 dev enp-rem3

## V4: 
ip link set enp0s3 name enp-rem3
ip link set enp0s8 name enp-rem4

ip link set up dev enp-rem3
ip link set up dev enp-rem4

ip addr add 192.168.3.4/24 dev enp-rem3
ip addr add 192.168.4.4/24 dev enp-rem4

## $V_{i}$:
ip route 

## V1
ping 192.168.4.4
ping 192.168.1.2

## V2
ping 192.168.1.1
ping 192.168.2.3

## V3
ping 192.168.2.2
ping 192.168.3.4

## V4
ping 192.168.3.3
ping 192.168.4.1

# tutorial 1

## V1
ip route add default via 192.168.1.2

## V2
ip route add default via 192.168.2.3

## V3
ip route add default via 192.168.3.4

## V4
ip route add default via 192.168.4.1

## $V_{i}$:
ip route 

## V1
ping 192.168.1.2
ping 192.168.2.2
ping 192.168.2.3
ping 192.168.3.3
ping 192.168.3.4
ping 192.168.4.4

## V2
ping 192.168.1.1
ping 192.168.4.1
ping 192.168.2.3
ping 192.168.3.3
ping 192.168.3.4
ping 192.168.4.4

## V3
ping 192.168.1.1
ping 192.168.4.1
ping 192.168.1.2
ping 192.168.2.2
ping 192.168.3.4
ping 192.168.4.4

## V4
ping 192.168.1.1
ping 192.168.4.1
ping 192.168.1.2
ping 192.168.2.2
ping 192.168.2.3
ping 192.168.3.3

## V1
traceroute -n 192.168.1.2
traceroute -n 192.168.2.2
traceroute -n 192.168.2.3
traceroute -n 192.168.3.3
traceroute -n 192.168.3.4
traceroute -n 192.168.4.4

## V1
ip route del default via 192.168.1.2
ip route

## V2
ip route del default via 192.168.2.3
ip route

## V3
ip route del default via 192.168.3.4
ip route

## V4
ip route del default via 192.168.4.1
ip route

# tutorial 2

## $V_i$
nano /etc/frr/daemons 
ospfd=no -> ospfd=yes

systemctl start frr
systemctl status frr
vtysh

## $V_{i}.vtysh$:
show ip route
configure terminal
router ospf

network 192.168.x.0/24 area 0  (w skórcie te sieci które wyszły wyżej)
end
show running-config
copy running-config startup-config

show ip route
quit

## V1
ping 192.168.1.2
ping 192.168.2.2
ping 192.168.2.3
ping 192.168.3.3
ping 192.168.3.4
ping 192.168.4.4

traceroute -n 192.168.1.2
traceroute -n 192.168.2.2
traceroute -n 192.168.2.3
traceroute -n 192.168.3.3
traceroute -n 192.168.3.4
traceroute -n 192.168.4.4

## V2
ping 192.168.1.1
ping 192.168.4.1
ping 192.168.2.3
ping 192.168.3.3
ping 192.168.3.4
ping 192.168.4.4

traceroute -n 192.168.1.1
traceroute -n 192.168.4.1
traceroute -n 192.168.2.3
traceroute -n 192.168.3.3
traceroute -n 192.168.3.4
traceroute -n 192.168.4.4

## V3
ping 192.168.1.1
ping 192.168.4.1
ping 192.168.1.2
ping 192.168.2.2
ping 192.168.3.4
ping 192.168.4.4

traceroute -n 192.168.1.1
traceroute -n 192.168.4.1
traceroute -n 192.168.1.2
traceroute -n 192.168.2.2
traceroute -n 192.168.3.4
traceroute -n 192.168.4.4

## V4
ping 192.168.1.1
ping 192.168.4.1
ping 192.168.1.2
ping 192.168.2.2
ping 192.168.2.3
ping 192.168.3.3

traceroute -n 192.168.1.1
traceroute -n 192.168.4.1
traceroute -n 192.168.1.2
traceroute -n 192.168.2.2
traceroute -n 192.168.2.3
traceroute -n 192.168.3.3

## $V_i$

ip link set enp0s9 name enp-all
ip link set up dev enp-all
ip addr add 172.16.16.i/16 dev enp-all
vtysh

## $V_{i}.vtysh$:
configure terminal
router ospf
network 172.16.0.0/16 area 0
end
show running-config
copy running-config startup-config

show ip route

Wsm dalej się już nie bawiłem w zmiany długości itd
