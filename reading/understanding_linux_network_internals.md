# [Understanding Linux Network Internals][homepage], by Christian Benvenuti, O'Reilly (2006)

topics that do not be covered - IPv6, IP Security protocol, IP multicast and IP
 multicast routing, L4 protocols, [Traffic Control][traffic_control],
 [Netfilter][netfilter], Network filesystems, Virtual devices,
 DECnet/IPX/AppleTalk/..., [IP virtual server][ip_virtual_server], SNMP, [Frame
 Diverter][frame_diverter], socket (`struct sock`)<br>
modular [Click router][click_router]

[homepage]: http://shop.oreilly.com/product/9780596002558.do
[traffic_control]: http://lartc.org/
[netfilter]: http://www.netfilter.org/
[ip_virtual_server]: http://www.linuxvirtualserver.org/
[frame_diverter]: https://sourceforge.net/projects/diverter/
[click_router]: http://www.read.cs.ucla.edu/click/

## Part I. General Background

### 1. Introduction

about read-copy update (RCU) [[1]][rcu_1] [[2]][rcu_2]<br>
iputils - ping, arping, rdisc, ...<br>
net-tools - ifconfig, route, netstat, arp, ipmaddr, iptunnel, ether-wake, netplugd, ...<br>
IPROUTE2 - ip, ...<br>
[Policy Routing Book and IPROUTE2 Documentation][policyrouting], [The Linux Network Development List Archives][linux_network_mailing_list] (old)<br>
please use cscope or tags file.

[rcu_1]: http://www.linuxjournal.com/article/6993
[rcu_2]: http://www2.rdrop.com/users/paulmck/rclock/
[policyrouting]: http://www.policyrouting.org/
[linux_network_mailing_list]: http://oss.sgi.com/projects/netdev/archive/

### 2. Critical Data Structures

`struct sk_buff` in `src/linux/skbuff.h`
* `destructor` - `skb_set_owner_r/w()` (in `include/net/sock.h`) sets this field to `sock_r/wfree()` (in `net/core/sock.c`)
* `dev` - `vortex_rx()` (in `drivers/net/ethernet/3com/3c59x.c`) calls `eth_type_trans()` (that sets `dev` field and returns `protocol` field, in `net/ethernet/eth.c`) and `netif_rx()` (pass the packet to the higher layer, in `net/core/dev.c`)
* `cb` - `struct tcp_skb_cb` in `include/net/tcp.h` (that defines constants such as `TCP_SYN_RETRIES`), TCP state constants in `include/net/tcp_states.h`
* `pkt_type` - in `include/uapi/linux/if_packet.h`
* `protocol` - in `include/uapi/linux/if_ether.h`
`struct net_device` in `include/linux/netdevice.h`
* `dma` - in `arch/x86/include/asm/dma.h`
* `*flags` - `enum net_device_flags` in `include/uapi/linux/if.h`
* `features` - in `include/linux/netdev_features.h`
* `mtu` - [why IEEE denied jumbo frames][jumbo_frames_denied]
* `type` - in `include/uapi/linux/if_arp.h`
* `hard_header_len` - `ETH_HLEN` in `include/uapi/linux/if_ether.h`
* `stats` - `struct net_device_stats` in the same file
* `state` - `enum netdev_state_t` in the same file
* `ip_ptr` - `struct in_device` in `include/linux/inetdevice.h`
* `tx_queue_len` - `/sys/class/net/eth0/tx_queue_len`
* `netdev_ops` - `struct net_device_ops` in the same file, see ndo_* function pointers
* `ethtool_ops` - `struct ethtool_ops` in `include/linux/ethtool.h`
kernel configuration options `Kconfig` in each directory<br>
[VLANs on Linux][linux_vlan]

[jumbo_frames_denied]: http://www.ietf.org/proceedings/51/I-D/draft-ietf-isis-ext-eth-01.txt
[linux_vlan]: http://www.linuxjournal.com/article/7268

### 3. User-Space-to-Kernel Interface

## Part II. System Ininitialization

### 4. Notification Chains

### 5. Network Device Initialization

### 6. The PCI Layer and Network Interface Cards

### 7. Kernel Infrastruture for Component Initialization

### 8. Device Registration and Initialization

## Part III. Transmission and Reception

### 9. Interrupts and Network Drivers

### 10. Frame Reception

### 11. Frame Transmission

### 12. General and Reference Material About Interrupts

### 13. Protocol Handlers

## Part IV. Bridging

### 14. Bridging: Concepts

### 15. Bridging: The Spanning Tree Protocol

### 16. Bridging: Linux Implementation

### 17. Bridging: Miscellaneous Topics

## Part V. Internet Protocol Version 4 (IPv4)

### 18. Internet Protocol Version 4 (IPv4): Concepts

### 19. Internet Protocol Version 4 (IPv4): Linux Foundationas and Features

### 20. Internet Protocol Version 4 (IPv4): Forwarding and Local Delivery

### 21. Internet Protocol Version 4 (IPv4): Transmission

### 22. Internet Protocol Version 4 (IPv4): Handling Fragmentation

### 23. Internet Protocol Version 4 (IPv4): Miscellaneous Topics

### 24. Layer Four Protocol and Raw IP Handling

### 25. Internet Control Message Protocol (ICMPv4)

## Part VI. Neighboring Subsystem

### 26. Neighboring Subsystem: Concepts

### 27. Neighboring Subsystem: Infrastructure

### 28. Neighboring Subsystem: Address Resolution Protocol (ARP)

### 29. Neighboring Subsystem: Miscellaneous Topics

## Part VII. Routing

### 30. Routing: Concepts

### 31. Routing: Advanced

### 32. Routing: Linux Implementation

### 33. Routing: The Routing Cache

### 34. Routing: Routing Tables

### 35. Routing: Lookups

### 36. Routing: Miscellaneous Topics

