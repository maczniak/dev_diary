# [Linux Kernel Networking: Implementation and Theory][book], by Rami Rosen, Apress (2013)

based on kernel 3.9

[book]: http://www.apress.com/9781430261964

## 1. Introduction

`struct net_device` in `include/linux/netdevice.h`<br>
decisions - lookup in the routing subsystem, netfilter subsystem/hook, IPSec subsystem, ttl field in the IPv4 header<br>
socket buffer (SKB) `struct sk_buff` in `include/linux/skbuff.h`<br>
SKB API - skb_transport_header(), skb_network_header(), skb_mac_header(), netdev_alloc_skb() <-> kfree_skb(), pkt_type == PACKET_MULTICASST|PACKET_BOARDCAST|PACKET_HOST, eth_type_trans() advances skb_pull() (i.e., skb->data)<br>
struct socket (interface to userspace), struct sock (interface to layer 3)<br>
The communication between userspace and the kernel is done with netlink sockets. The iproute2 userspace package is based on netlink sockets.

git send-email<br>
mailing list: netdev@vger.kernel.org archive: www.spinics.net/lists/netdev<br>
Development of the iproute2 and the ethtool userspace packages is also handled in the netdev mailing list. Wireless and Bluetooth have their own specific git tree, maintainer, and mailing list.<br>
Documentation/{SubmittingPatches,CodingStyle,networking}<br>
http://git.kernel.org/?p=linux/kernel/git/davem/{net,net-next}.git<br>
http://lxr.free-electrons.com/ http://lxr.sourceforge.net/en/index.shtml

## 2. NEtlink Sockets

## 3. Internet Control Message Protocol (ICMP)

## 4. IPv4

## 5. The IPv4 Routing Subsystem

## 6. Advanced Routing

## 7. Linux Neighbouring Subsystem

## 8. IPv6

## 9. Netfilter

## 10. IPsec

## 11. Layer 4 Protocols

## 12. Wireless in Linux

## 13. InfiniBand

## 14. Advanced Topics

## A. Linux API

## B. Network Administration

## C. Glossary

