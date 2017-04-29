# [TCP/IP Illustrated, Volume 1, Second Edition][homepage] by Kevin R. Fall and W. Richard Stevens (1951-1999), Addison-Wesley (2012)

[homepage]: https://www.pearsonhighered.com/program/Fall-TCP-IP-Illustrated-Volume-1-The-Protocols-2nd-Edition/PGM69698.html

## Chapter 1. Introduction

Designing an architecture is more art than science.<br>
"Patterns in Network Architecture: A Return to Fundamentals" (Prentice Hall,
 2008)<br>
In "The Design Philosophy of the DARPA Internet Protocols", Clark recounts that
 the primary goal was to "develop an effective technique for multiplexed
 utilization of existing interconnected networks." Beyond this primary goal,
 Clark provides a list of the following second-level goals:
* Internet communication must continue despite loss of networks or gateways.
* The Internet must support multiple types of communication services.
* The Internet architecture must accommodate a variety of networks.
* The Internet architecture must permit distributed management of its resources.
* The Internet architecture must be cost-effective.
* The Internet architecture must permit host attachment with a low level of
  effort.
* The resources used in the Internet architecture must be accountable.

One of the most important principles that influenced the design of the TCP/IP
 suite is called the *end-to-end argument*: The function in question can
 completely and correctly be implemented only with the knowledge and help of the
 application standing at the end points of the communication system.<br>
A nuanced reading reveals that this argument suggests that low-level functions
 should not aim for perfection because a perfect guess at what the application
 may require is unlikely to be possible. The end-to-end argument tends to
 support a design with a "dumb" network and "smart" systems connected to the
 network.<br>
*Fate sharing* suggests placing all the necessary state to maintain an active
 communication association (e.g., virtual connection) at the same location with
 the communicating endpoints. Fate sharing is one of the design philosophies
 that allows virtual connections (e.g., those implemented by TCP) to remain
 active even if connectivity within the network has failed for a (modest) period
 of time.<br>
If best-effort delivery is successful, a fast sender can produce information at
 a rate that exceeds the receiver's ability to consume it. In best-effort IP
 networks, slowing down a sender is achieved by *flow control* mechanisms that
 operate outside the network and at higher levels of the communication
 system.<br>
There are two additional transport-layer protocols tha are relatively new and
 available on some systems. The first is the *Datagram Congestion Control
 Protocol* (DCCP), specified in RFC 4340. It provides a type of service midway
 between TCP and UDP: connection-oriented exchange of unreliable datagrams but
 with congestion control. The other transport protocol available on some systems
 is called the *Stream Control Transmission Protocol* (SCTP), specified in
 RFC4960. SCTP provides reliable delivery like TCP but does not require the
 sequencing of data to be strictly maintained. It also allows for multiple
 streams to logically be carried on the same connection and provides a message
 abstraction, which differs from TCP. SCTP was designed for carrying signaling
 messages on IP networks that resemble those used in the telephone network.<br>
The value of 4 (and 41, which indeicates IPv6) is interesting because it
 indicates the possibility that an IP datagram may appear inside the payload
 area of an IP datagram. This violates the original concepts of layering and
 encapsulation but is the basis for a powerful technique known as tunneling.<br>
*well-known* port numbers (0-1023), the *registered* port numbers (1024-49151),
 and the *dynamic*/*private* port numbers (49152-65535)<br>
This notion is the basis for the so-called Metcalfe's Law, which states roughly
 that the value of a computer network is proportional to the square of the
 number of connected endpoints (e.g., users or devices).<br>
[RFC6250][rfc6250] Evolution of the IP Model<br>
the set of p2p applications together form a network among applications, also
 called an *overlay network*. One of the primary problems in p2p networks is
 called the *discovery problem*. This is usually handled by a bootstrapping
 procedure whereby each client is initially configured with the addresses and
 port numbers of some peers that are likely to be operating. Once connected, the new participant learns of other active peers and, depending on the protocol,
 what servies or files they provide.<br>
Modifications to sockets intended for use with IPv6 are described in:
 [RFC3493][rfc3493] Basic Socket Interface Extensions for IPv6,
 [RFC3542][rfc3542] Advanced Sockets Application Program Interface (API) for IPv6,
 [RFC3678][rfc3678] Socket Interface Extensions for Multicast Source Filters,
 [RFC4584][rfc4584] Extension to Sockets API for Mobile IPv6,
 [RFC5014][rfc5014] IPv6 Socket API for Source Address Selection.<br>
[IETF meetings][ietf_meetings] are held three times a year.<br>
The *Host Requirements* RFCs ([RFC1122][rfc1122] amd [RFC1123][rfc1123]) define
 requirements for protocol implementations in Internet IPv4 hosts, and the
 *Router Requirements* RFC (RFC 1812) does the same for routers. The *Node
 Requirements* RFC ([RFC4294][rfc4294]) does both for IPv6 systems.<br>
Unlike TCP, UDP supports multicast delivery.

[rfc6250]: https://tools.ietf.org/html/rfc6250
[rfc3493]: https://tools.ietf.org/html/rfc3493
[rfc3542]: https://tools.ietf.org/html/rfc3542
[rfc3678]: https://tools.ietf.org/html/rfc3678
[rfc4584]: https://tools.ietf.org/html/rfc4584
[rfc5014]: https://tools.ietf.org/html/rfc5014
[ietf_meetings]: https://www.ietf.org/meeting/
[rfc1122]: https://tools.ietf.org/html/rfc1122
[rfc1123]: https://tools.ietf.org/html/rfc1123
[rfc5014]: https://tools.ietf.org/html/rfc5014
[rfc4294]: https://tools.ietf.org/html/rfc4294

## Chapter 2. The Internet Address Architecture

The dotted-quad notation is simply a way of writing the whole IPv4 address--a
 32-bit nonnegative integer used throughout the Internet system--using
 convenient decimal numbers.<br>
a number of agreed-upon simplifications have been standardized for expressing
 IPv6 addresses ([RFC4291][rfc4291]).
* Embedded IPv4 addresses represented in the IPv6 format can use a form of
  hybrid notation in which the block immediately preceding the IPv4 portion of
  the address has the value ffff and the remaining part of he address is
  formatted using dotted-quad. For example, the IPv6 address ::ffff:10.0.0.1
  represents the IPv4 address 10.0.0.1. This is called an *IPv4-mapped IPv6
  address*.
* A conventional notation is adopted in which the low-order 32 bits of the IPv6
  address can be written using dotted-quad notation. The IPv6 address
  ::0102:f001 is therefore equivalent to the address ::1.2.240.1. This is called
  an *IPv4-compatible IPv6 address*. Note that IPv4-compatible addresses are not
  the same as IPv4-mapped addresses; the are compatible only in the sense that
  they can be written down or manipulated by software in a way similar to IPv4
  addresses. This type of addressing was originally required for transition
  plans between IPv4 and IPv6 but is now no longer required.

In some circumstances (e.g., when expressing a URL containing an address) the
 colon delimiter in an IPv6 address may be confused with another separator such
 as the colon used between an IP address and a port number. In such
 circumstances, bracket characters, [ and ], are used to surround the IPv6
 address.<br>
The flexibility provided by RFC4291 resulted in unnecessary confusion due to
 the ability to represent the same IPv6 address in multiple ways. To remedy this
 situation, [RFC5952][rfc5952] imposes some rules to narrow the range of
 options while remaining compatible with RFC 4291.
* The :: construct must be used to its maximum possible effect (most zeros
  suppressed) but not for only 16-bit blocks. If multiple blocks cotain
  equal-length runs of zeros, the first is replaced with ::.
* The hexadecimal digits a through f should be represented in lowercase.

[RFC0919][rfc0919] describes the various types of broadcasts for IPv4, and
 [RFC1812][rfc1812] suggests that support for forwarding directed broadcasts by
 routers should not only be avaiable but enabled by default. This policy was
 reversed by [RFC2644][rfc2644] so that by default routers must now disable the
 forwarding of directed broadcasts and are even free to omit support for the
 capability altogether. In addition to the subnet broadcast address, the
 *special-use* address 255.255.255.255 is reserved as the *local net broadcast*
 (also called *limited broadcast*), which is never forwarded by routers.<br>
Inportant examples of scopes include *node-local*, *link-local*, or *global*.
 (*site-local* scope level was deprecated.)

IPv4 special-use addresses ([RFC5735][rfc5735])
Prefix             | Special Use | Reference
-------------------+-------------+----------
0.0.0.0/8          | Hosts on the local network. May be used only as a source IP address. | [RFC1122][rfc1122]
10.0.0.0/8         | Address for private networks (intranets). Such addresses never appear on the public Internet. | [RFC1918][rfc1918]
127.0.0.0/8        | Internet host loopback addresses (same computer). Typically only 127.0.0.1 is used. | RFC1122
169.254.0.0/16     | "Link-local" addresses--used only on a single link and generally assigned automatically. | [RFC3927][rfc3927]
172.16.0.0./12     | Address for private networks (intranets). Such addresses never appear on the public Internet. | RFC1918
192.0.0.0/24       | IETF protocol assignements (IANA reserved). | [RFC5736][rfc5736]
192.0.2.0/24       | TEST-NET-1 addresses approved for use in documentation. Such addresses never appear on the public Internet. | [RFC5737][rfc5737]
192.88.99.0/24     | Used for 6to4 relays (anycast addresses). | [RFC3068][rfc3068]
192.168.0.0/16     | Address for private networks (intranets). Such addresses ne
ver appear on the public Internet. | RFC1918
198.18.0.0/15      | Used for benchmarks and performance testing. | [RFC2544][rfc2544]
198.51.100.0/24    | TEST-NET-2. Approved for use in documentation. | RFC5737
203.0.113.0/24     | TEST-NET-3. Approved for use in documentation. | RFC5737
224.0.0.0/4        | IPv4 multicast addresses (formerly class D); used only as destination addresses. | [RFC5771][rfc5771]
240.0.0.0/4        | Reserved space (formerly class E), except 255.255.255.255. | [RFC1112][rfc1112]
255.255.255.255/32 | Local network (limited) broadcast address. | RFC0919 [RFC0922][rfc0922]

IPv6 special-use addresses ([RFC5156][rfc5156]
Prefix              | Special Use | Reference
--------------------+-------------+----------
::/0                | Default route entry. Not used for addressing. | RFC5156
::/128              | The unspecified address; may be used as a source IP address. | RFC4291
::1/128             | The IPv6 host loopback address; not used in datagrams sent outside the local host. | RFC4291
::ffff:0:0/96       | IPv4-mapped addresses. Such addresses never appear in packet headers. For internal host use only. | RFC4291
::{ipv4-address}/96 | IPv4-compatible addresses. Deprecated; not to be used. | RFC4291
2001::/32           | Teredo addresses. | [RFC4380][rfc4380]
2001:10::/28        | Overlay Routable Cryptographic Hash Identifiers. Such addresses never appear on the public Internet. | [RFC4843][rfc4843]
2001:db8::/32       | Address range used for documentation and for examples. Such addresses never appear on the public Internet. | [RFC3849][rfc3849]
2002::/16           | 6to4 addresses of 6to4 tunnel relays. | [RFC3056][rfc3056]
3ffe::/16           | Used by 6bone experiments. Deprecated; not to be used. | [RFC3701][rfc3701]
5f00::/16           | Used by 6bone experiments. Deprecated; not to be used. | RFC3701
fc00::/7            | Unique, local unicast addresses; not used on the global Internet. | [RFC4193][rfc4193]
fe80::/10           | Link-local unicast addresses. | RFC4291
ff00::/8            | IPv6 multicast addresses; used only as destination addresses. | RFC4291

*IPv4-embedded IPv6 address* can be encoded using one of six formats, based on
 the length of the IPv6 prefix, which is required to be one of the following:
 32, 40, 48, 56, 64, or 96. The well-known prefix 64:ff9b::/96 can be used for
 automatic translation between IPv4 and IPv6 unicast addresses.<br>
The original multicast service model has become known as *any-source multicast*
 (ASM). A newer approach, called *source-specific multicast* (SSM,
 [RFC3569][rfc3569], [RFC4607][rfc4607]) uses only a single sender per group.
 In this case, when joining a group, a host specifies the address of a
 *channel*, which comprises both a group address *and* a source IP address. SSM
 was developed to avoid some of the complexities in deploying the ASM model.
 Although neigher form of multicast is widely available throughout the Internet,
 it seems that SSM is now the more likely candidate for adoption.<br>
These address ranges are owned and managed by the customer's ISP and are called
 *provider-aggregatable* (PA) addresses because they consist of one or more
 prefixes that can be aggregated with other prefixes the ISP owns. Such
 addresses are also sometimes called *non-protable* addresses. An alternative
 type of address space is called *provider-independent* (PI) address space.
 Addresses allocated from PI space are allocated to the user directly and may be
 used with any ISP.<br>
[ARIN's Whois RESTful Web Servoce][whois_rws]
 http://whois.arin.net/rest/ip/72.1.140.203.txt
 http://whois.arin.net/rest/net/NET-72-1-128-0-1.txt<br>
As we can see from the relatively large number of protocols and the complexity
 of the various multicast address formats, multicast address management is a
 formidable issue. From a typical user's point of view, multicasting is used
 rarely and may be of limited concern. From a programmer's point of view, it may
 be worthwhile to support multicast in application designs, and some insight has
 been provided into how to do so [RFC3170][rfc3170].<br>
`netstat -gn` (multicast group)<br>
Today, IP addresses serve as both *identifiers* (essentially a form of name) and
 *locators* (an address understood by the routing system) for a network
 interface attached to the Internet. Providing a separation would allow the
 network protocol implementation to function even if the underlying IP address
 changes. Protocols that provide this separation are sometimes called
 *identifier/locator separating* or *id/loc split* protocols. Shim6 introduces a
 "shim" network-layer protocol that separates the "upper-layer protocol
 identifier" used by the transport protocols from the IP address. Separation of
 identifiers from locators is the subject of several other efforts, including
 the experimental *Host Identity Protocol* (HIP) [RFC4423][rfc4423], which
 identifies hosts using cryptographic host identifiers. Such identifiers are
 effectively the public keys of public/private key pairs associated with hosts,
 so HIP traffic can be authenticated as having come from a particular host.

Link-local addresses are often created based on a standard prefix in combination
 with an IID that can be based on addresses provided by lower-layer protocols
 (such as hardware/MAC addresses) or random values. Variants of the IPv6
 multicast address format provide ways for allocating groups based on unicast
 prefixes, embedding routing information (RP addresses) in groups, and creating
 multicast addresses based on IIDs. The develpoment and deployment of CIDR was
 arguably the last fundamental change made to the Internet's core routing
 system.

[rfc4291]: https://tools.ietf.org/html/rfc4291
[rfc5952]: https://tools.ietf.org/html/rfc5952
[rfc0919]: https://tools.ietf.org/html/rfc0919
[rfc1812]: https://tools.ietf.org/html/rfc1812
[rfc2644]: https://tools.ietf.org/html/rfc2644
[rfc5735]: https://tools.ietf.org/html/rfc5735
[rfc1918]: https://tools.ietf.org/html/rfc1918
[rfc3927]: https://tools.ietf.org/html/rfc3927
[rfc5736]: https://tools.ietf.org/html/rfc5736
[rfc5737]: https://tools.ietf.org/html/rfc5737
[rfc3068]: https://tools.ietf.org/html/rfc3068
[rfc2544]: https://tools.ietf.org/html/rfc2544
[rfc5771]: https://tools.ietf.org/html/rfc5771
[rfc1112]: https://tools.ietf.org/html/rfc1112
[rfc0922]: https://tools.ietf.org/html/rfc0922
[rfc4380]: https://tools.ietf.org/html/rfc4380
[rfc4843]: https://tools.ietf.org/html/rfc4843
[rfc3849]: https://tools.ietf.org/html/rfc3849
[rfc3056]: https://tools.ietf.org/html/rfc3056
[rfc3701]: https://tools.ietf.org/html/rfc3701
[rfc4193]: https://tools.ietf.org/html/rfc4193
[rfc3569]: https://tools.ietf.org/html/rfc3569
[rfc4607]: https://tools.ietf.org/html/rfc4607
[whois_rws]: https://www.arin.net/resources/whoisrws/
[rfc4607]: https://tools.ietf.org/html/rfc3170
[rfc4423]: https://tools.ietf.org/html/rfc4423

## Chapter 3. Link Layer

Compliant Ethernet switches isolate traffic among hosts to common VLANs. Note
 that because of this isolation, two hosts attached to the same switch but
 operating on different VLANs require a router between them for traffic to flow.

```
vconfig add eth1 2
ifconfig eth1.2
vconfig rem eth1.2
vconfig set_name_type VLAN_PLUS_VID
vconfig add eth1 2
ifconfig vlan0002

modprove bonding
ifconfig bond0 10.0.0.111 netmask 255.255.255.128
ifenslave bond0 eth0 wlan0
```

The master device, `bond0`, is assigned the IPv4 address information we would
 typically assign to either of the individual interfaces, and it receives the
 first slave's MAC address by default.

```
ethtool eth0
ethtool -s eth0 wol umgb
wol 00:08:74:93:C8:3C

brctl addbr br0
brctl addif br0 eth0 # brctl delif
brctl addif br0 eth1
ifconfig eth0 up
ifconfig eth1 up
ifconfig br0 up
brctl show         # inspect the filter databases (called forwarding databases 
brctl showmacs br0 #  or fdbs in Linux terminology)
brctl setageing br0 1

brctl stp br0 on
brctl showstp br0
```

RSTP has been extended to include VLANs--a protocol called the *Multiple
 Spanning Tree Protocol* (MSTP). This protocol retains the RSTP (and hence STP)
 BPDU format, so backward compatibility is possible, but it also supports the
 formation of multiple spanning trees (one for each VLAN).<br>
The 802.11 standard also provides for an *ad hoc mode*. In this configuration
 there is no AP (access point) or DS (distribution serviec); instead, direct
 station-to-station (peer-to-peer) communication takes place. In IEEE
 terminology, the STAs (stations) participating in an ad hoc network form an
 *independent basic service set* (IBSS). A WLAN formed from a collection of BBSs
 and/or IBSSs is called a *service set*, identified by a *service set
 identifier* (SSID). An *extended service set identifier* (ESSID) is an SSID
 that names a collection of connected BSSs and is essentially a name for the LAN
 that can be up to 32 characters long. Such names are ordinarily assigned to
 Wi-Fi APs when a WLAN is first installed.

```
iwlist wlan0 scan
iwconfig wlan0 rts 250 # request-to-send
```

The major attraction of using 802.1X/[EAP] (Extensible Authenticaion Protocol,
 [RFC3748][rfc3748]) is that a managed authentication server can be used to
 provide access control decisions on a per-user basis to an AP. For this reason,
 authentication using 802.1X is sometimes referred to as "Enterprise" (e.g.,
 WPA-Enterprise).<br>
The IEEE is working on the 802.11s standard, which covers Wi-Fi *mesh*
 operation.<br>
The most recent step in the evolution of header compression is known as *Robust
 Header Compression* (ROHC) [RFC5225][rfc5225]. It further generalizes IP
 header compression to cover more transport protocols and allows more than one
 form of header compression to operate simultaneously. Like the IP header
 compression mentioned previously, it can be used over various types of links,
 including PPP.<br>
In Windows, the Microsoft Loopback Adapter is not installed by default, even
 though IP loopback is still supported. This adapter can be used for testing
 various network configurations even when a physical network interface is not
 available.<br>
The path MTU between any two hosts need not be constant over time and need not
 be the same in the two directions. [RFC1191][rfc1191] specifies the *path MTU
 discovery* (PMTUD) mecahnism for IPv4, and [RFC1981][rfc1981] describes it for
 IPv6. A complementary approach that avoids some of the issues with these
 mechanisms is described in [RFC4821][rfc4821].<br>
Tunneling, generally speaking, is the idea of carrying lower-layer traffic in
 higher-layer (or equal-layer) packets. Tunneling turns the idea of strict
 layering of protocols on its head and allows for the formation of *overlay
 networks* (i.e, networks where the "links" are really virtual links implemented
 in some other protocol instead of physical connections). Three of the more
 common protocols used to establish tunnels include *Generic Routing
 Encapsulation* (GRE) [RFC2784][rfc2784], the Microsoft proprietary
 *Point-to-Point Tunneling Protocol* (PPTP) [RFC2637][rfc2637], and the *Layer 2
 Tunneling Protocol* (L2TP) [RFC3931][rfc3931]. Others include the earlier
 nonstandard IP-in-IP tunneling protocol [RFC1853][rfc1853]. GRE and LT2P were
 developed to standardize and replace IP-in-IP and PPTP, respectively, but all
 of these approaches are still in use.<br>
some research is aimed at stretching this even farther--to cases where there may
 never be an end-to-end path between sender and receiver(s) at any single point
 in time. ([RFC4838][rfc4838] Delay-Tolerant Networking Architecture)

[rfc3748]: https://tools.ietf.org/html/rfc3748
[rfc5225]: https://tools.ietf.org/html/rfc5225
[rfc1191]: https://tools.ietf.org/html/rfc1191
[rfc1981]: https://tools.ietf.org/html/rfc1981
[rfc4821]: https://tools.ietf.org/html/rfc4821
[rfc2784]: https://tools.ietf.org/html/rfc2784
[rfc2637]: https://tools.ietf.org/html/rfc2637
[rfc3931]: https://tools.ietf.org/html/rfc3931
[rfc1853]: https://tools.ietf.org/html/rfc1853
[rfc4838]: https://tools.ietf.org/html/rfc4838

## Chapter 4. ARP: Address Resolution Protocol

ARP is used with IPv4 only; IPv6 use the Neighbor Discovery Protocol, which is
 incorporated into ICMPv6.<br>
On non-broadcast networks (sometimes called NBMA for *non-broadcast multiple
 access*), other, more complex mapping protocols may be required
 [RFC2332][rfc2332].<br>
`arp -a`<br>
Another feature of ARP is called *gratuitous ARP*. It occurs when a host sends
 an ARP request looking for its *own* address. It lets a host determine if
 another hos is already configured with the same IPv4 address. The host sending
 the gratuitous ARP is not expecting a reply to its request. If a reply is
 received, however, the error mesage "Duplicate IP address sent from Ethernet
 address ..." is usually displayed. If the host sending the gratuitous ARP has
 just changed its hardware address, this frame causes any other host receiving
 the broadcast that has an entry in its cache for the old hardware address to
 update its ARP cache entry accordingly.<br>
Although gratuitous ARP provides some indication that multiple stations may be
 attempting to use the same IPv4 address, it really provides no mechanism to
 react to the situation. To deal with this issue, [RFC5227][rfc5227] describes
 *IPv4 Address Conflict Detection* (ACD). ACE defines *ARP probe* and *ARP
 announcement* packets. An ARP probe is an ARP request packet in which the
 *Sender's Protocol* (IPv4) *Address* field is set to 0. Setting the *Sender's
 Protocol Address* field to 0 avoids cache pollution should the candidate IPv4
 address already be in use by another host, a difference from the way gratuitous
 ARP works. An ARP announcement is identical to an ARP probe, except both the
 *Sender's Protocol Address* and the *Target Protocol Address* fields are filled
 in with the candidate IPv4 address. It is used to announce the sender's
 intention to use the candidate IPv4 address as its own. To perform ACD, a host
 sends an ARP probe when an interfce is brought up or out of sleep, or when a
 new link is established. It first waits a andom amount of time before sending
 up to three probe packets. If a requesting host does not discover a conflict
 according to the procedure just described, it sends two ARP announcements
 spaced 2s apart to indicate to systems in the broadcast domain in the IPv4
 address it is now using. Once a host has announced an address it is using, it
 continues inspecting incoming ARP traffic to see if its address apears in the
 *Sender's Protocol Address* field.<br>
Entries can also be added using the `-s` option. This entry is made
 semipermanent (i.e., it does not time out from the cache, but it disappears
 when the system is rebooted). If `arp -s` is used to enable proxy ARP (with the
 keyword `pub`), Linux responds for the address specified even if the file
 `/proc/sys/net/ipv4/conf/*/proxy_arp` contains 0.<br>
Embedded devices are typically configured in one of two ways. First, DHCP can be
 used to automatically assign an address and other information. Another way is
 to use ARP to set an IPv4 address, although this method is less common. The
 basic idea is to manually establish an ARP mapping for the device (using the
 `arp -s` command), then send an IP packet to the address.<br>
Such static entries have been used in an attempt to enhance security.
 Unfortunately, most implementations of ARP have traditionally replaced even
 static cache entries with entries provided by ARP replies.

[rfc2332]: https://tools.ietf.org/html/rfc2332
[rfc5227]: https://tools.ietf.org/html/rfc5227

## Chapter 5. The Internet Protocol (IP)

a host is not required to be able to receive an IPv4 datagram larger than 576
 bytes. (In IPv6 a host must be able to process a datagram at least as large as
 the MTU of the link to which it is attached, and the minimum link MTU is 1280
 bytes.) Many applications that use the UDP oriticik fir data transport (e.g.,
 DNS, DHCP, etc.) use a limited data size of 512 bytes to avoid the 576-byte
 IPv4 limit. With IPv6, it is the *payload* length that is limited to 64KB, not
 the entire datagram. In addition, IPv6 supports a *jumbogram* option that
 provides for the possibility, at least theoretically, of single packets with
 payloads as large as 4GB!<br>
The *Header Checksum* field is calculated *over the IPv4 header only*. Perhaps
 surprisingly, the IPv6 header does not have any checksum field.<br>
The third and fourth fields of the IPv4 header (second and third fields of the
 IPv6 header) are the *Differentiated Services* (called *DS Field*) and *ECN*
 (Explicit Congestion Notification) fields. Differentiated Services (called
 *DiffServ*) is a framework and set of standards aimed at supporting
 differentiated classes of service (i.e., beyond just best-effort) on the
 Internet. A number is placed in the *DS Field* termed the *Differentiated
 Services Code Point* (DSCP). A "code point" refer to a particular predefined
 arrangement of bits with agreed-upon meaning. The pair of *ECN* bits in the
 header is used for marking a datagram with a *congestion indicator* when
 passing through a router that has a significant amount of internally queued
 traffic. Both bits are set by persustently congested ECN-aware routers when
 forwarding packets. Although the original uses for the ToS and Traffic Class
 bytes are not widely supported, the structure of the *DS Field* has been
 arranged to provide some backward compatibility with them.<br>
Most of these options were introduced in [RFC0791][rfc0791] at the time IPv4 was
 being designed, when the Internet was considerably smaller and when threats
 from malicious users were less of a concern. As a consequence, many of the
 options are no longer practical or desirable because of the limited size of the
 IPv4 header or concerns regarding security. Thus, IPv4 options are typically
 disallowed or stripped at the perimeter of enterprise networks by firewalls.
 In some cases IPv6 routers process extension headers, but many headers are
 designed to be processed only by end hosts.<br>
Home Address option holds the "home" address of the IPv6 node sending the
 datagram when IPv6 mobility options are in use. Mobile IP specifies a set of
 procedures for handling IP nodes that many change their point of network
 attachment without losing their higher-layer network connections. It has a
 concept of a node's "home," which is derived from the address prefix of its
 typical location. When roaming away from home, the node is generally assigned a
 different IP address. This option allows the node to provide its normal home
 address in addition to its (presumably temporarily assigned) new address while
 traveling. The home address can be used by other IPv6 nodes when communicating 
 with the mobile node.<br>
RH0 has been deprecated by [RFC5095][rfc5095] because of a security convern that
 allows RH0 to be used to increase the effectiveness of DoS attacks. The problem
 is that RH0 allows the same address to be specified in multiple locations
 within the Routing header. RH2 is equivalent to RH0 except it has room for only
 a single address and uses a different value in the *Routing Type* field.

A multiyear (actually, multidecade!) effort known as *Mobile IP* addresses this
 issue. (Other protocols have also been suggested; see [RFC6301][rfc6301].)
 Although there are versions of Mobile IP for both IPv4[RFC5944][rfc5944]
 (MIPv4) and IPv6 [RFC6275][rfc6275], we focus on Mobile IPv6 (called MIPv6)
 because it is more flexible and somewhat easier to explain. Note that we do not
 discuss MIPv6 comprehensively; it is sufficiently complex to merit a book on
 its own (e.g., "Mobile IP Technology and Applications"). We shall focus on the
 bsic messages specified in RFC6275. Other messages are used to implement "fast
 handovers" [RFC5568][rfc5568], changing of the home agent [RFC5142][rfc5142],
 and experiments [RFC5096][rfc5096].<br>
Much of the terminology also applies to MIPv4. A host that might move is called
 a *mobile node* (MN), and the hosts with which it is communicating are called
 *correspondent nodes* (CNs). The MN is given an IP address chosen from the
 network prefix used in its home network. This address is known as its *home
 address* (HoA). When it travels to a visited netowrk, it is given an additional
 address, called its *care-of address* (CoA). In the basic model, whenever a CN
 communicates with an MN, the trafic is routed through the MN's *home agent*
 (HA). The association between an MN's HoA and its CoA is called a *binding* for
 the MN.<br>
The basic model works in cases where an MN's CNs do not engage in the MIPv6
 protocol. When the MN (or mobile network router) attaches to a new point in the
 network, it receives its CoA and sends a *binding update* messages to its HA.
 The HA responds with a *binding acknowledgement*. Assuming that all goes well,
 traffic between the MN and CNs is thereafter routed through the MN's HA using a
 two-way form of IPv6 packet tunneling [RFC2473][rfc2473] called *bidirectional
 tunneling*.<br>
To improve upon the inefficient routing that may occur in basic MIPv6, a process
 called *route optimization* (RO) can be used, provided it is supported by the
 various nodes involved. For a more detailed discussion, see RFC6275 and
 [RFC4866][rfc4866]. For a discuss of the design rationale behind RO security,
 see [RFC4225][rfc4225]. When used, RO involves a *correspondent registration*
 whereby an MN notifies its CNS of its current CoA to allow routing to take
 place without help from the HA. To establish a binding with its CNs, an MN must
 prove to each CN that it is the proper MN. This is accomplished by a *Return
 Routability Procedure* (RRR). The RRR uses the following *mobility messages*,
 all of which are subtypes of the IPv6 Mobility extension header: Home Test Init
 (HoTI), Home Test (HoT), Care-of Test Init (CoTI), Care-of Test (COT). These
 messages verify to a CN that a particular MN is reachable both at its home
 address (HoTI and HoT messages) and at its care-of addresses (CoTI and CoT
 messages). The MN begins by sending both a HoTI and CoTI message to the CN. The
 CN receives both messages in some order and responds with a HoT and CoT message
 to each, respectively. The HoT message is sent to the MN via the HA. Inside
 these messages are random bit strings called *tokens*, which the MN uses to
 form a cryptographic key. One a binding has been established successfully, data
 may flow directly between an MN and its CNs without the inefficiency of
 bidirectional tunneling. This is accomplished using an IPv6 Destination option
 for traffic moving from the MN to a CN and a type 2 Routing header (RH2) for
 traffic headed in the reverse direction. so applications "believe" they are
 using the MN's HoA instead of its CoA for establishing connections and other
 actions.<br>
Such devices may be running real-time applications (e.g., VoIP) that have
 latency requirements. Consequently, several approaches are being explored to
 reduce the amount of time requried to execute binding updates. These include
 fast handovers RFC5568, a modification to MIPv6 called Hierarchical MIPv6
 (HMIPv6) [RFC5380][rfc5380], and a modification in which the mobile signaling
 ordinarily required of an MN is performed by a proxy (called proxy MIPv6 or
 PMIPv6 [RFC5213][rfc5213])

this decision depends on the *host model* of the receiving system
 [RFC1122][rfc1122] and is most relevant for multihomed hosts. There are two
 host models, the *strong host model* and *weak host model*. The attraction of
 using the strong host model relates to a security concern. In Windows (Vista
 and later), strong host behavior is the default for sending and receiving for
 IPv4 and IPv6. In Linux, the IP behavior defaults to the weak host model. BSD
 (including Mac OS X) uses the strong host model.

```
C:\> netsh interface ipv4/6 set interface <ifname> weakhostreceive=en/disabled
C:\> netsh interface ipv4/6 set interface <ifname> weakhostsend=en/disabled
```

In modern IP implementations, the IP addresses used in the *Source IP Address*
 and *Destination IP Address* fields of the datagram are selected using a set of
 procedures called *source address selection* and *destination address
 selection*. With the advent of multiple addresses per interface and the use of
 IPv6 in which simultaneous use of addresses with multiple scopes is normal,
 some procedure must be used. The situation is further complicated when
 communication is to take place between tow hosts that implement both IPv4 and
 IPv6 ("dual-stack" hosts; see [RFC4213][rfc4213]). [RFC3484][rfc3484] gives the
 rules for selecting IPv6 default addresses; IPv4-only hosts do not ordinarily
 have such complex issues. an update to RFC3484 is being considered
 RFC3484-revise [RFC6724][rfc6724]. Importantly, this revision addresses how
 so-called *Unique Local IPv6 Unicast Addresses* (ULAs) [RFC4193][rfc4193] are
 treated by the address selection algorithms. ULAs are globally scoped IPv6
 addresses that are constrained to be used only within a common (private)
 network.

[rfc0791]: https://tools.ietf.org/html/rfc0791
[rfc5095]: https://tools.ietf.org/html/rfc5095
[rfc6301]: https://tools.ietf.org/html/rfc6301
[rfc5944]: https://tools.ietf.org/html/rfc5944
[rfc6275]: https://tools.ietf.org/html/rfc6275
[rfc5568]: https://tools.ietf.org/html/rfc5568
[rfc5142]: https://tools.ietf.org/html/rfc5142
[rfc5096]: https://tools.ietf.org/html/rfc5096
[rfc2473]: https://tools.ietf.org/html/rfc2473
[rfc4866]: https://tools.ietf.org/html/rfc4866
[rfc4225]: https://tools.ietf.org/html/rfc4225
[rfc5380]: https://tools.ietf.org/html/rfc5380
[rfc5213]: https://tools.ietf.org/html/rfc5213
[rfc4213]: https://tools.ietf.org/html/rfc4213
[rfc3484]: https://tools.ietf.org/html/rfc3484
[rfc6724]: https://tools.ietf.org/html/rfc6724

## Chapter 6. System Configuration: DHCP and Autoconfiguration

There is now an agreed-upon procedure for *detecting network attachment* (DNA),
 specified in [RFC4436][rfc4436] for IPv4 and [RFC6059][rfc6059] for IPv6. These
 specifications do not contain new protocols but instead suggest how unicast ARP
 (for IPv4) and a combination of unicast and multicast Neighbor
 Solicitation/Router Discovery messages (for IPv6) can be used to reduce the
 latency of acquiring configuration information when a host switches network
 links. As these specifications are relatively new (especially for IPv6), not
 all systems implement them.<br>
`ipconfig /all /renew[6] /release[6]`, `dhclient [-r]`<br>
IPv6 address lifecycle - tentative or optimistic (DAD, duplicate address
 detection)→ preferred (timeout)→ deprecated (valid timeout)→ invalid<br>
The Types of addresses requested may be regular addresses or *temporary*
 addresses [RFC4941][rfc4941]. Temporary addresses are derived in part from
 random numbers to help improve privacy be frustrating the tracking of IPv6
 hosts based on IPv6 addresses. Temporary addresses are ordinarily assigned at
 the same time nontemporary addresses are assigned but are regenerated using a
 different random number more frequently.<br>
A *Private Enterprise Number* (PEN) is a 32-bit value given out by the IANA to
 an enterprise. It is usually used in conjunction with the SNMP protocol for
 network management purposes. About 38,000 of them have been assigned as of
 mid-2011. The current list is available from the IANA [IEPARAM][ieparam].<br>
An IETF effort known as Geoconf ("Geographic configuration") results in
 [RFC6225][rfc6225], which specifies how to provide such geospatial *Location
 Configuration Information* (LCI) to clients using the GeoConf (!23) and GeoLoc
 (144) DHCP options. [RFC4776][rfc4776] defines the GEOCONF_CIVIC (99) option
 for carrying civic location LCI. The IETF is undertaking this issue (langauge,
 character set and privacy convern) in a framework called "Geopriv."
 [RFC3693][rfc3693] An alternative high-layer protocol known as the
 *HTTP-Enabled Location Delivery* (HELD) protocol [RFC5985][rfc5985] may also be
 used to provide location information. Once a host knows its location, it may
 need to contact services associated with the location (e.g., the location of
 the nearest hospital). The IETF *Location-to-Service Translation* (LoST)
 framework [RFC5222][rfc5222] accomplishes this using an application-layer
 protocol accessed using a location-dependent URI.<br>
In cases where a host without a manually configured address attaches to a
 network lacking a DHCP server, IP-based communication is unable to take place
 unless the host somehow generates an IP address to use. [RFC3927][rfc3927]
 describes a mechanism whereby a host can automatically generate its own IPv4
 address from the link-local range 169.254.1.1 through 169.254.254.254 using the
 16-bit subnet mask 255.255.0.0 (see [RFC5735][rfc5735]). This method is known
 as dynamic link-local address configuration or *Automatic Private IP
 Addressing* (APIPA). (disable: `HKLM\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters\IPAutoconfigurationEnabled = 0`,
 `NOZEROCONF=yes` in `/etc/sysconfig/network` or `/etc/sysconfig/network-scripts/ifcfg-eth0`)<br>
IPv6 SLAAC (stateless address autoconfiguration) is described in
 [RFC4862][rfc4862]. It involves three major steps: obtaining a link-local
 address, obtaining a global address using stateless autoconfiguration, and
 detecting whether the link-local address is already in use on the link. When
 routers are present, a global address is formed using a combination of the
 prefix advertised by a router and locally generated information. (disable:
 `sysctl -w net.ipv6.conf.all.autoconf=0`,
 `C:\> netsh interface ipv6 set interface {ifname} managedaddress=disabled`)<br>
`C:\> netsh interface ipv6 set privacy state=disabled`,
 `sysctl -w net.ipv6.conf.all/default.use_tempaddr=2/0`<br>
combined DNS/DHCPO server (such as the Linux `dnsmasq` package)<br>
DHCPv6 servers may operate in a "stateful" mode, in which they lease IPv6
 addresses to requesting clients, or a "stateless" mode, in which they provide
 configuration information other than the addresses.

[rfc4436]: https://tools.ietf.org/html/rfc4436
[rfc6059]: https://tools.ietf.org/html/rfc6059
[rfc4941]: https://tools.ietf.org/html/rfc4941
[ieparam]: https://www.iana.org/assignments/enterprise-numbers/enterprise-numbers
[rfc6225]: https://tools.ietf.org/html/rfc6225
[rfc4776]: https://tools.ietf.org/html/rfc4776
[rfc3693]: https://tools.ietf.org/html/rfc3693
[rfc5985]: https://tools.ietf.org/html/rfc5985
[rfc5222]: https://tools.ietf.org/html/rfc5222
[rfc4862]: https://tools.ietf.org/html/rfc4862

## Chapter 7. Firewalls and Network Address Translation (NAT)

IP fragmentation can significantly complicate a firewall's job, and stateless
 packet-filtering firewalls are easily confused by fragments.<br>
Conversely, a number of so-called *tunneling proxy servers* are available on the
 Internet. These servers (e.g., psiphon, CGIProxy) essentially perform the
 opposite function--to allow users to avoid being blocked by content
 filters.<br>
Two versions of SOCKS are currently in use: version 4 and version 5. Version 4
 provides the basic support for proxy traversal, and version 5 adds strong
 authentication, UDP traversal, and IPv6 addressing. To use a SOCKS proxy, an
 application must be written to use SOCKS (it must be "socksified") and
 configured to know about the location of the proxy and which version of SOCKS
 to use. Once this is accomplished, the client uses the SOCKS protocol to
 request the proxy to perform network connections and, optionally, DNS
 lookups.<br>
NAT poses problems for some application protocols, especially those that send IP
 addressing information inside the application-layer payload. A more complete
 list of considerations regarding NAT appears in [RFC3027][rfc3027]. Today, NATs
 are so prevalent that application designers are encouraged to make their
 applications "NAT-friendly" [RFC3235][rfc3235]. The choice of packet-filtering
 policy may have a different granularity--for example, the treatment of
 unsolicited packets (those not associated with packets originating from behind
 the NAT) received by the NAT may depend on source and destination IP address
 and/or source and destination port number.<br>
The so-called *traditional NAT* includes both *basic NAT* and *Network Address
 Port Translation* (NAPT) [RFC3022][rfc3022]. NAPT, also known as IP
 masquerading, usually rewirte address to a single address. NAPT must sometimes
 rewirte port numbers in order to avoid collisions.<br>
Many of the bahavioral specifics for NATs have been the subject of the IETF
 Behavior Engineering for Hindrance Avoidance (BEHAVE) working group
 (concluded). BEHAVE has produced a number of documents, starting in 2007, that
 clarify consistent behaviors for NATs.<br>
The behavioral requirements for traditional NAT with TCP are defined in
 [RFC5382][rfc5382] and relate primarily to the TCP three-way handshake. In
 addition to forwarding the packet, the NAT creates internal state to remember
 the fact that a new connection is being handled by the NAT (called a *NAT
 session*). At a minimum, this state includes an entry (called a *NAT mapping*)
 containing the source port number and IP address of the client. This behavior
 is called *port preservation*. / Most NATs include a simplified version of the
 TCP connection establishment procedures and can distinguish between connection
 success and failure. In particular, when an outgoing SYN segment is observed, a
 *connection timer* is activated, and if no ACK is seen before the timer
 expires, the session state is cleared. If an ACK does arrive, the timer is
 canceled and a *session timer* is created, with a considerably longer timeout
 (e.g., hours instead of minutes). When this happens, the NAT may send an
 additional packet to the internal endpoint, just to double-check if the session
 is indeed dead (called *probing*). If it receives an ACK, the NAT realizes that
 the connection is still active, resets the session timer, and does not delete
 the session. If it receives either no response (after a *close timer* has
 expired) or an RST segment, the connection has gone dead, and the state is
 cleared. / RFC5382, a product of the BEHAVE working group, notes that a TCP
 connection can be configured to send "keepalive" packets, and the default rate
 is one packet every 2 hours, if enabled. Otherwise, a TCP connection can remain
 established indefinitely. While a connection is being set up or cleared,
 however, the maximum idle time is 4 minutes. Consequently, RFC5382 requires
 (REG-5) that a NAT wait at least 2 hours and 4 minutes before concluding that
 an established connection is dead and at least 4 minutes before concluding that
 a partially opened or closed connection is dead. / One of the tricky problems
 for a TCP NAT is handling peer-to-peer applications operating on hosts residing
 on the private sides of multiple NATs [RFC5128][rfc5128]. Some of these
 applications use a *simultaneous open* whereby each end of the connection acts
 as a client and sends SYN packets more or less simultaneously. TCP is able to
 handle this case by responding with SYN + ACK packets that complete the
 connection faster than with the three-way handshake, but many existing NATs do
 not handle it properly. RFC5382 addresses this by requiring (REQ-2) that a NAT
 handle all valid TCP packet exchanges, and simultaneous opens in particular.
 Some peer-to-peer applications (e.g., network games) use this behavior. In
 addition, RFC5382 specifies that an inbound SYN for a connection about which
 the NAT knows nothing should be silently discarded. This can occur when a
 simultanous open is attempted but the external host's SYN arrives at the NAT
 before the internal host's SYN. Although this may seem unlikely, it can happen
 as a result of clock skew, for example. If the incoming external SYN is
 dropped, the internal SYN has time to establish a NAT mapping for the same
 connection represented by the external SYN. If no internal SYN is forthcoming
 in 6s, the NAT may signal an error to the external host.<br>
The NAT behavioral requirements for unicast UDP are defined in
 [RFC4787][rfc4787]. UDP is somewhat different because there are no connection
 establishment and clearing procedures as there are in TCP. UDP can rely on only
 the two endpoint address/port number combinations. To handle these issues, UDP
 NATs use a *mapping timer* to clear NAT state if a binding has not been used
 "recently." A related consideration is when the timer should be considered
 refreshed. Because of the layering of UDP above IP, an IP fragment other than
 then first one does not contain the port number information needed by NAPT to
 operate properly. This also applies to TCP and ICMP. Thus, in general,
 fragments cannot be handled properly by simple NATs or NAPTs.<br>
The NAT behavioral requirements for ICMP are defined in [RFC5508][rfc5508].
 These are two issues involved when NAT is used for ICMP. ICMP has two
 categories of messages: information and error. Error messages generally contain
 a (partial or full) copy of the IP packet that induced the error condition.
 Ordinarily, this presents no difficulty, but when an ICMP error message passes
 through a NAT, the IP addresses in the included "offending datagram" need to be
 rewritten by the NAT in order for them to make sense to the end client (called
 *ICMP fix-up*). For informational messages, the same issues arise, but in this
 case most message types are of a query/response or client/server nature and
 include a *Query ID* field that is handled much like port numbers for TCP or
 UDP.<br>
At present, this is a contentious issue [RFC5902][rfc5902]. The other desirable
 NAT features (e.g., firewall-like functionality, topology hiding, and privacy)
 can be better achieved using *Local Network Protection* (LNP)
 [RFC4864][rfc4864]. LNP represents a collection of techniques with IPv6 that
 match or exceed the properties of NATs. Aside from its packet-filtering
 properties, NAT supports the coexistence of multiple address realms and thereby
 helps to avoid the problem of a site having to change its IP addresses when it
 switches ISPs. For example, [RFC4193][rfc4193] defines *Unique Local IPv6
 Unicast Addresses* (ULAs) that could conceivably be used with an experimental
 version of IPv6-to-IPv6 prefix translation called *NPTv6*
 [RFC6296][rfc6296].<br>
An interesting issue arises when a client wishes to reach a server and both
 reside on the same, private side of the same NAT. NATs that support this
 scenario implement so-called *hairpinning* or *NAT lookback*. If the NAT
 presents the hairpinned packet to *X2* with source addressing information
 *X1':x1'*, the NAT is said to have "external source IP address and port"
 hairpinning behavior. This behavior is required for TCP NAT RFC5382.<br>
This requires a NAT to rewrite not only the IP addresses and port numbers in the
 IP and TCP portions of a datagram, but also some of the application payload
 itself. NATs with this capability are sometimes called *NAT editors*. If a NAT
 changes the size of a packet's application payload, considerable work may be
 required. For example, TCP numbers every byte in the data transfer using
 sequence number.<br>
A relatively recent development involves the idea of moving NATs from the
 customer premises into the ISP. This is sometimes called *service provider NAT*
 (SPNAT), *carrier-grade NAT* (CGN), or *large-scale NAT* (LSN) and is intended
 to further mitigate the IPv4 address depletion problem. An arguably more
 pragmatic approach is now being undertaken that combines tunneling, address
 translation, and dual-stack systems in various configurations.

As an alternative to the complexity of placing ALGs and NAT editors in NAT
 devices, an application may attempt to perform its own *NAT traversal*.<br>
When a NAT mapping is established, traffic for a particular application is
 usually permitted to traverse the NAT in both directions. Such mappings are
 narrow; they usually apply only to a single application for its duration of
 execution. These types of mappings are called *pinholes*, because they are
 designed to permit only a certain temporary traffic flow (e.g., a pair of IP
 address and port number combinations). A method that attempts to allow two or
 more systems, each behind a NAT, to communicate directly using pinholes is
 called *hole punching*. The popular Skype peer-to-peer application uses this
 approach (and some others).<br>
Applcations employ a number of methods to determine the addresses their traffic
 will use when passed through a NAT. This is called *fixing* (learning and
 maintaining) the addressing information. There are indirect and direct methods
 for address fixing. The direct methods involve a direct conversation between
 the application and the NAT itself using one or more special protocols (that
 aren not currently IETF standards). An application attempting to fix its
 address without help from the NAT performs the address fixing in a so-called
 unilateral fashion. Applications that do so are said to perform *UNilateral
 Self-Address Fixing* (UNSAF, pronounced "unsafe") [RFC3424][rfc3424]. UNSAF
 involves a set of heuristics and is not guaranteed to work in all cases,
 especially because NAT behaviors vary significantly based on vendor and
 particular circumstance.<br>
One of the primary workhorses for UNSAF and NAT traversal is called *Session
 Traversal Utilities for NAT* (STUN) [RFC5389][rfc5389]. STUN has evolved from a
 previous version called *Simple Tunneling of UDP through NATs*, now known as
 "classic STUN." STUN is a relatively simple client/server protocol that is able
 to ascertain the external IP address and port numbers being used on a NAT in
 most circumstances. It can also keep NAT bindings current by using keepalive
 messages. It is desirable to use STUN servers that are likely to "see" the same
 IP addresses as the peer to which the application ultimately wishes to talk,
 although that may be difficult to determine. Some servers may be discovered
 using DNS Service (SRV) records. The RESPONSE-ORIGIN and OTHER-ADDRESS
 attributes are used by an experimental facility for discovering NAT behavior
 [RFC5780][rfc5780]. STUN can be used to perform address fixing as well as a
 number of other function called *mechanisms*, including DNS discovery, a method
 to redirect to an alternate server, and message integrity exchanges.<br>
*Traversal Using Relays around NAT* (TURN) [RFC5766][rfc5766] provides a way for
 two or more systems to communicate even if they are located behind relatively
 uncooperative NATs. If all NATs were compliant with the BEHAVE specifications,
 TURN would not be necessary. Finding the server's address and the appropriate
 protocol to use for communication is accomplished using a special DNS NAPTR
 record (see [RFC5928][rfc5928]) or by manual configuration. Peers also have
 server-reflexive transport addresses that represent their external addresses.
 These addresses are needed by the client and server to perform the "plumbing"
 necessary to interconnect the client and its peers. The method used to exchange
 this addressing information is not defined within the scope of TURN. Instead,
 this information must be exchanged using some other mechanism (e.g., ICE) in
 order for TURN servers to be used effectively. / Server/peer data is sent using
 straightforward TURN messages traditionally carried in UDP/IPv4. Enhancements
 support TCP [RFC6062][rfc6062] and IPv6 (and also relaying between IPv4 and
 IPv6) [RFC6156][rfc6156]. Establishing an allocation requires thae client to be
 authenticated, usually using the STUN long-term credential mechanism. / TURN
 supports two methods for copying data between a client and its peers. The first
 encodes data using STUN methods called Send and Data, defined in RFC5766, which
 are STUN indicators and therefore not authenticaed. The other uses a
 TURN-specific concept called *channels*. Messages carried over channels use a
 smaller, 4-byte header that is incompatible with the larger STUN-formatted
 messages ordinarily used by TURN. Up to 16K channels can be associated with an
 allocation. Channels were developed to help some applications such as VoIP that
 prefer to use relatively small packets to reduce latency and overhead. / In
 operation, the client makes a request to obtain an allocation using a
 TURN-defined STUN Allocate method. The client must now send refresh messages to
 keep the allocation alive. Allocations expire in 10 minutes if not refreshed,
 unless the client included an alternate lifetime value, encoded as a STUN
 LIFETIME attribute, in the allocation request. Allocations may be deleted by
 requesting an allocation with zero lifetime. / Allocations are represented
 using a "5-tuple." At the client, the 5-tuple includes the client's host
 transport address and port number, server transport address and port number,
 and the transport protocol used to communicate with the server. At the server,
 the same 5-tuple is used, except the client's host transport address and port
 are replaced with its server-reflexive address and port. An allocation may have
 zero or more associated *permissions*, to limit the patterns of connectivity
 that are permitted through the TURN server. Each permission includes an IP
 address restriction. Permissions are deleted if not refreshed within 5 minutes.
 / TURN enhances STUN with six methods, nine attributes, and six error response
 codes. The six methods and their method numbers are as follows: Allocate (3),
 Refresh (4), Send (6), Data (7), CreatePermission (8), and ChannelBind (9).
 CreatePermission establishes or refreshes a permission, and ChannelBind
 associates a particular peer with a 16-bit channel number. / Note that the
 BANDWIDTH attribute has been included in the allocation and refresh success
 indicators. This attribute, defined by a draft version of RFC5766 but
 ultimately deprecated, was intended to hold the peak bandwidth, in kilobytes
 per second, permitted on the allocation. This attribute may be redefined in the
 future. / In addition, certain other traffic contents are not passed through
 from peer to client using TURN. This includes ICMP values, *TTL* (*Hop Limit*)
 field values, and IP *DS Field* values. Also, a requesting TURN client must
 implement the STUN long-term credential mechanism and have some form of login
 credential or account assigned by the TURN server operator. This helps to avoid
 uncontrolled use of open TURN servers but creates somewhat greater
 configuration complexity.<br>
Given the large variety of NATs deployed and the various mechanisms that may be
 necessary to traverse them, a generic facility called *Interactive Connectivity
 Establishment* (ICE) [RFC5245][rfc5245] has been developed to help UDP-based
 applications hosted behind a NAT establish connectivity. ICE is a set of
 heuristics by which an application can perform UNSAF in a relatively
 predictable fashion. In its operation, ICE makes the of other protocols such as
 TURN and STUN. [RFC6544][rfc6544] extends the use of ICE to TCP-based
 applications. ICE works with and extends "offer/answer" protocols, such as the
 SEssion Description Protocol (SDP) used with unicast SIP connection
 establishment [RFC3264][rfc3264]. It is increasingly common to find ICE clients
 incorporated into VoIP applications that use SDP/SIP for establishing
 communications. However, in such circumstances, ICE is used for establishing
 NAT traversal for media streams (such as the audio or video portion of a call
 carried using RTP [RFC3550][rfc3550] or SRTP [RFC3711][rfc3711]), while another
 mechanism, clled *SIP Outbound* [RFC5626][rfc5626], handles the SIP signaling
 information such as who is being called. Although in practice ICE has been used
 primarily with SIP/SDP-based applications, it can also be used as a generic NAT
 traversal mechanism for other applications. One such example is the use of ICE
 (over UDP) with *Jingle* [XEP-0176][xep-0176], defined as an extension to the
 core *Extensible Messaging and Presence Protocol* (XMPP) [RFC6120][rfc6120]. /
 ICE begins by attempting to discover all available candidate addresses.
 Addresses may be locally assigned transport addresses (multiple of the agent is
 multihomed), server-reflexive addresses, or relayed addresses determined by
 TURN. After assigned each address a priority, an *agent* (SDP entity) sends the
 prioritized list to its peer using SDP. The peer performs the same operation,
 resulting in each agent having two prioritized lists. Each agent then forms an
 identical set of prioritized *candidate pairs* by pairing up the two lists. A
 set of *checks* are performed on the candidate pairs in a particular order to
 determine which addresses will ultimately be selected. ICE has several
 optimizations to decrease the latency of agreeing on the selected candidate,
 which are beyond the scope of this discussion ([Trickle ICE][trickle_ice]). The
 candidate pair ultimately selected is determined by a *controlling agent*
 assigned by ICE. The controlling agent *nominates* which valid candiate pairs
 are to be used, according to its order of preference. The controlling agent may
 try all pairs and subsequently make its choice (called *regular nomination*) or
 may use the first viable pair (called *aggressive nomination*). A nomination is
 expressed as a flag in a STUN message referring to a particular pair;
 aggressive nomination is performed by setting the nominate flag in every
 request. / Checks are sent as STUN binding request messages (containing the
 PRIORITY attribute) exchanged between the two agents using the addressing
 information being checked. Checks are initiated by timer, or scheduled as a
 result of an incoming check from a peer (called a *triggered check*). Responses
 arrive in the form of STUN binding responses that contain addressing
 information. In some circumstances this may reveal a new server-reflexive
 address to the agent (e.g., because a different NAT is used between agents from
 the one that was used when the candidate addresses were first determined using
 STUN or TURN servers). Should this happen, the agent gains a new address called
 a *peer-reflexive candidate*, which ICE adds to the set of candidate addresses.
 ICE checks are integrity-checked using STUN's short-term credential mechanism
 and use the STUN FINGERPRINT attribute. When TURN is used, the ICE client uses
 TURN permissions to limit the TURN binding to the remote candidate address of
 interest.

One of the popular systems for building firewalls is included with modern
 versions of Linux and is called `iptables`, built using a network filtering
 capability called [NetFilter][netfilter]. It is the evolution of an earlier
 facility called `ipchains` and provies stateless and stateful packet-filtering
 support as well as NAT and NAPT. `iptables` includes the concepts of filter
 *tables* and filter *chains*. A table contains several predefined chains and
 may contain zero or more user-defined chains. Three predefined tables are named
 as follows: `filter`, `nat`, and `mangle`. The default `filter` table is for
 basic packet filtering and contains the predefined chains INPUT, FORWARD, and
 OUTPUT. The `nat` table contains the chains PREROUTING, OUTPUT, and
 POSTROUTING. The `mangle` table has all five chains. It is used for arbitrary
 rewriting of packets. Each filter chain is a list of rules, and each rule has
 matching criteria and an action. The action (called a *target*) may be to
 execute a special user-defined chain or to perform one of the following
 predefined actions: ACCEPT, DROP, QUEUE, and RETURN. QUEUE means the packet is
 delivered to a user program for arbitrary processing, and RETURN means that
 processing continues in a previously invoked chain, which forms a sort of
 packet filter chain subroutine call.<br>
In most simple routers, NAT can be configured in conjunction with firewall
 rules. In basic Windows systems, NAT is called *Internet Connection Sharing*
 (ICS), and in Linux it is called *IP masquerading*. On Windows XP, for example,
 ICS has a number of special characteristics. It assigns the "internal" IPv4
 address as 192.168.0.1 to the machine running ICS and starts a DHCP server and
 DNS server. At this point, the user may also decide to allow other users to
 control or disable the shared Internet connection. This facility, known as
 *Internet Gateway Device Discovry and Control* (IGDDC), uses the Universal Plug
 and Play framework for controlling a local Internet gateway from a client.<br>
A number of protocols have been developed for supporting communication between
 clients and firewalls. The two most prevalent ones are called *Universal Plug
 and Play* (UPnP) and the *NAT Port Mapping Protocol* (NAT-PMP). The standards
 for UPnP are developed by an industry group called the UPnP Forum [UPNP][upnp].
 NAT-PMP [RFC6886][rfc6886] is supported by most Mac OS X systems. UPnP has
 native support on Windows systems and can be added to Mac OS and Linux systems.
 UPnP is also used in support of consumer electronics device discovery protocols
 for home networks being developed by the Digital Living Network Alliance
 ([DLNA][dlna]). / With UPnp, controlled devices are configured with IP
 addresses based first upon DHCP and using dynamic link-local address
 configuration if DHCP is not available. Next, the *Simple Service Discovery
 Protocol* (SSDP) [XIDS][xids] announces the presene of the device to control
 points (e.g., client computers) and allows the control points to query the
 devices for additional information. SSDP uses two variants of HTTP with UDP
 instead of the more standard TCP. They are called HTTPU and HTTPMU
 [XIDMU][xidmu], and the latter uses multicast addressing. Subsequent control
 and event notification ("eventing") is controlled by the *General Event
 Notification Architecture* (GENA), which uses the *Simple Object Access
 Protocol* (SOAP). UPnP is used for a wide variety of consumer electronic
 devices, including audio and video playvack and storage devices. NAT/firewall
 devices are controlled using the *Internet Gateway Device* (IGD) protocol. IGD
 supports a variety of capabilities, including the ability to learn NAT mapping
 and configure port forwarding. The interested reader may obtain a simple IGD
 client useful for experimentation from the [MiniUPnP Project HomePage][upnpc].
 A second version of UPnP IGD adds general IPv6 support to UPnP. / While UPnP is
 a board framework that includes NAT control and several other unrelated
 specifications, NAT-PMP provides an alternative specifically targeted at
 programmatic communications with NAT devices. NAT-PMP is part of Apple's set of
 Bonjour specifications for zero configuration networking. NAT-PMP does not use
 a discovery process, as the device being managed is usually a system's default
 gateway as learned by DHCP. NAT-PMP supports a simple request/response protocol
 for learning a NAT's outside address and configuring port mappings. It also
 supports a basic eventing mechanism that notifies listeners when a NAT outside
 address changes. The idea of NAT-PMP can be extended for use with SPNAT, as
 proposed by the *Port Control Protocol* (PCP) [RFC6887][rfc6887].

The two major approaches that have been used to support combinations of IPv4 and
 IPv6 are tunneling and translation. The tunneling approaches include Teredo,
 Dual-Stack Lite (DS-Lite), and IPv6 Rapid Deployment (6rd). Although DS-Lite
 involves SPNAT as part of its architecture, a purer translation approach is
 given by the framework described in [RFC6144][rfc6144], which use the
 IPv4-embedded IPv6 addresses.<br>
DS-Lite [RFC6333][rfc6333] is an approach to make transition to IPv6 (and
 support for legacy IPv4 users) easier for service providers that wish to run
 IPv6 internally. In essence, it allows providers to focus on deploying an
 operational IPv6 core network yet provide IPv4 and IPv6 connectiity to their
 customers using a small number of IPv4 addresses. The approache combines
 IPv4-in-IPv6 "software" tunneling [RFC5571][rfc5571] with SPNAT. Whereas
 DS-Lite provides IPv4 access to customers over a service provider's IPv6
 network, 6rd aims to provide IPv6 access to customers over a service provider's
 IPv4 network.<br>
The IPv4/IPv6 translation framework is given in RFC6144. The basic translation
 architecture involves both stateful and stateless methods to convert between
 IPv4 and IPv6 addresses, translations for DNS, and the definition of any
 additional behaviors or ALGs in cases where they are necessary (including for
 ICMP and FTP). In this section, we will discuss the basics of the stateless and
 stateful address translation for IP based on [RFC6145][rfc6145],
 [RFC6146][rfc6146], and the addressing from [RFC6052][rfc6052].<br>
*Stateless IP/ICMP Translation* (SIIT) refers to a method of translating between
 IPv4 and IPv6 packets without using state tables RFC6145. The translation is
 performed without table lookups and uses IPv4-translatable addresses along with
 a defined scheme to translate IP headers. For the most part, IPv4 options are
 not translated (they are ignored), nor are IPv6 extension headers (except the
 Fragment header).<br>
In stateful translation, *NAT64* RFC6146 is used to support IPv6-only clients
 communicating with IPv4 servers. It is compatible with the NAT traversal
 techniques (e.g., ICE, STUN, TURN) we discussed previously. Lacking these
 additional protocols, NAT64 support dynamic translation only for IPv6 hosts
 initiating communications with IPv4 hosts. NAT64 handles fragments differently
 from its stateful counterpart. For arriving TCP or UDP fragments where the
 transport checksum is nonzero, the NAT64 may either queue the fragments and
 translate them together or translate them individually. A NAT64 must handle
 fragments, even those arriving out of order. A NAT64 may be configured with a
 time limit (at least 2s) bounding the time during which fragments will be
 cached. Otherwise, the NAT could be subject to a DoS attack resulting from the
 exhaustion of packet buffers holding fragments.

[rfc3027]: https://tools.ietf.org/html/rfc3027
[rfc3235]: https://tools.ietf.org/html/rfc3235
[rfc3022]: https://tools.ietf.org/html/rfc3022
[rfc5382]: https://tools.ietf.org/html/rfc5382
[rfc5128]: https://tools.ietf.org/html/rfc5128
[rfc4787]: https://tools.ietf.org/html/rfc4787
[rfc5508]: https://tools.ietf.org/html/rfc5508
[rfc5902]: https://tools.ietf.org/html/rfc5902
[rfc4864]: https://tools.ietf.org/html/rfc4864
[rfc6296]: https://tools.ietf.org/html/rfc6296
[rfc3424]: https://tools.ietf.org/html/rfc3424
[rfc5389]: https://tools.ietf.org/html/rfc5389
[rfc5766]: https://tools.ietf.org/html/rfc5766
[rfc5928]: https://tools.ietf.org/html/rfc5928
[rfc6062]: https://tools.ietf.org/html/rfc6062
[rfc6156]: https://tools.ietf.org/html/rfc6156
[rfc5245]: https://tools.ietf.org/html/rfc5245
[rfc6544]: https://tools.ietf.org/html/rfc6544
[rfc3264]: https://tools.ietf.org/html/rfc3264
[rfc3550]: https://tools.ietf.org/html/rfc3550
[rfc3711]: https://tools.ietf.org/html/rfc3711
[rfc5626]: https://tools.ietf.org/html/rfc5626
[xep-0176]: https://xmpp.org/extensions/xep-0176.html
[rfc6120]: https://tools.ietf.org/html/rfc6120
[trickle_ice]: https://tools.ietf.org/html/draft-ietf-ice-trickle-08
[netfilter]: http://netfilter.org/
[upnp]: https://openconnectivity.org/
[rfc6886]: https://tools.ietf.org/html/rfc6886
[dlna]: https://www.dlna.org/
[xids]: https://tools.ietf.org/html/draft-cai-ssdp-v1-03
[xidmu]: https://tools.ietf.org/html/draft-goland-http-udp-01
[upnpc]: http://miniupnp.free.fr/
[rfc6887]: https://tools.ietf.org/html/rfc6887
[rfc6144]: https://tools.ietf.org/html/rfc6144
[rfc6333]: https://tools.ietf.org/html/rfc6333
[rfc5571]: https://tools.ietf.org/html/rfc5571
[rfc6145]: https://tools.ietf.org/html/rfc6145
[rfc6146]: https://tools.ietf.org/html/rfc6146
[rfc6052]: https://tools.ietf.org/html/rfc6052

## Chapter 8. ICMPv4 and ICMPv6: Internet Control Message Protocol

## Chapter 9. Broadcasting and Local Multicasting (IGMP and MLD)

## Chapter 10. User Datagram Protocol (UDP) and IP Fragmentation

## Chapter 11. Name Resolution and the Domain Name System (DNS)

## Chapter 12. TCP: The Transmission Control Protccol (Preliminaries)

## Chapter 13. TCP Connection Management

## Chapter 14. TCP Timeout and Retransmission

## Chapter 15. TCP Data Flow and Window Management

## Chapter 16. TCP Congestion Control

## Chapter 17. TCP Keepalive

## Chapter 18. Security: EAP, IPsec, TLS, DNSSEC, and DKIM

