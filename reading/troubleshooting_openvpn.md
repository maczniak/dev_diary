# [Troubleshooting OpenVPN][homepage] by Eric F Crist, Packt Publishing (2017)

2.3.11

[homepage]: https://www.packtpub.com/networking-and-servers/troubleshooting-openvpn

## 1. Troubleshooting Basics

The final pager of note is [`most`][most], which operates similar to `less`, but
 adds the capability for multiple windows within a single terminal session. The
 `most` pager also appears to support color escape sequences better than `less`.
 But the latest release of `most` was in 2007.<br>
[tcpdump tutorial][tcpdump_tutorial]<br>
`mtr` is a utility that combines the functionality of `ping` and
 `traceroute`.<br>
OpenVPN also supports [PolarSSL][polarssl] (recently known as ARM mbed) as a
 replacement for OpenSSL. The latest package, 2.2.1, includes some rudimentary
 programs for certification creation.
`openssl s_client -showcerts -connect openvpn.net:443`<br>
`openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes`,
 `sudo openssl s_server -key key.pem -cert cert.pem -WWW -accept 443`<br>
decipher SSL and TLS encrypted streams: Preferences → Protocols → SSL<br>
[OpenVPN page][wireshark_openvpn] in Wireshark wiki

[most]: http://www.jedsoft.org/most/
[tcpdump_tutorial]: https://danielmiessler.com/study/tcpdump/
[polarssl]: https://tls.mbed.org/
[wireshark_openvpn]: https://wiki.wireshark.org/OpenVPN

## 2. Common Problems

A common option used by OpenVPN administrators is to route all traffic through
 the VPN (see the option `--redirect-gateway`).<br>
[Dia][dia]<br>
Once a VPN is up and running successfully, it's a good practice to document the
 configuration of all aspects. This should include kernel changes such as
 `sysctl`, compiled options, network interface values, firewall rules, and
 routing tables. Having a flow chart of your authentication scheme is also
 useful.<br>
If the `tun` or `tap` device is not supported, it quickly rules out OpenVPN or
 limits the specific features of OpenVPN. Both iOS and Android do not support
 the `tap` device. There are other operating systems that don't support
 virtualized network devices at all. FreeBSD jails, for example, don't support
 the `tun` or `tap` devices without some significant configuration and startup
 tricks. Many embedded operating systems, generally on routers and switches, do
 not fully support OpenVPN. When evaluating an embedded firmware or platform,
 make certain that it supports either the `tap` or `tun` virtual network
 devices. The latter is most important, as it is the most common, and correct,
 device to use.<br>
OpenVPN is a unique protocol in a family of SSL-based VPNs. OpenVPN will not
 work with other protocols including other SSL VPNs, such as Cisco's AnyConnect
 or non-SSL-based VPN protocols such as IPSec, Point-to-Point Tunneling Protocol
 (PPTP), or others.<br>
[NAT with Linux and iptables][nat_with_linux_and_iptables],
 [OpenVPN HOWTO][openvpn_howto]<br>
The SSL decryption capabilities will generally work fine for normal web browsing
 traffic, but other applications that use TLS will break when they pass through
 such a firewall with this feature enabled.<br>
By default, OpenVPN expects each remote client to connect using a unique
 certificate for identification and encryption purposes. The common name (CN),
 is used to generate configuration options, identify a persistent IP
 (`--ifconfig-pool-persist`), and CCD (`--client-config-dir`) entries.

[dia]: http://dia-installer.de/
[nat_with_linux_and_iptables]: https://www.karlrupp.net/en/computer/nat_tutorial
[openvpn_howto]: https://openvpn.net/index.php/open-source/documentation/howto.html

## 3. Installing OpenVPN

Layer 2 (data link) traffic in the OSI model requires the `tap` adapter. This is
 useful for various routing protocols and applications or games that depend on
 broadcast traffic. More commonly, layer 3 (network) traffic is all that is
 required, which uses the `tun` adapter. Linux aliases the bridging Ethernet
 pseudo adapter to `tap` and `bond`, with a distinct `tun` kernel module.
 FreeBSD, on the other hand, includes both the `tun` and `tap` functionality in
 the `if_tap.ko` kernel module.<br>
On Windows, the OpenVPN project provides the [TAP-Windows][tap_windows] virtual
 network adapter. There are two distinct methods to install additional virtual
 network adapters: the `tapinstall.exe` utility and the Control Panel new
 hardward wizard. This tool can be used to query what adapters are currently
 installed, as well as adding or removing devices:
 `openvpn.exe --show-adapters`, `tapinstall.exe hwids tap*`,
 `tapinstall.exe install ..\driver\OemVista.inf tap0901`,
 `tapinstall.exe remove tap0901`<br>
Mobile platforms do not have a native OpenVPN open source build.<br>
There is an application (OpenVPN Connect), provided by the commercial venture of
 James Yonan, but that currently uses a large amount of experimental and
 out-of-band source that isn't shared with the community. There are known
 limitations and certain incompatibilities between the commercial application
 and other applications.

[tap_windows]: http://community.openvpn.net/openvpn/wiki/ManagingWindowsTAPDrivers

## 4. The Log File

The `--log-append` option works nearly identically to the `--log` option, except
 that the file, if it already exists, will be appended to, rather than
 truncated. Any of the other log options will supersede the `--daemon` option's
 Syslog call. If the `--daemon [program_name]` option specified, `program_name`
 will prepended to all Syslog lines related to OpenVPN.<br>
Verbosity level 4 (`--verb 4`) is the most useful in the majority of
 troubleshooting scenarios.<br>
LZO compression must be either enabled or disabled at both ends of the OpenVPN
 connection. During a connection initialization, the both endpoints perform a
 remote options hash to determine compatibility of the other side in the context
 of configuration. "Initialization Sequence Completed" message does not
 guarantee that you have a working and useful VPN, OpenVPN doesn't truly
 understand your entire routing table and the entirety of devices involved.
 This message simply illustrates that the OpenVPN process at both ends has
 successfully negotiated cryptographic keys, option parsing, and is prepared and
 ready to start doing the things you've asked of it.

## 5. Client and Server Startup

Another more recent advent is `polkit`, which allows the Linux administrator to
 instruct the system that certain users or groups can perform specific actions.
 `polkit` can be used to provide a normal user to make interface and routing
 table changes.<br>
The encapsulated traffic is already going to be engineered to handle either
 transmission assurance (TCP) or packet loss and delay in a graceful manner. So,
 using UDP for the overall VPN traffic, we allow the application transiting the
 VPN to handle an connection quality issues. Sometimes using TCP for a VPN
 tunnel is unavoidable, but do so if you can. The community support staff often
 references two links for why TCP within TCP is a bad idea:
 [Why TCP Over TCP Is A Bad Idea][why_tcp_over_tcp_is_a_bad_idea] and
 [Encapsulating Protocols][encapsulating_protocols]. Because of this
 *connectionless* state of a UDP tunnel, neither the client or server truly
 know when the link to the other end has gone away of failed. To help deal with
 lost connections, OpenVPN has the `--ping` and `--ping-restart` options. Using
 the `--ping` option, OpenVPN will spend periodic ping packets across VPN to the
 remote endpoint to keep these fake *keep-state* sessions active. Without this,
 the firewall may determine no further traffic is expected and shut down the
 sesion.<br>
There is a client-side option available named `--explicit-exit-notify`, which
 will the client system to notify the remote OpenVPN server that it is exiting.

```sh
#!/bin/sh

# Test OpenVPN combined --up/--down script

set -x # print each executed statement
exec 2>&1
printenv > /tmp/ovpn-env.$$

logger -p local3.notice -t LOGTEST "OpenVPN running as `whoami` for $script_type script."
```

[why_tcp_over_tcp_is_a_bad_idea]: http://sites.inka.de/~bigred/devel/tcp-tcp.html
[encapsulating_protocols]: https://openvpn.net/papers/BLUG-talk/14.html

## 6. Certificates and Authentication

When an option such as `--ping-restart` is used, the OpenVPN process will
 attempt to restart itself, requiring a re-read of the certificate, keys, and
 configuration. If privileges have been dropped to a user that does not have
 read access to these files or paths, the restart will fail and OpenVPN will
 exit. To accommodate this scenario, there are a pair of options that allow the
 OpenVPN process to reuse or retain data that was read before privileges were
 dropped. The `--persist-tun` option instructs OpenVPN to reuse the existing
 `tun` or `tap` device and to not re-execute the `--up` or `--down` scripts.
 Finally, the `--persist-key` option instructs OpenVPN not to re-read key files
 during `SIGUSR1` or `--ping-restart`.<br>
If your private keys become available or are easy to read, a client or other
 (OpenVPN, LDAP, mail, and so on) server could potentially be impersonated. If
 your CA key is exposed, there is potential for rogue-signed certificates that
 your existing systems would trust.<br>
In nearly every OpenVPN configuration I have seen deployed, the CA is going to
 be a self-signed unit that will not pre-exist in the operating system trust
 store. For this reason, OpenVPN client packages will contain, at a minimum, a
 configuration file and the CA certifcate. Most clients will require many pieces
 of certificate data: the CA certificate, the client certificate, and the client
 key. All of these can be embedded inline in a configuration file. It is not
 enough to include only the topmost root CA certificate; intermediate/sub CAs
 must also be included. Using the OpenVPN community server certificate, we can
 leverage the OpenSSL `verify` command to verify a certificate chain:
 `openssl verify -CAfile ca-root.crt ca-int.crt`,
 `openssl verify -CAfile ca-root.crt -untrusted ca-int.crt server.crt`<br>
For global PKI systems, a server certificate key may have been exposed or lost,
 or the operator may have needed to change the common name (CN) of the
 certificate. In the case of OpenVPN, a user certificate may be added to the
 local CRL because the employee left the company, or perhaps a given OpenVPN
 server has been decommisssioned so that server certificate is no longer
 required. There is talk of OpenVPN 3.0 adding support for CRL Distribution
 Points (CDPs) that would allow the client to query a special URL, LDAP, or
 other source to pull *on-the-fly* CRL data. As of OpenVPN 2.3.13, there is no
 message passed to the client indicating a connection failure is due to a
 revoked certificate. On the server side, however, we are given a very clear CRL
 error. We can verify the serial number of the certificate by querying the CRL
 file: `openssl crl -noout -text -in ../ssl-admin/prog/crl.pem`<br>
It is possible to remove the requirement for client certificates using
 `--client-cert-not-required` (deprecated in 2.4, removed in 2.5 in favor of
 `--verifty-client-cert`). In this case, authentication rests solely upon the
 `--auth-user-pass-verify` option. If `--client-config-dir` is still desired
 without client certificates, you will need to leverage
 `--username-as-common-name`. Of course, if you're going to require usernames
 and passwords, it is necessary to add the `--auth-user-path` option to all the
 client cofiguration files. OpenVPN can read usernames and passwords from a
 file, preventing a prompt on the client side. This is used with the
 `--auth-user-pass <file>` option where `<file>` is the path to a file
 containing the username and password on separate lines. The
 `--client-config-dir` option is often used to apply client-specific
 configuration and routing. OpenVPN provides a related option,
 `--ccd-exclusive`, which will prevent client connections from clients who do
 not have a file in the `client-config` directory.

## 7. Network and Routing

UDP test - (server) `nc -ukl 1194`, (client) `nc -u 203.0.113.9 1194`<br>
In UDP, there is no equivalent to a TCP SYN packet. However, if a UDP packet is
 sent to a port that is not open, the system will respond with an ICMP port
 unreachable message. However, if a port is blocked by a firewall, this method
 will falsely report that the port is open. If the port unreachable message is
 blocked, all ports will appear open. This method is also affected by ICMP rate
 limiting. As far as I know, the best way to scan UDP ports is to send an
 application specific probe packet with tools such as `nmap` and `nessu`s. Even
 this method is not really foolproof--in some cases, a service may be listening
 on the port, but configured not to respond to the particular probe packet.
 [source][unreliable_udp_port_check]<br>
OpenVPN provides a mechanism (`--tls-auth`) using a set of pre-shared keys to
 cryptographically sign every packet between the server and client. The
 mechanism for this is the same secret key used for a static-key OpenVPN setup,
 as was the original release. If the key direction is incorrect, or the
 pre-shared keys are out of sync, your VPN clients will be unable to connect,
 and errors will manifest as connectivity issues.<br>
When the admin wants VPN clients to connect to more than other VPN clients, it
 is generally necessary to push additional routes to those clients. These routes
 can be both internal and extenal to the VPN and can even include other OpenVPN
 processes. It is a good rule of thumb that for every pushed route, there should
 be a route in the server configuration, and vice versa. There are three primary
 steps to establish a full route within OpenVPN: 1. Establish process-specific
 routes (`--iroute`), 2. Apply necessary kernel routes (`--route`), 3. Push
 routes to clients (`--push "route ..."`).<br>
To push a new default gateway to OpenVPN clients, the `--redirect-gateway`
 configuration directive is provided. This directive does two primary things to
 create a new default route. First, it creates a static route (`0.0.0.0/0`) for
 the OpenVPN server, pointing to the current default gateway. Second, it creates
 two less-specific routes (`0.0.0.0/1` and `128.0.0.0/1`) functionally providing
 a new default, without deleting the original route.<br>
The OpenVPN server needs to NAT the VPN client traffic, and IP forwarding needs
 to be enabled.<br>
By adding the `--mut-test` option to your configuration or passing it on the
 command line, OpenVPN will attempt to calculate the largest packet your VPN is
 capable of processing. The BSD (on both OS X and FreeBSD) ping has some
 *sweeping* options for the packet size argument. This allows you to, without
 the need for an external `for-loop`, gradually increase the packet size until
 one begins to fail: `ping -G 1500 -g 1350 -h 10 -D 192.168.80.1`. The
 `--fragment` option forces the OpenVPN process to handle packet fragmentation
 for UDP packets. We can also add `--mssfix` to notify TCP connections of our
 reduced MTU, which will offload the packet fragmentation to the application or
 client system, reducing the load on the OpenVPN process.

```sh
#!/bin/sh
SIZE=10
MAX=1500
MTU=1350
while [ $MTU -lt $MAX ];
do
    ping -s $MTU -M do -c 1 10.3.14.255
    if [ $? -eq 1 ];
    then
        echo "MTU size is $MTU"
        exit
    fi
    MTU=`expr $MTU + $SIZE`
done
```

[unreliable_udp_port_check]: http://www.digitalinternals.com/unix/unix-linux-netcat-check-port-open/511/

## 8. Performance

OpenVPN is single-threaded. In various tests in recent years, a realistic limit
 of about 200 client connections is considered the maximum before performance
 falls off considerably. It is possible to work around this limitation using
 load balancing across multiple OpenVPN server instances.

## 9. External Problems

Wireshark recognizes the OpenVPN protocol and HMAC headers.<br>
In the wild, projects such as [obfsproxy][obfsproxy_1] ([1][obfsproxy_2])
 encapsulate VPN or other traffic inside an HTTPS tunnel, making it appear as
 normal web browsing.<br>
[OpenVPN Watchdog][openvpn_watchdog] (OpenVPN IP Leak and DNS Leak Preventer)

[obfsproxy_1]: https://www.torproject.org/docs/pluggable-transports.html.en
[obfsproxy_2]: https://community.openvpn.net/openvpn/wiki/TrafficObfuscation
[openvpn_watchdog]: http://www.anonyproz.com/

