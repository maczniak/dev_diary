# [Mastering Monero][homepage] by SerHack and Monero Community, self-publishing (2018)

(content) CC BY-NC-SA 4.0, (cover and images) CC BY-NC-ND 4.0, [GitHub][github]

 [Monero Forum Funding System (FFS)][monero_ffs]

[homepage]: https://masteringmonero.com/
[github]: https://github.com/monerobook/
[monero_ffs]: https://forum.getmonero.org/22/completed-tasks/88414/monero-integrations-second-ffs

## Chapter 1 Introduction to cryptocurrencies & Monero

In 2013 Nicolas van Saberhagen published the "CryptoNote" protocol, which became
 the foundation for many coins, starting with Bytecoin. Like Bitcoin's Satoshi
 Nakamoto, the creator of Bytecoin remained anonymous and promoted their coin
 through a Bitcointalk thread. Bitcointalk member "*thankful_for_today*"
 investigated the emissions curve and noted that approximately 82% of the coins
 had already been emitted. The Monero cryptocurrency, spearheaded by
 thankful_for_today, launched in April 2014. The coin was originally named
 "BitMonero," however the community quickly elected to shorten it to "Monero,"
 which is the word for "coin" in the Esperanto language.<br>
[Monero Malware Response Workgroup][monero_malware_response_workgroup]
[Project Coral Reef][project_coral_reef] (online stores that accept Monero)

[monero_malware_response_workgroup]: https://web.getmonero.org/2018/09/26/Introducing-the-Monero-Malware-Response-Workgroup-Website.html
[project_coral_reef]: https://www.projectcoralreef.com

## Chapter 2 Getting started: receiving, storing and sending Monero

moneroj<br>
Kastelo (open-source hardware wallet for Monero)<br>
Each Monero account has a single primary address (starting with a '4'). For
 convenience, you can generate an unlimited number of subaddresses (starting
 with an '8'). Funds received by any of the addresses are routed to your
 wallet's main balance.<br>
Initializing a view-only wallet involves sharing a *secret view key*.<br>
If you are sending moneroj to a business, they may also ask you to include a
 *payment ID* to connect your payment to your order. Some services use
 "integrated addresses", which include the payment ID and address in a single
 text string for convenience and increased privacy. In 2018, Monero added the
 ability for each wallet to generate a vast number of *subaddresses* for
 receiving payments. Consequently, payment IDs and integrated addresses are
 being used less frequently. Anybody using the free [OpenAlias][openalias]
 system can provide a human-readable Monero address instead of the raw address
 string.<br>
Besides optional payment IDs, Monero has a second feature to selectively reveal
 proof that funds were sent. This is accomplished by sharing a *transaction key*
 that only the true sender can generate. The Monero sender is always
 unknown.<br>
There is a famous saying "Not your keys? Not your Bitcoin!" referring to wallets
 and services that retain control over your keys, and thus your funds.<br>
The [Monero Integrations][monero_integrations] payment gateway allows any online
 shop to add a Monero payment option by simply installing any one of the plugins
 designed for several popular content management systems. [Kasisto][kasisto] was
 the first point of sale system that accepted Monero, and is an open-source
 project requiring no third parties. Another payment option is [GloBee][globee],
 which allows merchants to accept both cryptocurrency and credit card payments.

[openalias]: https://openalias.org
[monero_integrations]: https://monerointegrations.com
[kasisto]: https://github.com/amiuhle/kasisto
[globee]: https://globee.com

## Chapter 3 How Monero works

RingCT is a cryptography technology that conceals the amount of moneroj being
 sent in any transaction. RingCT was activated in January 2017, and rapid
 widespread adoption immediately followed. Within 1 month of its activation,
 approximately 98% of new transactions were already voluntarily using the RingCT
 protocol! In keeping with Monero's policy of enforced privacy-by-default,
 **RingCT became mandatory for all Monero transactions after September 2017**.
 To spend any old pre-RingCT outputs, they must first be converted to RingCT
 outputs with masked amounts.<br>
Your Monero wallet's public addresses are 95-character strings, which
 incorporate two public keys (the *public view* and *public spend* keys)
 mathematically derived from your seed. When somebody sends you funds, they will
 use the public keys in your address along with some random data to generate a
 unique one-time public key. These one-time public keys that are recorded in
 transactions on the blockchain named *stealth addresses*.<br>
Ring signatures are used in Monero to blend the keys from multiple outputs on
 the blockchain. Monero outputs can appear in ring signatures before and after
 they have been spent, so output reuse must be prevented by other means. This is
 accomplished by utilizing *key images* that are generated and recorded with
 each transaction, uniquely derived from the actual output being spent. Starting
 in 2016, the network began requiring two ring members for each signature,
 enforcing privacy-by-default for the sender. This was raised to a minimum *ring
 size* of five plausible signers in late 2017, and increased again to a minimum
 of seven potential signers in early 2018. When the vast majority of
 transactions use the minimum ring size, larger custom ring sizes stand out as
 unusual, which is counterproductive for privacy. This was addressed in the late
 2018 network upgrade; instead of specifying a minimum ring size, network
 policies now mandate a fixed ring size. At the time of writing, all
 transactions must use ring signatures with exactly eleven members.<br>
The Onion Router (Tor) was originally developed by the United States Naval
 Research Laboratory. The Monero community has also supported the development of
 Kovri, a privacy approach based on the decentralized Invisible Internet Project
 (I2P) specifications. Until Kovri or a similar feature is directly integrated
 into Monero, any users with particular concerns about internet traffic analysis
 are encouraged to use a Tor or a reliable VPN provider.

## Chapter 4 The Monero network

60 GB at the time of writing<br>
When Monero was launched, the coin emission rate was more than 30 XMR every 2
 minutes. This reward will "smoothly" decrease until it reaches 0.6 XMR per
 2-minute block in 2022. After 2022, Monero's *tail emission* will stay
 constant, guaranteeing that mining a block will always reward 0.6 XMR. The
 annual supply increase from Monero's 0.6 XMR tail emission is less than 1% per
 year.<br>
Monero uses a variation of the CryptoNight PoW algorithm. In early March 2018,
 the Monero community was shocked to realize that CryptoNight ASICs had been
 secretly produced and were mining Monero! These devices purported to mine
 Monero 25 times faster than the leading GPUs, and retrospective hashrate
 analysis suggests that they accounted for nearly half of the Monero network
 hashrate in late 2017 and early 2018. The Monero community reacted quickly,
 proactively taking steps to mitigate ASIC mining before the existence of the
 devices was even fully confirmed. The spring 2018 Monero routine upgrade
 included a minor tweak to the CryptoNight algorithm. When the minor CryptoNote
 tweak was implemented at block 1546000, the ASICs became instantly incompatible
 with the network, and approximately half of the total hashrate vanished. To
 permanently discourage ASICs, Monero now slightly changes the mining algorithm
 at each network update (routine hard forks every 6 months).

## Chapter 5 A deep dive into Monero & cryptography

Recently, it has become clear that a NIST-backed PRNG is flawed, and contains a
 [potential backdoor][dual_ec_drbg]. Since the NIST4 standard algorithms have
 had recent issues, and the Twisted Edwards curve was selected to address many
 concerns held by the cryptography community.<br>
four keys: public view key (used to verify the validity of addresses), private
 view key (used for viewing data such as the balance, fees and transactions
 amounts. the view key cannot create or sign transactions), public spend key,
 private spend key<br>
Monero uses the CryptoNight PoW system, which employs a special *CryptoNote*
 hash algorithm, which is build on the *Keccak* hash.<br>
The mnemonic phrase essentially converts the above 256-bit number into a
 24-*digit* (24-word) base-1626 "number". When your wallet presents the 24-word
 seed, it adds a 25th word that functions as a *checksum*. Monero's mnemonic
 method encodes with a minimum 4:3 ratio. In other words, four bytes create
 three words, plus one checksum word; eight bytes creates six words, plus one 
 checksum word; and so on.<br>
The Monero *seed* described above is actually your *secret spend key*, from
 which all other keys are derived. The *secret view key* is the reduced hash of
 your secret spend key, converted to a valid scalar for the ed25519 curve
 (`sc_reduce32` function).<br>
The inability to see outgoing transactions from a view-only wallet is a feature,
 not a bug! If the outgoing transactions were made public, it would reveal when
 an output has been spent. This would be very problematic, since ring
 singnatures rely on spend state ambiguity. Suppose a charity reveals when an
 output is spent; all appearances in future (and previous) ring signatures can
 be identified as decoys.<br>
A Monero wallet's standard address is composed of the two public keys (the
 public spend key + public view key) derived in the last section. It also
 contains a checksum and a *network byte* (code value) which identifies both the
 network and the address type. The hash digest is shortened to the first 4
 bytes, and used as the checksum. Lastly, this 69-byte output string is encoded
 into the Monero base-58 format. This conversion increases the length to a
 95-character string.

| Name                      | Code value | ASCII value for prefix |
|---------------------------|------------|------------------------|
| Main net primary address  | 18         | 4                      |
| Main net subaddress       | 42         | 8                      |
| Test net primary address  | 53         | 9                      |
| Test net subaddress       | 63         | B                      |
| Stage net primary address | 26         | 5                      |
| Stage net subaddress      | 36         | 7                      |

You can avoid this risk by generating multiple subaddresses, sharing a unique
 one with each sender.

PS<sub>i</sub> = Hs(pV<sub>0</sub>||i)G+PS<sub>0</sub> (in practice, the
 reference client wallet also concatenates the string `SubAddr` to the data, as
 a common salt for the hashing.)<br>
PV<sub>i</sub> = pV<sub>0</sub>\*PS<sub>i</sub>

When constructing a transaction, wallets typically generate 32 random bytes to
 serve as the *primary key*. When sending to a primary address, this random key
 is multiplied by the elliptic curve generator point to yield the transaction
 public key. However, when sending to a subaddress, the private transaction key
 is instead multiplied by the public spend key of the receiving subaddress.<br>
The CryptoNote protocol calculates the receiving one-time address according to
 the formula `X = Hs(r*PV|i)G + PS`. George's wallet scan over every transaction
 in the blockchain to identify outputs for which `X = Hs(pV*R|i)G + PS`.<br>
The MyMonero wallet family uses a method similar to the Electrum convention,
 however the seed phrase is 13 words instead of the usual 25 words. The 13 words
 convert to a 128-bit integer that is used for both spend and view key
 derivation. The seed integer is hashed with Keccak-256 and converted to the
 private spend key. This private spend key is hashed again with Keccak-256 and
 converted to the private view key.<br>
RingCT was implemented in January 2017 and became mandatory in all transactions
 after September 2017. All non-coinbase transactions employ RingCT to encrypt
 the transaction amount. The amount for each transaction is encrypted two
 different ways. First, the amount is encrypted by a key derived from the public
 information in the recipient's address. This version is recorded in the
 ecdhInfo field. Secondly, the amount is integrated into a Pedersen commitment,
 which allows other Monero users to verify the validity of the transaction
 themselves. The range proof demonstrates that the masked number can be
 generated as the sum of positive powers of 2, without revealing what those
 powers are.<br>
The private key owned by the active signer is mixed with public key information
 from the other members (other outputs from past transactions) to produce a
 single signature.<br>
c<sub>3</sub> = H<sub>s</sub>(M,uG,uH<sub>p</sub>(P<sub>2</sub>))<br>
c<sub>1</sub> = H<sub>s</sub>(M, s<sub>3</sub>G + c<sub>3</sub>P<sub>3</sub>,
 s<sub>3</sub>H<sub>p</sub>(P<sub>3</sub>) +
 c<sub>3</sub>p<sub>2</sub>H<sub>p</sub>(P<sub>2</sub>))<br>
c<sub>2</sub> = H<sub>s</sub>(M, s<sub>1</sub>G + c<sub>1</sub>P<sub>1</sub>,
 s<sub>1</sub>H<sub>p</sub>(P<sub>1</sub>) +
 c<sub>1</sub>p<sub>2</sub>H<sub>p</sub>(P<sub>2</sub>))<br>
random numbers s<sub>3</sub> and s<sub>1</sub>, s<sub>2</sub> = u -
 c<sub>2</sub>p<sub>2</sub>, J = p<sub>2</sub>H<sub>p</sub>(P<sub>2</sub>)<br>
The signature you send to the blockchain and the world contains several
 quantities: (c<sub>1</sub>, s<sub>1</sub>, s<sub>2</sub>, s<sub>3</sub>,
 J).<br>
c<sub>3</sub> = H<sub>s</sub>(M, s<sub>2</sub>G + c<sub>2</sub>P<sub>2</sub>,
 s<sub>2</sub>H<sub>p</sub>(P<sub>2</sub>) +
 c<sub>2</sub>p<sub>2</sub>H<sub>p</sub>(P<sub>2</sub>))<br>
Observe that the key image J = p<sub>2</sub>H<sub>p</sub>(P<sub>2</sub>) was
 uniquely calculated from the true output's keypair, without any random numbers
 or decoys' public keys. Thus, any fraudulent attempts to spend the output a
 second time will generate an identical key image.<br>
The above example of a Back-style LSAG ring signature is included for
 educational purposes.<br>
[Zero to Monero][zero_to_monero], a highly-technical mathematical tour that is
 also available as a free community-funded PDF

LMDB is written in C++, and is developed by Symas Corporation. Here are a few
 LMDB features:
* Arbitrary key/data pairs storage as byte arrays
* Range-based search capability
* Support for a single key with multiple data items
* Advanced methods for appending records at the end of the database, which gives
  a dramatic write performance increase over other similar stores

Each valid block contains a single *base transaction* that routes its coinbase
 reward to the miner.<br>
transaction structure?<br>
Base Reward = 2 * ((S - A) * 2<sup>-20</sup> * 10<sup>-12</sup>), current supply
 (A), initial number of atomic units (S = 2<sup>64</sup> - 1), atomic unit
 (1x10<sup>-12</sup> XMR)<br>
Penalty = BaseReward * ((B / M<sub>N</sub>) - 1)<sup>2</sup><br>
Miners receive the full reward for any sized block up to 300kB; for anything
 larger, the penalty function "kicks in". The maximum block size is
 2\*M<sub>N</sub>, at which point the entire coinbase is withheld.<br>
If the median size of the last 100 blocks grows larger than the penalty-free
 block size (300 kB), the dynamic fee algorithm comes into play!<br>
Fee per kB = (R/R<sub>0</sub>) * (M<sub>0</sub>/M) * F<sub>0</sub> * (60/300) *
 4
* 60/300 is the adjustment factor to account for the increase of the
  penalty-free block size limit (adjusted from 60 kB to 300 kB in 2017)
* 4 is the adjustment factor to account for the default free multiplier (the
  lowest fee level uses a multiplier of x1, and a normal priority transaction
  uses x4)

Ideally, an increase Monero's exchange rate and usage would result in a
 reduction of absolute fees (i.e. in terms of XMR). The dynamic fee algorithm is
 designed to function when the median block size is consistently above 300
 kB.<br>
Bulletproofs employ some clever mathematical tricks to construct the range proof
 with a
 [more efficient mechanism][efficient_zk_arguments_for_arithmetic_circuits].
 This reduces the size of a single range proof (from ~7 kB) to ~2 kB! With
 bulletproofs, size instead scales logarithmically with more outputs.
 Bulletproofs were enabled by the Monero v0.13.0 network upgrade in October
 2018, as an opt-in feature that will become mandatory during the subsequent
 upgrade.

[dual_ec_drbg]: https://en.wikipedia.org/wiki/Dual_EC_DRBG
[zero_to_monero]: https://www.getmonero.org/library/Zero-to-Monero-1-0-0.pdf
[efficient_zk_arguments_for_arithmetic_circuits]: https://eprint.iacr.org/2016/263

## Chapter 6 Community and contributing

At the time of writing, some components of the Monero ecosystem are shifting
 their repositories from GitHub to GitLab.<br>

## Chapter 7 Monero integration for developers

OpenAlias (human-readable) and the Monero URI (machine-readable)<br>
There is a famous trilemma known as [Zooko's triangle][zookos_triangle] that
 describes the inherent difficulty of designing name systems that simultaneously
 meet three criteria: secure, decentralized, and human-meaningful.<br>
The OpenAlias standard is a text DNS record on a fully-qualified domain name
 (FQDN). Each text record need only contain two pieces of information: the
 `prefix` (`oa1:xmr`), and the `recipient_address`. A `recipient_name` key-value
 pair can be added as well, however it is not necessary. Key-value pairs are
 separated by a semi-colon and, optionally, a space for legibility.<br>
`monero:4BKj...?tx_amount=0.0413&tx_description=...`, `tx_payment_id=...`,
 `recipient_name=...`<br>
The Monero wallet RPC (`monero-wallet-RPC`) allows you to manage all wallet
 functionality through JSON calls.<br>
The `getbalance` method returns two outputs: the [total] `balance` and the
 `unlocked_balance`, which only includes transactions deep enough in the
 blockchain to be considered "safe" for spending.

[zookos_triangle]: https://en.wikipedia.org/wiki/Zooko%27s_triangle

## Chapter 8 Wallet guide and troubleshooting tips

