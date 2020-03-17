# [Programming Bitcoin: Learn How to Program Bitcoin from Scratch][homepage] by Jimmy Song, O'Reilly (2019)

[GitHub (CC-BY-NC-ND license)][github], [two day seminar of the author][seminar]

[homepage]: https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/
[github]: https://github.com/jimmysong/programmingbitcoin
[seminar]: https://programmingblockchain.com/

## 1. Finite Fields

It turns out that fields **must** an order that is a power of a prime, but that
 the Finite Fields whose order is of prime order are the ones we're interested
 in. If the order of the set was composite number, multiplying the set by one of
 the divisors would result in a smaller set.<br>
Most operations in Finite Fields use the modulo operator in some capacity.<br>
We can use `FieldElement` instead of `self.__class__`, but this would not make
 the method easily inheritable.<br>
Why don't we force the exponent to be a `FieldElement` object? In fact, if it
 were, the exponents wouldn't display the intuitive behavior we would expect
 from exponents, like being able to add the exponents when you multiply with the
 same base.<br>
Fermat's Little Theorem - n<sup>(p-1)</sup>%p=1<br>
`pow(7,17,19)` will give the same results as 7<sup>17</sup>%19 but do so faster
 because the modulo function is done after each round of multiplication.
 [see][cpython_longobject]

[cpython_longobject]: https://github.com/python/cpython/blob/master/Objects/longobject.c

## 2. Elliptic Curves

secp256k1 - y<sup>2</sup>=x<sup>3</sup>+7<br>
We can define point addition using the fact that lines intersect one or three
 times with the Elliptic Curve (exceptions: vertical or tangent to the curve,
 and where the tangent line is vertical). That third point reflected over the
 x-axis is the result of the point addition. In mathematics parlance, point
 addition is *non-linear*. Associativity means that (A+B) + C = A + (B+C). This
 isn't obvious and is the reason for flipping over the x-axis.<br>
Since we can't easily use infinity in Python, we'll use the `None` value
 instead.<br>
x<sub>3</sub>=s<sup>2</sup>-x<sub>1</sub>-x<sub>2</sub>,
 y<sub>3</sub>=s(x<sub>1</sub>-x<sub>3</sub>)-y<sub>1</sub><br> There's a result
 from called the [Vieta's Formula][vietas_formula], which states that the
 coefficients have to equal each other if the roots are the same.

[vietas_formula]: https://en.wikipedia.org/wiki/Vieta%27s_formulas

## 3. Elliptic Curve Cryptography

```python
with self.assertRaises(ValueError): # unittest.TestCase
    Point(x, y, a, b)
```

Performing scalar multiplication is straightforward, but doing the opposite,
 Point division, is not. This is called the Discrete Log problem and is the
 basis of Elliptic Curve Cryptography.<br>
multiple additions (operations) : scalar multiplication = multiple
 multiplications : exponentiation (vs discrete log)<br>
An **asymmetric** problem is one that's easy to calculate in one direction, but
 hard to reverse.<br>
Unlike fields, groups have only a single operation. We also have a few other
 properties like closure, inveribility, commutativity and associativity.<br>
There are as many possible private keys in Bitcoin as there are atoms in a
 billion galaxies.<br>
uG + vP = kG → e = (k-u)/v (k: secret random, r: x coordinate of kG)<br>
u = z/s, v = r/s → s=(z+re)/k (z: hash)<br>
signature (r, s)<br>
If we were to reveal `k`, our secret would be revealed.<br>
[SHA1 collision][sha1_collision]<br>
It turns out that using the low-s value will get nodes to relay our transactions
 easier (if s > N/2, s = N - s). This is for malleability reasons.<br>
A `k` that's reused will result in you revealing your secret: e =
 (s<sub>2</sub>z<sub>1</sub> - s<sub>1</sub>z<sub>2</sub>) / (rs<sub>1</sub> -
 rs<sub>2</sub>). The [Playstation 3 hack][ps3_hack] back in 2010 was due to the
 reuse of the `k` value in multiple signatures. To combat this, there is a
 deterministic `k` generation standard ([RFC6979][rfc6979]) which uses our
 secret and `z` to create a unique, deterministic `k` every time. Deterministic
 `k` will be unique with very high probability. This is because sha256 is
 collision-resistant and no collisions to date have been found. Transactions
 which use deterministic `k` will create the same transaction every time as the
 signature will not change. This makes transactions less malleable.<br>
once [weapons grade munitions][weapons_grade_munitions]

[sha1_collision]: https://security.googleblog.com/2017/02/announcing-first-sha1-collision.html
[ps3_hack]: https://arstechnica.com/gaming/2010/12/ps3-hacked-through-poor-implementation-of-cryptography/
[rfc6979]: https://tools.ietf.org/html/rfc6979
[weapons_grade_munitions]: https://en.wikipedia.org/wiki/Export_of_cryptography_from_the_United_States

## 4. Serialization

There's already a standard for serializing ECDSA public keys called SEC. SEC
 stands for Standards for Efficient Cryptography and as the word "Efficient" in
 the name suggests, it's got minimal overhead. There are two forms: uncompressed
 SEC format (`0x04` prefix) and compressed SEC format (if `y` is even, `0x02`,
 otherwise `0x03` prefix).<br>
Unfortunately, some serializations in Bitcoin (like the SEC format x and y
 coordinates) are Big-Endian. Other serializations in Bitcoin (like the
 transaction version number) are in Little-Endian.<br>
(assumption: p % 4 = 3, that is, (p+1)/4 is an integer) w<sup>2</sup>=v,
 w<sup>2</sup>=w<sup>(p+1)</sup>,
 w=w<sup>(p+1)/2</sup>=(w<sup>2</sup>)<sup>(p+1)/4</sup>=v<sup>(p+1)/4</sup><br>
The standard for serializing (r, s) signatures (and lots of other things, for
 that matter) is called DER format. DER stands for Distinguished Encoding Rules
 and was used by Satoshi to serialize signatures. This was most likely because
 the standard was already defined in 2008, supported in the OpenSSL library
 (used in Bitcoin at the time). `0x30` + length of the afterwards (usually
 `0x44` or `0x45`, is `0x46` possible?) + marker byte `0x02` + length + (if the
 first byte >= `0x80`) `0x00` + stripped Big-Endian integer + ...<br>
Base58 - number + upper case + lower case - (`0/O` and `l/I`), convert leading
 `0x0`s to `1`s<br>
What's much better is the new Bech32 standard which is defined in BIP0173.
 Bech32 uses a 32-character alphabet that's just numbers and lower case letters
 except `1`, `b`, `i` and `o`. These are thus far only used for Segwit.<br>
address format - (mainnet) `0x00` (testnet, significantly more blocks) `0x6f` +
 ripemd160(sha256(compressed or uncompressed SEC)) + the first 4 bytes of
 hash256(the former) -> Base58<br>
WIF (Wallet Import Format, for private key) format  - (mainnet) `0x80` (testnet)
 `0xef` + 32-byte Big-Endian secret + (if the SEC format used for the public key
 address was compressed) `0x01` + the first 4 bytes of hash256(the former) ->
 Base58<br>
[testnet faucet][testnet_faucet] ([list][testnet_faucet_list])

P (finite field) vs N (multiplication, u, v, s)

[testnet_faucet]: https://faucet.programmingbitcoin.com/
[testnet_faucet_list]: https://en.bitcoin.it/wiki/Testnet#Faucets

## 5. Transactions

transaction - version (4 bytes, Little-Endian) + inputs + outputs + locktime (4
 bytes, Little-Endian)<br>
The transaction version is generally 1, but there are cases where it can be 2
 (transactions using an opcode called `OP_CHECKSEQUENCEVERIFY` as defined in
 BIP112 require use of version > 1).<br>
inputs - count (varint) + previous transaction id (Little-Endian) + previous
 transaction index (Little-Endian) + scriptsig (varint + ...) + sequence
 (Little-Endian) + ...<br>
varint - one bytes, `0xfd` with two bytes, `0xfe` with four bytes, `0xff` with
 eight bytes, all in Little-Endian<br>
Sequence was originally intended as a way to do what Satoshi called "high
 frequency trades" with the Locktime field, but is currently used with
 Replace-By-Fee and `OP_CHECKSEQUENCEVERIFY`. / A continuously updating
 mini-ledger between the two parties involved that gets settled on-chain.
 Satoshi's idea was to use Sequence and Locktime to update the "high frequency
 trade" transaction every time a new payment between the two parties occurred.
 The trade transaction would start with sequence at 0 with a far away Locktime
 (say 500 blocks from now), valid in 500 blocks. Unfortunately, it turns out
 that it's quite easy for a miner to cheat. A much better deisn was created
 later with "payment channels", which is the basis for the Lightning
 Network.<br>
outputs - count + amount (8 bytes, Little-Endian) + scriptpubkey (varint + ...)
 + ...<br>
Locktime is a way to time-delay a transaction. If the Locktime is greater or
 equal to 500,000,000, Locktime is a UNIX timestamp. If Locktime is less than
 500,000,000, it is a block number. BIP0065 introduced `OP_CHECKLOCKTIMEVERIFY`
 which makes Locktime more useful by making an output unspendable until a
 certain Locktime.<br>
Nick Szabo's
 [Trusted Third Parties are Security Holes][trusted_third_parties_are_security_holes]

[trusted_third_parties_are_security_holes]: https://nakamotoinstitute.org/trusted-third-parties/

## 6. Script

At the end of evaluating all the instructions, the top element of the stack must
 be non-zero for the Script to resolve as valid. Resolving as invalid means that
 the transaction which includes the unlocking Script is not accepted on the
 network.<br>
Anything larger than 520 bytes is not allowed on the network, so `OP_PUSHDATA4`
 is unnecessary. It is possible to encode a number below 76 using `OP_PUSHDATA1`
 or ... These are considered non-standard transactions, meaning most bitcoin
 nodes (particularly those running Bitcoin Core software) will not relay
 them.<br>
standard Scripts - p2pk, p2pkh, p2sh, p2wpkh, p2wsh<br>
Most coins thought to belong to Satoshi are in p2pk UTXOs. Early on in Bitcoin's history when p2pk was more prominent, uncompressed pubkey was the only one
 being used. Satoshi was using the OpenSSL library to do the SEC format
 conversions and the OpenSSL library at the time Satoshi wrote Bitcoin (circa
 2018) did not document the compressed format very well. When Pieter Wuille
 discovered that the compressed SEC format existed in OpenSSL, more people
 started using the compressed SEC format in Bitcoin. The UTXO set becomes bigger
 since this large public key has to be kept around and indexed to see if it's
 spendable. Because we're storing the public key in the ScriptPubKey field, it's
 known to everyone.<br>
The ScriptSig is evaluated separately from the ScriptPubKey as to not allow
 operations from ScriptSig to affect the ScriptPubKey instructions.
 Specifically, the stack after all the ScriptSig instructions are evaluated are
 stored and then the ScriptPubKey instructions are evaluated on their own with
 the stack from the first execution.<br>
Any byte-string is interpreted as a Little-Endian number for arithmetic opcodes.
 The integer 0 is **not** stored as the `00` byte, but as the empty
 byte-string.<br>
The original use-cases for p2pk were for IP-to-IP payments and mining outputs.
 For IP-to-IP payments, IP addresses were queried for their public keys, so
 communicating the public keys were done machine-to-machine, which meant that
 human communication wasn't necessarily a problem. Use for mining outputs also
 don't require human communication. Incidentally, this IP-to-IP payment system
 was phased out as it's not secure and prone to man-in-the-middle attacks. Given
 p2pkh is shorter and more secure, p2pk use declined significantly after 2010.
 p2pkh was used during the early days of bitcoin, though not as much as
 p2pk.<br>
`OP_MUL` is no longer allowed on the Bitcoin network. Version 0.3.5 of Bitcoin
 disabled a lot of different opcodes as anything that had even a little bit of
 potential to create vulnerabilities on the network were disabled. This is just
 as well since most of the functionality in Script is actually not used
 much.<br>
In 2013, Peter Todd created a Script and put some Bitcoins into it to create an
 economic incentive for people to find hash collisions. The donations reached
 2.49153717 BTC and when
 [Google actually found a hash collision for sha1][sha1_collision] in February
 of 2017, this Script was promptly redeemed. The transaction output was 2.48
 Bitcoins which was $2848.88 USD at the time. Peter created more piñatas for
 sha256, hash256 and hash160, which add economic incentives to find collisions
 for these hashing functions. /
 `[OP_2DUP, OP_EQUAL, OP_NOT, OP_VERIFY, OP_SHA1, OP_SWAP, OP_SHA1, OP_EQUAL]`

[sha1_collision]: https://security.googleblog.com/2017/02/announcing-first-sha1-collision.html

## 7. Transaction Creation and Validation

Back in 2010, there was a transaction that created 184 billion new Bitcoins.
 The fee was negative by enough that the C++ code was tricked into believing
 that the fee was actually positive by 0.1 BTC! The vulnerability is detailed in
 CVE-2010-5139 and was patched via soft-fork in Bitcoin Core 0.3.11. The
 transaction and the extra Bitcoins it created was invalidated retroactively by
 a block reorganization.<br>
We take the ScriptPubKey that the input is pointing to and put that in place of
 the empty ScriptSig. Lastly, we add a (Little-Endian) 4-byte hash type to the
 end of the transaction (and Big-Endian 1 byte to the end of the der signature).
 The signature can authorize that this input has to go with all the other inputs
 and outputs (`SIGHASH_ALL`), go with a specific output (`SIGHASH_SINGLE`) or go
 with any outpput whatever (`SIGHASH_NONE`). There's also rarely used hash type
 called `SIGHASH_ANYONECANPAY` which can be combined with any of the previous
 three, which we won't get into here. For `SIGHASH_ALL`, the final transaction
 must have the exact outputs that were signed or the input signature is invalid.
 The integer corresponding to `SIGHASH_ALL` is 1 and this has to be encoded in
 Little-Endian over 4 bytes.<br>
The signature hashing algorithm is inefficient and wasteful and is called the
 Quadratic Hashing problem. The biggest transaction mined to date,
 `bb41a757f405890fb0f5856228e23b715702d714d59bf2b1feb70d8b2b4e3e08`, had 5569
 inputs and 1 output and took many miners over a minute to validate as the
 signature hashes for the transaction were expensive to calculate. Segwit fixes
 this with a different way of calculating the signature hash, which is specified
 in BIP0143.<br>
A full node would verify more things like checking for double-spends and
 checking some other consensus rules not discussed in this chapter (max sigops,
 size of ScriptSig, etc), but htis is good enough for our library.<br>
[send transaction][send_transaction]

We've successfully validated existing transactions on the blockchain and we've
 also created our own transactions on testnet! This is a major accomplishment
 and you should be proud.

[send_transaction]: https://www.blockchain.com/btc/pushtx

## 8. Pay To Script Hash

`m` and `n` can be anything from 1 to 20, though there's numerical opcodes only
 go up to `OP_16`, 17 to 20 would require `0112` to push a number like 18 to the
 stack. As a way to combat malleability, most nodes on the Bitcoin network will
 not relay the transaction unless that extra element is `OP_0`.<br>
Because the ScriptPubKey can be so much bigger, bare multisig can and has been
 abused. The entire PDF of the Satoshi's original whitepaper is encoded in this
 transaction in block 230009:
 `54e48e5f5c656b26c3bca14a8c95aa583d07ebe84dde3b7dd4a78f4e4186e713`
 The creator of this transaction split up the whitepaper PDF into 64 byte chunks
 which were then made into invalid uncompressed public keys. The whitepaper was
 encoded into 947 outputs as 1-of-3 Bare Multisig outputs.<br>
Pay-to-script-hash was introduced in 2011 to a lot of controversy. There were
 multiple proposals, but as we'll see, p2sh is kludgy, but works. Unfortunately,
 this more elegant solution
 `[OP_DUP, OP_HASH160, <hash>, OP_EQUALVERIFY, OP_EVAL]` comes with an unwanted
 side-effect, namely Turing-Completeness. Turing-Completeness is undesirable as
 it makes the security of a smart contract much harder to guarantee.<br>
If the exact instruction set ends with a `1` on the stack (that makes
 pre-BIP0016 pass), then the RedeemScript is parsed and then added to the Script
 instruction set. This special pattern
 `[<redeemScript>, OP_HASH160, <hash>, OP_EQUAL]` was introduced in BIP0016 and
 Bitcoin software that implements BIP0016 (anything post 2011) checks for the
 pattern. The RedeemScript does not add new Script instructions for processing
 unless this exact sequence is encountered and ends with a `1`.<br>
If you are receiving to a p2sh address, be sure to store and backup the
 RedeemScript! Better yet, make it easy to reconstruct!<br>
The nice thing about p2sh is that the RedeemScript can be as long as the largest
 single element from `OP_PUSHDATA2`, which is 520 bytes. The main feature of
 ps2h is that it's flexible and at the same time reduces the UTXO set size by
 pushing the burden of storing part of the Script back to the user. p2sh is also
 used to make Segwit backwards compatible.<br>
Mainnet p2sh uses the `0x05` byte which causes addresses to start with a `3` in
 Base58 while testnet p2sh uses the `0xc4` byte to cause addresses to start with
 a `2`.<br>
Signatures have to be in the same order as the pubkeys or the signatures are not
 considered valid. (verify) We take the RedeemScript and put that in place of
 the empty ScriptSig.

## 9. Blocks

The ScriptSig of the Coinbase transaction has to be at least 2 bytes and no
 longer than 100 bytes. Other than those restrictions and BIP0034 the ScriptSig
 can be anything the miner wants as long as the evaluation of the ScriptSig by
 itself, with no corresponding ScriptPubKey, is valid. To prevent transaction ID
 duplication, Gavin Andresen authored BIP0034 which is a soft-fork rule that
 adds the height of the block being mined into the first element of the Coinbase
 ScriptSig. The height is interpreted as a Little-Endian integer and must equal
 the height of the block.<br>
(almost Little-Endian) A Block Header takes up exactly 80 bytes. As of this
 writing there are roughly 550,000 blocks or ~45 megabytes in Block Headers. The
 entire blockchain is roughly 200 GB, so the headers are roughly .023% of the
 size. The fact that headers are so much smaller is an important feature when we
 look at Simplified Payment Verification.<br>
In the past the version field was used as a way to indicate a single feature
 that was ready to be deployed by the miner who mined the block. Version 2 meant
 that the software was ready for BIP0034, the Coinbase transaction block height
 feature. Version 3 meant that the software was ready for BIP0066, the
 enforcement of strict DER encoding. Version 4 meant that the software was ready
 for BIP0065, which speficied `OP_CHECKLOCKTIMEVERIFY`. The wy BIP9 works is by
 fixing the first 3 bits of the 4-byte (32-bit) header to be `001` to indicate
 that the miner is using BIP9. The first 3 bits have to be `001` as that forces
 older clients to interpret the version field as a numebr greater than or equal
 4, which was the last version number that was used pre-BIP9. BIP9 requires that
 95% of blocks signal readiness in a given 2016 block epoch (the period for a
 difficulty adjustment) brfore the soft fork feature gets activated on the
 network. Soft forks which used BIP9 as of this writing have been
 BIP68/BIP112/BIP113 (`OP_CHECKSEQUENCEVERIFY` and related changes) and BIP141
 (segwit). These BIPs used bits 0 and 1 for signaling respectively. BIP91 used
 something like BIP9, but used an 80% threshold and used a smaller block period,
 so wasn't strictly using BIP9. Bit 4 was used to signal BIP91.<br>
The Locktimes were at one point used directly for transactions within a block,
 but BIP113 changed the behavior to not use the current block's Locktime
 directly, but the median-time-past (MTP) of the past 11 blocks.<br>
A typical gold mining operation processes 45 tons of dirt and rock before
 accumulating 1 oz of gold.<br>

Unfortunately, the 4 bytes or 2<sup>32</sup> possible nonces that a miner can
 try is insufficient space. Modern ASIC equipment can calculate way more than
 2<sup>32</sup> different hashes per second. The AntMiner S9 calculates 12 Th/s,
 or 12,000,000,000,000 hashes per second. That is approximately 2<sup>43</sup>
 hashes per second which means that the entire nonce space can be consumed in
 just 0.0003 seconds. What miners can do when the nonce field is exhausted is
 change the Coinbase transaction, which then changes the Merkle Root, giving
 miners a fresh nonce space each time. The other option is to roll the version
 field or use overt ASICBOOST.<br>
Target is a 256-bit number that is computed directly from the bits field. The
 first is the exponent, which is the last byte. The second is the coefficient
 which is the other three bytes in Little-Endian. target = coefficient *
 256<sup>exponent-3</sup> / To find a single proof-of-work with the above
 target, the network as a whole must calculate 3.8 * 10<sup>21</sup> hashes
 roughly every 10 minutes. To give this number some context, the best GPU miner
 in the world would need to run for 50,000 years on average to find a single
 proof-of-work below this target.<br>
Target is hard to comprehend for human beings. The difficulty of Bitcoin at the
 Genesis Block was 1. difficulty = 0xffff * 256<sup>0x1d-3</sup> / target<br>
`new_target = previous_target * time_differential / (2 weeks)` (between 0.5 week
 and 8 weeks, 4x at the most) / Satoshi unfortunately had another off-by-one
 error here. The time differential is the difference of blocks that are 2015
 blocks apart instead of 2016 blocks apart.

## 10. Networking

65000+ nodes are running on th network as of this writing and communicate
 constantly.<br>
network magic, command (12 bytes, human readable), payload length (4 bytes,
 little-endian, payload checksum (first 4 bytes of double-sha256), payload<br>
Bitcoin testnet also has a different magic `0b110907` as opposed to the Bitcoin
 mainnet magic `f9beb4d9`.<br>
The reference client rejects any payloads over 32MB.<br>
Version command - IP addresses can be IPv6, IPv4 or OnionCat (a mapping of TOR's
 `.onion` addresses to IPv6). If IPv4, the first 12 bytes are
 `00000000000000000000ffff` and the last 4 bytes are the IP. Relay is used for
 Bloom Filters.<br>
handshake - > Version, < Verack, < Version, > Verack<br>
`getheaders` command - The maximum number of headers that we can get back is
 2000, or almost a single difficulty adjustment period (2016 blocks).<br>
The difficulty adjustment algorithm is different. If a block hasn't been found
 on testnet in 20 minutes, the difficulty drops to 1, making it very easy to
 find a block. A $30 USB ASIC can typically find a few blocks per minute at the
 minimum difficulty.

## 11. Simplified Payment Verification

Proof of Inclusion<br>
There was a vulnerability in Bitcoin 0.4-0.6 which is detailed in CVE-2012-2459.
 There was a Denial of Service vector due to the duplication of the last item in
 Merkle Trees, which caused some nodes to invalidate blocks even if they were
 valid.<br>
We use Little-Endian ordering for the leaves for the Merkle Tree. After we
 calculate the Merkle Root, we use Little-Endian ordering again.<br>
`merkleblock` command - block header, number of total transactions (4 bytes,
 LE), number of hashes (varint), hashes, flag bits (strange order, depth-first
 search)<br>
Most light wallets do not use SPV and trust data from the wallet vendor servers.
 The main drawback of SPV is that the nodes you are connecting to know something
 about the transactions you are interested in.

## 12. Bloom Filters

The light client wants to "hide" the transaction among a group of transactions.
 This grouping would have to be *deterministic*.<br>
BIP0037 specifies Bloom Filters in network communication.<br>
The hash function we use is called `murmur3`. Unlike sha256, murmur3 is not
 cryptographically secure. seed: `i*0xfba4c795 + tweak`<br>
send `filterload` → send `getheaders` with `start_block` → receive `headers` →
 send `getdata` (type 1 - transaction, type 2 - normal block, type 3 - merkle
 block, type 4 - compact block (not covered in this book)) → receive
 `MerkleBlock`s and `Tx`s (when a bloom filter is applied tx objects are sent
 automatically for matching transactions following the `merkleblock`
 [1][protocol_documentation_tx])

[protocol_documentation_tx]: https://en.bitcoin.it/wiki/Protocol_documentation#tx

## 13. Segwit

Segwit stands for segregated witness and is a backwards-compatible upgrade or
 "soft fork" that activated on the Bitcoin network in August of 2017. As a brief
 overview, Segwit did a multitude of things: 1) block size increase 2)
 transaction malleability fix 3) introduced Segwit versioning for clear upgrade
 paths 4) quadratic hashing fix 5) offline wallet fee calculation security<br>
Pay to witness pubkey hash (p2wpkh) is one of four types of Scripts defined by
 Segwit in BIP0141 and BIP0143.<br>
Transaction Malleability is the ability to change the transaction's ID without
 altering the transaction's meaning. Mt. Gov CEO Mark Karpeles cited transaction
 malleability as the reason why his exchange was not allowing withdrawals back
 in 2013. The ScriptSig field on each input is emptied before creating the
 signature hash, so it's possible to change the ScriptSig without invalidating
 the signature. Also, signature contain a random component. This means that two
 different ScriptSigs can essentially mean the same thing but be different
 bytewise. A malleable transaction ID means that any *dependent* transactions
 (that is, a transaction spending one of the malleable transaction's outputs),
 cannot be constructed in a way to guarantee validity. This is not usually a
 problem as once a transaction enters the blockchain, the transaction ID is
 fixed and no longer malleable. However, with Payment Channels, there are
 dependent transactions created before the funding transaction is added to the
 blockchain.<br>
Transaction malleability is fixed by emptying the ScriptSig field and putting
 the data in another field that's not used for ID calculation. For p2wpkh, the
 signature and pubkey are the items from ScriptSig, so those get moved to the
 Witness field, which is not used for ID calculation (see BIP0143). The Witness
 field and the whole Segwit serialization of a transaction is only sent to nodes
 that ask for it. In other words, old nodes that haven't upgraded to Segwit
 don't receive the Witness field and don't verify the pubkey and signature.
 (similar to p2sh, soft fork)<br>
Segwit Marker, Segwit Flag, Witness Program<br>
The ScriptPubKey for a p2wpkh (+ empty ScriptSig) is `OP_0` `<20-byte hash>`.
 When that Script sequence is encountered, the pubkey and signature from the
 Witness field and the 20-byte hash are added to the instruction set in exactly
 the same sequence as p2pkh.<br>
This is a special rule for Segwit Version 0. `<20-byte hash> 1` (Version 1)
 could be the special Script sequence that triggers a different rule. Upgrades
 of Segwit can introduce Schnorr Signatures, Graftroot or even a different
 scripting system altogher like Simplicity. Segwit gives us a clear upgrade
 path.<br>
p2sh-p2wpkh transaction - P2wpkh uses a new address format called bech32, whose
 ScriptPubKeys older wallets don't know how to create. The Segwit authors found
 an ingenious way to make Segwit backwards compatible by using p2sh. We can
 "wrap" p2wpkh inside a p2sh. This is called "nested" Segwit as the Segwit
 Script is nested in a p2sh RedeemScript.<br>
Pay-to-witness-script-hash (p2wsh) is like p2sh, but with all the ScriptSig data
 (+ Witness Script) in the Witness field instead. The WitnessScript is very
 similar to the RedeemScript in that the "sha256" of the serialization is
 addressed in the ScriptPubKey, but only revealed when being spent. Once the
 sha256 of the serialization is found to be the same as the 32-byte hash (in the
 ScriptPubKey), the WitnessScript is interpreted as Script instructions and
 added to the instruction set. The rest of the Witness is then put on the
 instruction set as well, producing the final set of instructions to be
 evaluated. p2wsh is particularly important as unmalleable multisig transactions
 are required for creating bi-directional Payment Channels for the Lightning
 Network.<br>
p2sh-p2wsh<br>
Other improvements to Segwit include fixing the quadratic hashing problem
 throught a different calculation of the signature hash (BIP0143). A lot of the
 calculations for signature hash, or `z`, can be reused instead of requiring a
 new hash256 hash for each input. Another improvement is that by policy,
 uncompressed SEC pubkeys are now forbidden and only compressed SEC pubkeys are
 used for Segwit, saving space.

p2wpkh (20 bytes hash) vs p2wsh (32 bytes hash)

```python
# p2wsh
instructions.extend(witness[:-1])
witness_script = witness[-1]
assert(s256 == sha256(witness_script))
instructions.extend(parsed_script_instructions(witness_script))

# parsing witness
for tx_in in inputs:
    num_items = read_varint(s)
    items = []
    for _ in range(num_items):
        item_len = read_varint(s)
        if item_len == 0:
            items.append(0)
        else:
            items.append(s.read(item_len))
    tx_in.witness = items
```

## 14. Advanced Topics and Next Steps

This book only scratches the surface and there's quite a bit more to learn. In
 this chapter, we'll go through what other things you can learn, how to
 bootstrap your career as a Bitcoin developer and ways to contribute to the
 community.<br>
To combat this problem, Armory, an early Bitcoin wallet, first implemented
 deterministic wallets. The Armory style deterministic wallets were great,
 except people wanted some grouping of addresses so the Hierarchical
 Deterministic wallet standard or BIP0032 was born. BIP0032 wallets have
 multiple layers and keys, each with a unique derivation path. Additionally,
 BIP0044 defines what each layer of the BIP0032 hierarchy can mean and the best
 practices for using a single HD seed to store coins from a lot of different
 cryptocurrencies. While many wallets (Trezor, Coinomi, etc) implement both
 BIP0032 and BIP0044, some wallets ignore BIP0044 altogether and use their own
 BIP0032 hierarchy (Electrum and Edge being two). BIP0039 is a way to encode the
 seed into a bunch of English words.

