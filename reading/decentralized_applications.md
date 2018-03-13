# [Decentralized Applications][homepage] by Siraj Raval ([YouTube][youtube], [GitHub][github]), O'Reilly (2016)

[GitHub][source_code], [Decentralized Twitter][decentralized_twitter]

[homepage]: http://shop.oreilly.com/product/0636920039334.do
[youtube]: https://www.youtube.com/c/sirajology
[github]: https://github.com/llSourcell
[source_code]: https://github.com/oreillymedia/decentralized_applications
[decentralized_twitter]: https://github.com/llSourcell/dapp

## 1. What Is a Decentralized Application?

[Automated Clearing House (ACH)][ach], Recent changes to NACHA's operating rules
 now allows for same day settlement of most ACH transactions. The ACH system
 oversees more than 90% of the total value of all electronic payment
 transactions in the United States.<br>
Proof-of-work is expensive in terms of the cost of electricity and compute
 workload but it's the only known prevention mechanism against Sybil attacks, in
 which a bad actor claims to be multiple people in a network and gains resources
 that they shouldn't by doing so.<br>
Distributed means computation is spread across multiple nodes. Decentralized
 means no node is instructing any other node as to what to do. Centralized
 systems can be distributed as well. Software applications that are able to
 achieve decentralized consensus are a real innovation.<br>
The focus of this book is to talk about profitable dapps. The reason for the
 profit focus is because profit is the cornerstone of a successful, robust, and
 sustainable dapp. Incentives keep developers building, users loyal, and miners
 maintaining a blockchain.<br>
features - open source, internal currency, decentralized consensus, no central
 point of failure<br>
What makes the blockchain unique is that it solves the major security issue of
 DHTs: not forcing nodes to trust each other on the validity of data. The
 blockchain is a decentralized database of transactions and it's the first
 decentralized database that is highly tamper-resistant. The blockchain's
 security was a dominant design goal.<br>
A simple example is a username system for which it doesn't really matter who has
 the "@user" username; what matters is that everyone agrees who has it. There
 have been lots of decentralized protocols in the past, but they all required
 nodes to trust one another.<br>
A more formal phrase for smart contract would be a "cryptoeconomically secured
 execution of code."<br>
dapps - PopcornTime, OpenBazaar, FireChat (iOS 7 multipeer connectivity, mesh
 networking), Lighthouse, Gems (social-messaging, letting advertisers pay users
 directly with its own currency (as shares) for ther data)

[ach]: https://en.wikipedia.org/wiki/Automated_Clearing_House

## 2. A Flourishing Dapp Ecosystem

Decentralized Data / Wealth / Identity / Computing, Decentralized Bandwidth /
 Markets for Decentralized Assets

One of the original proposals for the Internet had link permanence baked in.
 That idea was called Project Xanadu, and it called for a Web in which every
 link worked two ways: one toward the destination, and one toward its source.
 That means that the content creator would always be able to get credit for
 their data because it would always link back to them.<br>

Interplanetary File System (IPFS) takes the best ideas from Git, DHTs, SFS,
 BitTorrent, and Bitcoin and combines them to create a decentralized
 data-storage network.
It's based on the popular Kademlia DHT, and it borrows ideas from Chord and
 BitTorrent's DHT. Chord's killer feature was its DHT circles, which created
 "chords" to maximize DHT lookups among nodes across the globe that were in
 close proximity to one another within larger chords.<br>
To give structure to the DHT and let users find the data they need, IPFS uses
 a *merkleDAG*. When adding data to the DHT, the system generates an SHA-256
 multihash public-private key pair, and the user gets both. Developers can link
 hashes programmatically to form their own mini-merkleDAGs, and it's important
 to note that all data in IPFS forms the same generalized merkleDAG consisting
 of all nodes.<br>
IPFS has a sister protocol called Filecoin. Filecoin is used to pay miners
 (nodes that store data) using a novel value-for-data mechanism called BitSwap.
 Filecoin will most likely be an asset built directly on Bitcoin's blockchain,
 so users can just use their Bitcoin to pay for storage.<br>
Ethereum Swarm, StorJ, Maidsafe

The sidechain proposal is based on a paper coauthored by Adam Back, the inventor
 of proof-of-work that Satoshi referenced in his Bitcoin Paper.<br>
There is no need to create a new (side)chain. You can simply create an asset
 directly on Bitcoin itself.
* Counterparty - if you want to create any kind of asset, according to the
  protocol, you must destroy 0.5 XCP. colored coins (better alternatives)
* Hyperledger - token agnostic

Gavin Andresen said most of Ethereum's aims could be implemented in Bitcoin, and
 the core developers have already started implementing some of them.

BitAuth, OpenID<br>
The problem is also known as Zooko's triangle (human-meaningful, decentralized,
 secure), and Namecoin was developed to help solve it. Namecoin has proved 
 popular for registing *.bit* domains as a decentralized alternative to ICANN.
 To access them, the user currently must use either a *.bit* web proxy or
 download an extension. NameID combined the best of both worlds; a user can use
 his Namecoin */id* to log in to all of openID-enabled websites.<br>
Combine that with the average level of computer-security literacy, and I think
 it's fair to say that most people are probably not able or willing to securely
 store encryption keys. The market demand for centralized storage is evident in
 the success of Coinbase, Bitcoin's largest application so far. Coinbase is the
 exact opposite of decentralized: it's a bank for Bitcoin. It provides
 private-key storage as a service. Decentralization is not good for its own
 sake; it must have purpose and a real use case.

Basically, astralboot is a golang server that pulls its files directly from IPFS
 and lets you run a Debian environment based on IPFS.<br>
The problem with Ethereum's EVM is that you can't get data from outside the
 blockchain unless it's been preconfigured to work with Ethereum by setting up a
 smart contract inside of its server.<br>
Go-circuit creates small server processes that run instances on a machine
 cluster. They form a churn-resilient and efficient network, which enables
 distributed process orchestration and synchronization from any single machine.

Tunneling can only be done by switching from tunnel to mesh networking and back.
 Currently, doing both simultaneously isn't supported by any major hardware
 manufacturer, but you can have a hybrid and that is being worked on by projects
 like Open Garden and CJDNS. This one is a little harder than the rest because
 in addition to software changes, it required hardward changes.

Users will eventually want to trade these assets for more durable, stable forms
 of assets like currencies--maybe a USD-backed Bitcoin sidechain like bitUSD.
 BitUSD is a novel cryptocurrency pegged to USD that can be traded for BTC
 seamlessly.<br>
Ripple-style meaning you can choose who to reach consensus with. Stellar is
 quite literally the Ripple source code wite-labeled with a new consensus
 mechanism that is still being ironed out. The basic idea is that you can trade
 your currency with anyone else by trusting a third-party intermediary to
 administrater the exchange. In the Stellar model, the intermediary nodes are
 held accountable for any mishaps, and if any occur, their reputation goes
 down.<br>
The most decentralized exchange I know is called Mercury. Mercury is a multicoin
 wallet that uses the *cross-atomic chain* (CAC) transfer protocol of Bitcoin to
 exchange value between cryptocurrencies. The CAC transfer protocol lets Alice
 and Bob, who own coins in separate cryptocurrencies, to exchange them without
 having to trust a third party.

Darkcoin keeps your transaction history private by scrambling and encrypting the
 transaction data on the blockchain.

## 3. Building Your First Dapp

Go (web frameworks - Martini, Goji, Gorilla, `net/http`), IPFS (written in Go),
 Kerala (IPFS interface), Coinprism (colored coins wallet service),
 `github.com/julienschmidt/httprouter` to make Mikro (decentralized messaging
 app with an internal economy)<br>
CRUD and REST become one and the same in a decentralized architecture.<br>
The right way to deploy a dapp is as a downloadable binary. Users can download
 it to their desktops and then access the dapp using either a web browser or
 directly within a client interface.<br>
Notice how there is no `delete` command. IPFS is the permanent Web! Also notice
 how there is no `update` command, because IPFS has Git's methodology built in.
 IPNS (the naming layer on top of IPFS) gives the appearance that updating and
 deleting are possible through mutable names. With IPNS you can publish a DAG of
 data under your immutable peer ID.<br>
In a dapp, there are certain elements that need consensus via smart contracts
 that would usually require a server. Smart contracts are technically "models",
 and you can feed data into them via transactions, but they are not the de facto
 "model" in MVC architecture. The saying goes that we need smart models, thin
 controllers, and dumb views. Decerver (a framework for building dapps) says
 that the models are the smart contracts, but the problem is that smart
 contracts are pay-to-play and should be orthogonal to model creation. Your
 controller will speak to blockchains and DHTs instead of servers.<br>
The patches are rough because Twister employs techniques like making the new
 user complete a proof-of-work to verify her identity after signing up, which
 was done to prevent Sybil attacks.<br>
Kerala utilizes both IPNS and IPFS to work seamlessly together. It will
 associate the HEAD node of a particular DAG with a particular peerID,
 republishing to IPNS as necessary when new data is added.<br>
Coinprism is an online colored coins wallet. Creating a colored coin requires a
 fee of 0.0001 BTC. Now with this new address, you can issue colored coins to
 yourself. Your main address will store both Bitcoins and colored coins. The fee
 is 0.001 BTC, and you can choose an arbitrary number of shares to be associated
 with the colored coins. But for now, because we don't have a system like that
 yet in IPFS, we will make it free to post, pay to view.<br>
`Pay(fee string, from_address string, to_address string, amount string, asset_id string, private_key string) (string)`<br>
The returned value is the hash of the transaction that you can verify on the
 Bitcoin blockchain. For your dapp, you can set it so `GenerateAddress` is
 called when a user first runs the app so that he has his own asset address from
 which to send and receive funds.<br>
The idea is that your DAG is encrypted with a public-private keypair, and only
 those nodes that you trust on the network can gain access to your private
 keypair to unlock and view your data. If you unfriend someone, the app can
 generate a new public-private keypair and rebroadcast the private key to all of
 your remaining friends. It's called [IPFS Keystore][ipfs_keystore], and it's
 still under development.<br>
(Tamper-Free Payments) Each user can have a listener running that waits for a
 payment to occur to their asset address. If a payment occurs, then and only
 then does the client send the payment sender a copy of the user's private key
 to access her data.<br>
The unfortunate truth for Ethereum is that when it comes to decentralized apps,
 most often global consensus just isn't necessary and proves to be too
 expensive.

[ipfs_keystore]: https://github.com/ipfs/specs/tree/master/keystore

## 4. OpenBazaar

A [Ricardian contract][ricardian_contract] is basically a way to track the
 liability of Party A when selling goods to Party B. Additionally, it links your
 product to your Bitcoin address and GUID, the buyer's Bitcoin address and GUID,
 and a third-party notary that you both deem trustworthy. If the buyer does not
 receive the item, the notary will act as a dispute resolution party, and after
 compiling information from both sides, will decide as to which party is most
 likely telling the truth.<br>
Anyone can be a notary by simply turning on a checkbox in their profile. If the
 notary is needed to refund the buyer or engage in dispute resolution, the
 notaary will receive a percentage from the multisig. Notaries publicly display
 their fee under the services tab in their "storefront".<br>
[RPyC][rpyc]<br>
`ast.literal_eval()` (the stringor node provided may only consist of the
 following Python literal structures: strings, bytes, numbers, tuples, lists,
 dicts, sets, booleans, `None`.) <br>
Data isn't stored on a DHT in OpenBazaar; it's stored locally in a SQLite
 datastore at each node. The DHT in OpenBazaar was inspired by Kademlia (like
 BitTorrent and IPFS) and is used as a sort of "yellow pages" for peers.<br>
[BIP32][bip32] (hierarchical deterministic wallets), [SIN][sin] (secure identity
 number or system identification number)<br>
DNSChain is a hybrid DNS server for easy access to Namecoin data via an API.<br>
Trust (reputation) is dealt with in OpenBazaar through two different type of
 synergistic systems: *global trust* and *projected trust*. Global trust is
 established through *proof-of-burn* and *proof-of-timelock*. Projected trust is
 trust directed toward a certain node, which might be different for each user of
 the network. This trust is established through a pseudonymous partial knowledge
 web of trust.<br>
Publicly and verifiably burning some coins in a set-limit currency is
 [*remurrage*][remurrage] on the remainder. Remurrage is the opposite of
 [demurrage][demurrage] (the cost of holding currency over a given period).<br>
The Bitcoin blockchain currently doesn't allow for a proof-of-timelock mechanism
 directly. Although the Bitcoin protocol supports the `nLockTime` value, the
 mechanism is not currently honored by running nodes. This means that the
 transaction won't be broadcast in a publicly verifiable way. The sidechain
 proposal will mitigate some of this risk eventually.<br>
So the idea would be that if you really trust someone, you can give them 0.1
 Bitcoin in a line of credit via a multisignature transaction. Because
 OpenBazaar developers don't want to add to blockchain bloat on Bitcoin, they
 are leaning toward using Namecoin as a good alternative.<br>
Having an internal currency is a win-win situation for early adopters and the
 developers themselves. Using OpenBazaar is a risk for early adopters anyway,
 given that their Bitcoin can be stolen because of the lack of reputable
 notaries and reputation takes time to build. These coins would be colored
 coins.

[ricardian_contract]: https://gist.github.com/drwasho/a5380544c170bdbbbad8
[rpyc]: https://rpyc.readthedocs.io/en/latest/
[bip32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
[sin]: https://en.bitcoin.it/wiki/Identity_protocol_v1
[remurrage]: https://en.bitcoin.it/wiki/Proof_of_burn
[demurrage]: https://en.wikipedia.org/wiki/Demurrage_(currency)

## 5. Lighthouse

by [Mike Hearn][mike_hearn]<br>
The first thought you might have is how inefficient it is to be forced to import
 and export project files instead of having a discovery page in the app.
 Lighthouse's creator decided to avoid it, simply because it's too difficult to
 do. We come up against the problem that has historically plagued decentralized
 software development: lack of funds. More recently, a service called Lightlist
 has popped up; it acts as a server to host all Lighthouse projects.<br>
An interesting part of Lighthouse is that it uses a Bitcoin feature that has
 been available since Bitcoin 0.1 but seems to have been entirely overlooked:
 `SIGHASH_ANYONECANPAY`. It allows users to tag their signatures with an
 annotation that says it's OK for other people to take part in this payment.
 It lets you merge transactions together into one big transaction. With it, if
 you get enough of these pledges, it will merge them all together and you'll end
 up with a valid paymeny that will be merged into the blockchain.<br>
[Chain][chain]'s [Sequence][sequence]<br>
[BIP70][bip70] payment request message, The memo field can also contain a
 message from the user (commending the project).<br>
There also exists an SPV colored coins wallet called ChromaWallet, but it
 doesn't have the pretty starter template that this
 [BitcoinJ template][bitcoinj_template] has.

[mike_hearn]: http://plan99.net/~mike/
[chain]: https://chain.com
[sequence]: https://seq.com
[bip70]: https://github.com/bitcoin/bips/blob/master/bip-0070.mediawiki
[bitcoinj_template]: https://github.com/bitcoinj/wallet-template

## 6. La'Zooz

La'Zooz ([GitHub][lazooz_github]) implements what they call a proof-of-movement
 algorithm. It uses GPS triangulation data to track whether the driver is
 driving. If they are driving, they'll be able to mine Zooz currency. The amount
 that they are rewared for mining decreases with time, similar to the Bitcoin
 network.<br>
Za'Zooz aims to be a community-run network, meaning that there is no difference
 between creators and the users. Everyone who use La'Zooz belongs to the same
 DAO. There is an end of the month vote during which members decide the weight
 of each member's vote and the reward for each member in the community.<br>
La'Zooz put on a [collaborative white paper][lazooz_white_paper] that describes
 their distribution mechanism, some of the math behind their distribution
 algorithm, their roadmap, andd theirr vision.<br>
An altcoin called bitshares created by Dan Larimer uses a consensus algorithm
 called Delegated Proof of Stake that functions similarly.<br>
I do know that they are using the Maskercoin blockchain from what they've stated
 on various social media outlets. Mastercoin is a layer on top of the Bitcoin
 blockchain. Nothing will stop someone from publishing conflicting Mastercoin
 transactions on the blockchain, and the only thing the Mastercoin protocol does
 defines a rule by which a single transaction is ignored. The Mastercoin
 protocol has a feature called "register a data stream," in which the owner of a
 Bitcoin address can declare that they'll be publishing data hidden in
 transactions from it.<br>
A quick look on the Ethereum home page leads us to find that La'Zooz is one of
 the projects using Ethereum for smart contracts. La'Zooz shouldn't use Ethereum
 for smart contracts until the sidechain proposal is ready. The Bitcoin
 scripting language, though not fully Turing-complete, can handle most use cases
 that involve escrowing automatic payments.

[lazooz_github]: https://github.com/lazooz
[lazooz_white_paper]: https://docs.google.com/file/d/0B8DXTngEHOSmN2ZzTDJBTHppc1E/edit

