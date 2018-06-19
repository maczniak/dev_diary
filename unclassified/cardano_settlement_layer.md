# [Cardano Settlement Layer Documentation][homepage]

[homepage]: https://cardanodocs.com/introduction/

## Introduction

## Installation

## Cardano overview

### Ouroboros Proof of Stake Algorithm

slot (significantly larger than the expected difference in clocks on different
 nodes, for example, 20 seconds)<br>
One or more slots can remain empty, but the majority of blocks (at least 50% +
 1) must be generated during an epoch.<br>
Not all stakeholders participates in this election, but only ones who have
 enough stake (for example, 2% of the total stake). Electors elect slot leaders
 for the next epoch during the current epoch.<br>
MPC (multiparty computation) - commitment phase → reveal phase (sending an
 "opening") → recovery phase → FTS (follow the satoshi) algorithm<br>
The fundamental assumption of a protocol is known as honest majority. This means
 that participants owning at least 50% + 1 of the total stake are honest ones.

### Differences Between Paper and the Implementation

There are no details on how to obtain the current time value securely and with
 enough precision.<br>
The paper suggests PVSS (Publicly Verifiable Secret Sharing) scheme by
 Schoenmakers. However, currently Cardano SL uses "SCRAPE: Scalable Randomness
 Attested by Public Entities" PVSS scheme.<br>
One of the challenges while using a VSS scheme is associating the public key
 used for signing with the public key used for the VSS scheme (`VssPublicKey`).
 This is solved by introducing `VssCertificate`s. This certificate is a
 signature given by a signing key for a pair consisting of `VssPublicKey` and
 the epoch until which this certificate is valid. When a new stakeholder
 appears, or when an existing certificate expires, a new certificate should be
 generated and submitted to the network. `VssCertificate`s are stored in
 blocks.<br>
There is a special constant called "network diameter" which approximates maximal
 time necessary to broadcast a block to all nodes in the network. For example,
 if network diameter is 3, then block is generated and announced 3 seconds
 before the end of a slot.<br>
There are two types of delegation in Cardano SL: heavyweight and lightweight.
 There is a threshold on stake that one has to possess in order to participate
 in heavyweight delegation. Proxy signing certificates from heavyweight
 delegation are stored within the blockchain.

To overcome this problem, a number of (VSS participation) shares for each
 stakeholder proportional to their stake is generated in Cardano SL.<br>
Cardano SL implementation uses a seed consisting of all zeroes if there are no
 commitments that could be recovered.

The sections on *Input Endorsers* and *Incentive Structure* are not implemented
 yet. Those sections are to be implemented together with the pending research on
 Side-chains and released within the Side-chains release.

### Addresses

public key address, script address (P2SH, redemption script), redeem address
 (P2PKH)<br>
Moreover, Cardano SL support `Unknown` address type as well. This type will
 allow us to use custom types of addresses in the future.<br>
Address consists of 3 parts: address root (hash of
 `(addrType, addrSpendingData, addrAttributes)`), address attributes (derivation
 path, stake distribution, ...), address type

### Transactions

similar to Bitcoin transactions<br>
all unspent outputs called *utxo*, and this is a part of the special key-value
 database called *Global State*.<br>
After some time a node may delete old proofs in order to save space. The
 technique of storing transactions separately from their proofs is called
 "segregated witness".<br>
Some addresses have multiple owners, which poses a problem of stake computation
 as per Follow-the-Satoshi each coin should only be counted once towards each
 stakeholer's stake total. Unlike balance, stake gives user power to control
 different algorithm parts: being the slot leader, voting in Update system,
 taking part in MPC/SSC.<br>
Stake distribution is a value associated with each address. Technically stake
 distribution is a value which is a part of address' attributes. This value
 corresponds to one of three different cases: Bootstrap era distribution, single
 key distribution, multiple key distribution<br>
Stake distribution feature can be used in similar way to delegation, but there
 are differences: there is no certificate(s), and the portion of `A`'s stake can
 be delegated via distribution.

### Balance and Stake

It is possible to change the association between coin and stake using what is
 known as stake delegation.

### Update Mechanism

This is done automatically, and updates are announced directly via the
 blockchain.<br>
Update proposal can be approved if at least one of two agreements is reached:
 explicit agreement (positive votes from majority of total stake) and implicit
 agreement (positive votes from more stake than negative votes, been in
 blockchain for at least `U` slots, `updateProposalThd` in
 `configuration.yaml`)<br>
One requires enough collected signatures from stakeholders to publish their
 system update. An alternative client is updated via the same mechanisms as the
 official client.<br>
We're considering migrating to courgette in the future.<br>
BIP-99 - soft fork (invalid → invalid, valid → invalid), hard fork (valid →
 invalid)<br>
The protocol version is to be put later into each block created by updated
 software.<br>
If 95% of the latest 2016 blocks have a newer block version, the blocks with the
 older version are rejected.<br>
The soft fork resolution rule works as follows:
* Informally, a block version becomes *adopted* when a certain percentage of
  stake (75% in the current implementation) creates a block with version `X`.
* Formally, we do the following. First, recall that by design, our system does
  not allow rollbacks of blockchain deeper than a certain fixed global threshold
  `k` (*stable* stake). When we process genesis block for epoch `e`, we compute
  the stable stake of all leaders of all slots from the very beginning of the
  network's existence. For each block with version `X` that is currently
  *competing*, we take set of all leaders of stable blocks and accumulate their
  stakes. If one of versions has over 75%, it gets *adopted*. If more than one
  version has over 75%, we take one of them deterministically.

Adopted block version can't be changed during epoch.<br>
nodes that have been updated - (before the soft fork is resolved) issue blocks
 with new version, but do not include any new attributes. validate blocks
 of new version as blocks of old version<br>
nodes that have not been updated - (before the soft fork is resolved) do not
 accept blocks with unknown attributes. (once the soft fork is resolved) start
 accepting blocks with new version and validate them as old version<br>
Hard forks are resolved using modified proof of burn. It is not implemented yet.

### Topology

Currently all nodes are divided into 3 groups: core, relay, and edge<br>
Core nodes never create currency transactions.<br>
You can think of relay nodes as of proxy between core nodes and public internet.
 Relay nodes do not have any stake. Relay nodes are fully under the control of
 the federated committee of initial stakeholders.<br>
Edge node is a simple node that anyone can run on their computer. Edge nodes do
 not have any stake. Edge nodes cannot directly ommunicate with core nodes.

### Monetary Policy

Cardano is currently in its bootstrapping phase. During this phase, fees are not
 being collected and no Ada is being minted.<br>
This treasury will be endowed via some portion (yet to be defined), of
 newly-minted Ada and transaction fees.<br>
The minimal fee (current calculation) = 0.155381 ADA + 0.000043946 (ADA/Byte) x
 size-of-transaction<br>
Ada has six decimal places (unit: Lovelace).

### Transaction Fees

In Cardano SL, transaction fees are the only source of income for participants
 in the protocol.<br>
All transaction fees of a given epoch are collected in a virtual pool, and the
 idea is to then redistribute the money from that pool amongst people who were
 elected slot leaders.<br>
At this stage of Cardano SL, where all blocks are created by nodes operated by
 IOHK and our partners, fees are already collected (to prevent DDoS attacks),
 but they will not be distributed and instead will be burnt. As soon as Cardano
 SL enters its next, full decentralized stage, fees will be distributed as
 described above.

### Transaction Assurance Level

### Explorer

## Cardano Timeline

### Testnet Era

### Bootstrap Era

As people who purchased Ada redeem their coins, the stake will automatically get
 delegated to a pool of trusted nodes that will maintain the network. During
 this time no block rewards will be issued.<br>
The Bootstrap era is the period of Cardano SL existence that allows only fixed
 predefined users to have control over the system. The set of such users (the
 bootstrap stakeholders) and proportion of total stake each of them controls is
 defined in genesis block. During Bootstrap era stake should be effectively
 delegated to a fixed set of 7 keys. At the end of Bootstrap era stake should be
 unlocked.<br>
Purpose of Bootstrap era is to address concern that at the beginning of mainnet
 majority of stake will probably be offline (which breaks the protocol at the
 start). Bootstrap era is to be ended when network stabilizes and majority of
 stake is present online.<br>
Bootstrap stakeholders will vote for Bootstrap era ending. Users need to
 explicitly state they understanding owning their stake leads to responsibility
 to handle the node.

### Reward Era

## Technical details

slot duration: 120 seconds. security parameter *k*: 60. an epoch has `10×k`
 slots in it.<br>
slot leader may delegate its right to another node.<br>
This worker also creates a *genesis block* at the beginning of the epoch. There
 are two kinds of blocks: "genesis blocks" and "main blocks". Main blocks are
 stored in the blockchain; genesis blocks are generated by each node internally
 between epochs. Genesis blocks aren't announced to other nodes.<br>
(fork choice rule) Recall that only blocks `k` and more slots deep are
 considered stable. From the standpoint of state, we store and maintain all the
 alternative chains that are viable. If it appears that an alternative chain is
 longer than the main chain, they are swapped.<br>
We only take advantage of Kademlia DHT peer discovery mechanism, and use none of
 its hash table capabilities.

### Cardano SL CLI Options

### Cardano SL Launcher

### Cardano SL Updater

### Cardano SL Explorer

[Explorer Web API][explorer_web_api]

[explorer_web_api]: https://cardanodocs.com/technical/explorer/api/

### Blocks in Cardano SL

The block's body payloads includes transaction payload, SSC payload, delegation
 payload, and update payload.<br>
A genesis block doesn't contain transactions. There is the list of slot-leaders
 for this epoch in the body of the block.

### Leader Selection in Cardano SL

With P2SH addresses, node doesn't know who is going to end up with funds sent to
 them. Therefore, P2SH addresses can contain destination address which specifies
 which addresses should count as "owning" funds for the purposes of FTS.

### Cardano SL PVSS

We use `prime256v1` elliptic curve, see [RFC][rfc_5480].<br>
DLEQ (discrete log equivalence)

[rfc_5480]: https://www.ietf.org/rfc/rfc5480.txt

### Stake Delegation in Cardano SL

Heavyweight delegation is using stake threshold `T`. Heavyweight delegation has
 transitive relation.<br>
WARNING: Currently, lightweight delegation is disabled and will be reworked in
 Shelley release, so information below can be outdated.<br>
Proxy signing certificates for lightweight delegation are not stored in the
 blockchain, so lightweight delegation certificate must be broadcasted to reach
 delegate.<br>
The rule "only one certificate per epoch" doesn't apply to lightweight
 delegation.<br>
On the contrary, stake for lightweight delegation won't be counted in delegate's
 MPC-related stake. So lightweight delegation can be used for block generation
 only.

### HD wallets

Crucial point in design: root public keys are not used to actually store
 money.<br>
It is split into two parts: 1. Extension of wallet backend API to support HD
 wallet structure locally (as it is done in Bitcoin). 2. Extension to blockchain
 handling to utilize new address attribute to keep HD structure on several
 client instances in sync.<br>
This maps to a HD tree: wallet set corresponds to key of 0-th level (root).
 wallet corresponds to key of 1-th level (children of root). address corresponds
 to key of 2-th level (grandchildren of root).

### Cardano SL Wallet Backend

### Formal specification for a Cardano wallet

[Formal specification for a Cardano wallet][formal_specification_for_a_cardano_wallet]

[formal_specification_for_a_cardano_wallet]: https://cardanodocs.com/files/formal-specification-of-the-cardano-wallet.pdf

### Cardano SL Wallet Frontend

`npm install -g wscat`

### Protocols

#### CSL Application-Level Messaing

...

### Plutus

eagerly evaluated<br>
`Comp A -> success A | failure`

#### Types

`addInt : (Int,Int) ⇀  Int`, The use of these built-in functions is by prefixing the name with `!`: `!addInt 2 3`<br>
`case foo0 | foo1 of { Bar | Bar -> 0 ; Baz x | Baz y -> !addInt x y }`<br>
Case analysis is not required to be total. Any failed match causes the entire
 program to fail to run, and will cause a transaction to be considered invalid.

#### Examples

## Glossary

Peer discovery we use is based on Kademlia DHT.<br>
SSC (Shared Seed Computation) is a part of slot leader election process. This
 part is implemented as a Coin Tossing protocol with G.O.D. (Guaranteed Output
 Delivery).

