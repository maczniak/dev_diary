# [Mastering Ethereum][homepage] by Andreas M. Antonopoulos and Gavin Wood, O'Reilly (not yet)

(May 9, 2018 version)

[homepage]: https://ethereumbook.info/

## Glossary

Frontier is the initial test development stage of Ethereum, lasted from July
 30th 2015 to March 2016.<br>
Homestead is the second development stage of Ethereum, launched in March 2016 at
 the block #1,150,000.<br>
Ice Age is a hard fork of Ethereum at block #200,000 to introduce an exponential
 difficulty increase (aka Difficulty Bomb), motivating a transition to
 Proof-of-Stake.<br>
Metropolis is the third development stage of Ethereum, launched in October
 2017.<br>
Byzantium is the first of two hard forks for Metropolis development stage. It
 included EIP-649: Metropolis Difficulty Bomb Delay and Block Reward Reduction,
 where the Ice Age was delayed by 1 year, and the block reward was reduced from
 5 to 3 ether.<br>
Constantinople is the second part of the Metropolis stage, planned for mid-2018.
 Expected to include a switch to hybrid Proof-of-Work/Proof-of-Stake consensus
 algorithm, among other changes.

This blockchain varies from the Bitcoin protocol's in that it does not have a
 block size limit, it instead uses varying gas limits.<br>
consensus vs consensus rules<br>
DAO also may refer to a contract named "The DAO" launched on 30th April 2016,
 which was then hacked in June 2016 and ultimately motivated a hard fork
 (codenamed DAO) at block #1,192,000 that reserved the hacked DAO contract.<br>
Deed is non-fungible token standard introduced in ERC721 proposal. Unlike ERC20
 tokens, deeds prove ownership and are not interchangeable, though they are not
 recognized as "legal documents" in any jurisdiction, at least not
 currently.<br>
[ENS][ens] (Ethereum Name Service)<br>
ERC (Ethereum Request for Comments). Some EIPs are labeled as ERCs, which
 denotes proposals attempting to define a specific standard of Ethereum
 usage.<br>
Ganache is personal Ethereum blockchain which you can use to run tests, execute
 commands, and inspect state while controlling how the chain operates.<br>
Hard fork is a permanent divergence in the blockchain, commonly occurs when
 non-upgraded nodes can't validate blocks created by upgraded nodes that follow
 newer consensus rules.<br>
Inter exchange Client Address Protocol (ICAP) is an Ethereum Address encoding
 that is partly compatible with the International Bank Account Number (IBAN)
 encoding, offering a versatile, checksummed and interoperable encoding for
 Ethereum Addresses. ICAP addresses can encode Ethereum Addresses or common
 names registered with an Ethereum name registry. They always begin with XE. The
 aim is to introduce a new IBAN country code: XE, Ethereum E prefixed with the
 "extended" X, as used in non-jurisdictional currencies (e.g. XBT, XRP,
 XCP).<br>
Key Derivation Function (KDF) is also known as a password stretching algorithm,
 it is used by keystore format which to protect against brute-force, dictionary,
 or rainbow table attacks against the passphrase encryption. It repeatedly
 hashes the passphrase.<br>
Keystore File is a JSON-encoded file that contains a single private key,
 encypted by a passphrase for extra security.<br>
Message is an internal transaction that is never serialized and only sent within
 the EVM.<br>
Mist is the first ever Ethereum enabled browser, built by the Ethereum
 Foundation. It also contains a browser based wallet that was the first ever
 implementation of the ERC20 token standard (Fabian Vogelsteller, author of
 ERC20 was also the main developer in Mist). Mist was also the first wallet to
 introduce the camelCase checksum (EIP-155). Mist runs a full node, and offers a
 full DApp browser with support for Swarm based storage and ENS addresses.<br>
In cryptography, the term nonce is used to refer to a value that can only be
 used once.

[ens]: https://github.com/ethereum/ens/

## 1. What is Ethereum

The term used to describe this evolution (decentralization with peer-to-peer
 protocols) is *Web3*, meaning the third "version" of the web. First proposed
 by Gavin Wood.

## 2. Introduction

Emerald Wallet is designed to work with Ethereum Classic blockchain, but
 compatible with other Ethereum based blockchains.<br>
The issue with Ropsten was that the attacker minted tens of thousands of blocks,
 producing huge reorgs and pushing the gas limit up to 9B. A new public testnet
 was required then, but later (on 25th March 2017) Ropsten was also revived!<br>
Kovan Test Network is the Ethereum public test blockchain and network, using
 "Aura" protocol for Proof-of-Authority consensus (federated signing). This test
 network is supported by "Parity" only. Other Ethereum clients use 'Clique'
 protocol, which was propsed later, for Proof-of-Authority.<br>
This function is a so-called "fallback" or default function, which is called if
 the transaction that triggered the contract didn't name any of the declared
 functions in the contract, or any function at all, or didn't contain data.
 Contracts can have one such default function (without a name) and it is usually
 the one that receives ether.<br>
A new tab has appeared in the contract's address history page, named "Internal
 Transactions". Because the 0.1 ether transfer originated from the contract
 code, it is an internal transaction (also called a message).<br>
We enclose it in double quotes, to allow Remix to receive it as a string and
 manipulate it as a BigNumber. If we don't enclose it in quotes, the Remix IDE
 will fail to process it and display "Error encoding arguments: Error: Assertion
 failed"

## 3. Ethereum Clients

Among these Ethereum-based networks are: Ethereum, Ethereum Classic, Ella,
 Expanse, Ubiq, Musicoin, and many others.<br>
mantis in Scala<br>
Usually, a lightweight client offers an API (such as the web3js API) in addition
 to the transaction functionality of a wallet.<br>
Do not confuse the concept of a lightweight wallet in Ethereum with that of a
 Simplified Payment Verification (SPV) client in Bitcoin. Ethereum lightweight
 clients, generally, do not validate block headers or transactions.<br>
TestRPC creates a local-only, private blockchain that you can interact with,
 without any other participants.<br>
blockchain's latest size - [Ethereum][bitinfocharts_eth],
 [Ethereum Classic][bitinfocharts_etc]<br>
Typically every blockchain will have their own version of Geth, while Parity
 provides support for multiple Ethereum-based blockchains (Ethereum, Ethereum
 Classic, Ellaism, Expanse, Musicoin).<br>
Parity requires Rust version 1.24 or greater. Parity also requires some software
 libraries, such as OpenSSL and libudev (introspect and enumerate devices on the
 local system).<br>
Parity represents about 28% of the installed Ethereum client base.<br>
You will see this referred to as the JSON-RPC API. The JSON-RPC request is
 formatted according to the [JSON-RPC 2.0 specification][json_rpc_2_spec].

[bitinfocharts_eth]: https://bitinfocharts.com/ethereum/
[bitinfocharts_etc]: https://bitinfocharts.com/ethereum%20classic/
[json_rpc_2_spec]: http://www.jsonrpc.org/specification

## 4. Ethereum Testnets

...<br>
Parity has a special "Geth Compatibility Mode", where it offers a JSON-RPC API
 that is identical to that offered by geth.<br>
...<br>
Geth and Parity can only operate fast synchronization when starting with an
 empty block database.

[eip_161]: https://eips.ethereum.org/EIPS/eip-161

### (break) [JSON-RPC][json_rpc]

Geth 1.4 has experimental [pub/sub][geth_pub_sub] support. Parity 1.6 has
 experimental [pub/sub][parity_pub_sub] support.<br>
[JavaScript API][javascript_api], [console][javascript_console]<br>
When encoding QUANTITIES (integers, numbers): encode as hex, prefix with "0x",
 the most compact representation (slight exception: zero should be represented
 as "0x0"). When encoding  UNFORMATTED DATA (byte arrays, account addresses,
 hashes, bytecode arrays): encode as hex, prefix with "0x", two hex digits per
 byte (allows `0x`).<br>
The position of an element in the map is calculated with:
 `keccack(LeftPad32(key, 0), LeftPad32(map position, 0))`<br>
The sign method `eth_sign` calculates an Ethereum specific signature with:
 `sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))`<br>
An example how to use solidity ecrecover to verify the signature calculated with
 `eth_sign` can be found [here][solidity_ecrecover]. The contract is deployed on
 the testnet Ropsten and Rinkeby.<br>
`eth_sendTransaction` - `gas` (default: 90000), `gasPrice` (default:
 To-Be-Determined), `nonce` (optional), returns `DATA`, 32 bytes (the
 transaction hash, or the zero hash if the transaction is not yet available),
 use `eth_getTransactionReceipt` to get the contract address<br>
[Ethereum Contract ABI][ethereum_contract_abi]<br>
`eth_call` executes a new message call immediately without creating a
 transaction on the block chain.<br>
The `eth_estimateGas` estimate may be significantly more than the amount of gas
 actually used by the transaction, for a variety of reasons including EVM
 mechanics and node performance.<br>
`eth_getBlockByHash` - returns `Object` - `totalDifficulty` integer of the total
 difficulty of the chain until this block<br>
`eth_getTransactionReceipt` - returns `Object` - `logs` (`Array`) array of log
 objects, which this transaction generated<br>
An uncle doesn't contain individual transactions.<br>
`eth_getCompilers` returns a list of available compilers in the client.<br>
`eth_newFilter` - `topics` (optional) array of 32 bytes `DATA` topics. Topics
 are order-dependent. `[[A, B], [A, B]]` "(A OR B) in first position AND (A OR
 B) in second position (and anything after)"<br>
(`eth_uninstallFilter`) Additionally Filters timeout when they aren't requested
 with `eth_getFilterChanges` for a period of time.<br>
`eth_getFilterChanges` - `logIndex` integer of the log index position in the
 block, `null` when its pending log, `topics` array of 0 to 4 32 bytes `DATA` of
 indexed log arguments (In solidity, The first topic is the hash of the
 signature of the event (e.g. `Deposit(address,bytes32,uint256)`), except you
 declared the event with the `anonymous` specifier.)<br>
`shh_version` returns the current whisper protocol version.

[json_rpc]: https://github.com/ethereum/wiki/wiki/JSON-RPC
[geth_pub_sub]: https://github.com/ethereum/go-ethereum/wiki/RPC-PUB-SUB
[parity_pub_sub]: https://github.com/paritytech/wiki/blob/master/JSONRPC-Eth-Pub-Sub-Module.md
[javascript_api]: https://github.com/ethereum/wiki/wiki/JavaScript-API
[javascript_console]: https://github.com/ethereum/go-ethereum/wiki/JavaScript-Console
[solidity_ecrecover]: https://gist.github.com/bas-vk/d46d83da2b2b4721efb0907aecdb7ebd
[ethereum_contract_abi]: https://solidity.readthedocs.io/en/develop/abi-spec.html

## 5. Keys and Addresses

## 6. Wallets

## 7. Transactions

## 8. Smart Contracts

## 9. Dev Tools and Frameworks

## 10. Tokens

## 11. DApps

## 12. Oracles

## 13. Accounting & Gas

## 14. EVM

## 15. Consensus

## 16. Vyper

## 17. DevP2P Protocol

## 18. Ethereum Standards (EIPs/ERCs)

## ens, contrib/

