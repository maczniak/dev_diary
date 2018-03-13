# [The Book of Satoshi][homepage] by [Phil Champagne][phil_champagne], e53 Publishing LLC (2014)

Where is a free copy in eBook format?

[homepage]: http://www.bookofsatoshi.com/
[phil_champagne]: https://github.com/philchampagne

## 1. Introduction

Satoshi's two-year "public life" overlapping Bitcoin's development and launch
 began with the publication of his paper "Bitcoin: A Peer-to-Peer Electronic
 Cash System", which he announced on November 1st, 2008, on the Cryptography
 Mailing List. At that time, this paper could be downloaded at domain name
 *bitcoin.org*, which had been registered a few months earlier on August 18th,
 2008, through *anonymousspeech.com*. On November 9th, 2008, the Bitcoin project
 was registered on *SourceForge.net* and, at the beginning of 2009, the genesis
 block was created. Satoshi incorporated this interesting quote into the genesis
 block in reference to the bank bailouts occurring at the time: "The Times
 03/Jan/2009 Chancellor on brink of second bailout for banks". Six days later,
 on January 9th, 2009, Nakamoto published the source code of Bitcoin version
 0.01 on *SourceForge.net*. Satoshi's last post was published on the
 *bitcointalk.org* forum on December 12th, 2010. His last known communication is
 a private email sent a few months later to Gavin Andersen, current Lead Core
 Developer of the Bitcoin project.<br>
Below is a chart of the public trade data from *bitcoinmarket.com*, the first
 Bitcoin exchange, which is no longer in business. The value of one bitcoin went
 from 10 cents to a dollar in a very short time. At the time of Satoshi's last
 post on the forum, it was trading around 25 cents and was approaching 30 cents
 per bitcoin.<br>
I have chosen to exclude posts of a technical nature, such as those related to
 coding, software compilation, and the detailed technical operation of the
 Bitcoin software.<br>
Satoshi's post seems to indicate that he was not comfortable with Bitcoin
 getting this kind of attention and was not ready for such a relationship, at
 least not yet: "It would have been nice to get this attention in any other
 context. WikiLeaks has kicked the hornet's nest, and the swarm is headed toward
 us." How much this event influenced his decision to "retire" from Bitcoin's
 development is unknown, but the iming is interesting, to say the least.
 Significantly, this post was written just nineteen hours before his last post
 on the forum, the announcement of the release of Bitcoin version 0.3.19.<br>
What is most relevant to this book concerning this episode is that it apparently
 caused Bitcoin's Satoshi Natamoto to break his silence and post this message on
 the *p2pfoundation* forum on Friday March 7th, 2014: "I am not Dorian
 Nakamoto."

## 2. How and Why Bitcoin works

If miners were to reject all blocks but their very own, no consensus would ever
 be reached, the value of the overall system would be destroyed. In such a case,
 whatever amounts of bitcoins the miners hold would then become worthless.
 Therefore, all miners benefit if all respect the Bitcoin protocol established
 within the shared Bitcoin software. Thus, Bitcoin embodies the inverse of the
 tragedy of the commons described earlier.<br>
In essence, this is the Bitcoin equivalent of giving your private encryption
 keys to the merchant. The high number of frauds resulting from this security
 weakness has manifested itself in the form of high fees and chargeback with
 which merchants have to cope.<br>
Unlike money, a currency is subject to inflation. Paper currencies (fiat) allow
 governments to fund deficit spending by stealing from the value of the currency
 in circulation. Printing dollars do not create more barrels of oil, more
 gigawatts of electricity, or more hours in a day. In an economic system based
 on money---currencyh that holds its value over the long term---savers are not
 competing for resources with manufacturers, builders, factories, and those
 extracting commodities (i.e., marketable items) by deferring spending.

## 3. The First Post on Crypto Mailing List

01 Nov 2008

## 4. Scalability Concerns

02 Nov 2008

It seems that Satoshi did not assume the block size limit.

## 5. The 51% Attack

03 Nov 2008

I don't think he could make as much money trying to pull a carding scheme like
 that as he could by generating bitcoins. The Bitcoin network might actually
 reduce spam by diverting zombie farms to generating bitcoins instead.

## 6. About Centrally Controlled Networks Versus Peer-to-Peer Networks

07 Nov 2008

## 7. Satoshi on the Initial Inflation Rate of 35%

08 Nov 2008

The fact that new coins are produced means the money supply increases by a
 planned amount, but this does not necessarily result in inflation. Coins have
 to get initially distributed somehow, and a constant rate seems like the best
 formula.

## 8. About Transactions

Hal Finney, the first recipient of a bitcoin transaction, posed the questions.
 Satoshi refers to six blocks as an appropriate amount of time for a transaction
 to be confirmed and forever made a part of the block chain. Finally, Satoshi
 reports that he wrote the code prior to writing the white paper announcing
 Bitcoin in order to prove to himself that all issues were resolved.

09 Nov 2008

The CPU power proof-of-work vote must have the final say. The only way for
 everyone to stay on the same page is to believe that the longest chain is
 always the valid one, no matter what.

## 9. On the Orphan Blocks

09 Nov 2008

It's not a problem if transactions have to wait one or a few extra cycles to get
 into a block.

## 10. About Synchronization of Transactions

09 Nov 2008

The proof-of-work chain is the solution to the synchronisation problem, and to
 knowing what the globally shared view is without having to trust anyone.

## 11. Satoshi Discusses Transaction Fee

*Seigniorage* is an economic term used to describe the creation of additional
 units of a currency.

10 Nov 2008

## 12. On Confirmation and Block Time

11 Nov 2008

Every block includes its creation time. If the time is off by more than 36
 hours, other nodes won't work on it.<br>
Instant non-repudiability is not a feature, but it's still much faster than
 existing systems. Paper cheques can bounce up to a week or two later. Credit
 card transactions can be contested up to 60 to 180 days later. Bitcoin
 transactions can be sufficiently irreversible in an hour or two.

## 13. The Byzantine General's Problem

[Two Generals' Problem][two_generals_problem],
 [Byzantine fault tolerance][byzantine_fault_tolerance]

13 Nov 2008

The proof-of-work chain is a solution to the Byzantine Generals' Problem. Once
 each general receives whatever attack time he hears first, he sets his computer
 to solve an extremely difficult proof-of-work problem that includes the attack
 time in its hash. If the CPU power exhibited by the proof-of-work chain is
 sufficient to crack the password, they can safely attack at the agreed time.

[two_generals_problem]: https://en.wikipedia.org/wiki/Two_Generals%27_Problem
[byzantine_fault_tolerance]: https://en.wikipedia.org/wiki/Byzantine_fault_tolerance

## 14. On Block Time, an Automated Test, and the Libertarian Viewpoint

14 Nov 2008

If broadcasts turn tou to be slower in practice than expected, the target time
 between blocks may have to be increased to avoid wasting resources. We want
 blocks to usually propagate in much less time than it takes to generate them,
 otherwise nodes would spend too much time working on obsolete blocks.<br>
I'm better with code than with words though.

## 15. More on Double Spend, Proof-of-Work, and Transaction Fees

17 Nov 2008

## 16. On Elliptic Curve Cryptograpghy, Denial of Service Attacks, and Confirmation

17 Nov 2008

## 17. More on the Transaction Pool, Networking Broadcast, and Coding Details

17 Nov 2008

I believe I've worked through all those little details over the last year and a
 half while coding it, and there were a lot of them. The functional details are
 not covered in the paper, but the sourcecode is coming soon. I sent you the
 main files. (available by request at the moment, full release soon)

## 18. First Release of Bitcoin

09 Jan 2009

http://downloads.sourceforge.net/bitcoin/bitcoin-0.1.0.rar<br>
Windows only for now.<br>
Generated coins must wait 120 blocks to mature before they can be spent.<br>
If the recipient is online, you can enter their IP address and it will connect,
 get a new public key and send the transaction with comments.<br>
It's based on open market competition, and there will probably always be nodes
 willing to process transactions for free.

## 19. On the Purpose For Which Bitcoin Could Be Used First

17 Jan 2009

I would be surprised if 10 years from now we're not using electronic currency in
 some way, now that we know a way to do it that won't inevitably get dumbed down
 when the trusted third party *(based systems like Digicash)* gets cold
 feet.<br>
It could get started in a narrow niche like reward points, donation tokens,
 currency for a game or micropayments for adult sites.<br>
It can already be used for pay-to-send e-mail. ... If someone famous is getting
 more e-mail than they can read, but would still kile to have a way for fans to
 contact them, they could set up Bitcoin and give out the IP address on their
 website. "Send X bitcoins to my priority hotline at this IP and I'll read the
 message personally."<br>
If enough people think the same way, that becomes a self fulfilling prophecy.
 Once it gets bootstrapped, there are so many applications if you could
 effortlessly pay a few cents to a website as easily as dropping coins in a
 vending machine.

23 Sep 2010

Bitcoin would be convenient for people who don't have a credit card or don't
 want to use the cards they have, either don't want the spouse to see it on the
 bill or don't trust giving their number to "porn guys", or afraid of recurring
 billing.

## 20. "Proof-of-Work" Tokens and Spammers

25 Jan 2009

Another factor that would mitigate spam if POW tokens have value: there would be
 a profit motive for people to set up massive quantities of fake e-mail accounts
 to harvest POW tokens from spam.

## 21. Bitcoin Announced on P2P Foundation

11 Feb 2009

It's completely decentralized, with no central server or trusted parties,
 because everything is based on crypto proof instead of trust.<br>
Their massive overhead costs make micropayments impossible.<br>
A generation ago, multi-user time-sharing computer systems had a similar
 problem.`Before strong encryption, users had to rely on password protection to
 secure their files, placing trust in the system administrator to keep their
 information private.

## 22. On Decentralization as Key to Success

15 Feb 2009

## 23. On the Subject of Money Supply

18 Feb 2009

## 24. Release of Bitcoin v0.1.3

12 Jan 2009

## 25. On Timestamping Documents

04 Mar 2009 (Bitcoin v0.1.5 released)

Indeed, Bitcoin is a distributed secure timestamp server for transactions. A few
 lines of code could create a transaction with an extra hash in it of anything
 that needs to be timestamped. I should add a command to timestamp a file that
 way.

## 26. Bitcointalk Forum Welcome Message

22 Nov 2009

## 27. On Bitcoin Maturation

22 Nov 2009

22 Oct 2009

22 Nov 2009

## 28. How Anonymous Are Bitcoins?

25 Nov 2009

When you send to an IP address, the transaction is still written to a bitcoin
 address. The IP address is only used to connect to the recipient's computer to
 request a fresh bitcoin address, give the transaction directly to the recipient
 and get a confirmation.<br>
For greater privacy, it's best to use bitcoin addresses only once. ... Transfers
 by IP address automatically use a new bitcoin address each time.<br>
The problem for TOR is that the IRC server which Bitcoin uses to initially
 discover other nodes bans the TOR exit nodes, as all IRC servers do.

## 29. A Few Questions Answered by Satoshi

10 Dec 2009

Since the effective circulation is reduced, all the remaining coins are worth
 slightly more. It's the opposite of when a government prints money and the
 value of existing money goes down.<br>
(How many coins are created by a machine in 24h in average ?) Typically a few
 hundred right now.<br>
I think it's essential for a program of this nature to be open source.<br>
The Bitcoin network has been running for almost a year now. The design and
 coding started in 2007.<br>
That's true, with the send-to-IP option, you are sending to whoever answers that
 IP. Sending to a bitcoin address doesn't have that problem. The plan is to
 implement an IP + bitcoin address option that would have the benefits of both.
 It would still use a different address for each transaction, but the receiver
 would sign the one-time-use address with the given bitcoin address to prove it
 belongs to the intended receiver.

## 30. On "Natural Deflation"

Note that natural currencies today are born out of debt.<br>
On the other hand, when a currency is intrinsically fixed in amount, loans are
 extremely rare. Before the creation of the Federal Reserve in the USA in 1913,
 the majority of purchases were done in cash, even for houses. People would not
 have to speculate in mutual funds for their retirement; instead one could
 simply save the money to make a purchase. This is typically called "hoarding"
 by the financial media, but so are retirement funds. Essentially, saving means
 you are delaying consumption of material, resources, and time so that others,
 including companies investing in new plants, can improve productivity now.

13 Dec 2009

"natural deflation"... I like that name for it.

21 Jun 2010

Lost coins only make everyone else's coins worth slightly more. Think of it as a
 donation to everyone.<br>
Computers have to get about 2 ^ 200 times faster before that starts to be a
 problem. Someone with lots of compute power could make more money by generating
 than by trying to steal.

## 31. Bitcoin Version 0.2 is Here!

16 Dec 2009

## 32. Recommendation on Ways to Do a Payment For an Order

29 Jan 2010

RSA vs ECDSA: it's not the size of the executable but the size of the data.

## 33. On the Proof-of-Work Difficulty
 
05 Feb 2010

We had our first automatic adjustment of the proof-of-work difficulty on 30 Dec
 2009. The minimum difficulty is 32 zero bits, so even if only one person was
 running a node, the difficulty doesn't get any easier than that. For most of
 last year, we were hovering below the minimum. On 30 Dec we broke above it and
 the algorithm adjusted to more difficulty. It's been getting more difficult at
 each adjustment since then.<br>
(difficulty table from 30 Dec 2009 to 26 Aug 2010)

## 34. On the Bitcoin Limit and Profitability of Nodes

21 Feb 2010

## 35. On the Possibility of Bitcoin Address Collisions

23 Feb 2010

## 36. QR Code

24 Feb 2010

24 Feb 2010

## 37. Bitcoin Icon/Logo

24 Feb 2010

## 38. GPL License Versus MIT License

24 Feb 2010 (about a "we accept Bitcoin" logo)

## 39. On Money Transfer Regulations

03 Mar 2010

## 40. On the Poissibility of a Cryptographic Weakness

14 Jun 2010

10 Jul 2010

16 Jul 2010

## 41. On a Variety of Transaction Types

17 Jun 2010

The nature of Bitcoin is such that once version 0.1 was released, the core
 design was set in stone for the rest of its lifetime. Because of that, I wanted
 to design it to support every possible transaction type I could think of. ...
 The solution was script, which generalizes the problem so transacting parties
 can describe their transaction as a predicate that the node network evaluates.
 ... The script is actually a predicate. It's just an equation that evaluates to
 true or false. Predicate is a long and unfamiliar word so I called it script.
 ... All versions of nodes in the network can verify and process any new
 transactions into blocks, even though they may not know how to read them. The
 design supports a tremendous variety of possible transaction types that I
 designed years ago. Escrow transactions, bonded contracts, third party
 arbitration, multi-party signature, etc. If Bitcoin catches on in a big way,
 these are things we'll want to explore in the future, but they all had to be
 designed at the beginning to make sure they would be possible later. I don't
 believe a second, compatible implementation of Bitcoin will ever be a good
 idea. The MIT license is compatible with all other licenses and commercial
 uses, so there is no need to rewrite it from a licensing standpoint. A second
 version would be a massive development and maintenance hassle for me. If
 someone was getting ready to fork a second version, I would have to air a lot
 of disclaimers about the risks of using a minority version. ... I know, most
 developers don't like their software forked, but I have real technical reasons
 in this case.

(from Gavin Andresen) I could encode all sorts of interesting information in the
 TxOut script, and if non-hacked clients validated-and-then-ignored those
 transactions it would be a useful covert broadcast communication channel.
 That's a cool feature until it gets popular and somebody decides it would be
 fun to flood payment network with millions of transactions to transfer the
 latest Lady Gaga video to all their friends...

That's one of the reasons for transaction fees.

Since 2007. At some point I became convinced there was a way to do this without
 any trust required at all and couldn't resist to keep thinking about it. Much
 more of the work was designing than coding. Fortunately, so far all the issues
 raised have been things I previously considered and planned for.

## 42. First Bitcoin Faucet

18 Jun 2010

(from Gavin Andresen, currently Lead Core Bitcoin Developer) For my first
 Bitcoin coding project, I decided to do something that sounds really dumb: I
 created a web site that gives away Bitcoin. ... Five ₿ per customer, first come
 first served, I've stocked it with ₿1,100 to start. I'll add more once I', sure
 it is working properly. Why? Because I want the Bitcoin project to succeed, and
 I think it is more likely to be a success if people can get a handful of coins
 to try it out. ... Please try it out and get some free coins, even if you
 already have more Bitcoins than you know what to do with. You can get some and
 then donate them right back; ...

Excellent choice of a first project, nice work. I had planned to do this exact
 thing if someone else didn't do it, so when it gets too hard for mortals to
 generate 50BTC, new users could get some coins to play with right away.
 Donations should be able to keep it filled.

16 July 2010

(from Gavin Andresen) Bitcoin Faucet is handling the slashdotting really well...
 except that I'm running out of coins to give away. over 5,000 have flowed out
 of the Faucet since I refilled it last night. Any of you early adopters who
 generated tens of thousands of coins back in the early days, are you willing to
 send a few to the Faucet to be given away so more people can try out Bitcoin?

5 BTC seems like a lot these days, maybe the normal amount should be 1 or 2 BTC.

## 43. Bitcoin 0.3 Released!

06 Jun 2010

Escape the arbitrary inflation risk of centrally managed currencies! Bitcoin's
 total circulation is limited to 21 million coins.

## 44. On the Segmentation or "Internet Kill Switch"

08 Jun 2010

03 Aug 2010

## 45. On Cornering the Market

09 Jul 2010

[How the Hunt Brothers Capped Gold…Yes, GOLD!][how_the_hunt_brothers_capped_gold],
 [Cornering the market][cornering_the_market]

[how_the_hunt_brothers_capped_gold]: https://web.archive.org/web/20150328060731/http://wealthcycles.com/features/the-hunt-brothers-capped-the-price-of-gold-not-50-silver
[cornering_the_market]: https://en.wikipedia.org/wiki/Cornering_the_market

## 46. On Scalability and Lightweight Clients

14 Jul 2010

I anticipate there will never be more than 100K nodes, probably less. It will
 reach an equilibrium where it's not worth it for more nodes to join in. The
 rest will be lightweight clients, which could be millions. At equilibrium size,
 many nodes will be server farms with one or two network nodes that feed the
 rest of the farm over a LAN.

## 47. On Fast Transaction Problems

17 Jul 2010

29 Jul 2010

The current system where every user is a network node is not the intended
 configuration for large scale. That would be like every Usenet user runs their
 own NNTP server. The design supports letting users just be users. The more 
 burden it is to run a node, the fewer nodes there will be. Those few nodes will
 be big server farms.

If you don't believe me or don't get it, I don't have time to try to convince
 you, sorry.

## 48. Wikipedia Article Entry on Bitcoin

On July 31th, the article was officially deleted, and then later restored (maybe
 on Dec 2010).

20 Jul 2010

Bitcoin is an implementation of Wei Dai's b-money proposal
 http://weidai.com/bmoney.txt on Cypherpunks
 http://en.wikipedia.org/wiki/Cypherpunks in 1998 and Nick Szabo's Bitgold
 proposal http://unenumerated.blogspot.com/2005/12/bit-gold.html<br>
The timing is strange, just as we are getting a rapid increase in 3rd party
 coverage after getting slashdotted.

30 Sep 2010

## 49. On the Possibility of Stealing Coins

Sayoshi did this for two reasons. One was to reduce the size of each transaction
 as the hash is only 160 bits long. The second benefit was that it conveniently
 added one more layer of security in case a "backdoor" or security flaw should
 one day be discovered in the asymmetric cryptography algorithm used by Bitcoin.

25 Jul 2010

25 Jul 2010

25 Jul 2010

25 Jul 2010

25 Jul 2010

## 50. Major Flaw Discovered

## 51. On Flood Attack Prevention

## 52. Drainage of Bitcoin Faucet

## 53. Transaction to IP Address Rather than Bitcoin Address

## 54. On Escrow and Multi-Signature Transactions

## 55. On Bitcoin Mining as a Waste of Resources

## 56. On an Alternate Type of Block Chain with Just Hash Records

## 57. On the Higher Cost of Mining

## 58. On the Development of an Alert System

## 59. On the Definition of Money and Bitcoin

## 60. On the Requirement of a Transaction Fee

## 61. On Sites with CAPTCHA and Paypal Requirements

## 62. On Short Messages in the Block Chain

## 63. On Handling a Transaction Spam Flood Attack

## 64. On Pool Mining Techicalities

## 65. On WikiLeaks Using Bitcoin

## 66. On a Distributed Domain Name Server

## 67. On a *PC World* Article on Bitcoin and WikiLeaks Kicking the Hornet's Nest

## 68. Satoshi's Last Forum Post: Release of Bitcoin 0.3.19

## 69. Emails to Dustin Trammel

## 70. Last Private Correspondence

## 71. Bitcoin and Me (Hal Finney)

## 72. Conclusion

## (Addendum) 73. On Bitcoin XT

