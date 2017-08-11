# [빅 데이터 시대를 위한 NoSQL 핵심 가이드][homepage], [Tatsuya Sasaki][author] ([twitter][author_twitter]) 저, 손은영 역, 로드북 (2011)

[source code][source_code], [errata][errata]<br>
memcached 1.4.5, Tokyo Tyrant 1.1.41 (Tokyo Cabinet 1.4.46), Redis 2.0.4,
 MongoDB 1.6.5, Cassandra 0.7.9, HBase 0.90.4 (Hadoop 0.20.203.0)<br>
CentOS 5.4, Ruby 1.8.7, MySQL 5.0.77

[homepage]: http://roadbook.co.kr/62
[author]: http://blog.livedoor.jp/sasata299
[author_twitter]: https://twitter.com/sasata299
[source_code]: http://roadbook.co.kr/61
[errata]: http://roadbook.co.kr/60

## Chapter 1. NoSQL 데이터베이스의 기초지식

## Chapter 2. NoSQL 데이터베이스의 종류와 특징

### memcached

`set <key> <flag> <expires> <byte>`, `get`, `incr`, `desr`, `delete`, `append`,
 `flush_all`<br>
[repcached][repcached] for replication<br>
`stats item`, `stats cachedump <slab_id> <num>`, `memcached-tool`<br>
`gets`, `cas <key> <flag> <expires> <byte> <casid>`

[repcached]: http://repcached.lab.klab.org/

### Tokyo Tyrant

[Tokyo Tyrant][tokyo_tyrant] → [Kyoto Tycoon][kyoto_tycoon]<br>
Ruby tools - MiyazakiResistance, ActiveTokyoCabinet

[tokyo_tyrant]: http://fallabs.com/tokyotyrant/
[kyoto_tycoon]: http://fallabs.com/kyototycoon/

### Redis

[linearizability][linearizability]<br>
2.0 버전부터는 독자적인 가상 메모리 기능을 추가하여 실제 메모리에 올릴 수 없는
 테이터를 디스크에 올려서 사용하도록 하고 있습니다. 가상 메모리 기능은 OS에서
 사용하는 SWAP과 동일합니다만, Redis에서는 Linux 커널을 참고하여 단독으로
 구현하였다고 합니다.<br>
(string) `set <key> <byte>`, `get`, `setex <key> <expires> <byte>`<br>
(list) `lpush <key> <byte>`, `lrange <key> <start> <end>`, `rpush`, `llen`,
 `ltrim`, `lindex`<br>
(set) `sadd`, `scard` (size), `smembers`, `sunion`, `sinter`, `spop`
 (random)<br>
(sorted set) `zadd <key> <score> <byte>`, `zrange`,
 `zrangebyscore <key> <min> <max>`<br>
(hash) `hset`, `hget`, `hkeys`, `hexists` (glob), `hdel`<br>
`del`, `keys` (glob), `exists`, `rename`, `type`

[linearizability]: https://en.wikipedia.org/wiki/Linearizability

### MongoDB

table - collection, record - document<br>
[Why are the files in my data directory larger than the data in my database?][faq_disk_size]<br>
`mongo <database, default 'test'>`, `show dbs`, `show collections`

```javascript
db.users.save({ age: 20 })
db.users.find()
var result = db.users.group({ // SELECT age, COUNT(*) FROM users GROUP BY age
 key: { age : true },
 cond: null,
 reduce: function(obj, v) { v.count += 1 },
 initial: { count : 0 }
 })
result.forEach(function(x){
 printjson(x)
 })
```

`mongos` (frontend proxy server, connect to `--configdb`) and multiple
 `mongod`s<br>
consistent hashing 대신 range partitioning<br>
듀얼마스터 구성 (`--replSet`)

```
> use admin
> db.runCommand({ addshard: "localhost:27018", allowLocal : true })
> db.runCommand({ enablesharding : "test" })
> db.runCommand({ shardcollection : "test.users", key : {user_id : 1} })
> use test
> db.users.ensureIndex({ user_id : 1 })
> use admin
> db.printShardingStatus()

# Replica Sets
> config = {_id: 'hoge', members: [
    {_id: 0, host: 'localhost:27017'}]
    }
> rs.initiate(config)
> rs.status()
```

[faq_disk_size]: https://docs.mongodb.com/manual/faq/storage/#why-are-the-files-in-my-data-directory-larger-than-the-data-in-my-database

### Cassandra

keyspace (복제 단위) > super column family > column family > column<br>
consistency level<br>
(클러스터 구성 확인) `nodetool -h 10.62.xxx.109 ring`

```
create keyspace TEST;
use TEST;
create column family User with comparator=UTF8Type and default_validation_class=UTF8Type;
set <column family>[<key>][<column>] = <value>;
get <column family>[<key>];
list User;
```

### HBase

```
status
create 'table1', {NAME => 'cf1', VERSIONS => 3}
list
put 'table1', 'row1', 'cf1:c', 'val1'
scan 'table1'
get 'table1', 'row2'
disable 'table1'
drop 'table1'
```

## Chapter 3. NoSQL 데이터베이스를 사용해 본다

MongoDB에서는 embed라는 방식으로 JOIN고 같은 처리를 수행할 수 있습니다. embed된
 콜렉션에서는 find 등의 메소드가 없어서 단독으로 사용할 수 없습니다.

## Chapter 4. 퍼포먼스 검증

## Chapter 5. NoSQL인 관계형 데이터베이스

[히구치][yoshinori_matsunobu]가 만든 [HandlerSocket][handlersocket]
 ([Handler 인터페이스][handler_interface]를 직접 사용하는 MySQL 플러그인), MySQL
 5.1 이상 소스코드 필요 (MySQL 5.1.57)

[yoshinori_matsunobu]: http://yoshinorimatsunobu.blogspot.com/2010/10/using-mysql-as-nosql-story-for.html
[handlersocket]: https://github.com/DeNA/HandlerSocket-Plugin-for-MySQL
[handler_interface]: https://dev.mysql.com/doc/refman/5.7/en/handler.html

