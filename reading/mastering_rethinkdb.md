# [Mastering RethinkDB][homepage] by [Shahid Shaikh][author], Packt Publishing (2016)

[color images][color_images], [source code][source_code]

[homepage]: https://www.packtpub.com/big-data-and-business-intelligence/mastering-rethinkdb
[author]: https://github.com/codeforgeek
[color_images]: http://www.packtpub.com/sites/default/files/downloads/MasteringRethinkDB_ColorImages.pdf
[source_code]: https://github.com/PacktPublishing/Mastering-RethinkDB

## 1. The RethinkDB Architecture and Data Model

RethinkDB is a real-time, open source distributed database.<br>
Once you subscribe to a particular table to look for its changes, RethinkDB just
 keeps pushing the old and new values of changes to the connected client.
 (change feed) But why are we using socket when RethinkDB can provide us the
 changes in the data? RethinkDB provides it to the middle layer and not the
 client. But the RethinkDB team is working on another project called Horizon,
 which solves the issue mentioned previously, to allow clients to communicate to
 the database using secure layer of the middle tier.<br>
transmission of that data in streams rather than as a whole<br>
if a client only requests a certain result that is not in a shared or replicated
 server, it will not execute the parallel operation and just return the result
 set. (lazy execution)<br>
block-level MVCC (Multiversion Concurrency Control). RethinkDB provides
 atomicity to a document no matter what combination of operations is being
 performed. RethinkDB limits this atomicity to a few operations. For example,
 results coming from JavaScript code cannot be performed atomically. The result
 of a subquery is also not atomic. Replace cannot be performed atomically.<br>
Direct I/O is a filesystem for those applications that want to avoid the
 buffering at the main memory and directly read files from disk. (direct I/O
 filesystem mount, `O_DIRECT` option in the `open()` system call)<br>
RethinkDB uses a custom-built storage engine inspired by the Binary tree file
 system by Oracle (BTRFS).<br>
RethinkDB uses the table's primary key to perform all sharding operations and it
 cannot use any other keys to do so. You cannot specify the split point
 manually. You cannot have less server than your shard. You can always visit the
 RethinkDB administrative screen to increase the number of shards or replicas.
 The directory maintains a list of node (RethinkDB instance) responsibilities
 for each shard. Each node is responsible for maintaining the updated version of
 the directory. RethinkDB allows users to provide the location of shards.
 However, you need to set up RethinkDB servers manually using the command line
 and it cannot be done via web-based interfaces. Currently, RethinkDB does not
 allow more than one replica in a single RethinkDB instance due to some
 technical limitations.<br>
If the user does not provide primary key information during the creation of the
 table, RethinkDB uses its default name ID. The default-generated primary key
 contains information about the shard's location in order to directly fetch the
 information from the appropriate shard. The primary key of each shard is
 indexed using the B-Tree data structure. RethinkDB also provides the secondary
 key and compound key (combination of keys) features. It even provides
 multi-index features that allow you to have arrays of values acting as keys,
 which again can be single compound keys. HAving system-generated keys for
 primary is very efficient and fast, because the query execution engine can
 immediately determine on which shard the data is present.<br>
For the automatic failover:
* The cluster must have three or more nodes (RethinkDB servers)
* The table must be set to have three or more replicas set with the voting
  option
* During failover, the majority of replicas (greater than half of all replicas)
  for the table must be online

Every table, by default, has a primary replica created by RethinkDB. You can
 always change that using the `reconfigure()` command.<br>
In such case--where RethinnkDB cannot perform failover--you need to do it
 manually using `reconfigure()` command:
```
r.table(users).reconfigure(
{emergencyRepair: "unsafe_rollback_or_erase"}
).run(conn, callback);
```
healthy (if more than half of the total shards are available), repairable (if
 the shard is not healthy but has one replica of the table, regardless of voting or nonvoting, which can be used), beyond repair (if the shard has no available
 replica and cannot be used)<br>
RethinkDB will first change all the offline replicas into non-voting replicas.
 If there is no voting replica available, RethinkDB will choose one non-voting
 replica and forcefully convert it into a voting replica: `unsafe_rollback`
 (leave those shards that are beyond repair), `unsafe_rollback_or_erase` (delete
 those shards that are beyond repair and create one on the available server that
 holds another shard for that table)<br>
One of the amazing functionalities of RethinkDB is calling external APIs from
 RethinkDB native drivers.<br>
RethinkDB provides and supports geospatial features to help you build
 location-based applications.
* There is a limit on shard creation, which is a maximum of 64 shards
* An empty table will need up to 4 MB
* Each table, after population of documents, requires at last 10 MB of disk
  space on each server wherever it is replicated in the cluster
* Each table consumes 8 MB of RAM on each server

RethinkDB organizes the data into blocks, with size ranging from 512 bytes to 4
 KB. Out of these blocks, approximately 10 to 26 bytes per block are kept in
 memory.<br>
Page cache is a very important aspect of performance. Cache size = (available
 memory - 1024 MB) / 2. If the cache size is less than 1224 MB, then RethinkDB
 set the size of page cache to 100 MB. This is why it is recommended to have at
 least 2 GB of RAM allocated for RethinkDB processes.<br>
Every database uses some memory to store the results of ongoing running queries.
 a rough estimate is between 1 MB and 20 MB, including background processes such
 as transferring data between nodes, voting processes, and so on.

## 2. RethinkDB Query Language

ReQL means RethinkDB query language. It is built on three important principles:
 embedding ReQL in a programming language, ReQL queries being chainable, and
 ReQL queries being executed on the server.<br>
administrative web console (port 8080)<br>
One of the best ways to manage a callback hell situation and which I use very
 frequently while coding Node.js programs is by using the [async][async] node
 module.<br>
`replace()` method generates a new document and do not preserve the same ID as
 the `update()` method does.<br>
ReQL supports conditional queries using subqueries, expressions, and the lambda
 funtion.<br>
In the web administrative screen, you do not need to provide the `run` function
 with a connection; it automatically appedns and execute the query on the
 server.

```javascript
rethinkdb.table('users').get('e546...')("address")("map")("latitude")
  .run(connection,function(err,data) {
// the connection variable contains the database information.

rethinkdb.table("users").filter(function(user) {
  return user("age").lt(30).and(user("name").eq("John"))
  // between(), during(),
  // match("^J"), split(optional separator, optional count), upcase(), ...
}).run(connection,function(err,cursor) {
  if(err) {
    throw new Error(err);
  }
  cursor.toArray(function(err,data) {
    console.log(data);
  })
});

rethinkdb.expr(2).add(2).run(connection,function(err,reuslt) {

rethinkdb.table("users").group("name").run(connection,function(err,cursor) {
// It is a Group MapReduce (GMR) operation. The group step is optional. Every
//  matching data for the group resides under a `reduction` array. In order to
//  work on each reduction array, you can use ungroup() ReQL function, which in
//  turns takes grouped streams of data and converts it into an array of an object.

// count(), sun(), avg(), max(), min(), distinct(), contains(...)
// Aggregate functions such as count() and sum() already makes use of map and
//  reduce internally, and if required, then group() too.

rethinkdb.table("movies").insert(
  http("http://www.omdbapi.com/?t=avengers&y=2015&plot=short&r=json")
  ).run(connection,function(err,data) {
// r.http('<< URL >>', { method: POST, data: {userId : 10, name : "Shahid",
//   auth : { user: userName, pass: password },
//   resultFormat: 'binary'
// });
```

In case you receive the response in a paginated way, RethinkDB also provides a
 way to solve that. You need to either provide the `page` or `pageLimit`
 parameter depending upon the API you are calling or which one is getting a
 response. RethinkDB will call the API and provide the result in streams, which
 we can access using the `cursor` API.<br>
In order to avail the feature of changefeed, you just need to attach your
 listener for the particular table, and you should receive every single minor
 update happening in the table such as addition, deletion, update, and so on.

```javascript
rethinkdb.table("users").changes().run(connection,function(err,cursor) {
  if(err) {
    throw new Error(err);
  }
  cursor.each(console.log);
});
// rethinkdb.table("users").get("4228...").changes().run(...
// rethinkdb.table("users").filter(...).changes().run(...
// changes({includeInitalKey: true, includeStateskey: true, squash: false})
//  it can buffer a maximum 100,000 changes in a document.
```

[async]: http://caolan.github.io/async/

## 3. Data Exploration Using RethinkDB

[Mockaroo][mockaroo] (online mock data generator)<br>
`r.http(..., {timeout: 220, resultFormat: 'json'})`<br>
(Python tool?)
 `rethinkdb import -f FILE_PATH --table DB.table_name --format JSON/CSV`,
 `-c HOST:PORT`, `-p password_file`, `--pkey primary_key`, `--shards #`,
 `--replicas #`, `--force` (overwrite)

```javascript
cursor.each(function(err,data) {
  data.ctc = parseInt(data.ctc.split("$")[1]);
  rethinkdb.db("company").table("employees")
  .get(data.id)
  .update({ctc : data.ctc})
  .run(connection,function(err,response) {

r.db("company").table('employees').without('id').distinct().count()

r.db("company").table("employees").orderBy(r.desc("ctc")).limit(10)

r.db("company").table('employees').indexCreate('ip_decimal')
  // because the between method of ReQL works on indexes only
r.db("comapny").table('employees')
  .between(3221225729,3758096126,{index : 'ip_decimal'})
```

[mockaroo]: http://www.mockaroo.com/

## 4. Performance Tuning in RethinkDB

```
## Creating a RethinkDB cluster in the same machine
rethinkdb
rethinkdb --port-offset 1 --directory rethinkdb_data2 --join localhost:29015

## Creating a RethinkDB cluster using different machines
rethinkdb --bind all
rethinkdb --join 104.121.23.24:29015 --bind all

## Creating a RethinkDB cluster in production
# in /etc/rethinkdb/instances.d/default.conf of the SEED server
server-name = rethink_main
bind = all
# in /etc/rethinkdb/instances.d/default.conf of the second machine
server-name=rethink_child
bind=all
join=104.121.23.24:29015

## Securing our RethinkDB cluster
openssl genrsa -out key.pem 2048
openssl req -new -x509 -key key.pem -out cert.pem -days 365
# in /etc/rethinkdb/instances.d/default.conf
http-tls-key=...
http-tls-cert=...
bind-http=localhost
rethinkdb --driver-tls-key key.pem --driver-tls-cert cert.pem \
          # for clients
   --cluster-tls-key key.pem --cluster-tls-cert cert.pem --cluster-tls-ca ca.pem
          # for the cluster
connect(..., {ssl: {ca: ...}})
# in /etc/nginx/nginx.conf
location /rethinkdb/admin/
{
  proxy_pass http://127.0.0.1:8080;
}
```

[about the configuration files][config_file_doc]<br>
[Comodo's free SSL certificate][comodo_free_ssl_certificate]<br>
Choose the number of replications and click on "Apply configuration". This takes
 up to a minute depending on the size of the table. During that time, your data
 will not be available to the application.<br>
Once the partitioning has been done by RethinkDB, you can view the number of
 shards in the data distribution graph and how many documents each shard
 contains. The splitting of data was based on the ID column, and according to my
 observation, all the even number IDs go to shard 1, while the odd ones go to
 shard 2. This is purely an assumption as RethinkDB does not provide any
 insights into how their algorithm works. RethinkDB uses the range sharding
 algorithm for now but may switch to the hash sharding algorithm in future. For
 information on why RethinkDB is going to switch to hash sharding, follow [this
 github pull request][hash_sharding_issue].<br>
Replicas which can be used during the failover proess are called voting
 replicas, one which can't is called non-voting replicas. We have one instance
 running in the cluter out of three. This implies that RethinkDB is not able to
 select the primary replica as a part of automatic failover handling.<br>
The SEED server is acting as a master server (however, there is no such
 master/child concept in RethinkDB; we are phrasing this for understanding) to
 perform routing of queries and run various kinds of business logic to determine
 where the data is stored and where to point the requst. This in turn increases
 the load and dependency on one RethinkDB instance. RethinkDB allows us to
 create a proxy node to solve such problems. The proxy node will act like a
 query redirector and will not store any data (and do not execute). We can
 create a proxy node in our cluster and join our other instances to it to form
 the cluster. If there is no RethinkDB instance connected to a proxy, you cannot
 execute the query. A proxy is not counted as a server in the web admin
 console.<br>
`rethinkdb proxy --join localhost:29015` (start RethinkDB on the default port
 (29015) as a proxy node)<br>
Data Explorer section in the web admin console (database profiler) - round trip
 time, server time, shared access<br>
`r.db('company').table('employees').getAll("John", {index: "first_name"})`

[config_file_doc]: https://rethinkdb.com/docs/config-file/
[comodo_free_ssl_certificate]: https://ssl.comodo.com/free-ssl-certificate.php
[hash_sharding_issue]: https://github.com/rethinkdb/rethinkdb/issues/364

## 5. Administration and Troubleshooting Tasks in RethinkDB

user access control - permission + scope<br>
permission - `read`, `write`, `connect` (can perform an `http()` command),
 `config`<br>
scope - Table, Database, Global<br>
Every permission is stored in a `permissions` table in the `rethinkdb` database.
 You can change the permission by using ReQL queries such as `update` or
 `grant`. RethinkDB encourages you to use `grant` for such operations.<br>
The default `admin` user is a global scope user with all the permissions.
 Initially, it is not associated with a password, but you can do so by running a
 simple query such as the following:
 `r.db('rethinkdb').table('users').get('admin').update({password: 'test@2016'});`
 or start RethinkDB using `--inital-password` parameter. However, in the case of
 the RethinkDB web console, it will bypass the password and directly show the
 administrative screen.<br>
`r.db("rethinkdb").table("users").insert({id: 'shahid',password: false})`
 (create new user)<br>
`r.grant("shahid",{read:true,write:true,config:true});` (global scope)<br>
`r.db("company").grant("shahid",...);` (database scope)<br>
`r.db("company").table("user').grant("shahid",...);` (table scope)<br>
To revoke the permissions of a user, you can run the `grant` query again and
 specify `null` or `false` to the permission you need to revoke. If you remove
 the user, the permissions table is flushed automatically.

[`reconfigure` command][reconfigure_command] parameters - `shards`, `replicas`,
 `primaryReplicaTag`, `dryRun`, `nonvotingReplicaTags`, `emergencyRepair`<br>
RethinkDB uses a client driver to perform the backup, which provides the
 concurrency facility, so it doesn't lock any clients while the backup process
 is running.<br>
`rethinkdb dump` (under the admin user privilege) generates a .tar.gz at the
 location where the command is executed. parameters - `-c[onnect]`, `-f[ile]`,
 `-e[xport]` (databases or tables), `-p[assword]`, `-password-file`,
 `-tls-cert`, `-clients` (simultaneous backup), `-temp-dir`<br>
For Windows users, there are utilties such as Task Scheduler and `schtasks`.<br>
[rethinkdb_nightly][nithinkdb_nightly] (backup to the Amazon S3)<br>
`rethinkdb restore` parameters - ..., `--no-secondary-indexes`<br>
methink (migration from MySQL to RethinkDB) - `npm i --g methink`,
 `mysql2rethink -h <host> -u <user name> -p <password> -d <mysql database> -t <mysql table name> -D <rethinkdb database> -T <rethinkdb table name>`<br>
All the terms are the same, except tables in RehinkDB are called collections in
 MongoDB. `mongoexport --db test --collection user --out user.json`<br>
After the completion of the restore operation, you need to manually rebuild the
 secondary indexes. `rethinkdb index-rebuild`<br>
If the error relates to a cache size, say it's too low or out of size, the quick
 way to fix it is of course increasing the cache size. You can increase it by
 passing `--cache-size` followed by memory unit in MBs or putting it in a
 RethinkDB configuration file.<br>
[ReQLPro][reqlpro] (GUI client)<br>
[Chateau][chateau] (web-based data browser) npm package,
 `/usr/local/lib/node_modules/chateau/config.template.js`

[reconfigure_command]: https://www.rethinkdb.com/api/javascript/reconfigure/
[nithinkdb_nightly]: https://github.com/robconery/rethinkdb_nightly
[reqlpro]: http://utils.codehangar.io/rethink
[chateau]: https://github.com/neumino/chateau

## 6. RethinkDB Deployment

All three services are paid but you can get free access for a limited time. If
 you choose the Amazon EC2 server, then it's free for 750 hours. While
 Compose.io provides 30 days' free access, DigitalOcean allows free access only
 if there is an offer running or you were referred by someone.<br>
AWS Marketplace → access Public IP → "Setting up a RethinkDB AMI"<br>
IBM Compose.io services MongoDB, Elasticsearch, RethinkDB, Redis, PostgreSQL,
 etcd, RabbitMQ, ScyllaDB and MySQL.<br>
DigitalOcean → Create Droplet → One-click apps →
 [Horizon w/ RethinkDB on 14.04][digitalocean_horizon]

```
## Dockerfile
FROM ubuntu:latest
# Install RethinkDB.
RUN \
  apt-get update && \
  echo "deb http://download.rethinkdb.com/apt `lsb_release -cs` main" > /etc/apt/sources.list.d/rethinkdb.list && \
  apt-get install -y wget && \
  wget -O- http://download.rethinkdb.com/apt/pubkey.gpg | apt-key add - && \
  apt-get update && \
  apt-get install -y rethinkdb python-pip && \
  rm -rf /var/lib/apt/lists/*
# Install python driver for rethinkdb
RUN pip install rethinkdb
# Define mountable directories.
VOLUME ["/data"]
# Define working directory.
WORKDIR /data
# Define default command.
CMD ["rethinkdb", "--bind", "all"]
# Export ports.
#   - 8080: web UI
#   - 28015: process
#   - 29015: cluster
EXPOSE 8080
EXPOSE 28015
EXPORT 29015

docker pull ubuntu
docker build -t docker-rethinkdb
docker images
docker run -p 3000:8080 -d docker-rethinkdb
dockre ps
docker logs <container id>
docker-machine ip default
docker exec -i -t <container-id> /bin/bash # import/export
```

[digitalocean_horizon]: https://www.digitalocean.com/community/tutorials/how-to-use-the-horizon-one-click-install-image

## 7. Extending RethinkDB

In ElasticSearch, a database is called an index and a table is called a
 type.<br>
[search document][es_search]

```
GET localhost:9200

POST to http://localhost:9002/twitter/tweets
{
   "tweet" : "Hello World!",
   "user": "John",
   "userId": 1024
}

POST to http://localhost:9200/twitter/tweets/_search
{
    "query" : {
        "match_all" : {}
    }
}
or
{
    "query" : {
        "match" : {
          "tweet": "elasticsearch"
        }
    }
}
```

RethinkDB → logstash → ElasticSearch<br>
[about elasticsearch output plugin][about_elasticsearch_output_plugin]<br>
Since we haven't provided the Index and type name, Logstash will create one with
 the name logstash-current-timestamp like `logstash-2016.09.27`. By default,
 Logstash creates a type named `logs`.<br>
ElasticSearch maintains versions of records; so if you update any document, then
 instead of updating that directly to the ElasticSearch document, ElasticSearch
 will create a new copy and call it version 2.

```
# in the logstash directory
./plugin install logstash-input-rethinkdb

# in the bin folder of logstash
./logstash -e '
 input {rethinkdb
    {host => "localhost"
     port => 28015
     auth_key => ""
     watch_dbs => ["company"]
     watch_tables => ["company.employees"]
     backfill => true # whether or not to synchronize existing data in source
     }}
 output { elasticsearch {}}'

POST into http://localhost:9200/logstash-2016.09.27/logs/_count
```

`npm i -S amqplib` (make the entry in `package.json`)

```javascript
// producer.js
var amqp = require('amqplib/callback_api')
amqp.connect('amqp://localhost:5672', function(err, conn) {
  conn.createChannel(function(err, ch) {
     // Create queue
     var q = 'hello'
     ch.assertQueue(q, {durable: false})
     ch.sendToQueue(q, new Buffer('Hello World!'));
     console.log(" [x] Sent 'Hello World!'")
  })
})

// consumer.js
var amqp = require('amqplib/callback_api')
amqp.connect('amqp://localhost:5672', function(err, conn) {
  conn.createChannel(function(err, ch) {
    var q = 'hello'
    ch.assertQueue(q, {durable: false})
    console.log(" [*] Waiting for messages in %s. To exit press CTRL+C", q)
    ch.consume(q, function(msg) {
      console.log(" [x] Received %s", msg.content.toString())
    }, {noAck: true})
  })
})

// producer.js
var r = require('rethinkdb')
var amqp = require('amqplib')
var rethinkConn
var rabbitConn
var channel
var exchange = 'rethinkdb'
r.connect({host: 'localhost', port: 28015}).then(function(conn) {
    // Setup RethinkDB connection
    rethinkConn = conn
}).catch(r.Error.RqlDriverError, function(err){
    console.log(err.message)
    process.exit(1)
}).then(function createDB(){
    return r.dbCreate('change_example').run(rethinkConn)
}).finally(function createTable(){
    return r.db('change_example').tableCreate('mytable').run(rethinkConn)
}).catch(r.Error.RqlRuntimeError, function(){
}).then(function(){
    // Setup rabbit connection
    return amqp.connect('amqp://localhost:5672')
}).then(function(conn){
    rabbitConn = conn
    return rabbitConn.createChannel()
}).then(function(ch){
    channel = ch
    return channel.assertExchange(exchange, 'topic', {durable: false})
}).then(function(){
    // Listen for changes on our table
    return r.db('change_example').table('mytable').changes().run(rethinkConn)
}).then(function(changeCursor){
    // Feed changes into rabbitmq
    changeCursor.each(function(err, change){
        if(err){
            console.log(err.msg)
            process.exit(1)
        }
        var routingKey = 'mytable.' + typeOfChange(change)
        console.log('RethinkDB -(', routingKey, ')-> RabbitMQ')
        channel.publish(exchange, routingKey, new Buffer(JSON.stringify(change)))
    })
}).catch(function(err){
    console.log(err.message)
    process.exit(1)
})
function typeOfChange(change) {
    // Determines whether the change is a create, delete or update
    if(change.old_val === null){
        return 'create'
    } else if(change.new_val === null){
        return 'delete'
    } else {
        return 'update'
    }
    return 'something'
}

// listener.js
var amqp = require('amqplib')
var rabbitConn
var channel
var exchange = 'rethinkdb'
var queue
amqp.connect('amqp://localhost:5672').then(function(conn){
    rabbitConn = conn
    return rabbitConn.createChannel()
}).then(function(ch){
    channel = ch
    return channel.assertExchage(exchange, 'topic', {durable: false})
}).then(function(){
    return channel.assertQueue('', {exclusive: true})
}).then(function(q){
    queue = q.queue
    // Bind the queue to all topics about 'mytable'
    return channel.bindQueue(queue, exchange, 'mytable.*')
}).then(function(){
    console.log('Started listening...')
    channel.consume(queue, function(msg){
        // Handle each message as it comes in from RabbitMQ
        var change = JSON.parse(msg.content)
        var tablename = msg.fields.routingKey.split('.')
        console.log(tablename, '-> RabbitMQ -(',
                    msg.fields.routingKey, ')-> Listener')
        console.log(JSON.stringify(change, undefined, 2))
        console.log(new Array(81).join('=') + '\n')
    })
})
```

[Third-party libraries][third_party_libraries] such as express-session-rethinkdb

[es_search]: https://www.elastic.co/guide/en/elasticsearch/reference/current/search-search.html
[about_elasticsearch_output_plugin]: https://www.elastic.co/guide/en/logstash/2.1/plugins-outputs-elasticsearch.html
[third_party_libraries]: https://www.rethinkdb.com/docs/frameworks-and-libraries/

## 8. Full Stack Development with RethinkDB

`npm install --save express rethinkdb socket.io async body-parser`<br>
[Yarn][yarn] (new package manager based on NPM)<br>
[how `require` actually works][how_require_works]<br>
`async.waterfall`<br>

```
npm instll bower -g --save-dev
bower install angular-material -D &&
bower install angular-route &&
bower install angular-messages
```

[yarn]: https://yarnpkg.com/
[how_require_works]: http://requirejs.org/docs/node.html

## 9. Polyglot Persistence Using RethinkDB

[Why does Quora use MySQL as the data store instead of NoSQLs such as Cassandra, MongoDB, or CouchDB?][why_does_quora_use_mysql_as_the_data_store]<br>
We will develop an application where you can perform the CRUD operation in
 RethinkDB (entry database) and all changed data will be synchronized
 automatically to MySQL and MongoDB.

```javascript
// ployglot.js
const rethinkdb = require('rethinkdb')
const db = require('./db')
const userObject = new db()
const events = require('events')
class Polyglot extends events {
  constructor() {
    super()
    this.assingChangeFeed()
  }
  assingChangeFeed() {
    let self = this
    userObject.connectToDb(function(err,connection) {
      if(err) {
        return callback(true,"Error connecting to database")
      }
      rethinkdb.table('users').changes({"include_types": true})
                              .run(connection,function(err,cursor) {
        if(err) {
          console.log(err)
        }
        cursor.each(function(err,row) {
          console.log(JSON.stringify(row))
          if(Object.keys(row).length > 0) {
            if(!!row.type) {
              switch(row.type) {
                case "add": self.emit('insert',row);break
                case "remove": self.emit('delete',row);break
                case "change": self.emit('update',row);break
              }
            } else {
              console.log("No type field found in the result")
            }
          }
        })
      })
    })
  }
}
module.exports = Polyglot
```

```javascript
// Here is how we can consume the events.
const stream = new MyStream()
stream.on('data', (data) => {
  console.log('Received data: "${data}"')
})
stream.on('fileadded', (fileName) => {
  console.log("New file added",fileName)
})
stream.read("abc.txt")
stream.read("pqr.txt")
stream.read("eee.txt")
stream.write()
```

[MySQL node module][mysql_node_module], [Mongoose][mongoose]

```javascript
sync.on('delete',function(data) {
  let query = "DELETE FROM ?? WHERE ?? = ?"
  query = mysql.format(query,["users","rethinkdb",data.old_val.id])
  connection.query(query,function(err,result) {
    if(err) {
      console.log(err)
    } else {
      console.log("Synced to MySQL")
    }
  })
})

// mongo.js
const mongoose = require('mongoose')
const polyglot = require('./ployglot')
class mongoSync {
  constructor() {
    mongoose.connect('mongodb://localhost:27017/codeforgeek')
    var userSchema = mongoose.Schema({
      "rethinkId" : String,
      "name" : String,
      "dob": Date,
      "gender": String,
      "location": String
    })
    let model = mongoose.model('users',userSchema)
    this.handleOperation(model)
  }
  handleOperation(model) {
    let self = this
    let sync = new polyglot()
    sync.on('insert',function(data) {
      let mongoOp = new model()
      mongoOp.rethinkId = data.new_val.id
      mongoOp.name = data.new_val.name
      mongoOp.dob = new Date(data.new_val.dob)
      mongoOp.gender = data.new_val.gender
      mongoOp.location = data.new_val.location
      mongoOp.save(function(err) {
        if(err) {
          console.log("Error creating mongo data\n",err)
        } else {
          console.log("Synced to MongoDB")
        }
      })
    })
    sync.on('update',function(data) {
      model.findOne({"rethinkId": data.old_val.id},function(err,mongoData) {
        mongoData.name = data.new_val.name
        mongoData.dob = new Date(data.new_val.dob)
        mongoData.gender = data.new_val.gender
        mongoData.location = data.new_val.location
        mongoData.save(function(err) {
          if(err) {
            console.log("Error updating mongo data\n",err)
          } else {
            console.log("Syncted to MongoDB")
          }
        })
      })
    })
    sync.on('delete',function(data) {
      model.find({"rethinkId": data.old_val.id}.function(err,mongoData) {
        mongoData = data
        model.remove({"rethinkId": data.old_val.id},function(err) {
          if(err) {
            console.log("Error deleting mongo data\n",err)
          } else {
            console.log("Synced to MongoDB")
          }
        })
      })
    })
  }
}
module.exports = mongoSync
```

```
curl -H "Content-Type: application/json"
-X POST
-d '{ "name": "Shahid", "dob": "03/18/1992", "gender": "male", "location": "mumbai" }' http://localhost:4000

in the Mongo shell
> use codeforgeek;
> db.users.find().pretty();
```

[why_does_quora_use_mysql_as_the_data_store]: https://www.quora.com/Why-does-Quora-use-MySQL-as-the-data-store-instead-of-NoSQLs-such-as-Cassandra-MongoDB-or-CouchDB
[mysql_node_module]: https://github.com/mysqljs/mysql
[mongoose]: http://mongoosejs.com/

## 10. Using RethinkDB and Horizon

Horizon is an open-source platform for building real-time, scalable web/mobile
 apps. It is built on top of RethinkDB, and allows the developer to get started
 with building modern, real-time apps without writing any backend code. Horizon
 is assembled with three major components: Horizon server, Horizon client and
 [Horizon CLI][horizon_cli].

```
hz init horizonDemo # .hz/{config,schema,secrets}.toml
hz serve --dev
```

The `--dev` flag tells Horizon to start the application in development mode and
 do the following:
* start RethinkDB Server if it's not already started, and store the data in the
  `rethinkdb_data` folder inside the Horizon app
* start the server in insecure mode
* disable the permission system
* automatically create the table and indexes if they don't exist
  (`auto-create-collection`)
* serve static files from the `dist` directory

Horizon contains two APIs: Horizon (connection management class),
 [Collection][collection_api] (data management class)<br>
Horizon constructor options:
* `host` (default `window.location`)
* `secure` (default `true`)
* `path` (default `horizon`)
* `lazyWrites` (default `false`)
* `authType` - `unauthenticated`, `anonymous`, JWT `token`
* If `storeLocally` is true, Horizon stores the authentication tokens on local
  storage of the browser, otherwise the user needs to authenticate the token
  every time he/she makes a request to the application.

`connect()` method is optional.<br>
events - `onReady()`, `onDisconnected()`, `onSocketError()`<br>
`status().type` - unconnected, connected, ready, error, disconnected<br>
`hasAuthToken()` and `currentUser()` methods in the users collection<br>

```javascript
hz("users").find({name: "John"}).fetch().subscribe(msg => console.log(msg))
hz("users").fetch().subscribe(
    result => console.log('Users:', result), # next(result)
    err => console.error(err),               # error(error)
    () => console.log('All users fetched')   # complete()
)
hz("users").store([
     {
        id: 10,
        name: "John doe"
     },
     {
        id: 11,
        name: "Mary"
     }
]).subscribe(
    (id) => console.log("id: ", id),
    (err) => console.error(err)
)
users.watch({rawChanges: true}).subscribe(... # changefeed
```

`npm i --S vue director todomvc-common todomvc-app-css`

```javascript
(function (app, Router) {
   var router = new Router()
   ['all', 'active', 'completed'].forEach(function (visibility) {
         router.on(visibility, function () {
               app.visibility = visibility
         })
   })
   router.configure({
         nonfound: function () {
               window.location.hash = ''
               app.visibility = 'all'
         }
   })
   router.init()
})(app, Router)
```

[about the third-party authentication][horizon_auth],
 [about the permission system][horizon_permissions]<br>
You can make changes to the document in the `users` collection, but not to the
 collection itself. No user is allowed to perform any changes in the collection.

[horizon_cli]: http://horizon.io/docs/cli/
[collection_api]: http://horizon.io/api/collection/
[horizon_auth]: http://horizon.io/docs/auth/
[horizon_permissions]: http://horizon.io/docs/permissions/

