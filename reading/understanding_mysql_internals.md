# [Understanding MySQL Internals][homepage] by Sasha Pachev, O'Reilly (2007)

mainly based on the version 4.1 (5.0 current stable version, 5.1 current beta
 version, 5.2 current development version)<br>
[MySQL Internals Manual][mysql_internals_manual],
 [Source Code Documentation][source_code_documentation]

[homepage]: http://shop.oreilly.com/product/9780596009571.do
[mysql_internals_manual]: https://dev.mysql.com/doc/internals/en/
[source_code_documentation]: https://dev.mysql.com/doc/dev/mysql-server/latest/

## 1. MySQL History and Architecture

In MySQL server terminology, there are two types of client requests: a query and
 a command. A *query* is anything that has to go through the parser. A *command*
 is a request that can be executed without the need to invoke the parser.<br>
If full query logging is enabled, the Command Dispatcher will ask the Logging
 Module to log the query or the command to the plain-text log prior to the
 dispatch. Thus in the full logging configuration all queries will be logging,
 even the ones that are not syntactically correct and will never be executed,
 immediately returning an error.<br>
Note that since MySQL does not use exceptions for reasons of implementation
 stability and portability, all calls on all levels must be checked for errors
 with the appropriate transfer of control in the case of failure. MySQL
 developers are encouraged to avoid direct *libc* calls, and use the Core API to
 facilitate ports to new platforms and code optimization in the future.<br>
If the low-level module has made a modification to the data in some way and if
 the binary update logging is enabled, the module will be responsible for asking
 the Logging Module to log the update event to the binary update log, sometimes
 known as the replication log, or, among MySQL developers and power users, the
 *binlog*. In addition to interacting with regular clients, a server may receive
 a command from a replication slave to continuously read its binary update log.
 If the server is configured as a replication slave, the Initialization Module
 will call the Replication Slave Module, which in turn will start two threads,
 called the SQL Thread and the I/O thread. They take care of propagating updates
 that happended on the master to the slave. It is possible for the same server
 to be configured as both a master and a slave.<br>
The Optimizer is perhaps the most complex module in the MySQL code.<br>
The Table Manager is responsible for creating, reading, and modifying the table
 definition files (*.frm* extension), maintaining a cache of table descriptors
 called table cache, and managing table-level locks. Most of the code is found
 in *sql/sql_base.cc*, *sql/table.cc*, *sql/unireg.cc*, and *sql/lock.cc*.<br>
Unfortunately, due to the space contraints, this book will not cover Table
 Modification Modules in detail.<br>
Note that the MEMORY storage engine used to be called HEAP.<br>
The Logging Module is responsible for maintaining higher-level (logical) logs.
 The logical logs at this point include the binary update log (used mostly for
 replication, otherwise), command log (used mostly for server monitoring and
 application debugging), and slow query log (used for tracking down poorly
 optimized queries). However, most of the work in logging happens in the binary
 replication log. The classes for log event creation and reading for the binary
 replication log are defined in *sql/log_event.h* and implemented in
 *sql/log_event.cc*. Both the Replication Master and Replication Slave modules
 rely heavily on this functionality of the Logging Module. Version 5.0 brought
 on some changes required for XA transactions. Version 5.1 added the capability
 to search logs as if they were an SQL table, which required a significant
 refactoring of this code. The binary logging part required significant changes
 to accommodate row-based replication.<br>
The Core API is the Swiss Army knife of MySQL. It is perhaps the core component
 of the Miracle of MySQL. Do not use direct *libc* calls. Instead, use the
 portability wrappers from *mysys* and *strings*. Usually a wrapper call from
 *mysys* will have the same name as the *libc* call prefixed with `my_`.

## 2. Nuts and Bolts of Working with MySQL Source Code

```
$ ./mysql-test-run --local --record example # mysql-test/t/example.test
$ ./mysql-test-run --gdb example
```

Operation | Good place for an entry breakpoint | Interesting variables to examine
----------|------------------------------------|---------------------------------
Logging an update on a replication master | `MYSQL_LOG::write(Log_event *)` | `*event_info`<br>`*event_info->thd`<br>`event_info->thd->query`
Replication slave startup | `start_slave_threads()` | `*mi`
Executing of replication thread on the slave that handles network I/O | `handle_slave_io()` | `*mi`
Execution of replication thread on the slave that handles SQL commands | `handle_slave_sql()` | `*rli`

Coding Guidelines
* Most global variables have an associated mutex that other threads will lock
  before accessing it. Make sure to learn which mutex is associated with each
  global variable.
* Do not free pointers allocated with `sql_alloc()`. The memory pool will be
  freed at once with a call to `free_root()` at the end of the query.
* Do not use exceptions. Whenever possible, the code is compiled with exceptions
  disabled.
* Do not use STL, *iostream*, or any other C++ extension that would require
  linking against *libstdc++*.
* Avoid unnecessary system calls. Think of ways to combine several into one; for
  example, use `IO_CACHE` functions and macros instead of `my_read()` and
  `my_write()`.
* When possible, use the following syntx to call several functions
  short-circuiting out if one fails: `if (a() || b() || c()) goto err;`
* Pass by pointer instead of by reference in the C++ code.

## 3. Core Classes, Structures, Variables, and APIs

The `THD` class defines a thread descriptor. `THD` is perhaps the most
 frequently referenced type in the server code. It is passed as the first
 arugment in most of the higher-level calls.<br>
This structure is defined in *sql/table.h* as `struct st_table`, but then is
 aliased to `TABLE` with a `typedef` in *sql/handler.h*. Note that in version
 5.1 `TABLE` was refactored, and portions of it were moved to `TABLE_SHARE`
 class, which is shared between the instances of the same physical table.<br>
Due to portability requirements, the standard C library is used very sparingly,
 and the C++ libraries are not used at all. MySQL makes heavy use of the C
 preprocessor.

## 4. Client/Server Communication

It is, however, possible for the body to be stored without compression when the
 compressed body would turn out no smaller than the uncompressed one, or when
 `compress()` fails for some reason--e.g., due to the lack of available memory.
 If this happens, the uncompressed length field will contain 0.<br>
Version 4.0 added a compatible improvement to the protocol that overcame this
 limitation. If the length of the packet is greater than the value of
 `MAX_PACKET_LENGTH`, which is defined to be 2<sup>24</sup>-1 in
 *sql/net_serv.cc*, the packet gets split into smaller packets with the bodies
 of `MAX_PACKET_LENGTH` plus the last packet with a body that is shorter than
 `MAX_PACKET_LENGTH`.<br>
If the SSL capability option is enabed both on the client and the server, the
 client will first send the initial part of the response packet without the
 credentials string. When the server receives it, it will see the SSL capability
 bit enabled in the capabilities mask, and know that it should expect the rest
 of the communication in SSL. The client switches to the SSL layer, and resends
 the entire response packet securely this time.<br>
When reporting the result of a regular query (sent with `COM_QUERY`), the field
 data is converted to the string format. When using a prepared statement
 (`COM_PREPARE`), the field data is sent in its native format with the low byte
 first.

## 5. Configuration Variables

There is a long road from "it works for me" to "it is ready for production
 release."<br>
The degradation in performance under high load was believed to be caused by the
 lock contention. The assumption turned out to be incorrect, at least to a large
 extent. The problem was attributed to the inability of LinuxThreads to deal
 efficiently with frequently acquired and released mutexes, something MySQL
 server had to do a lot of. After a patch was applied to LinuxThreads, the
 benchmarks that performed a heavy mix of reads and writes scaled just fine as
 long as both types of queries were properly optimized.<br>
In the unfortunate event of a crash, `core-file` will engage the full power of
 the voodoo black magic known as MySQL in order to coax the uncooperative kernel
 to write out a core file. The magic is implemented in `my_write_core()` in
 *mysys/stacktrace.c*. / On an x86 or Alpha Linux, the MySQL server binary is
 capable of unwinding its own stack and printing the stack trace when it
 receives a fatal signal such as `SIGSEGV`. In addition, the postmortem
 diagnostic message includes the query that was executing as well as the
 settings of the variables that are most likely to cause a crash. The stack
 tracing code can be found in *mysys/stacktrace.c*. The entry point is
 `my_print_stacktrace()`.<br>
MyISAM provides persistent storage and a number of fancy features such as
 full-text search and spatial indexing, but does not have transactions or
 row-level locks. InnoDB provides transactions and row-level locks, but it is
 slower on some operations than MyISAM and requires more disk space.<br>
`innodb_buffer_pool_size` is one of the most important InnoDB variables. MyISAM
 caches only the keys, and simply hopes the OS will do a good job caching the
 data. InnoDB does not put any faith in the OS and takes the matter of caching
 the data into its own hands.<br>
MyISAM tables, since the very beginning, have had the advantage of easy backup
 and copying on a per-table basis without any involvement on the part of the
 server. This is possible because a MyISAM table is stored in three files:
 *`table_name`.frm* for the table definition, *`table_name`.MYD* for the data,
 *`table_name`.MYI* for the keys. When InnoDB was introduced into MySQL, many
 users missed the convenience of table manipulation on the file system level.
 Initially InnoDB tables could reside only in the table file or raw device.
 However, version 4.1.1 added the ability to place each table in its own file.
 When enabed, `innodb_file_per_table` causes new tables to have their index and
 data stored in a separate file, *`table_name`.ibd*. Nevertheless, this does not
 give the user the freedom to manipulate those file like MyISAM. As of this
 writing, InnoDB still stores a lot of meta information in its global
 tablespace, which makes such manipluations impossible, although work is in
 progress to facilitate those operations and make it possible to copy *.ibd*
 files from one server instance to another.<br>
Therefore, instead of preventing deadlocks, InnoDB just lets them happen, but it
 periodically runs a lock detection monitor that frees the deadlock "prisoner"
 threads and allows them to return and report to the client that they have been
 aborted because they've been waiting for their lock longer than the value of
 `innodb_lock_wait_timeout`. Note that the deadlock monitoring thread does not
 actually examine the sequence of the locks each thread is holding to figure out
 the existence of a logical database deadlock.<br>
When you are trying to run some quick and dirty test on a system with a particular binary without having to install all of the MySQL files, you can copy *mysqld* and *erromsg.sys* to some directory, e.g., */tmp/mysql* and execute something like this to start the server: `/tmp/mysql/mysqld --skip-grant --skip-net \`
 `--datadir=/tmp/mysql --socket=/tmp/mysql/mysql.sock \`
 `--language=/tmp/mysql &`<br>
`log-bin` enables the update log in binary format. It is primarily used for
 replication on a replication master, but it can also be used for incremental
 backup. The logging happens on the logical level; i.e., queries along with some
 meta information are being logged.<br>
If you ask a MySQL expert to help you troubleshoot a performance problem,
 probably the first thing he will tell you to do is to enable `log-slow-queries`
 along with `log-queries-not-using-indexes`, and examine it to account for every
 query that hits that log.<br>
The slave now has two threads: one for network I/O, and the other for applying
 the SQL updates. The I/O thread reads the updates from the master and appends
 them to the so-called relay log. The SQL thread in turn reads the contents of
 the relay log, and applies them to the slave data.<br>
When records need to be sorted, MySQL uses an algorithm that is known as
 *filesort* in MySQL jargon. The record set is broken into chunks, and each
 chunk is sorted with a radix sort. If there is more than one chunk, each sorted
 chunk is written out to a temporary file while being merged with the already
 sorted collection. This way, we can get the best of both worlds: the speed of a
 radix sort and the ability to sort large collections of records.

## 6. Thread-Based Request Handling

In practice the address space starts getting very crowded at a much smaller
 limit, somewhere around 1.5 GB on x86 Linux.

## 7. The Storage Engine Interface

blackhole (/dev/null) storage engine as an example<br>
For an example of a CSV storage engine that has write capabilities, take a look
 at *storage/csv/ha_tina.h* and *storage/csv/ha_tina.cc*.<br>
InnoDB uses regular file I/O. MyISAM uses `mmap()` for compressed tables. In
 version 5.1, MyISAM has the option to use `mmap()` for regular tables as well.

## 8. Concurrent Access and Locking

(row-level locks) It is also very difficult to completely avoid deadlocks, and
 many implementations do deadlock detection instead.<br>
MyISAM and MEMORY storage engines can only work with table-level locks. InnoDB
 supports row-level locks, and Berkeley DB supports page-level locks.<br>
InnoDB asks the table lock manager to defer locking to the storage engine by
 changing the lock type to `TL_WRITE_ALLOW_WRITE` for write locks. Internally,
 it implements a complex row-level locking system that includes deadlock
 detection. NDB is a distributed storage engine that also supports row-level
 locks. It deals with the table locks in a manner similar to InnoDB.<br>
What InnoDB actually locks is the index entry, the space before it, and the
 space after the last record. This method is called *next-key locking*. The
 next-key locking is necessary to avoid the *phantom row problem* in
 transactions and meet the requirement of the serializable read transaction
 isolation level. The deadlock detection algorithm fails in some cases; for
 example, if tables from other storage engines are used, or if some tables were
 locked with `LOCK TABLES`. Any transaction can potentially be caught in a
 deadlock. It is important for the application programmer to write code that
 deals with this possibility. Usually, retrying a rolled-back transaction is
 sufficient.

## 9. Parser and Optimizer

Prior to version 5.0, only the exhaustive search (*n*! combinations) was
 available. Version 5.0 implemented the greedy search (`optimizer_search_depth`!
 * (*n* - `optimizer_search_depth`) combinations). The join is performed via a
 sequence of nested loops, starting from the first table. For ech record of the
 first table, the optimizer loops through the second to create combinations. The
 combination is then compared against the `WHERE` clause of the query--or more
 precisely, the optimized filter expression generated from the original `WHERE`
 clause.<br>
The purpose of the `\G` switch at the end of the query is to request that the
 result set be displayed vertically. The output of `EXPLAIN` contains a lot of
 columns, which often makes the default mode of horizontal output
 unreadable.<br>
The `STRAIGHT_JOIN` directive tells the optimizer that the `orders` table must
 come before the `customer` table in all of the possible join orders it may
 consider.<br>
The output of `EXPLAIN` is in essence a human-readable dump of the `JOIN` class
 (see *sql/sql_optimizer.h*), which serves as the query plan descriptor.

## 10. Storage Engines

Unlike MyISAM, which always stores its data in files, InnoDB uses tablespaces. A
 tablespace can be stored in a file or on a raw partition. All tables may be
 stored in one common tablespace, or every table may have its own tablespace.
 The data is stored in a special structure called a *clustered index*, which is
 a B-tree with the primary ke acting as the key value, and the actual record
 (rather than a pointer) in the data part. Thus, each InnoDB table must have a
 primary key. If one is not supplied, a special row ID column not normally
 visible to the user is added to act as a primary key. / Additionally, in
 comparison with the operating system's file cache, accessing the data avoids an
 extra system call. On the other hand, with the operating system's file cache
 still enabled, *double caching* is possible, which only wastes memory. However,
 the operating system's file caching can be disabled by starting InnoDB with
 `innodb_flush_method` set to `O_DIRECT`.<br>
The MERGE engine was created to solve a very common problem. Suppose your system
 collects some historical data over time. Most of the queries are restricted to
 a fairly narrow and easily predictable time range. However, once in a while you
 need to query the whole table. For more information on MERGE, refer to the *.c*
 files in the *storage/myisammrg* directory.<br>
The NDB acronym stands for *Network DataBase*. This storage engine is capable of
 storing the data on a fail-safe cluster of database servers. In 2003, MySQL AB
 acquired the division of Ericsson (a Swedish telecom company) that had
 developed the NDB code to handle Ericsson's phone system, and started the work
 on integrating it into MySQL. A running MySQL server provides a central point
 of access to the NDB cluster. At that time, the calls to the handler class
 method are translated into NDB, API calls (see the *.hpp* and *.cpp* files in
 the *storage/ndb/src/ndbapi* directory), which in turn communicate with the
 cluster nodes. The cluster nodes are divided into two types: *management nodes*
 (`ndb_mgmd`), and *data nodes* (`ndbd`). NDB supports transactions, row-level
 locking, B-tree and hash keys, internal synchronous replication two-phase
 commit (separate from MySQL server replication), and data partitioning based on
 the primary key. Each data node loads its entire dataset into memory on
 startup, and writes it to disk on shutdown. There are periodic asynchronous
 writes to disk to ensure that not much gets lost in case of a catastrophic
 failure of the entire cluster (e.g., the power goes down on all nodes at the
 same time). If the nodes are on the same computer, shared memory can also be
 used.<br>
The purpose of the ARCHIVE storage engine is to provide the functionality to
 store large amounts of data using the minimum amount of space. The idea is to
 compress and archive away the data but still be able to query or append to it
 on occasion with minimum hassle. This engine was created during several
 inspired coding sessions by Brian Aker, the Director of Architecture at MySQL
 AB. It supports only two operations: `SELECT` and `INSERT`. the ARCHIVE storage
 engine currently does not support keys.<br>
This is another simple storage engine, and again the fruit of Brian Aker's
 coding inspiration. Its purpose is to allow access to tables stored on a remote
 MySQL server as if they were local. the FEDERATED storage engine stores the
 information about how to access the remote server, and which table to map to in
 the comments field of the `CREATE TABLE` statement. This information is stored
 in the *.frm* file. There are no other datafiles created or used by this
 storage engine. When the optimizer requests the data from the storage engine,
 the storage engine in turn issues an SQL query to the remote server using the
 regular MySQL client/server communication protocol.
 
## 11. Transactions

In the MySQL architecture, the majority of the burden for implementing
 transaction is placed on the storage engine. While the proper implementation of
 transactions definitely requires a great attention to detail in implementing
 the virtual methods of the handler subclass, the core of the
 transaction-specific work happens in a few `handlerton` functions. This is
 understandable: the `handler` subclass methods are conceptually associated with
 a particular table instance, while the `handlerton` functions are associated
 only with the thread or connection. Thus, operations such as `COMMIT`,
 `ROLLBACK`, and `SAVEPOINT` naturally fit into the `handlerton` mode of
 integration. Even if you already have a fully functional transactional storage
 engine, the process of integreation is not trivial. There are a number of
 issues to deal with. How do you work with nontransactional or even
 transactional tables belonging to another sotrage engine? How do you handle the
 possible caching of queries? How do you handle replication logging? How do you
 avoid deadlocks?<br>
A `handlerton` is in essence a singleton that is connected to a table handler,
 hence the name. The introduction of the `handlerton` initiated from the need to
 support XA transactions, which have caused a major refactoring of the
 transaction handling within MySQL.<br>
MySQL has a unique feature for a database: a query cache. The server can be
 configured to cache the results of every `SELECT`. This feature provides a
 great performance boost for a number of applications, especially web
 applications that heavily rely on a database and are frequently accessed by a
 large number of users in a way that makes them send identical queries to the
 MySQL server. Since the introduction of the query cache, many MySQL users have
 reported two- to threefold improvements in performance. The main issue of
 working with the query cache is being able to easily tell if the table has
 changed or not. Thus, the `handler` interface provides a method
 `handler::register_query_cache_table()` to give transactional storage engines a
 chance to answer the question of whether it is safe to cache the query. This
 method is optional. If a handler does not support it, the query cache will use
 the pessimistic approach: on every commit it will invalidate all queries that
 refer to tables used in the committed transaction.<br>
The core MySQL code already provides a lot of help. The SQL statements are not
 written into the binary log until the transaction commits, and they are not
 written at all if the transaction gets rolled back. However, there are a couple
 of critical issues a transactional storage engine might need to address:
* To guarantee consistency of binlog and table data in case of a crash, the
  storage engine must implement XA transactions.
* In a statement-based replication, slaves execute binlog updates sequentially
  and in one thread. Thus, all of the updates on the master must happen under
  the `SERIALIZABLE` transaction isolation level in order to guarantee the same
  results on the slave.

## 12. Replication

The majority of events are merely SQL queries that update the database in one
 way or another. However, it is also necessary to store some metadata that the
 slave must use to recreate the context of the update in order for the update
 query to yield the same results.<br>
The slave keeps track of where it is in the replication process via two
 processes: current log name and current log position. It is possible to tell
 the slave to start replication from an arbitary position using the
 `CHANGE MASTER TO` command.<br>
The replication is asynchronous. It is possible to synchronize the master and
 the slave programatically using a combination of `FLUSH TABLES WITH READ LOCK`,
 `SHOW MASTER STATUS`, and `SELECT MASTER_POS_WAIT()` queries. While this
 technique may be useful in a number of situations, it is frequently
 impractical.<br>
Replicating the data between two SQL databases can take place on the SQL level
 or on the row level. (Statement-based replication) On the other hand, it is
 necessary to log a lot of execution context information in order for the update
 to produce the same results on the slave as it did originally on the master. In
 some cases it is not possible to provide such a context. Row-based replication
 is more straightforward. In some situations the performance overhead associated
 with the increased I/O could become unacceptable.<br>
MySQL initially began with statement-based replication. Up to version 5.0, the
 developers managed to deal with the drawbacks of this approach. However, with
 the introduction of stored procedures it became impossible to keep up. A stored
 procedure has the ability to branch on a number of conditions and in a number
 of different ways. MySQL replication developers addressed the problem by adding
 an option to replicate row by row, starting in version 5.1.5. As of version
 5.1.8, MySQL can take advantage of three *replication modes*: row, statement,
 and mixed. The mode is controlled by the configuration variable
 `binlog_format`. In *row mode*, however, when a new table is created, dropped,
 or altered, the actual SQL statement is recorded. In *mixed mode*, the master
 decides on a per-query basis which logging to use, statement-based or
 row-based.<br>
In the mean time, there exists a very popular configuration that in essence
 serves as a multi-master. Two servers are bound in a *mutual master-slave
 relationship*. This configuration, however, will maintain a consistent data
 snapshot as long the stream of updates is guaranteed to produce the same
 results regardless of their order, or as long as the updates are serialized.

Let us first configure one server as a master by enabling the `log-bin` option
 and setting a server ID with the `server-id` option to some unique number,
 e.g., the last byte of the IP address. Then we run: `SHOW MASTER STATUS\G`. The
 combination of the `File` and the `Position` fields is sometimes referred to as
 the *replication coordinates*. First, create a replication user on the master:
 `GRANT REPLICATION SLAVE ON *.* TO 'rpl_user@slave-host' IDENTIFIED BY 'rpl_pass';`.
 Then we configure the slave. We choose a unique server ID, load the current
 dataset from the master, and instruct the slave of the location of the master
 with the following command:
 `CHANGE MASTER TO MASTER_HOST='master-host', MASTER_USER='rpl_user', MASTER_PASSWORD='rpl_pass';`.
 Then the slave threads can be started: `START SLAVE;`. Next run:
 `SHOW SLAVE STATUS\G`.
* `Relay_Log_File` - The name of the relay log that the SQL thread is on.
* `Relay_Master_Log_File` - The logfile currently being written to by the I/O
  thread.
* `Exec_Master_Log_Pos` - The position in the master log corresponding to the
  current position of the SQL thread.
* `Relay_Log_Space` - The amount of disk space (in bytes) occupied by the relay
  logs.
* `Until_Condition` - Sometimes a slave can be instructed to replicate until a
  certain position in the master or relay log is reached. This parameter tells
  whether there is an `UNTIL` condition, and whether it is in the context of the
  master or the relay log.

Learning some details of the binary log format can reveal a lot about the
 replication internals. The code that deals with the binary logging is found in
 *sql/log_event.h* and *sql/log_event.cc*. `Log_event` is the base class for a
 family of classes responsible for each event. `write_data_body()` and
 `write_data_header()` are virtual methods that each class implements in its own
 way to handle the specifics of the event-type data storage. The names of the
 classes begin with the name of the event, and end with `_log_event`.<br>
*mysqlbinlog* is a very helpful tool for analyzing and understanding the
 replication binary logs. The simpler approach is to create a custom client
 using the source of *mysqlbinlog* (found in *client/mysqlbinlog.cc*) as a base,
 and add a few custom features as necessary. *mysqlbinlog* can already read a
 remote log. One can modify the output loop found in *dump_remote_log_entries()*
 as needed to execute custom event filtering.

