# [Understanding MySQL Internals][homepage] by Sasha Pachev, O'Reilly (2007)

mainly based on the version 4.1 (5.0 current stable version, 5.1 current beta
 version, 5.2 current development version)<br>
[MySQL Internals Manual][mysql_internals_manual]

[homepage]: http://shop.oreilly.com/product/9780596009571.do
[mysql_internals_manual]: https://dev.mysql.com/doc/internals/en/

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
 arugment in most of the higher-level calls.

## 4. Client/Server Communication

## 5. Configuration Variables

## 6. Thread-Based Request Handling

## 7. The Storage Engine Interface

## 8. Concurrent Access and Locking

## 9. Parser and Optimizer

## 10. Storage Engines

## 11. Transactions

## 12. Replication

