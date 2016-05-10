# [Learn You Some Erlang for Great Good!][homepage] by Fred Hébert, No Starch Press (2013)

Chapter 1-10 are for sequential (functional) Erlang, and chapter 11- are for concurrent Erlang.

[homepage]: http://learnyousomeerlang.com/

## Introduction

`erl -man *topic*`<br>
links - [erldocs.com][erldocs], [Programming Rules and Conventions][conventions], [ErlangCentral Wiki][central_wiki]

[erldocs]: http://erldocs.com
[conventions]: http://www.erlang.se/doc/programming_rules.shtml
[central_wiki]: http://erlangcentral.org/wiki/index.php/

## 1. Starting Out

`help()`<br>
`q()` or `^G h` - `c` (connect to job), `i` (interrupt job), `k` (kill job), `j` (list all jobs), `s` (start local shell) `r` (start remote shell), `q` (quit erlang)

## 2. Starting Out (for real)

`*base*#*value*`<br>
`_` is always unbounded, but `_Variable` is bounded.<br>
(shell only) `f(Variable)` clear a variable or all variables<br>
`'atom'` if a name does not start with a lower case or contains characters out of alphanum+_+&commat;. An atom takes 4 bytes in the 32-bit system and 8 bytes in the 64-bit system.<br>
`=:=`, `=/='`, `==`, `/=`<br>
number < atom < reference < fun < port < pid < tuple < list < bit string

{} tuple, [] (mixed type) list<br>
improper list (ex, `[1 | 2]`) - [Head|Tail] pattern matched but cannot be used with standard functions<br>
list comprehension `[Expression || GeneratorExp1, ..., Conditional1, ...]`, generator expression `Pattern <- List`

`<<R:8, Rest/binary>> = Pixels`<br>
Value:Size/TypeSpecifierList ('-'-separated)
* type - (default) integer, float, binary = bytes, bitstring = bits, utf8, utf16, utf32
* signedness - signed, (default) unsigned
* endianness - (default) big, little, native
* unit:Integer - 1-256, size * unit must be 8 times

bsl, bsr, band, bor, bxor, bnot<br>
<<"...">> bit string (not linked list)<br>
binary comprehension ([specification][binary_comprehension]) became standard from R13B<br>
[ or << <<with size>> || <<...>> <= binary ] or >>

[binary_comprehension]: http://user.it.uu.se/~pergu/papers/erlang05.pdf

## 3. Modules

There are some functions from the `erlang` module which are not automatically imported, but they're seldom used.<br>
module attributes
* `-module(Name).` mandatory
* `-export([Function1/Arity, ...).`
* `-import(Module, [Function1/Arity, ...]).`
* `-define(MACRO, some_value).`, `-define(sub(X,Y), X-Y).`

how to compile - erlc, `compile:file(FileName)`, (in shell) c/1<br>
(in shell) cd/1<br>
beam means Bogdan/Björn's Erlang Abstract Machine. The other VMs are JAM (Joe's Abstract Machine, inspired by Prolog WAM, Warren Abstract Machine) and old BEAM (that compiles Erlang to C).<br>
[compile options][compile_options] - `-debug_info`, `-{outdoor,Dir}`, `-export_all`, `-{d,Macro}`, `-{d,Macro,Value}`, (as a module attribute) `-compile([debug_info, export_all]).`<br>
native compile - `hope:c(Module,OptionsList)` or `c(Module,[native])` (beam contains both native and non-native codes)<br>
`module_info()`, `module_info(attributes)` (used in [this example][example1])

[compile_options]: http://erlang.org/doc/man/compile.html
[example1]: http://learnyousomeerlang.com/static/erlang/tester.erl

## 4. Syntax in Functions

[io:format][io_format]<br>
In guard, `,` (andalso) and `;` (orelse) catch exceptions.

[io_format]: http://erlang.org/doc/man/io.html#format-3

## 5. Types (or Lack Thereof)

about [Ericsson AXD 301 ATM switches][ericsson_switch]<br>
`list_to_integer/float` (string to number), `integer_to_list` (number to string), atom_to_list (atom to string), can convert between list and bitstring/binary/tuple<br>
`is_*type*/1`<br>
to use success typing, use TypEr or Dialyzer. (`typer --help`, `dialyzer --help`) see also [Types and Function Specifications][type_fun_spec] (official document).<br>
Erlang Enhancement Proposals ([EEPs][eep]) [list][eep_list]

[ericsson_switch]: http://www.erlang.se/publications/Ulf_Wiger.pdf
[type_fun_spec]: http://erlang.org/doc/reference_manual/typespec.html
[eep]: https://github.com/erlang/eep
[eep_list]: http://www.erlang.org/erlang-enhancement-proposals/home

## 6. Recursion

use lists:reverse/1, lists:sort/1, gb_trees in production.

## 7. Higher Order Functions

use `fun Module:Function/Arity` because atom (`Function` only) cannot be called.<br>
anonymous functions (can have an internal name  within their scopes from version 17.0)
```
fun (Args1) Guard1 -> Expr, ...;
    (ArgsN) GuardN -> Expr, ...
end
```
closure - children inherit parents' scope<br>
[lists module][lists_module] - map/2, filter/2, foldl/3, foldr/3, all/2, any/2, dropwhile/2, takewhile/2, partition/2 (by condition), flatten/1, flatlength/1, flatmap/2, merge/1 (sort), nth/2, nthtail/2, split/2 (by offset), ...

[lists_module]: http://erldocs.com/18.0/stdlib/lists.html

## 8. Errors and Exceptions

[debugger][debugger], [tracing module][tracing_module], [system limits][system_limits]<br>
default Erlang search path is the current directory. `code:add_patha/1`, `code:add_pathz/1`<br>
three kinds of exceptions
* error - `erlang:error(Reason)` or `1/0` with stack trace `erlang:get_stacktrace/0`
* throw - `throw(permission_denied)`, expected to handle, may be used for non-local returns
* exit - `exit/1` internal exit, `exit/2` external exit
```
try ProtectedExpressions [of]
    % like "case", from R10B
    % tail recursion is unavailable within the protected part.
  catch TypeOfError:ExceptionPattern1 -> Expression
    % _:_, error|throw|exit:_ (default: throw)
  after Expression
    % expect side effects due to no return values
```
catch Expression
* if throw, throwed expression
* if exit, `{'EXIT', expression}`
* if error, `{'EXIT', {badarith, [{Module, Function, Arguments}, {Module, Function, Arity}, ...]}}
* commonly used within "case"
* cons - cannot determine exception types, need parentheses due to precedence

[debugger]: http://erlang.org/doc/apps/debugger/debugger_chapter.html
[tracing_module]: http://erldocs.com/18.0/runtime_tools/dbg.html
[system_limits]: http://erlang.org/doc/efficiency_guide/advanced.html#id2265856

## 9. Functionally Solviing Problems

`=` simple assertion, [Common Test][common_test], [EUnit][eunit]<br>
string:tokens/2 tokenizer, file:open/2, file:close/1, file:read/2, file:read_line/1, file:position/3, file:read_file/1, file:consult/1 (read "."-separated terms), file:pread/2, file:pwrite/2<br>
`min/2` can compare tuples.<br>
`erl -noshell -run Module Function Arguments` or `[escript][escript]` with `erlang:halt(0)`

[common_test]: http://erlang.org/doc/apps/common_test/write_test_chapter.html
[eunit]: http://erlang.org/doc/apps/eunit/chapter.html
[escript]: http://erlang.org/doc/man/escript.html

## 10. A Short Visit to Common Data Structures

**record**

compiler trick, pros - pattern matching, cons - duplicate names<br>
`-record(robot, {name, type=industrial})`<br>
`#robot{name="Mechatron", type=handmade)` (`undefined` if not given value)<br>
`Crusher#robot.name`<br>
`(NestedBot#robot.details)#robot.name` nested record (from R14A, `NestedRobot#robot.details#robot.name`)<br>
`#robot.name` field index, from record name (1)<br>
`Rob#robot{name=...}` update, use `erlang:setelement/3` internally<br>
can pattern match parial fields, `foo(#user{name=Name})`, `foo(U = #user{})`<br>
`-include("reoords.hrl")` header file<br>
(in shell)
* `rr(Module)` load record definitions, make to show record output instead of plain tuple output, `rr("*")`
* `rd(Name, Definition)` define records
* `rf()` unload records
* `rl()` list record definitions
* `rp(Term)` convert tuples to records (record =/= tuple)

**key/value**

* proplists module - [{Key,Value}] format, delete/2, get_value/2, get_all_values/2, lookup/2, lookup_all/2, [NewElement|OldList] add, lists:keyreplace/4 update
* orddict module - ordered, store/3, find/2, fetch/2, erase/2
* dicts module - for large data, orddict functions + map/2, fold/2, use maps instead from version 17.0
* gb_trees - general balanced trees, for large data, map/2, next(Iterator), smallest/1, largest/1
* ETS tables, DETS tables, mnesia database

[benchmark][keyvalue_benchmark]

**array**

arrays module is not a C-like array. if you need C-like arrays, use Ports, C-Nodes, Linked in drivers (Port Drivers), [NIFs][nif] (experimental, R13B03+) instead. then 0-based index is used (in re module, too).

**set**

* ordsets - sorted list, new/0, is_element/2, add_element/2, del_element/2, union/1, intersection/1
* sets - dicts data structure, ordsets functions
* gb_sets - ordsets + gb_trees functions
* sofs (sets of sets) - sorted list, support mathematics concept, digraph-convertable

**directed graph**

use digraph and digraph_util modules

**queue**

queue module (double-ended fifo queue)
* Original API - new/0, in/2, out/1
* Extended API - get/1, peek/1, drop/1
* Okasaki API - see [Purely Functional Data Structures][purely_functional_data_structures] by Chris Okasaki

[keyvalue_benchmark]: http://learnyousomeerlang.com/static/erlang/keyval_benchmark.erl
[nif]: http://erldocs.com/18.0/erts/erl_nif.html
[purely_functional_data_structures]: http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504/

## 11. The Hitchhiker's Guide to Concurrency

concurrency vs parallelism<br>
Erlang has supported SMP after the mid 2000s, but it is common to run multiple VMs due to performance. SMP became popular from R13B (2009).<br>
about concurrency
* scalability - lightweight process, more hardwares, avoid memory sharing
* fault-tolerance - errors are unavoidable, monitor other processes, location (local or remote) is transparent to programmers, asynchronous message passing, no assumption of the other processes
* implementation - 300 words memory per process, process can be created in microseconds, one scheduler thread per core, balanced run queue, limit the rate of messages sent to overloaded processes

no linear scaling due to Amdahl's Law, even can be slow on many cores, `erl -smp disable`<br>
`[smp:2:2]` (two cores available, with two schedulers) `[rq:2]` (two run queues active, if SMP disabled, "rq:1")<br>
one run queue per scheduler, (before R13B) multiple schedulers with only one shared run queue<br>
three primitives:
* spawn new processes - `spawn/1`, `spawn/3` MFA, <0.44.0> process identifier (pid), `self/0`
* sending messages - `pid ! term` returns a sent message, (in shell) `flush()` flush and print any messages sent to the shell
* receiving messages - `receive` like "case" expression

## 12. More On Multiprocessing

use the function argument as the process state.<br>
`start(FoodList) -> spawn(?MODULE, fridge2, [FoodList]).`<br>
`pid(0,250,0)` function<br>
receive ... after Delay -> Expression end. % Delay is milliseconds (up to about 50 days) or infinity<br>
selective receive (move messages back and forth between mailbox and save queue) can make performance problems.<br>
solution - 1) receive and output unexpected messages, 2) receive into min-heap or gb_trees and postprocess<br>
R14A optimization make_ref()
> If no message can match unless it contains the same reference, the compiler automatically makes sure the VM will skip messages received before the creation of that reference. ???

## 13. Errors and Processes

**link** - bidirectional, not stacked<br>
link/1, unlink/1, race condition safe spawn_link/1,3<br>
if the linked process crashes (except natural causes), a special kind of message (cannot be caught by `try ... catch`) is sent.<br>
system process - convert exit signals to regular messages (`{'EXIT', Pid, Reason}`), `process_flag(trap_exit, true)`<br>
shell is restarted automatically.

action | untrapped result | trapped result
-----: | :--------------- | :-------------
linked ends normally or calls exit(normal) | - nothing - | `{'EXIT', Pid, normal}`
linked calls exit(reason) | `** exception exit: reason` | `{'EXIT', Pid, reason}`
linked executes 1/0 | `Error in process Pid with exit value: `{badarith, [{erlang, '/', [1,0]}]}` | `{'EXIT', Pid, {badarith, [{erlang, '/', [1,0]}]}}`
linked calls erlang:error(reason) | `Error in process Pid with exit value: {reason, [{erlang, apply, 2}]}` | `{'EXIT', Pid, {reason, [{erlang, apply, 2}]}}`
linked calls throw(rocks) | `Error in process Pid with exit value: {{nocatch, rocks}, [{erlang, apply, 2}]}` | `{'EXIT', Pid, {{nocatch, rocks}, [{erlang, apply, 2}]}}`
exit(SelfPid, normal) | `** exception exit: normal` | `{'EXIT', Pid, normal}`
exit(LinkedPid, normal) | - nothing - | - nothing - cannot be killed by "normal"
exit(LinkedPid, reason) | `** exception exit: reason` | `{'EXIT', Pid, reason}`
exit(LinkedPid, kill) | `** exception exit: killed` | `{'EXIT', Pid, killed}`
exit(SelfPid, kill) | `** exception exit: killed` | `** exception exit: killed` cannot be trapped, last resort
linked calls exit(kill) | `** exception exit: killed` | `{'EXIT', Pid, kill}`

**monitor** - special type of link, unidirectional, stacked<br>
erlang:monitor(process or time_offset, Pid), spawn_monitor/1, erlang:demonitor(MonitorReference, [ [flush *flush messages and remove a monitor, prevent race conditions*, info *return aliveness*] ])<br>
`{'DOWN', MonitorReference #Ref<0.0.0.77>, process, Pid, Reason}`<br>

**named process** - convert unpredictable Pid to atom<br>
erlang:register/2, unregister/1, registered/0 or (in shell) regs()<br>
whereis(atom) (shared state) can make race conditions, then use unique reference make_ref() instead.

## 14. Designing a Concurrent Application

timer:send_after/2,3 implementation ???<br>
standard Erlang directory structure (by [standard OTP practices][otp_practices]):
* ebin/
* include/ - .hrl files that are used by the other applications
* priv/ - executables that interact with Erlang, such as specific drivers
* src/ - private .hrl files here
* conf/, /doc/, lib/

Erlang's datetime `{{Year, Month, Day}, {Hour, Minute, Second}}`, see also [calendar module][calendar_module]<br>
code server is basically a VM process in charge of an ETS table (VM-native in-memory database), and can hold two versions of each module. if a third version is loaded, all processes that run the oldest version get killed.<br>
a new version is loaded by `c(Module)`, `l(Module)` or code module functions.<br>
local call (within current version) vs external call (MFA form, always call the newest version)<br>
we do not use the code server directly, but use code_change message and code upgrade function that transforms the state data structure.<br>
you can use global module, [gproc library][gproc_library] instead of `register/2`.<br>
`Emakefile` example
```
{'src/*', [debug_info,
           {i, "src"},
           {i, "include"},
           {outdir, "ebin"}]}.
```
1) `erl -make`, `erl -pa ebin/` (add search path option), 2) `make:all([load])` (recompile and load)<br>
`apply(M,F,A)`<br>
`code:crash/0` checks module name conflicts.

[otp_practices]: http://erlang.org/doc/design_principles/applications.html
[calendar_module]: http://erldocs.com/18.0/stdlib/calendar.html
[gproc_library]: https://github.com/uwiger/gproc

## 15. What is OTP?

Erlang pros - 1) concurrency and distribution, 2) error handling capabilities, 3) OTP (Open Telecom Platform) framework<br>
groups essential practices into a set of libraries, modules and standards designed to help you build applications. OTP reduces reduce complexity of testing, maintenance and understanding.<br>
behaviors (gen_*, supervisors) over basic abstraction libraries (gen, sys, proc_lib) over Erlang<br>
abstracted result
```erlang
-module(my_server).
-export([start/2, start_link/2, call/2, cast/2, reply/2]).

%%% Public API
start(Module, InitialState) ->
  spawn(fun() -> init(Module, InitialState) end).

start_link(Module, InitialState) ->
  spawn_link(fun() -> init(Module, InitialState) end).

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

reply({Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.

%%% Private stuff
init(Module, InitialState) ->
  loop(Module, Module:init(InitialState)).

loop(Module, State) ->
  receive
    {async, Msg} ->
      loop(Module, Module:handle_cast(Msg, State)); % handle_* returns NewState.
    {sync, Pid, Ref, Msg} ->
      loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
  end.
```

## 16. Clients and Servers

about gen_server
* init/1 - return `{ok, State}`, `{ok, State, TimeOut}`, `{ok, State, hibernate}`, `{stop, Reason}` or `ignore`, blocking call
 * `erlang:hiberate(M,F,A)` BIF - reduce memory usage while waiting long time replies, 1) discard running process call stack, 2) do garbage collection to make one continuous heap, and 3) apply(M,F,A)
* handle_call(Request, From, State) - return `{reply,Reply,NewState}`, `{reply,Reply,Timeout}`, `{reply,Reply,NewState,hibernate}`, `{noreply,NewState}`, `{noreply,NewState,Timeout}`, `{noreply,NewState,hibernate}`, `{stop,Reason,Reply,NewState}` or `{stop,Reason,NewState}`
 * noreply - you must use `gen_server:reply/2` to reply afterwards
* handle_cast(Message, State) - return `{noreply,NewState}`, `{noreply,NewState,Timeout}`, `{noreply,NewState,hibernate}` or `{stop,Reason,NewState}`
* handle_info(Msg, State) - called by direct `!` operator (unexpected message), init/1's Timeout, monitor notification and `'EXIT'` signal, return like handle_cast/2
* terminate(Reason, State) <-> init/1 - called by parent's death (only in this case, gen_server traps exits) or `stop` tuples, log unless one of `normal`, `shutdown` and `{shutdown, Term}` reasons is used, VM clean up (delete all ETS tables, close all ports, ...), stop code execution
* code_change(PreviousVersion, State, Extra) - return `{ok, NewState}`
 * PreviousVersion - Version or `{down, Version}`
 * Extra - for upgrading entire releases by specific tools in larger OTP deployments

behaviour specifies functions that it expects another module to have.<br>
`-behaviour(gen_server)` or `-behavior(gen_server)`<br>
custom behaviour:
```erlang
-module(my_behaviour).
-export([behaviour_info/1]).
 
%% init/1, some_fun/0 and other/3 are now expected callbacks
behaviour_info(callbacks) -> [{init,1}, {some_fun, 0}, {other, 3}];
behaviour_info(_) -> undefined.
```
[gen_server:start_link][gen_server_start_link]([ServerName,] Module, InitArgs, DebugOptions) -> {ok,Pid}<br>
[gen_server:call][gen_server_call](ServerRef, Request[, Timeout]) - timeout is milliseconds or `infinity`, default 5 seconds

gen_server:... | YourModule implements
:------------- | :--------------------
start/3-4      | init/1
start_link/3-4 | init/1
call/2-3       | handle_call/3
cast/2         | handle_cast/2

[gen_server_start_link]: http://erldocs.com/18.0/stdlib/gen_server.html#start_link/4
[gen_server_call]: http://erldocs.com/18.0/stdlib/gen_server.html#call/2

## 17. Rage Against The Finite-State Machines

## 18. Event Handlers

## 19. Who Supervises The Supervisors

## 20. Building an Application With OTP

## 21. Building OTP Applications

## 22. The Count of Applications

## 23. Release is the Word

## 24. Leveling Up in The Process Quest

## 25. Buckets Of Sockets

## 26. EUnited Naions Council

## 27. Bears, ETS, Beets

## 28. Distribunomicon

## 29. Distributed OTP Applications

## 30. Common Test for Uncommon Tests

## 31. Mnesia And The Art of Remembering

## 32. Type Sepcifications and Erlang

## 33. Conclusion

## 34. Postscript: Maps

## 35. Postscript: Time Goes On

