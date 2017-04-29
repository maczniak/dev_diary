# [The Little Elixir & OTP Guidebook][homepage] by Benjamin Tan Wei Hao, Manning (2016)

[source code][source_code], covers Elixir 1.2 and 1.3

[homepage]: https://www.manning.com/books/the-little-elixir-and-otp-guidebook
[author]: http://benjamintan.io/
[source_code]: https://github.com/benjamintanweihao/the-little-elixir-otp-guidebook-code

## Part 1. Getting started with Elixir and OTP

### 1. Introduction

`iex` has another superpower, inspired by the Ruby library Pry. If you've used
 Pry, you know that it's a debugger that allows you to pry into the state of
 your program. `iex` comes with a similarly named function called `IEx.pry`.<br>
ExUnit can perform nifty tricks with error reporting mainly due to
 [macros][macro], which I won't ocver in this book.<br>
Elixir has modules that aren't in he standard Erlang library. Streams are
 basically composable, lazy enumerables. Elixir has also added functionality to
 the OTP framework. For example, it's added a number of abstractions, such as
 `Agent` to handle state and `Task` to handle one-off asynchronous computation.

[macro]: http://elixir-lang.org/getting-started/meta/macros.html

### 2. A whirlwind tour

```elixir
if true, do: ( # each argument is separated by comma.
  a = 1 + 2    # this syntax is using keyword lists.
  a + 10
)

if true do  # do/end blocks are a syntactic convenience. do/end blocks do not
  a = 1 + 2 # require a comma between the previous argument and the block.
  a + 10
end
```

[alchemist.el][alchemist_el]<br>
`System.halt`<br>
`h Dict`, `h Dict.`Tab

```elixir
def convert(:feet, m) do
  m * 3.28084
end

def convert(:feet, m), do: m * 3.28084

Enum.map([1, 2, 3], fn x -> x*x end) # anonymous functions

"Strings are #{:great}!" # a function is allowed here
"ohai" <> <<0>> # binary concatenation operator
?o # 111
```

Whereas strings (binary) are always enclosed in double qoutes, char lists are
 enclosed in single quotes. You usually won't use char lists in Elixir. But when
 talking to some Erlang libraries, you'll have to.<br>
You won't use ports in this book. References are useful in giving messages a
 unique identity.

```elixir
%{tolkien: "Elvish"} = programmers # ** (MatchError) no match of right hand ...
Map.fetch(programmers, :rich)      # {:ok, "Clojure"}
Map.fetch(programmers, :rasmus)    # :error
```

the `|>` operators is inspired by F#.<br>
`:inets.start`, [`:httpc.request`][httpc_request_1], `:observer.start`

[alchemist_el]: https://github.com/tonini/alchemist.el
[httpc_request_1]: http://erlang.org/doc/man/httpc.html#request-1

### 3. Processes 101

In fact, the Erlang VM supports up to [134 million processes][max_processes].<br>
Joe Armstrong, [Concurrency Oriented Programming in Erlang][concurrency_oriented_programming_in_erlang]<br>
`mix new metex`, edit `mix.exs`, `mix deps.get`, `iex -S mix`<br>
Inside a module, we can define functions with `def/2` and private functions with
 `defp/2`. (from [Getting Started][getting_started_modules_and_functions])<br>
[OpenWeatherMap][openweathermap]<br>
`try do ... rescue ... end`, `r(Metex.Worker)` (reload module)<br>
`receive do ... end`, `send(pid, {self, "Singapore"})`, `flush/0`<br>
`\\` (default argument)<br>
[send][send_2], [receive][receive_1], [Process][process]
 ([getting started][process_in_getting_started]),
 [Announcing GenStage][announcing_genstage]<br>

[max_processes]: http://erlang.org/doc/man/erl.html#max_processes
[concurrency_oriented_programming_in_erlang]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.116.1969&rep=rep1&type=pdf
[getting_started_modules_and_functions]: http://elixir-lang.org/getting-started/modules-and-functions.html
[openweathermap]: http://openweathermap.org/
[send_2]: https://hexdocs.pm/elixir/Kernel.html#send/2
[receive_1]: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#receive/1
[process]: https://hexdocs.pm/elixir/Process.html
[process_in_getting_started]: http://elixir-lang.org/getting-started/processes.html
[announcing_genstage]: http://elixir-lang.org/blog/2016/07/14/announcing-genstage/

### 4. Writing server applications with GenServer

common OTP behaviors - GenServer, GenFSM, GenEvent, Supervisor, Application<br>
figure 4.1 shows the supervisor hierarchy for the `kernel` application, which is
 the first application started, even before the `elixir` application starts. If
 you look closely, you'll notice that the supervisors have `sup` appended to
 their names. `kernel_sup`, for example, supervises 10 oterh processes. These
 processes cloud be `GenServer`s (`code_server` and `file_server`, for example)
 or even other supervisors (`kernel_safe_sup` and `standard_error_sup`).<br>
Elixir automatically defines all the callbacks needed by the `GenServer`. In
 Erlang, you'd have to specify quite a bit of boilerplate.

GenServer callback | Expected return values
-------------------|-----------------------
`init(args)`<br>by `GenServer.start_link/3` | `{:ok, state}`<br>`{:ok, state, timeout}`<br>`:ignore`<br>`{:stop, reason}`
`handle_call(msg, {from, ref}, state}`<br>by `GenServer.call/3` | `{:reply, reply, state}`<br>`{:reply, reply, state, timeout}`<br>`{:reply, reply, state, :hibernate}`<br>`{:noreply, state}`<br>`{:noreply, state, timeout}`<br>`{:noreply, state, :hibernate}`<br>`{:stop, reason, reply, state}`<br>`{:stop, reason, state}`
`handle_cast(msg, state)`<br>by `GenServer.cast/2` | `{:noreply, state}`<br>`{:reply, state, timeout}`<br>`{:noreply, state, :hibernate}`<br>`{:stop, reason, state}`
`handle_info(msg, state)` | `{:noreply, state}`<br>`{:noreply, state, timeout}`<br>`{:stop, reason, state}`
`terminate(reason, state)` | `:ok`
`code_change(old_vsn, state, extra)` | `{:ok, new_state}`<br>`{:error, reason}`

Messages may arrive from processes that aren't defined in
 `handle_call/3`/`handle_cast/2`. That's where `handle_info/2` comes in. It's
 invoked to handle any other messages that are received by the process,
 sometimes referred to as *out-of-band* messages. You don't need to supply a
 client API counterpart for `handle_info/2`.<br>
If the name is registered globally, then it's unique across a cluster of
 connected nodes. On the other hand, a locally registered name is visible only
 from within the local node.

```elixir
Map.update!(old_stats, location, &(&1 + 1))
Map.update!(old_stats, location, fn(val) -> val + 1 end)

IO.puts "server terminated because of #{inspect reason}"

Process.alive? pid

@name MW # module attribute definition
GenServer.start_link(__MODULE__, :ok, opts ++ [name: MW])
GenServer.call(@name, {:location, location})
```

## Part 2. Fault tolerance, supervision, and distribution

### 5. Concurrent error-handling and fault tolerance with links, monitors, and processes

`Process.link/1`, unidirectional `Process.monitor/1`,
 `Process.info(self, :links)`, atomic `spawn_link/3`<br>
You need to make the process `trap exit signals`. To do so, the process needs to
 call `Process.flag(:trap_exit, true)`. Calling this turns the process from a
 normal process to a system process.

When a process in its link set... | What happens then? | When trapping exits?
----------------------------------|--------------------|---------------------
Exit normally                     | Nothing            | `{:EXIT, pid, :normal}`
`Process.exit(pid, :kill)` | Terminates with `:killed` | `{:EXIT, pid, killed}`
`Process.exit(pid, other)`   | Terminates with `other` | `{:EXIT, pid, other}`

```elixir
# monitor
#  A reference is unique, and can be used to identify where the message comes from.
{:DOWN, #Reference<0.0.0.80>, :process, #PID<0.60.0>,
 {:badarith, [{:erlang, :/, [1, 0], []}]}}
{:DOWN, #Reference<0.0.0.114>, :process, #PID<0.60.0>, :noproc}
```

`Enum.into/2` takes an enumerable (such as a `List`0 and inserts it into a
 `Collectable` (such as a `HashDict`).

```elixir
def handle_info({:EXIT, from, :normal}, state) do
  new_state = state |> HashDict.delete(from)
  {:noreply, new_state}
end
```

### 6. Fault tolerance with Supervisors

If you're familiar with the Poolboy library, much of its design has been adapted
 for this example.

```
Pooly.Supervisor
    +- Pooly.Server
    +- Pooly.WorkerSupervisor
        +- Worker
        +- Worker

Pooly.Supervisor
    +- Pooly.Server
    +- Pooly.PoolsSupervisor
        +- Pooly.PoolSupervisor
            +- Pooly.PoolServer
            +- Pooly.WorkerSupervisor
                +- Worker
                +- Worker
        +- Pooly.PoolSupervisor
```

`mix new pooly --sup` generates an OTP application skeleton including a
 supervision tree.<br>
`def start_link({_,_,_} = mfa) do` pattern-matches the arguments to make sure
 they're a tuple containing tree elements.<br>
The child specification is created with `Supervisor.Spec.worker/3`. The
 `Supervisor.Spec` module is imported by the `Supervisor` behavior by default,
 so there's no need to supply the fully qualified version. The return value of
 the `init/1` callback must be a *supervisor specification*. In order to
 construct a supervisor specification, you use the `Supervisor.Spec.supervise/2`
 function. if the child isn't a `Supervisor`, you should use `worker/3`. If
 you're supervising a `Supervisor`, then use `supervise/3`.<br>
restart strategies - `:one_for_one`, `:one_for_all`, `:rest_for_one`,
 `:simple_one_for_one`<br>
In `:simple_one_for_one`, you specify only one entry in the child specification.
 Every child process that's spawned from this `Supervisor` is the same kind of
 process. The best way to think about the `:simple_one_for_one` strategy is like
 a factory method (or a constructor in OOP languages), where the workers that
 are produced are alike. `:simple_one_for_one` is used when you want to
 dynamically create workers.<br>
by default, `max_restarts` and `max_seconds` are set to 3 and 5
 respectively.

```elixir
# default options of worker/2
[id: module,
 function: :start_link,
 restart: :permanent,
 shutdown: :infinity,
 modules: [module]]
```

```elixir
defmodule Pooly.Server do
  # ...

  def init([], state) do
    send(self, :start_worker_supervisor) # a local invocation with changing a state
    {:ok, state}
  end

  def handle_info(:start_worker_supervisor, state = %{sup: sup, mfa: mfa,
     size: size}) do
    # ...
    {:noreply, %{state | worker_sup: worker_sup, workers: workers}}
  end
end

# vs

defmodule Pooly.PoolServer do # in Chapter 7
  # ...

  def checkout(pool_name) do
    GenServer.call(name(pool_name), :checkout) # a plain function invocation
  end

  defp name(pool_name) do
    :"#{pool_name}Server"
  end
end
```

Erlang Term Storage (ETS)<br>
`ets.new(:mum_faves, [])` - `:set` (default), `:ordered_set`, `:bag`,
 `:duplicate_bag` | `:protected` (default), `:public`, `:private` |
 `:named_table`<br>
The process that created the ETS table is called the *owner process*.<br>
`:ets.insert(:mum_faves, {"Michael Bolton", 1953, "Pop"})`<br>
`:ets.tab2list(:mum_faves)`<br>
`:ets.delete(:mum_faves, "Justin Beiber")`<br>
`:ets.lookup(:mum_faves, "Michael Bolton")`<br>
`:ets.match(:mum_faves, {:"$2", 1953, :"$1"})` `:"_"`

```elixir
true = Process.demonitor(ref)
true = :ets.delete(monitors, pid)

{length(worders), :ets.info(monitors, :size)}
```

`strt/2` is called first when OTP `Application` behavior is initialized.<br>
In Observer, right-click a process: Process info, Trace process, Trace named
 process, Trace process tree, Trace named process tree, Send Msg, Kill process

```elixir
# mix.exs
defmodule Pooly.Mixfile do
  use Mix.project

  # ...

  def application do
    [applications: [:logger],
              mod: {Pooly, []}]}]}]}]
  end

  # ...
end
```

### 7. Completing the worker-pool application

```elixir
GenServer.call(:"#{pool_name}Server", :checkout)

defp supervisor_spec(pool_config) do
  opts = [id: :"#{pool_config[:name]}Supervisor"] # dynamically constructed atom
  # id option vs GenServer.start_link(__MODULE__, pools_config, name: __MODULE__)
  # If you forget the id option, you'll get a cryptic error message.
  supervisor(Pooly.PoolSupervisor, [pool_config], opts)
end
```

You give individual pool Supervisors a name, although this isn't strictly
 necessary. It helps you easily pinpoint the pool Supervisors when viewing them
 in Observer.<br>
Each table is created by a process. When the process terminates, the table is
 automatically destroyed. Notice that there is no automatic garbage collection
 for tables, Even if there are no references to a table from any process, it is
 not automatically destroyed unless the owner process terminates. To destry a
 table explicitly, use function `delete/1`. The default owner is the process
 that created the table. To transfer table ownership at process termination, use
 option `heir` or call `give_away/3`.
 (from [STDLIB Reference Manaual][ets_reference])

The `id` key is mandatory. [`shutdown` key][shutdown_in_child_specification]
 (from Child Specification)

```elixir
iex(1)> q = :queue.new
{[], []}

iex(4)> q = :queue.in("tres", q)
{["tres", "dos"], ["uno"]}

iex(5)> :queue.out(q)
{{:value, "uno"}, {["tres"], ["dos"]}}

iex(9)> :queue.out(q)
{:empty, {[], []}}
```

TODO: "When a consumer is willing to block, you'll first monitor it. That's
 because if it crashes for some reason, you must know about it and remove it
 from the queue."<br>
`{:noreply, %{state | waiting: waiting}, :infinity}` Returning `:noreply` means
 `GenServer.reply(from_pid, message)` mst be called from somewhere else. Because
 you don't know how long you must wait, you pass in `:infinity`.<br>
`try do ... after ... end`
 ([try, catch, and rescue][try_catch_and_rescue_in_getting_started])

[etc_reference]: http://erlang.org/doc/man/ets.html
[shutdown_in_child_specification]: http://erlang.org/doc/design_principles/sup_princ.html#shutdown
[try_catch_and_rescue_in_getting_started]: http://elixir-lang.org/getting-started/try-catch-and-rescue.html

### 8. Distribution and load balancing

```elixir
def foo(x) do           # not a closure
  ...
end
foo(1)

bar = fn x -> x + y end # closure
bar.(1)
```

```elixir
Logger.info "worker [#{node}-#{inspect self}] completed in #{msecs} msecs"

def start(url, caller, func \\ &HTTPoison.get/1) do
  {timestamp, response} = Duration.measure(fn -> func.(url) end)

task = Task.async(fn -> Blitzy.Worker.start("http://www.bieberfever.com") end)
# %Task{pid: #PID<0.154.0>, ref: #Reference<0.0.3.67>}
Task.await(task)
# {:ok, 3362.655}

1..n_workers
  |> Enum.map(fn _ -> Task.async(worker_fun) end)
  |> Enum.map(&Task.await(&1, :infinity)) # default - 5 seconds
```

Processes in an Elixir/Erlang cluster are *location transparent*. A *node* is a
 system running the Erlang VM wih a given name. A name is represented as an atom
 such as `:justin@bieber.com`, much like an email address. Names come in two
 flavors: `short` and `long`. Using short names assumes that all the nodes will
 be located within the same IP domain.<br>
`iex --sname barry`, `Kernel.node/0`, `Node.connect(:robin@imac)`,
 `Node.list/0`<br>
Node connections are *transitive*. This means that even though you didn't
 connect `barry` to `maurice` explicitly, this was done because `barry` is
 connected to `robin` and `robin` is connected to `maurice`, so `barry` is
 connected to `maurice`.<br>
Disconnecting a node disconnects it from all the members of the cluster. A node
 may disconnect, for example, if `Node.disconnect/1` is called or if the node
 dies due to a network disruption.<br>
`:rpc.multicall(cluster, Blitzy.Worker, :start, ["http://www.bieberfever.com"])`<br>
`:rpc.multicall(Blitzy.Worker, :start, ["http://www.bieberfever.com"])`<br>
`:rpc.pmap/3`, `parallel_eval/1`

```elixir
# config/config.exs
use Mix.Config
config :blitzy, master_node: :"a@127.0.0.1"
config :blitzy, slave_nodes: [:"b@127.0.0.1",
                              :"c@127.0.0.1",
                              :"d@127.0.0.1"]
# Application.get_env(:blitzy, :master_node)

# mix.exs
escript: [main_module: Blitzy.CLI] # mix escript.build

([requests: n], [url], []) = OptionParser.parse(args, aliases: [n: :requests],
                                                       script: [requests: :integer])

nodes
|> Enum.flat_map(fn node ->
     1..req_per_node |> Enum.map(fn _ ->
       # Task.Supervisor.async(supervisor, module, fun, args)
       Task.Supervisor.async({Blitzy.TasksSupervisor, node}, Blitzy.Woker, :start, [url])
     end)
   end)
```

Elixir comes equipped with a `Task`-specific `Supervisor`, aptly called
 `Task.Supervisor`. This `Supervisor` is a `:simple_one_for_one` where all
 supervised `Task`s are temporary (they aren't restarted when crashed).

### 9. Distribution and fault tolerance

This has the effect of registering `Chucky.Server` onto the `global_name_server`
 proecss.

```elixir
def start_link do
  GenServer.start_link(__MODULE__, [], [name: {:global, __MODULE__}])
end

def fact do
  GenServer.call({:global, __MODULE__}, :fact)
end

def start(type, _args) do # Application
  import Supervisor.Spec
  # exposes the worker/2 function (that creates the child specification)
  children = [
    worker(Chucky.Server, [])
  ]

  case type do
    :normal ->
      Logger.info("Application is started on #{node}")

    {:takeover, old_node} ->
      Logger.info("#{node} is taking over #{old_node}")

    {:failover, old_node) ->
      Logger.info("#{old_node} is failing over to #{node}")
  end

  opts = [strategy: :one_for_one, name: {:global, Chucky.Supervisor}]
  Supervisor.start_link(children, opts)
end
```

When `a` fails, the remaining nodes will, after a timeout period, detect the
 failure.

```elixir
[{kernel,
  [{distributes, [{chucky, 5000, [a@manticore, {b@manticore, c@manticore},
     d@manticore]}]},
   {sync_nodes_mandatory, [b@manticore, c@manticore]},
   # in b.config, {sync_nodes_mandatory, [a@manticore, c@manticore]},
   {sync_nodes_timeout, 30000}
  ]}].
```

* `sync_nodes_mandatory` - List of nodes that "must" be started within te time
  specified by `sync_nodes_timeout`.
* `sync_nodes_optional` - List of node that "can" be started within the time
  specified by `sync_nodes_timeout`.
* `sync_nodes_timeout` - How long to wait for the other nodes to start (in
  milliseconds).

`mix compile`<br>
`iex --sname a -pa _build/dev/lib/chucky/ebin --app chucky --erl "-config config/a.config"` (prepends the given path to the Erlang code path, `-pz` appends)<br>
`Application.started_aplications/1` (tell what applications are running on the
 current node)<br>
In order for two nodes to communicate, all they need to do is share a *cookie*.
 This cookie is a plain text file usually stored in your home directory
 (`~/.erlang.cookie`).<br>
`iex --name one@192.168.0.100` (long name flag, name@<ip-address>)<br>
`iex --name one@192.168.0.100 --cookie monster`

## Part 3.

### 10. Dialyzer and type specifications

*Dialyzer* stands for *DIscrepancy Analyze for ERlang*. type errors, code that
 raises exceptions, unsatisfiable conditions, Redundant code, race
 conditions<br>
Dialyzer uses a different typing-inference algorithm called *success typings*.
 Success typings are optimistic. They always assume that all your functions are
 used correctly. At first it assumes that your function can take anything and
 return anything. But as the algorithm develops a better understanding of your
 code, it generates constraints. Once the constraints are generated, it's time
 to solve them, just like a puzzle. The solution to the puzzle is the success
 typing of the function. Conversely, if no solution is found, the constraints
 are *unsatisfiable*, and you have a type violation on your hands. To reiterate,
 Dialyzer won't discover all type violations. But if it finds a problem, then
 your code is guaranteed to be problematic.<br>
`i/1` prints information about the given data type. `t/1` prints the types for
 the given module or for the given function/arity pair.<br>
Dialyzer can use either Erlang source code or debug-compiled BEAM bytecode.<br>
Dialyzer use a *persistent lookup table* (PLT) to store the result of its
 analysis.<br>
Dialyzer has found a problem: "no local return" in Dialyzer-speak means the
 function will definitely fail. This usually means Dialyzer has found a type
 error and has therefore determined that the function can never return.

```elixir
@spec foldl([elem], acc, (elem, acc -> acc)) :: acc when
  elem: var, acc: var
def foldl(list, acc, function)
  when is_list(list) and is_function(function) do
  # ...
end

@spec map(f, list_1) :: list_2 when
  f: ((a) -> b),
  list_1: [a],
  list_2: [b],
  a: term,
  b: term
def map(f, [h:t]), do: [f.(h)| map(f, t)]
def map(f, []) when is_function(f, 1), do: []

@type rgb() :: {0..255, 0..255, 0..255}
@type hex() :: binary
@spec rgb_to_hex(rgb) :: hex | {:error, :invalid}
def rgb_to_hex(rgb) # bodiless function clause
def rgb_to_hex({r, g, b}) do
  ...
end
def rgb_to_hex(_) do
  ...
end

@spec amount({:value, number}) :: number
def amount({:value, value}) do
  ...
end
```

### 11. Property-based and concurrency testing

QuickCheck (property-based testing tool), Concuerror (tool that systematically
 detects a concurrency errors in programs)<br>
To be precise, you'll use Erlang QuickCheck, developed by Quviq. Although the
 full version of Erlang QuickCheck requires a commercial license, here you'll
 use a scaled-down version called Erlang QuickCheck *Mini*. The paid version
 includes other niceties, such as testing with state machines, parallel
 execution of test cases to detect race conditions (you'll have Concuerror for
 that), and, of course, commercial support. Be aware that in addition to Erlang
 QuickCheck, a couple of other flavors of similar property-based testing tools
 are available:
* [Trifork QuickCheck][trifork_quickcheck] or *Triq*
* [PropEr][proper], a QuickCheck-inspired property-based testing tool for Erlang

```elixir
defmodule QuickcheckPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :quickcheck_playground,
     version: "0.0.1",
     elixir: "~> 1.2-rc",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     test_pattern: "*_{test,eqc}.exs",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:eqc_ex, "~> 1.2.4"}]
  end
end

defmodule ListsEQC do
  use ExUnit.Case
  use EQC.EXUnit

  # 100 is the default number of tests QuickCheck generates.
  @tag numtests: 1000
  property "reversing a list twice yields the original list" do
    forall l <- list(int) do
      ensure l |> Enum.reverse |> Enum.reverse == l
    end
  end
end
```

[download][quviq_download] and `:eqc_install.install()`<br>
`mix test test/lists_eqc.exs`<br>
Patterns for designing properties:
* inverse functions
* exploiting invariants
* using an existing implementation
* using a simpler implementation
* performing operations in different orders
* idempotent operations

Of course, these six cases aren't the only ones, but they're a good starting
 point.

```elixir
forall l <- list(int) do
  implies l != [] do
    [_head|tail] = l
    ensure tl(l) == tail
  end
end

forall s <- list(char) do
  s = to_string(s)
  collect string: s, in: # outputs the entire generated data set
    ensure string.split(s, ",") |> join(",") == s
end

forall s <- list(char) do
  s = to_string(s)
  :eqc.classify(String.contains?(s, ","), # outputs the probability
                :string_with_commas,
                ensure String.split(s, ",") |> join(",") == s)
end

:eqc_gen.sample(...)

# NOTE: This doesn't work!
# because the first argument of vector/1 should be an integer, not a generator.
def string_with_variable_length do
  vector(choose(1, 10), char)
end

def string_with_variable_length do
  let len <- choose(1, 10) do # let/2
    vector(len, char)
  end
end

def nested_list(gen) do
  sized size do # size???
    nested_list(size, gen)
  end
end

defp nestd_list(0, _gen) do
  []
end

defp nested_list(n, gen) do
  lazy do # lazy evaluation
    oneof [[gen|nested_list(n-1, gen)],
           [nested_list(n-1, gen)]]
  end
end
```

When solving problems with recursion, you must take care not to have infinite
 recursion. you can prevent that by having the input to the recursive calls be
 smaller at each invocation and reach a terminal condition somehow. The standard
 way to handle recursive generators in QuickCheck is to use `sized/2`. `sized/2`
 gives you access to the current size parameter of the test data being
 generated.<br>
There are other (advanced) areas that we haven't explored, such as shrinking
 test data and verifying state machines.

`git clone https://github.com/parapluu/Concuerror.git` → `make` → append a path
 → `mix test` → run Concuerror<br>
By default, Elixir tests end with .exs. This means they aren't compiled.
 Concuerror doesn't understand .exs files (or even .ex files, for that matter),
 so you need to tell Elixir to compile these files into .beam. You also turn off
 the option for `warn_test_pattern`, which complains when there're an .ex file
 in the test directory.<br>
Concuerror can display its output in a helpful diagram. The output is a Graphviz
 .dot file. Make sure Graphviz is properly installed. `dot -V dot -`

```elixir
defmodule ConcuerrorPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :concuerror_playground,
     version: "0.0.1",
     elixir: "~> 1.2-rc",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     elixir_paths: elixirc_paths(Mix.env),
     test_pattern: "*_test.ex",
     warn_test_pattern: nil,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end

  defp elixirc_paths(:test), do: ["lib", "test/concurrency"]
  defp elixirc_paths(_),     do: ["lib"]
end

# ---

Code.require_file "../test_helper.exs", __DIR__

defmodule PingPong.ConcurrencyTest do
  import PingPong

  def test do # test/0 by default, -t test_function
    ping_pid = spawn(fn -> ping end)
    spawn(fn -> pong(ping_pid) end)
  end
end
```

The tool instruments your code (usually in the form of a test), and it knows the
 points at which process-interleaving can happen. Armed with this knowledge, it
 systematically searches for and reports any errors it finds. deadlocks, race
 conditions, unexpected process crashes<br>
When Concuerror finds a proram state where one or more processes are blocked on
 a `receive` and no other processes are available for scheduling, it considers
 that state to be deadlocked.

```
% concuerror --pa /usr/local/Cellar/elixir/HEAD/lib/elixir/ebin/ \
             --pa /usr/local/Cellar/elixir/HEAD/lib/ex_unit/ebin \
             --pa _build/test/lib/concuerror_playground/ebin \
             -m Elixir.Stacky.ConcurrencyTest \
             --graph stacky.dot \
             --show_races true \
             --after_timeout 1000 \
             --treat_as_normal normal
% dot -Tpng stacky.dot > stacky.png
```

* [Software Testing with QuickCheck][software_testing_with_quickcheck] by John
  Hughes
* [Testing Erlang Data Types with Quviq QuickCheck][testing_erlang_data_types_with_quviq_quickcheck]
  by Thomas Arts, Laura M. Castro, and John Hughes
* Jesper Louis Anderson's [posts][jesper_louis_anderson_medium]
* [Test-Driven Development of Concurrent Programs Using Concuerror][test_driven_development_of_concurrent_programs_using_concuerror]
  by Alkis Gotovos, Maria Christakis, and Konstantinos Sangonas

[trifork_quickcheck]: http://krestenkrab.github.io/triq/
[proper]: https://github.com/manopapad/proper
[quviq_download]: http://www.quviq.com/downloads/
[software_testing_with_quickcheck]: https://www.researchgate.net/publication/225219256_Software_Testing_with_QuickCheck
[testing_erlang_data_types_with_quviq_quickcheck]: http://people.inf.elte.hu/center/p1-arts.pdf
[jesper_louis_anderson_medium]: https://medium.com/@jlouis666
[test_driven_development_of_concurrent_programs_using_concuerror]: http://concuerror.com/assets/pdf/erlang1110-gotovos.pdf

### appendix. Installing Erlang and Elixir

Fortunately, the only thing Elixir has a dependency on is Erlang.

