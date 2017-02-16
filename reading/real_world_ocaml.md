# [Real World OCaml][homepage] by [Yaron Minsky][yaron_minsky], [Anil Ma`.dhavapeddy][anil_madhavapeddy] and [Jason Hickey][jason_hickey], O'Reilly (2013)

under a [CC license][cc_by_nc_nd_3_0_us], [@RealWorldOCaml][twitter], [source code][source_code]

[homepage]: https://realworldocaml.org/
[yaron_minsky]: https://twitter.com/yminsky
[anil_madhavapeddy]: http://anil.recoil.org/
[jason_hickey]: https://plus.google.com/+JasonHickeyX
[cc_by_nc_nd_3_0_us]: https://realworldocaml.org/
[twitter]: https://twitter.com/realworldocaml
[source_code]: https://github.com/realworldocaml/examples

## Prologue

ML was originally the *meta language* of the LCF (Logic for Computable
 Functions) proof assistant released by Robin Milner in 1972 (at Stanford, and
 later at Cambridge).<br>
The first implementation of Caml appeared in 1987. It was created by Ascánder
 Suárez and later continued by Pierre Weis and Michel Mauny.<br>
In 1990, Xavier Leroy and Damien Doligez built a new implementation called Caml
 Light that was based on a bytecode interpreter with a fast, sequential garbage
 collector.<br>
Xavier Leroy continued extending Caml Light with new features, which resulted in
 the 1995 release of Caml Special Light.<br>
OCaml was written in 1996 by Xavier Leroy, Jérôme Vouillon, Damien Doligez, and
 Didier Rémy at INRIA in France.

the standard library isn't a general-purpose tool. it was developed for use in
 bootstrapping the compiler and is purposefully kept small and simple. Jane
 Street developed Core for its own internal use. Core is distributed with syntax
 extensions that provide useful new functionality to OCaml, and there are
 additional libraries such as the Async network communications library that
 extend the reach of Core into building complex distributed systems. All of
 these libraries are distributed under a liberal Apache 2 license. Code that
 uses only the traditional compiler standard library will always exist, but
 there are other online resources for learning how that works.

[OPAM][opam] package manager

(4.01) As of publication time, the Windows operating system is unsupported by
 Core, and so only Mac OS X, Linux, FreeBSD, and OpenBSD can be expected to work
 reliably.
 
[installation instructions][installation_instructions], [API reference][api_reference]<br>

[opam]: http://opam.ocaml.org
[installation_instructions]: https://github.com/realworldocaml/book/wiki/Installation-Instructions
[api_reference]: https://ocaml.janestreet.com/ocaml-core/latest/doc/

## I. Language Concepts

### 1. A Guided Tour

Core (standard library replacement), utop (easier-to-use version of standard toplevel)<br>
`open Core.Std;;`<br>
variables must start with a lowercase letter or an underscore, and allow _ and '. module names always start with a capital letter. you can tell it's a type variable by the leading single quote mark.<br>
compile-time Error vs. runtime Exception<br>
`val a_tuple : int * string = (3, "three")` (optional surrounding parens), `val languages : string list = ["OCaml"; "Perl"; "C"]`<br>
`List.length`, `List.map languages ~f:String.length` (labeled argument), `@` (list concatenation operator)

```ocaml
let my_favorite_language languages =
  match languages with
  | first :: the_rest -> first (* the first pipe character is optional *)
  | [] -> "OCaml" (* A good default! *)

let rec sum l =
  match l with
  | [] -> 0                   (* base case *)
  | hd :: tl -> hd + sum tl   (* inductive case *)
```

`None` and `Some x` (`'a option` type)<br>
`^` (string concatenation operator in `Pervasives` module), `float` (=`Float.of_int`, from `Pervasives`), `In_channel.input_line In_channel.stdin`, `printf`

```ocaml
type point2d = { x : float; y : float };;
let p = { x = 3.; y = -4. };;
let magnitude { x = x_pos; y = y_pos } =
  sqrt (x_pos ** 2. +. y_pos ** 2.);;
let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.);; (* field punning *)
let distance v1 v2 =
   magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;

type scene_element = (* variant type *)
  | Circle  of circle_desc
  | Rect    of rect_desc
  | Segment of segment_desc

(fun el -> is_inside_scene_element point el) (* anonymous function °)
```

Functional code is the default in OCaml, with variable bindings and most data
 structures being immutable. But OCaml also has excellent support for imperative
 programming, including mutable data structures like arrays and hash tables, and control-flow constructs like `for` and `while` loops.

```ocaml
# let numbers = [| 1; 2; 3; 4 |];;
 val numbers : int array = [|1; 2; 3; 4|]
# numbers.(2) <- 4;;
 - : unit = ()

# type running_sum =
   { mutable sum: float;
     mutable sum_sq: float; (* sum of squares *)
     mutable samples: int;
   }
  ;;
# let create () = { sum = 0.; sum_sq = 0.; samples = 0 }
  let update rsum x =
     rsum.samples <- rsum.samples + 1; (* due to imperative code *)
     rsum.sum     <- rsum.sum     +. x;
     rsum.sum_sq  <- rsum.sum_sq  +. x *. x
  ;;
# List.iter [1.;3.;2.;-7.;4.;5.] ~f:(fun x -> update rsum x);;
 - : unit = ()

  (* just a record type with a single mutable field called contents *)
# let x = { contents = 0 }
 val x : int ref = {contents = 0}
# x.contents <- x.contents + 1;;
 - : unit = ()
# let x = ref 0  (* create a ref, i.e., { contents = 0 } *) ;;
# !x             (* get the contents of a ref, i.e., x.contents *) ;;
# x := !x + 1    (* assignment, i.e., x.contents <- ... *) ;;
  (* reimplement the ref type and all of these operators *)
# type 'a ref = { mutable contents : 'a }
  let ref x = { contents = x }
  let (!) r = r.contents
  let (:=) r x = r.contents <- x
  (* parentheses are needed because thses are operators, rather than ordinary functions *)

for i = 0 to length - 2 do
   ...
done
while !pos < Array.length array && array.(!pos) >= 0 do
  pos := !pos + 1
done
```

[algorithms for calculating variance][algorithms_for_calculating_variance]<br>
`corebuild sum.native` (a small wrapper on top of `ocamlbuild`)

[algorithms_for_calculating_variance]: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance

### 2. Variables and Functions

`let languages = String.split languages ~on:',' in ...` (shadow)<br>
There are many kinds of mutable values in OCaml, but there are no mutable
 variables.<br>
Tuple and record patterns are irrefutable, i.e., where any value of the type in
 question is guaranteed to match the pattern, but list patterns are not.<br>
OCaml distinguishes between nonrecursive definitions and recursive definitions
 largely for technical reasons: the type-inference algorithm needs to know when
 a set of function definitions are mutually recursive, and for reasons that
 don't apply to a pure language like Haskell, these have to be marked explicitly
 by the programmer.<br>
`! $ % & * + - . / : < = > ? @ ^ | ~`, `or`, `mod`, `lsl`, `( *** )`

operator prefix | associativity
----------------|--------------
`!...`, `?...`, `~...` | prefix
`.`, `.(`, `.[` | -
function application, constructor, `assert`, `lazy` | left
`-`, `-.` | prefix
`**...`, `lsl`, `lsr`, `asr` | right
`*...`, `/...`, `%...`, `mod`, `land`, `lor`, `lxor` | left
`+...`, `-...` | left
`::` | right
`@...`, `^...` | right
`=...`, `<...`, `>...`, `|...`, `&...`, `$...` | left
`&`, `&&` | right
`or`, `||` | right
`,` | -
`<-`, `:=` | right
`if` | -
`l` | right

```ocaml
# let (|>) x f = f x ;; (* left-associative, (^>) right-associative *)
 val ( |> ) : 'a -> ('a -> 'b) -> 'b = <fun>
#   String.split ~on:':' path
  |> List.dedup ~compare:String.compare
  |> List.iter ~f:print_endline
  ;;

# let some_or_default default = function (* function's built-in pattern matching *)
     | Some x -> x
     | None -> default

# let ratio ~num ~denom = ...;; (* labeled arguments *)
 val ratio : num:int -> denom:int -> float = <fun>
# ratio ~num:3 ~denom:10;;
# let num = 3 in (* label punning *)
let denum = 4 in
ratio ~num ~denom;;

  (* optional arguments *)
# let concat ?sep x y = ... ;; (* sep is seen as a string option *)
 val concat : ?sep:string -> string -> string -> string = <fun>
# let concat ?(sep="") x y = ... ;;
# concat ~sep:":" "foo" "bar"
# concat ?sep:(Some ":") "foo" "bar"
# let uppercase_concat ?(sep="") a b = concat ~sep (String.uppercase a) b ;;
# let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b ;;
```

when passing labeled functions as arguments, you need to take care to be
 consistent in your ordering of labeled arguments.<br>
The heuristic the compiler uses to infer labeled and optional arguments is to
 prefer labels to options and to choose the order of arguments that shows up in
 the source code.<br>
(partial application of optional arguments) an optional argument is erased as
 soon as the first positional (i.e., neither labeled nor optional) argument
 defined *after* the optional argument is passed in. An optional argument that
 doesn't have any following positional arguments can't be erased at all, which
 leads to a compiler warning.

### 3. Lists and Patterns

```ocaml
# let rec drop_value l to_drop =
    match l with
    | [] -> []
    | to_drop :: tl -> drop_value tl to_drop (* alert! shadowing! *)
    | hd :: tl -> hd :: drop_value tl to_drop (* unused! *)
  ;;
```

A pattern can check if a list has two elements, but it can't check if the first
 two elements are equal to each other. You can think of patterns as a
 specialized sublanguage that can express a limited (though still quite rich)
 set of conditions. The fact that the pattern language is limited turns out to
 be a very good thing, making it possible to build better support for patterns
 in the compiler. In particular, both the efficiency of `match` statements and
 the ability of the compiler to detect errors in matches depend on the
 constrained nature of patterns.

```ocaml
# #require "core_bench";;
# open Core_bench.Std;;
# let run_bench tests =
  Bench.bench
    ~ascii_table:true
    ~display:Textutils.Ascii_table.Display.column_titles
    tests
;;
 val run_bench : Bench.Test.t list -> unit = <fun>
# [ Bench.Test.create ~name:"plus_one_match" (fun () ->
      ignore (plus_one_match 10))
  ; Bench.Test.create ~name:"plus_one_if" (fun () ->
      ignore (plus_one_if 10)) ]
  |> run_bench
  ;;
```

`List.map`, `List.map2_exn` (throws an exception if the lists are of mismatched length), `List.fold`<br>
`List.reduce` (without a starting value), `List.filter`, `List.filter_map`, `List.partition_tf`, `List.append` = `@`, `List.concat`, `List.concat_map`, `^/` = `Filename.concat`, `List.count`

```ocaml
# let rec destutter = function
    | [] | [_] as l -> l (* Or pattern, as pattern *)
    | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
    | hd :: tl -> hd :: destutter tl
  ;;
```

It turns out that you *can't* build these functions on your own. OCaml's
 polymorphic comparison functions are built into the runtime to a low level.
 These comparisons are polymorphic on the basis of ignoring almost everything
 about the types of the values that are being compared, paying attention only to
 the structure of the values as they're laid out in memory.<br>
Note that `when` clauses have some downsides. In particular, the ability of the
 compiler to determine if a match is exhaustive, or if some case is redundant,
 is compromised.

### 4. Files, Modules, and Programs

`List.Assoc.find`, `List.Assoc.add`<br>
`List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)`<br>
programs in OCaml do not have a unique `main` function. When an OCaml program is
 evaluated, all the statements in the implementation files are evaluated in the
 order in which they were linked together. In this example, the declaration
 starting with `let () =` plays the role of the `main` function.<br>
If we weren't using Core or any other external libraries, `ocamlc freq.ml -o freq.byte`. `ocamlfind ocamlc -linkpkg -thread -package core freq.ml -o freq.byte`, `ocamlbuild` or (simple wrapper) `corebuild freq.byte|native`<br>
`ocamlc` bytecode compiler and `ocamlopt` native-code compiler. the bytecode
 compiler can be used on more architectures, and has some tools that are not
 available for native code. For example, the OCaml debugger only works with
 bytecode. In addition, in order to run a bytecode executable, you typically
 need to have OCaml installed on the system in question. That's not strictly
 required, though, since you can build a bytecode executable with an embedded
 runtime, using the `-custom` compiler flag.<br>
The module name is capitalized even if the file is not.<br>
in the context of OCaml, the terms *interface*, *signature*, and *module type*
 are all used interchangeably. A module defined by a file `filename.ml` can be
 constrained by a signature placed in a file called `filename.mli`.
 (`val <identifier> : <type>`)<br>
autogenerating mil files `corebuild counter.inferred.mli`, `_build/counter.inferred.mli`<br>
A type is *abstract* if its name is exposed in the interface, but its definition
 is not. (`type t`)<br>
values (of which functions are an example) and types have distinct namespaces,
 so there's no name clash here.

```ocaml
type median = | Median of string
              | Before_and_after of string * string
Median (nth (len/2))
Before_and_after (nth (len/2 - 1), nth (len/2));;

(* nested modules *)
 (* module <name> [: <signature>] = <implementation> *)
module Username : sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

module type ID = sig
  ...
end
module String_id = struct
  ....
end
module Username : ID = String_id
module Hostname : ID = String_id
```

Opening modules at the toplevel of a module should be done quite sparingly, and
 generally only with modules that have been specifically designed to be opened,
 like `Core.Std` or `Option.Monad_infix`. Rebinding modules to very short names
 at the top level of your module is usually a mistake.<br>
While opening a module affects the environment used to search for identifiers,
 *including* a module is a way of actually adding new identifiers to a module
 proper.<br>
Note that the order of declarations in the `mli` does not need to match the
 order of declarations in the `ml`. The order of declarations in the `ml` mostly
 matters insofar as it affects which values are shadowed. If we wanted to replace a function in `List` with a new function of the same name, the declaration of that function in the ml would have to come after the `include List` declaration.

```ocaml
let average x y =
  let open Int64 in (* local open *)
  x + y / of_int 2;;
let average x y =
  Int64.(x + y / of_int 2);; (* more lightweight syntax for local opens *)
let print_median m =
  let module C = Counter in (* locally rebind the name of a module *)
  ...

(* if you build an extended version of the List module, *)
include List
include (module type of List) (* in mli *)

(* to use Ext_list as a replacement for List *)
open Core.Std
open Common (* contains "module List = Ext_list" *)
```

By default, cyclic dependencies between modules are not allowed, and cyclic
 dependencies among files are never allowed. Recursive modules are possible but
 are a rare case, and we won't discuss them further here. The simplest example
 of a forbidden circular reference is a module referring to its own module
 name.<br>
guidelines that are used in Core:

* A module for (almost) every type.
* Put `t` first (among arguments).
* Functions that routinely throw an exception should end in `_exn`. Otherwise,
  errors should be signaled by returning an `option` or an `Or_error.t`.

### 5. Records

Note that record field names must start with a lowercase letter.<br>
field punning in pattern matchings and record constructions<br>
`open Core_extended.Std`<br>
`ocaml -warn-help | egrep '\b9\b'`, (in this book) `-w @A-4-33-41-42-43-34-44`

```ocaml
# type host_info =
    { hostname   : string;
      os_name    : string;
      cpu_arch   : string;
      timestamp  : Time.t;
    };;
# let my_host =
    let sh = Shell.sh_one_exn in
    { hostname   = sh "hostname";
      os_name    = sh "uname -s";
      cpu_arch   = sh "uname -p";
      timestamp  = Time.now ();
    };;
# my_host.cpu_arch;;

  (* parameterizing by a polymorphic type *)
# type 'a timestamped = { item: 'a; time: Time.t };;

  (* warning for missing fields in a record pattern *)
# #warnings "+9";;
  (* explicitly acknowledging that we are ignoring extra fields *)
# let host_info_to_string { hostname = h; os_name = os;
                            cpu_arch = c; timestamp = ts; _
                          } = ...;;
```

Reusing field names can lead to some ambiguity. In this case, OCaml just picks
 the most recent definition of that record field. When the order was switched,
 Ocaml may fail type inferences witout type annotations.<br>
We can avoid this ambiguity altogether, either by using nonoverlapping field
 names or, more generally, by minting a module for each type. Packing types into
 modules is a broadly useful idiom. When using this style, it is standard
 practice to name the type associated with the module `t`.

```ocaml
# module Log_entry = struct
    type t =
      { session_id: string;
        time: Time.t;
        important: bool;
        message: string;
      }
  end;;
# let is_important t = t.Log_entry.important;;
  (* the first dot is a record field access. <...>.<field name> *)
  (* the second dot is accessing the contents of a module. *)

  (* functional update syntax *)
# let register_heartbeat t hb =
    { t with last_heartbeat_time = hb.Heartbeat.time };;
 val register_heartbeat : client_info -> Heartbeat.t -> client_info = <fun>

# type client_info =
   { addr: Unix.Inet_addr.t;
     port: int;
     user: string;
     credentials: string;
     mutable last_heartbeat_time: Time.t;
     mutable last_heartbeat_status: string;
   };;

# module Logon = struct
    type t =
      { session_id: string;
        time: Time.t;
        user: string;
        credentials: string;
      }
    with fields (* fieldslib syntax extension that ships with Core *)
  end;;
 module Logon :
   sig
     type t = {
       session_id : string;
       time : Time.t;
       user : string;
       credentials : string;
     }
     val credentials : t -> string
     val user : t -> string
     val time : t -> Time.t
     val session_id : t -> string
     module Fields : (* this submodule contains first-class fields (Field.t type) *)
       sig
         val names : string list
         val credentials :
           ([< `Read | `Set_and_create ], t, string) Field.to_with_perm
         val user :
           ([< `Read | `Set_and_create ], t, string) Field.to_with_perm
         val time :
           ([< `Read | `Set_and_create ], t, string) Field.to_with_perm
         val session_id :
           ([< `Read | `Set_and_create ], t, string) Field.to_with_perm

         [ ... many definitions omitted ... ]

       end
   end
```

`Field.name`, `Field.get`, `Field.fset` (functional update), `Field.setter` (if mutable, `Some f`)<br>
`Logon.Fields.session_id : (Logon.t, string) Field.t`<br>
`Field.get Logon.Fields.user : Logon.t -> string = <fun>`, in fact, `Field.get : ('b, 'r, 'a) Field.t_with_perm -> 'r -> 'a = <fun>` (we expose the ability to read a field from a record, but not the ability to create new records, and so we can't expose functional updates.)<br>
`Fields.fold`, `Fields.iter`<br>
Field iterators are useful for a variety of record-related tasks, from building
 record-validation functions to scaffolding the definition of a web form from a
 record type. Such applications can benefit from the guarantee that all fields
 of the record type in question have been considered.

```ocaml
# let show_field field to_string record =
    let name = Field.name field in
    let field_string = to_string (Field.get field record) in
    name ^ ": " ^ field_string
  ;;
 val show_field :
   ('a, 'b, 'c) Field.t_with_perm -> ('c -> string) -> 'b -> string = <fun>
# show_field Logon.Fields.user Fn.id login;; (* Fn (function) module *)
# show_field Logon.Fields.time Time.to_string logon;;

# Logon.Fields.iter;;
 - : session_id:(([< `Read | `Set_and_create ], Logon.t, string)
                 Field.t_with_perm -> 'a) ->
     time:(([< `Read | `Set_and_create ], Logon.t, Time.t) Field.t_with_perm
           'b) ->
     user:(([< `Read | `Set_and_create ], Logon.t, string) Field.t_with_perm
           'c) ->
     credentials:(([< `Read | `Set_and_create ], Logon.t, string)
                  Field.to_with_perm -> 'd) ->
     'd
 = <fun>

# let print_logon logon =
    let print to_string field =
      printf "%s\n" (show_field field to_string logon)
    in
    Logon.Fields.iter
      ~session_id:(print Fn.id)
      ~time:(print Time.to_string)
      ~user:(print Fn.id)
      ~credentials:(print Fn.id)
  ;;
 val print_logon : Logon.t -> unit = <fun>
```

### 6. Variants

### 7. Error Handling

### 8. Imperative Programming

### 9. Functors

### 10. First-Class Modules

### 11. Objects

### 12. Classes

## II. Tools and Techniques

### 13. Maps and Hash Tables

### 14. Command-Line Parsing

### 15. Handling JSON Data

### 16. Parsing with OCamllex and Menhir

### 17. Data Serialization with S-Expressions

### 18. Concurrent Programming with Async

## III. The Runtime System

### 19. Foreign Function Interface

### 20. Memory Representation of Values

### 21. Understanding the Garbage Collector

### 22. The Compiler Frontend: Parsing and Type Checking

### 23. The Compiler Backend: Bytecode and Native code

