# Data Serialization with S-Expressions {#data-serialization-with-s-expressions}

S-expressions are nested parenthetical expressions whose atomic values are
strings. They were first popularized by the Lisp programming language in the
1960s. They have remained one of the simplest and most effective ways to
encode structured data in a human-readable and editable form. [serialization
formats/s-expressions]{.idx #SERFORMsexp}[s-expressions/uses for]{.idx}[data
serialization/with s-expressions]{.idx}

An example s-expression might look like this.

```
(this (is an) (s expression))
```

S-expressions play a major role in Core, effectively acting as the default
serialization format. Indeed, we've encountered s-expressions multiple times
already, including in
[Error Handling](error-handling.html#error-handling){data-type=xref},
[Functors](functors.html#functors){data-type=xref}, and
[First Class Modules](first-class-modules.html#first-class-modules){data-type=xref}.

This chapter will go into s-expressions in more depth. In particular, we'll
discuss:

- The details of the s-expression format, including how to parse it while
  generating good error messages for debugging malformed inputs

- How to generate s-expressions from arbitrary OCaml types

- How to use custom type annotations to control the exact printing behavior
  for s-expression converters

- How to integrate s-expressions into your interfaces, in particular how to
  add s-expression converters to a module without breaking abstraction
  boundaries

We'll tie this together at the end of the chapter with a simple s-expression
formatted configuration file for a web server

## Basic Usage {#basic-usage}

The type used to represent an s-expression is quite simple:
[s-expressions/basic usage of]{.idx}

```ocaml
module Sexp : sig
  type t =
  | Atom of string
  | List of t list
end
```

An s-expression can be thought of as a tree where each node contains a list
of its children, and where the leaves of the tree are strings. Core provides
good support for s-expressions in its `Sexp` module, including functions for
converting s-expressions to and from strings. Let's rewrite our example
s-expression in terms of this type:

```ocaml
open Core_kernel;;

Sexp.List [
  Sexp.Atom "this";
  Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an"];
  Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression" ];
];;
:: - : Sexp.t = (this (is an) (s expression))
```

This prints out nicely because Core registers a pretty printer with the
toplevel. This pretty printer is based on the functions in `Sexp` for
converting s-expressions to and from strings: [pretty printers]{.idx}

```ocaml
Sexp.to_string (Sexp.List [Sexp.Atom "1"; Sexp.Atom "2"]) ;;
:: - : string = "(1 2)"
Sexp.of_string ("(1 2 (3 4))") ;;
:: - : Sexp.t = (1 2 (3 4))
```

In addition to providing the `Sexp` module, most of the base types in Core
support conversion to and from s-expressions. For example, we can use the
conversion functions defined in the respective modules for integers, strings,
and exceptions:

```ocaml
Int.sexp_of_t 3;;
:: - : Sexplib0.Sexp.t = 3
String.sexp_of_t "hello";;
:: - : Sexp.t = hello
Exn.sexp_of_t (Invalid_argument "foo");;
:: - : Sexp.t = (Invalid_argument foo)
```

It's also possible to convert more complex types such as lists or arrays that
are polymorphic across the types that they can contain:

```ocaml
List.sexp_of_t;;
:: - : ('a -> Sexp.t) -> 'a list -> Sexp.t = <fun>
List.sexp_of_t Int.sexp_of_t [1; 2; 3];;
:: - : Sexp.t = (1 2 3)
```

Notice that `List.sexp_of_t` is polymorphic and takes as its first argument
another conversion function to handle the elements of the list to be
converted. Core uses this scheme more generally for defining sexp converters
for polymorphic types.

The functions that go in the other direction, *i.e.*, reconstruct an OCaml
value from an s-expression, use essentially the same trick for handling
polymorphic types, as shown in the following example. Note that these
functions will fail with an exception when presented with an s-expression
that doesn't match the structure of the OCaml type in question.

```ocaml
List.t_of_sexp;;
:: - : (Sexp.t -> 'a) -> Sexp.t -> 'a list = <fun>
List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 3)");;
:: - : int list = [1; 2; 3]
List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 three)");;
1> Exception:
1> (Sexplib.Conv.Of_sexp_error (Failure "int_of_sexp: (Failure int_of_string)")
1>  three).
```

::: {data-type=note}
### More on Top-Level Printing

The values of the s-expressions that we created were printed properly as
s-expressions in the toplevel, instead of as the tree of `Atom` and `List`
variants that they're actually made of. [top-level printers]{.idx}

This is due to OCaml's facility for installing custom *top-level printers*
that can rewrite some values into more top-level-friendly equivalents. They
are generally installed as `ocamlfind` packages ending in `top`:
:::

```sh
%% --non-deterministic
  $ ocamlfind list | grep top
  astring.top         (version: 0.8.3)
  cohttp.top          (version: n/a)
  compiler-libs.toplevel (version: [distributed with Ocaml])
  core.top            (version: v0.10.0)
  ctypes.top          (version: 0.13.1)
  findlib.top         (version: 1.7.3)
  fmt.top             (version: 0.8.5)
  ipaddr.top          (version: 2.8.0)
  js_of_ocaml.toplevel (version: n/a)
  logs.top            (version: 0.6.2)
  lwt.simple-top      (version: 3.2.1)
  mtime.top           (version: 1.1.0)
  num-top             (version: 1.1)
  ocaml-compiler-libs.toplevel (version: v0.10.0)
  react.top           (version: 1.2.1)
  topkg               (version: 0.9.1)
  toplevel_expect_test (version: v0.10.0)
  toplevel_expect_test.types (version: v0.10.0)
  uri.top             (version: 1.9.6)
  utop                (version: 2.1.0)

```

The `core.top` package (which you should have loaded by default in your
`.ocamlinit` file) loads in printers for the Core extensions already, so you
don't need to do anything special to use the s-expression printer.

### Generating S-Expressions from OCaml Types {#generating-s-expressions-from-ocaml-types}

But what if you want a function to convert a brand new type to an
s-expression? You can of course write it yourself manually. Here's an
example: [s-expressions/generating from OCaml types]{.idx}

```ocaml
type t = { foo: int; bar: float } ;;
:: type t = { foo : int; bar : float; }
let sexp_of_t t =
  let a x = Sexp.Atom x and l x = Sexp.List x in
  l [ l [a "foo"; Int.sexp_of_t t.foo  ];
      l [a "bar"; Float.sexp_of_t t.bar]; ] ;;
:: val sexp_of_t : t -> Sexp.t = <fun>
sexp_of_t { foo = 3; bar = -5.5 } ;;
:: - : Sexp.t = ((foo 3) (bar -5.5))
```

This is somewhat tiresome to write, and it gets more so when you consider the
parser, i.e., `t_of_sexp`, which is considerably more complex. Writing this
kind of parsing and printing code by hand is mechanical and error prone, not
to mention a drag.

Given how mechanical the code is, you could imagine writing a program that
inspects the type definition and automatically generates the conversion code
for you. As it turns out, there's a *syntax extension* called `ppx_sexp_conv`
which does just that, creating the required functions for every type
annotated with `[@@deriving sexp]`. [Sexplib package/syntax extension
in]{.idx}[syntax extension/in Sexplib package]{.idx}

```ocaml
type t = { foo: int; bar: float } [@@deriving sexp] ;;
:: type t = { foo : int; bar : float; }
:: val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = <fun>
:: val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = <fun>
t_of_sexp (Sexp.of_string "((bar 35) (foo 3))") ;;
:: - : t = {foo = 3; bar = 35.}
```

The syntax extension can be used outside of type declarations as well. As
discussed in
[Error Handling](error-handling.html#error-handling){data-type=xref},
`with sexp` can be attached to the declaration of an exception, which will
improve the ability of Core to generate a useful string representation:

```ocaml
exception Bad_message of string list ;;
:: exception Bad_message of string list
Exn.to_string (Bad_message ["1";"2";"3"]) ;;
:: - : string = "(\"Bad_message(_)\")"
exception Good_message of string list [@@deriving sexp];;
:: exception Good_message of string list
Exn.to_string (Good_message ["1";"2";"3"]) ;;
:: - : string = "(//toplevel//.Good_message (1 2 3))"
```

You don't always have to declare a named type to create an s-expression
converter. The following syntax lets you create one inline, as part of a
larger expression:

```ocaml
let l = [(1,"one"); (2,"two")] ;;
:: val l : (int * string) list = [(1, "one"); (2, "two")]
List.iter l ~f:(fun x ->
  [%sexp_of: int * string ] x
  |> Sexp.to_string
  |> print_endline) ;;
1> (1 one)
1> (2 two):: - : unit = ()
```

The declaration `[%sexp_of: int * string]` simply gets expanded to the sexp
converter for the type `int * string`. This is useful whenever you need a
sexp converter for an anonymous type.

The syntax extensions bundled with Core almost all have the same basic
structure: they autogenerate code based on type definitions, implementing
functionality that you could in theory have implemented by hand, but with far
less programmer effort.

::: {data-type=note}
#### Syntax Extensions and PPX

OCaml doesn't directly support deriving s-expression converters from type
definitions. Instead, it provides a mechanism called *PPX* which allows you
to add to the compilation pipeline code for transforming OCaml programs at
the syntactic level, via the `-ppx` compiler flag.

PPXs operate on OCaml's *abstract syntax tree*, or AST, which is a data type
that represents the syntax of a well-formed OCaml program. Annotations like
`[%sexp_of: int]` or `[@@deriving sexp]` are part of special extensions to
the syntax, called *extension points*, which were added to the language to
give a place to put information that would be consumed by syntax extensions
like `ppx_sexp_conv`. [PPX syntax extensions]{.idx}[syntax extension with
PPX]{.idx}

`ppx_sexp_conv` is part of a family of syntax extensions, including
`ppx_compare`, described in
[Maps And Hash Tables](maps-and-hashtables.html#maps-and-hash-tables){data-type=xref},
and `ppx_fields`, described in
[Records](records.html#records){data-type=xref}, that generate code based
on type declarations. [Type_conv library]{.idx}[Sexplib package/Type_conv
library and]{.idx}
:::



## The Sexp Format {#the-sexp-format}

The textual representation of s-expressions is pretty straightforward. An
s-expression is written down as a nested parenthetical expression, with
whitespace-separated strings as the atoms. Quotes are used for atoms that
contain parentheses or spaces themselves; backslash is the escape character;
and semicolons are used to introduce single-line comments. Thus, the
following file, <em class="filename">example.scm</em>: [s-expressions/format
of]{.idx}

```
;; example.scm

((foo 3.3) ;; This is a comment
 (bar "this is () an \" atom"))
```

can be loaded using Sexplib. As you can see, the commented data is not part
of the resulting s-expression:

```ocaml
Sexp.load_sexp "example.scm";;
:: - : Sexp.t = ((foo 3.3) (bar "this is () an \" atom"))
```

All in, the s-expression format supports three comment syntaxes:

`;`
: Comments out everything to the end of line

`#|,|#`
: Delimiters for commenting out a block

`#;`
: Comments out the first complete s-expression that follows

The following example shows all of these in action:

```
;; comment_heavy_example.scm
((this is included)
 ; (this is commented out
 (this stays)
 #; (all of this is commented
     out (even though it crosses lines.))
  (and #| block delimiters #| which can be nested |#
     will comment out
    an arbitrary multi-line block))) |#
   now we're done
   ))
```

Again, loading the file as an s-expression drops the comments:

```ocaml
Sexp.load_sexp "comment_heavy.scm" ;;
:: - : Sexp.t = ((this is included) (this stays) (and now we're done))
```

If we introduce an error into our s-expression, by, say, creating a file
`broken_example.scm` which is `example.scm`, without open-paren in front of
`bar`, we'll get a parse error:

```ocaml
Exn.handle_uncaught ~exit:false (fun () ->
  ignore (Sexp.load_sexp "example_broken.scm")) ;;
1> Uncaught exception:
1>   
1>   (Sexplib.Sexp.Parse_error
1>    ((err_msg "unexpected character: ')'") (text_line 4) (text_char 30)
1>     (global_offset 78) (buf_pos 78))):: - : unit = ()
```

In the preceding example, we use `Exn.handle_uncaught` to make sure that the
exception gets printed out in full detail. You should generally wrap every
Core program in this handler to get good error messages for any unexpected
exceptions.

## Preserving Invariants {#preserving-invariants}

The most important functionality provided by Sexplib is the autogeneration of
converters for new types. We've seen a bit of how this works already, but
let's walk through a complete example. Here's the source for a simple library
for representing integer intervals, very similar to the one described in
[Functors](functors.html#functors){data-type=xref}:
[s-expressions/preserving invariants in]{.idx}

```ocaml
(* Module for representing closed integer intervals *)
open Core

(* Invariant: For any Range (x,y), y >= x *)
type t =
  | Range of int * int
  | Empty
[@@deriving sexp]

let is_empty =
  function
  | Empty -> true
  | Range _ -> false

let create x y =
  if x > y then
    Empty
  else
    Range (x,y)

let contains i x =
  match i with
  | Empty -> false
  | Range (low,high) -> x >= low && x <= high
```

We can now use this module as follows:

```ocaml
open Core

let intervals =
  let module I = Int_interval in
  [ I.create 3 4;
    I.create 5 4; (* should be empty *)
    I.create 2 3;
    I.create 1 6;
  ]

let () =
  intervals
  |> List.sexp_of_t Int_interval.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
```

But we're still missing something: we haven't created an `mli` signature for
`Int_interval` yet. Note that we need to explicitly export the s-expression
converters that were created within the `ml` file. For example, here's an
interface that doesn't export the s-expression functions:

```ocaml
type t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

Building this will give us the following error:

```
(executable (
  (name test_interval_nosexp)
  (libraries (core))
))
```



```sh
  $ jbuilder build test_interval_nosexp.exe
        ocamlc .test_interval_nosexp.eobjs/test_interval_nosexp.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /home/yminsky/.opam/fresh-4.06.1/bin/ocamlc.opt -w -40 -g -bin-annot -I .test_interval_nosexp.eobjs -I /home/yminsky/.opam/fresh-4.06.1/lib/base -I /home/yminsky/.opam/fresh-4.06.1/lib/base/caml -I /home/yminsky/.opam/fresh-4.06.1/lib/base/md5 -I /home/yminsky/.opam/fresh-4.06.1/lib/base/shadow_stdlib -I /home/yminsky/.opam/fresh-4.06.1/lib/bin_prot -I /home/yminsky/.opam/fresh-4.06.1/lib/bin_prot/shape -I /home/yminsky/.opam/fresh-4.06.1/lib/core -I /home/yminsky/.opam/fresh-4.06.1/lib/core_kernel -I /home/yminsky/.opam/fresh-4.06.1/lib/core_kernel/base_for_tests -I /home/yminsky/.opam/fresh-4.06.1/lib/fieldslib -I /home/yminsky/.opam/fresh-4.06.1/lib/jane-street-headers -I /home/yminsky/.opam/fresh-4.06.1/lib/ocaml/threads -I /home/yminsky/.opam/fresh-4.06.1/lib/parsexp -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_assert/runtime-lib -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_bench/runtime-lib -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_compare/runtime-lib -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_expect/collector -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_expect/common -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_expect/config -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_hash/runtime-lib -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_inline_test/config -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_inline_test/runtime-lib -I /home/yminsky/.opam/fresh-4.06.1/lib/ppx_sexp_conv/runtime-lib -I /home/yminsky/.opam/fresh-4.06.1/lib/sexplib -I /home/yminsky/.opam/fresh-4.06.1/lib/sexplib/unix -I /home/yminsky/.opam/fresh-4.06.1/lib/sexplib0 -I /home/yminsky/.opam/fresh-4.06.1/lib/spawn -I /home/yminsky/.opam/fresh-4.06.1/lib/splittable_random -I /home/yminsky/.opam/fresh-4.06.1/lib/stdio -I /home/yminsky/.opam/fresh-4.06.1/lib/typerep -I /home/yminsky/.opam/fresh-4.06.1/lib/variantslib -no-alias-deps -o .test_interval_nosexp.eobjs/test_interval_nosexp.cmo -c -impl test_interval_nosexp.ml)
  File "test_interval_nosexp.ml", line 13, characters 20-42:
  Error: Unbound value Int_interval.sexp_of_t
@@ exit 1

```

We could export the types by hand in the signature, by writing the signatures
for the extra functions generated by Sexplib:

```ocaml
open Core

type t
val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

This isn't an ideal solution, as it makes you repeatedly expose these extra
functions in every signature you create where you want to serialize values.
Sexplib solves this by exposing the same syntax extension in signature
definitions so that we can just use the same `with` shorthand in the 
`mli` file. Here's the final version of the signature that does just this:

```ocaml
type t [@@deriving sexp]

val is_empty : t -> bool
val create : int -> int -> t
val contains : t -> int -> bool
```

At this point, `test_interval.ml` will compile again, and if we run it, we'll
get the following output:

```
(executable
  ((name test_interval)
   (libraries (core sexplib))
   (preprocess (pps (ppx_sexp_conv)))
  )
)
```



```sh
  $ jbuilder build test_interval.exe
  $ ./_build/default/test_interval.exe
  ((Range 3 4) Empty (Range 2 3) (Range 1 6))

```

One easy mistake to make when dealing with sexp converters is to ignore the
fact that those converters can violate the invariants of your code. For
example, the `Int_interval` module depends for the correctness of the
`is_empty` check on the fact that for any value `Range (x,y)`, `y` is greater
than or equal to `x`. The `create` function preserves this invariant, but the
`t_of_sexp` function does not. [invariant checks]{.idx}

We can fix this problem by overriding the autogenerated function and writing
a custom sexp converter that wraps the autogenerated converter with whatever
invariant checks are necessary:

```ocaml
type t =
  | Range of int * int
  | Empty
[@@deriving sexp]

let create x y =
  if x > y then Empty else Range (x,y)

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  begin match t with
    | Empty -> ()
    | Range (x,y) ->
      if y < x then of_sexp_error "Upper and lower bound of Range swapped" sexp
  end;
  t
```

This trick of overriding an existing function definition with a new one is
perfectly acceptable in OCaml. Since `t_of_sexp` is defined with an ordinary
`let` rather than a `let rec`, the call to the `t_of_sexp` goes to the
Sexplib-generated version of the function, rather than being a recursive
call.

Another important aspect of our definition is that we call the function
`of_sexp_error` to raise an exception when the parsing process fails. This
improves the error reporting that Sexplib can provide when a conversion
fails, as we'll see in the next section.

## Getting Good Error Messages {#getting-good-error-messages}

There are two steps to deserializing a type from an s-expression: first,
converting the bytes in a file to an s-expression; and the second, converting
that s-expression into the type in question. One problem with this is that it
can be hard to localize errors to the right place using this scheme. Consider
the following example: [debugging/s-expressions]{.idx}[errors/error messages
with s-expressions]{.idx}[s-expressions/deserializing a type from]{.idx}

```
(executable
  ((name read_foo)
  (libraries (core sexplib))
  (preprocess (pps (ppx_sexp_conv)))
  )
)
```



```ocaml
open Core

type t = { 
  a: string;
  b: int;
  c: float option 
} [@@deriving sexp]

let run () =
  let t =
    Sexp.load_sexp "foo_broken_example.scm"
    |> t_of_sexp
  in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
```

If you were to run this on a malformatted file, say, this one:

```
((a "not-an-integer")
 (b "not-an-integer")
 (c 1.0))
```

you'll get the following error:

```sh
  $ jbuilder build read_foo.exe
  $ ./_build/default/read_foo.exe foo_example_broken.scm
  Uncaught exception:
    
    (Sexplib.Conv.Of_sexp_error
     (Failure "int_of_sexp: (Failure int_of_string)") not-an-integer)
  
  Raised at file "sexp_conv.ml", line 195, characters 30-72
  Called from file "read_foo.ml", line 5, characters 2-3
  Called from file "read_foo.ml", line 3, characters 0-73
  Called from file "read_foo.ml", line 11, characters 4-60
  Called from file "src/exn.ml", line 113, characters 6-10
@@ exit 1

```

If all you have is the error message and the string, it's not terribly
informative. In particular, you know that the parsing errored out on the atom
"not-an-integer," but you don't know which one! In a large file, this kind of
bad error message can be pure misery.

But there's hope! We can make a small change to the code to improve the error
message greatly:

```
(executable
  ((name read_foo_better_errors)
   (libraries (core sexplib))
   (preprocess (pps (ppx_sexp_conv)))
  )
)
```



```ocaml
open Core

type t = { 
  a: string;
  b: int;
  c: float option 
} [@@deriving sexp]

let run () =
  let t = Sexp.load_sexp_conv_exn "foo_broken_example.scm" t_of_sexp in
  printf "b is: %d\n%!" t.b

let () =
  Exn.handle_uncaught ~exit:true run
```

If we run it again, we'll see a much more specific error:

```sh
  $ jbuilder build read_foo_better_errors.exe
  $ ./_build/default/read_foo_better_errors.exe foo_example_broken.scm
  Uncaught exception:
    
    (Sexplib.Conv.Of_sexp_error
     (Sexplib.Sexp.Annotated.Conv_exn foo_broken_example.scm:2:4
      (Failure "int_of_sexp: (Failure int_of_string)"))
     not-an-integer)
  
  Raised at file "src/pre_sexp.ml", line 715, characters 4-56
  Called from file "read_foo_better_errors.ml", line 10, characters 10-68
  Called from file "src/exn.ml", line 113, characters 6-10
@@ exit 1

```

In the preceding error, `foo_broken_example.scm:2:5` tells us that the error
occurred in the file `"foo_broken_example.scm"` on line 2, character 5. This
is a much better start for figuring out what went wrong. The ability to find
the precise location of the error depends on the sexp converter reporting
errors using the function `of_sexp_error`. This is already done by converters
generated by Sexplib, but you should make sure to do the same when you write
custom converters.

## Sexp-Conversion Directives {#sexp-conversion-directives}

Sexplib supports a collection of directives for modifying the default
behavior of the autogenerated sexp converters. These directives allow you to
customize the way in which types are represented as s-expressions without
having to write a custom converter. [s-expressions/modifying default behavior
of]{.idx}

Note that the extra directives aren't part of the standard OCaml syntax, but
are added via the Sexplib syntax extension. However, since Sexplib is used
throughout Core and is part of the standard bundle activated by `corebuild`,
you can use these in your own Core code without any special effort.

### sexp_opaque {#sexp_opaque}

The most commonly used directive is `sexp_opaque`, whose purpose is to mark a
given component of a type as being unconvertible. Anything marked with
`sexp_opaque` will be presented as the atom `<opaque>` by the to-sexp
converter, and will trigger an exception from the from-sexp converter.
[Sexplib package/sexp_opaque]{.idx}

Note that the type of a component marked as opaque doesn't need to have a
sexp converter defined. Here, if we define a type without a sexp converter
and then try to use another type with a sexp converter, we'll error out:

```ocaml
type no_converter = int * int ;;
:: type no_converter = int * int
type t = { a: no_converter; b: string } [@@deriving sexp] ;;
1> Characters 14-26:
1> Error: Unbound value no_converter_of_sexp
```

But with `sexp_opaque`, we can embed our opaque `no_converter` type within
the other data structure without an error.

```ocaml
type t = { a: no_converter sexp_opaque; b: string } [@@deriving sexp] ;;
:: type t = { a : no_converter; b : string; }
:: val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = <fun>
:: val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = <fun>
```

And if we now convert a value of this type to an s-expression, we'll see the
contents of field `a` marked as opaque:

```ocaml
sexp_of_t { a = (3,4); b = "foo" } ;;
:: - : Sexp.t = ((a <opaque>) (b foo))
```

Note that the `t_of_sexp` function for an opaque type is generated, but will
fail at runtime if it is used:

```ocaml
t_of_sexp (Sexp.of_string "((a whatever) (b foo))") ;;
1> Exception:
1> (Sexplib.Conv.Of_sexp_error
1>  (Failure "opaque_of_sexp: cannot convert opaque values") whatever).
```

This is there to allow for s-expression converters to be created for types
containing `sexp_opaque` values. This is useful because the resulting
converters won't necessarily fail on all inputs. For example, if you have a
record containing a `no_converter list`, the `t_of_sexp` function would still
succeed when the list is empty:

```ocaml
type t = { a: no_converter sexp_opaque list; b: string } [@@deriving sexp] ;;
:: type t = { a : no_converter list; b : string; }
:: val t_of_sexp : Sexp.t -> t = <fun>
:: val sexp_of_t : t -> Sexp.t = <fun>
t_of_sexp (Sexp.of_string "((a ()) (b foo))") ;;
:: - : t = {a = []; b = "foo"}
```

If you really only want to generate one direction of converter, one can do
this by annotating the type with `[@@deriving sexp_of]` or
`[@@deriving of_sexp]` instead of `[@@deriving sexp]`:

```ocaml
type t = { a: no_converter sexp_opaque; b: string } [@@deriving sexp_of] ;;
:: type t = { a : no_converter; b : string; }
:: val sexp_of_t : t -> Sexp.t = <fun>
type t = { a: no_converter sexp_opaque; b: string } [@@deriving of_sexp] ;;
:: type t = { a : no_converter; b : string; }
:: val t_of_sexp : Sexp.t -> t = <fun>
```

### sexp_list {#sexp_list}

Sometimes, sexp converters have more parentheses than one would ideally like.
Consider, for example, the following variant type: [Sexplib
package/sexp_list]{.idx}

```ocaml
type compatible_versions =
  | Specific of string list
  | All [@@deriving sexp] ;;
:: type compatible_versions = Specific of string list | All
:: val compatible_versions_of_sexp :
::   Ppx_sexp_conv_lib.Sexp.t -> compatible_versions = <fun>
:: val sexp_of_compatible_versions :
::   compatible_versions -> Ppx_sexp_conv_lib.Sexp.t = <fun>
sexp_of_compatible_versions
  (Specific ["3.12.0"; "3.12.1"; "3.13.0"]) ;;
:: - : Sexp.t = (Specific (3.12.0 3.12.1 3.13.0))
```

You might prefer to make the syntax a bit less parenthesis-laden by dropping
the parentheses around the list. We can replace the `string list` in the type
declaration with `string sexp_list` to give us this alternate syntax:

```ocaml
type compatible_versions =
  | Specific of string sexp_list
  | All [@@deriving sexp] ;;
:: type compatible_versions = Specific of string list | All
:: val compatible_versions_of_sexp : Sexp.t -> compatible_versions = <fun>
:: val sexp_of_compatible_versions : compatible_versions -> Sexp.t = <fun>
sexp_of_compatible_versions
  (Specific ["3.12.0"; "3.12.1"; "3.13.0"]) ;;
:: - : Sexp.t = (Specific 3.12.0 3.12.1 3.13.0)
```

### sexp_option {#sexp_option}

Another common directive is `sexp_option`, which is used to make a record
field optional in the s-expression. Normally, optional values are represented
either as `()` for `None`, or as `(x)` for `Some x`, and a record field
containing an option would be rendered accordingly. For example: [Sexplib
package/sexp_option]{.idx}

```ocaml
type t = { a: int option; b: string } [@@deriving sexp] ;;
:: type t = { a : int option; b : string; }
:: val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = <fun>
:: val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = <fun>
sexp_of_t { a = None; b = "hello" } ;;
:: - : Sexp.t = ((a ()) (b hello))
sexp_of_t { a = Some 3; b = "hello" } ;;
:: - : Sexp.t = ((a (3)) (b hello))
```

But what if we want a field to be optional, i.e., we want to allow it to be
omitted from the record entirely? In that case, we can mark it with
`sexp_option`:

```ocaml
type t = { a: int sexp_option; b: string } [@@deriving sexp] ;;
:: type t = { a : int option; b : string; }
:: val t_of_sexp : Sexp.t -> t = <fun>
:: val sexp_of_t : t -> Sexp.t = <fun>
sexp_of_t { a = Some 3; b = "hello" } ;;
:: - : Sexp.t = ((a 3) (b hello))
sexp_of_t { a = None; b = "hello" } ;;
:: - : Sexp.t = ((b hello))
```

### Specifying Defaults {#specifying-defaults}

The `sexp_option` declaration is really just an example of specifying a
default behavior for dealing with an unspecified field. In particular,
`sexp_option` fills in absent fields with `None`. But you might want to allow
other ways of filling in default values. [s-expressions/specifying defaults
in]{.idx}

Consider the following type, which represents the configuration of a very
simple web server:

```ocaml
type http_server_config = {
  web_root: string;
  port: int;
  addr: string;
} [@@deriving sexp] ;;
:: type http_server_config = { web_root : string; port : int; addr : string; }
:: val http_server_config_of_sexp :
::   Ppx_sexp_conv_lib.Sexp.t -> http_server_config = <fun>
:: val sexp_of_http_server_config :
::   http_server_config -> Ppx_sexp_conv_lib.Sexp.t = <fun>
```

One could imagine making some of these parameters optional; in particular, by
default, we might want the web server to bind to port 80, and to listen as
localhost. We can do this as follows:

```ocaml
type http_server_config = {
  web_root: string;
  port: int [@default 80];
  addr: string [@default "localhost"];
} [@@deriving sexp] ;;
:: type http_server_config = { web_root : string; port : int; addr : string; }
:: val http_server_config_of_sexp : Sexp.t -> http_server_config = <fun>
:: val sexp_of_http_server_config : http_server_config -> Sexp.t = <fun>
```

Now, if we try to convert an s-expression that specifies only the `web_root`,
we'll see that the other values are filled in with the desired defaults:

```ocaml
let cfg = http_server_config_of_sexp
            (Sexp.of_string "((web_root /var/www/html))") ;;
:: val cfg : http_server_config =
::   {web_root = "/var/www/html"; port = 80; addr = "localhost"}
```

If we convert the configuration back out to an s-expression, you'll notice
that all of the fields are present, even though they're not strictly
necessary:

```ocaml
sexp_of_http_server_config cfg ;;
:: - : Sexp.t = ((web_root /var/www/html) (port 80) (addr localhost))
```

We could make the generated s-expression also drop exported values, by using
the `sexp_drop_default` directive:

```ocaml
type http_server_config = {
  web_root: string;
  port: int [@default 80] [@sexp_drop_default];
  addr: string [@default "localhost"] [@sexp_drop_default];
} [@@deriving sexp] ;;
:: type http_server_config = { web_root : string; port : int; addr : string; }
:: val http_server_config_of_sexp : Sexp.t -> http_server_config = <fun>
:: val sexp_of_http_server_config : http_server_config -> Sexp.t = <fun>
let cfg = http_server_config_of_sexp
            (Sexp.of_string "((web_root /var/www/html))") ;;
:: val cfg : http_server_config =
::   {web_root = "/var/www/html"; port = 80; addr = "localhost"}
sexp_of_http_server_config cfg ;;
:: - : Sexp.t = ((web_root /var/www/html))
```

As you can see, the fields that are at their default values are simply
omitted from the s-expression. On the other hand, if we convert a config with
other values, then those values will be included in the s-expression:

```ocaml
sexp_of_http_server_config { cfg with port = 8080 } ;;
:: - : Sexp.t = ((web_root /var/www/html) (port 8080))
sexp_of_http_server_config
  { cfg with port = 8080; addr = "192.168.0.1" } ;;
:: - : Sexp.t = ((web_root /var/www/html) (port 8080) (addr 192.168.0.1))
```

This can be very useful in designing config file formats that are both
reasonably terse and easy to generate and maintain. It can also be useful for
backwards compatibility: if you add a new field to your config record, but
you make that field optional, then you should still be able to parse older
version of your config.
<a data-type="indexterm" data-startref="SERFORMsexp">&nbsp;</a>[files/config
files]{.idx}[config file formats]{.idx}

