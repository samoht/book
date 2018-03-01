module Chunk : sig
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t [@@deriving sexp]
  val v : ocaml_code:string -> toplevel_responses:response list -> t

  val code : t -> string
  val warnings : t -> string
  val responses : t -> response list
  val stdout : t -> string
  val evaluated : t -> bool

end

module Part : sig
  type t [@@deriving sexp]
  val v : name:string -> chunks:Chunk.t list -> t

  val name : t -> string
  val chunks : t -> Chunk.t list
end

module Document : sig
  type t [@@deriving sexp]
  type rwo [@@deriving sexp]
  val v : parts:Part.t list -> matched:bool -> t

  val rwo : t -> rwo
  val parts : t -> Part.t list
  val matched : t -> bool
end

module Raw: sig
  type t
  val v: fname:string -> string -> t
  val of_file: string -> t
  val position_mapper: Lexing.position -> Ast_mapper.mapper
end

module Phrase: sig
  type t

  val result: t -> (Parsetree.toplevel_phrase, exn) result
  val start: t -> Lexing.position
  val is_findlib_directive: t -> bool

  val read: Raw.t -> t option

  type 'a kind =
    | Code of 'a
    | Expect of { location: Location.t;
                  responses: Chunk.response list;
                  nondeterministic: bool }
    | Part of { location: Location.t; name: string }

  val kind: t -> unit kind

  type v = (Chunk.kind * string) list kind
  (** The type for evaluated phrases. *)

  val read_all: Raw.t -> (t * v) list
  (** [read_all d] is the list of phrases where [Code] parameters are
      filled with what is in the corresponding [Expext] segments. *)

  val whitespace: Raw.t -> t -> (t * 'a) list -> string

  val document_of_phrases: Raw.t -> bool -> (t * v) list -> Document.t

  val contents: Raw.t -> ?start:int -> ?stop:int -> t -> string

end
