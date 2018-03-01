open Sexplib.Conv

module Chunk = struct
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t =
    { ocaml_code : string;
      toplevel_responses : response list; }
    [@@deriving sexp]

  let v ~ocaml_code ~toplevel_responses = {ocaml_code; toplevel_responses}
  let code c = c.ocaml_code
  let warnings (_ : t) : string =  ""
  let responses c = c.toplevel_responses
  let stdout (_ : t) = ""
  let evaluated (_ : t) = true
end

module Part = struct
  type t =
    { name : string;
      chunks : Chunk.t list; }
    [@@deriving sexp]

  let v ~name ~chunks = { name; chunks }
  let name {name;_} = name
  let chunks {chunks;_} = chunks
end

module Document = struct
  type t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]

  type rwo = [
    `OCaml_toplevel of t
  ] [@@deriving sexp]

  let v ~parts ~matched = {parts; matched}

  let rwo t = `OCaml_toplevel t
  let parts {parts;_} = parts
  let matched {matched;_} = matched
end

module Raw = struct

  open Lexing

  type t = {
    contents: string;
    lexbuf  : lexbuf;
  }

  let toplevel_fname = "//toplevel//"

  let shift_toplevel_position ~start pos = {
    pos_fname = toplevel_fname;
    pos_lnum = pos.pos_lnum - start.pos_lnum + 1;
    pos_bol  = pos.pos_bol  - start.pos_cnum - 1;
    pos_cnum = pos.pos_cnum - start.pos_cnum - 1;
  }

  let shift_toplevel_location ~start loc =
    let open Location in
    {loc with loc_start = shift_toplevel_position ~start loc.loc_start;
              loc_end = shift_toplevel_position ~start loc.loc_end}

  let initial_pos = {
    pos_fname = toplevel_fname;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0;
  }

  let semisemi_action =
    let lexbuf = Lexing.from_string ";;" in
    match Lexer.token lexbuf with
    | Parser.SEMISEMI ->
      lexbuf.Lexing.lex_last_action
    | _ -> assert false

  let v ~fname contents =
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- {initial_pos with pos_fname = fname};
    Location.input_name := fname;
    { contents; lexbuf }

  let of_file fname =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    v ~fname result

  let shift_location_error start =
    let open Location in
    let rec aux (error : Location.error) =
      {error with sub = List.map aux error.sub;
                  loc = shift_toplevel_location ~start error.loc}
    in
    aux

  let position_mapper start =
    let open Ast_mapper in
    let start = {start with pos_fname = toplevel_fname} in
    let location mapper loc =
      shift_toplevel_location ~start (default_mapper.location mapper loc)
    in
    {default_mapper with location}

end

module Phrase = struct

  open Lexing
  open Parsetree

  (** {1 Phrase parsing} *)

  type t = {
    startpos : position;
    endpos   : position;
    parsed   : (toplevel_phrase, exn) result;
  }

  let result t = t.parsed
  let start t = t.startpos

  let read lexbuf =
    let startpos = lexbuf.Lexing.lex_curr_p in
    let parsed = match Parse.toplevel_phrase lexbuf with
      | phrase -> Ok phrase
      | exception exn ->
        let exn = match Location.error_of_exn exn with
          | None -> raise exn
          | Some `Already_displayed -> raise exn
          | Some (`Ok error) ->
            Location.Error (Raw.shift_location_error startpos error)
        in
        if lexbuf.Lexing.lex_last_action <> Raw.semisemi_action then begin
          let rec aux () = match Lexer.token lexbuf with
            | Parser.SEMISEMI | Parser.EOF -> ()
            | _ -> aux ()
          in
          aux ();
        end;
        Error exn
    in
    let endpos = lexbuf.Lexing.lex_curr_p in
    { startpos; endpos; parsed }

  let read doc = match read doc.Raw.lexbuf with
    | exception End_of_file -> None
    | t -> Some t

  (** *)

  type 'a kind =
    | Code of 'a
    | Expect of { location: Location.t;
                  responses: Chunk.response list;
                  nondeterministic: bool }
    | Part of { location: Location.t; name: string }

  type v = (Chunk.kind * string) list kind

  exception Cannot_parse_payload of Location.t

  let string_of_location
      {Location.loc_start = {pos_fname; pos_lnum; pos_bol; pos_cnum};_}
    =
    Printf.sprintf "%s, line %d, col %d" pos_fname pos_lnum (pos_cnum - pos_bol)

  let payload_constants loc = function
    | PStr [{pstr_desc = Pstr_eval (expr, _); _}] ->
      let one {pexp_loc; pexp_desc; _} = match pexp_desc with
        | Pexp_apply ({pexp_desc = Pexp_ident ident; _},
                      [Asttypes.Nolabel, {pexp_desc = Pexp_constant const; _}]) ->
          (pexp_loc, Some ident, const)
        | Pexp_constant const -> (pexp_loc, None, const)
        | _ -> raise (Cannot_parse_payload pexp_loc)
      in
      let rec consts = function
        | {pexp_desc=Pexp_sequence(e, rest); _} -> one e :: consts rest
        | e -> [one e]
      in
      consts expr
    | PStr [] -> []
    | _ -> raise (Cannot_parse_payload loc)

  let payload_strings loc = function
    | PStr [] -> []
    | x ->
      let aux = function
        | _, Some {Location.txt = Longident.Lident "ocaml"; _},
          Pconst_string (str, _) -> (Chunk.OCaml, str)
        | _, None, Pconst_string (str, _) -> (Chunk.Raw, str)
        | loc, _, _ -> raise (Cannot_parse_payload loc)
      in
      List.map aux (payload_constants loc x)

  let attr_is x name = x.Asttypes.txt = name

  let kind phrase = match phrase.parsed with
    | Ok (Ptop_def [{pstr_desc = Pstr_extension((attr, payload), _attrs); pstr_loc}])
      when List.exists (attr_is attr) ["expect"; "expect.nondeterministic"] ->
      begin match payload_strings pstr_loc payload with
        | responses ->
          let nondeterministic = attr_is attr "expect.nondeterministic" in
          Expect { location = pstr_loc; responses; nondeterministic }
        | exception (Cannot_parse_payload loc) ->
          prerr_endline (string_of_location loc ^ ": cannot parse [%%expect] payload");
          Code ()
      end
    | Ok (Ptop_def [{pstr_desc = Pstr_attribute (name, payload); pstr_loc}])
      when name.Asttypes.txt = "part" ->
      begin match payload_strings pstr_loc payload with
        | [Chunk.Raw, part] -> Part { location = pstr_loc; name = part }
        | _ ->
          prerr_endline (string_of_location pstr_loc ^ ": cannot parse [@@@part] payload");
          Code ()
        | exception (Cannot_parse_payload loc) ->
          prerr_endline
            (string_of_location loc ^ ": cannot parse [@@@part] payload");
          Code ()
      end
    | _ -> Code ()

  (* Skip spaces as well as ';;' *)
  let skip_whitespace contents ?(stop=String.length contents) start =
    let rec loop start =
      if start >= stop then start else
        match contents.[start] with
        | ' ' | '\t' | '\n' -> loop (start + 1)
        | ';' when start + 1 < stop && contents.[start+1] = ';' ->
          loop (start + 2)
        | _ -> start
    in
    loop start

  let contents doc ?start ?stop phrase =
    let stop = match stop with
      | None -> phrase.endpos.pos_cnum
      | Some stop -> stop
    in
    let start = match start with
      | None -> phrase.startpos.pos_cnum
      | Some start -> start
    in
    let start = skip_whitespace doc.Raw.contents ~stop start in
    String.sub doc.contents start (stop - start)

  let whitespace doc phrase rest =
    let start = phrase.endpos.pos_cnum in
    let stop = match rest with
      | [] -> String.length doc.Raw.contents
      | (p, _) :: _ -> skip_whitespace doc.contents p.startpos.pos_cnum
    in
    String.sub doc.contents start (stop - start)

  let document_of_phrases doc matched phrases =
    let rec parts_of_phrase part acc = function
      | (_, Part { name; _ }) :: rest ->
        Part.v ~name:part ~chunks:(List.rev acc) ::
        parts_of_phrase name [] rest
      | (_, Expect _) :: rest ->
        parts_of_phrase part acc rest
      | (phrase, Code toplevel_responses) :: rest ->
        let ocaml_code = contents doc phrase in
        let chunk = Chunk.v ~ocaml_code ~toplevel_responses in
        parts_of_phrase part (chunk :: acc) rest
      | [] ->
        if part <> "" || acc <> [] then
          [Part.v ~name:part ~chunks:(List.rev acc)]
        else
          []
    in
    let parts = parts_of_phrase "" [] phrases in
    Document.v ~matched ~parts

  let is_findlib_directive =
    let findlib_directive = function
      | "require" | "use" | "camlp4o" | "camlp4r" | "thread" -> true
      | _ -> false
    in
    function
    | { parsed = Ok (Ptop_dir (dir, _)); _ } -> findlib_directive dir
    | _ -> false

  let dry_exec phrases =
    let rec aux acc = function
      | [] -> List.rev acc
      | (phrase, Code ()) :: rest ->
        begin match rest with
          | (_, Expect { responses; _ }) :: _ ->
            aux ((phrase, Code responses) :: acc) rest
          | _ -> aux ((phrase, Code []) :: acc) rest
        end
      | (_, (Part _ | Expect _) as phrase) :: rest ->
        aux (phrase :: acc) rest
    in
    aux [] phrases

  let read_all doc =
    let rec aux phrases = match read doc with
      | None        ->  List.rev phrases
      | Some phrase -> aux ((phrase, kind phrase) :: phrases)
    in
    dry_exec (aux [])

end
