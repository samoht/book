module Linked = struct
  include (Topdirs : sig end)
  include (Ephemeron : sig end)
  include (Uchar : sig end)
  include (Condition : sig end)
end

open Parsetree
open Sexplib.Conv
open Ocaml_topexpect

(* Standard outputs should be disable: multi threaded code could print
 * at anytime, so we just disable output by defaul. *)

let stdout_backup = Unix.out_channel_of_descr (Unix.dup Unix.stdout)
let stderr_backup = Unix.out_channel_of_descr (Unix.dup Unix.stderr)

let prerr_endline str =
  output_string stderr_backup str;
  output_char stderr_backup '\n';
  flush stderr_backup

let disable_outputs = lazy (
  let fd_out = Unix.openfile "/dev/null" Unix.[O_WRONLY] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  Unix.close fd_out;
)

let verbose = ref false
let () = Hashtbl.add Toploop.directive_table "verbose"
    (Toploop.Directive_bool (fun x -> verbose := x))
let silent = ref false
let () = Hashtbl.add Toploop.directive_table "silent"
    (Toploop.Directive_bool (fun x -> silent := x))
let verbose_findlib = ref false

module Async_autorun = struct
  (* Inspired by Utop auto run rewriter *)
  let (async_typ, async_runner, async_rewrite) =
    let typ = Longident.parse "Async.Deferred.t" in
    let runner = Longident.parse "Async.Thread_safe.block_on_async_exn" in
    let open Ast_helper in
    let rewrite loc e =
      let punit =
        Pat.construct (Location.mkloc (Longident.Lident "()") loc) None in
      with_default_loc loc @@ fun () ->
      Exp.apply
        (Exp.ident (Location.mkloc runner loc))
        [(Asttypes.Nolabel, Exp.fun_ Asttypes.Nolabel None punit e)]
    in
    (typ, runner, rewrite)

  let normalize_type_path env path =
    match Env.find_type path env with
    | { Types.type_manifest = Some ty; _ } -> begin
        match Ctype.expand_head env ty with
        | { Types.desc = Types.Tconstr (path, _, _);_ } -> path
        | _ -> path
      end
    | _ -> path

  let is_persistent_value env longident =
    let rec is_persistent_path = function
      | Path.Pident id -> Ident.persistent id
      | Path.Pdot (p, _, _) -> is_persistent_path p
      | Path.Papply (_, p) -> is_persistent_path p
    in
    try is_persistent_path (fst (Env.lookup_value longident env))
    with Not_found -> false

  let rewrite_item env async_typ pstr_item tstr_item =
    match pstr_item.Parsetree.pstr_desc, tstr_item.Typedtree.str_desc with
    | (Parsetree.Pstr_eval (e, _),
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ; _ }, _)) ->
      begin match (Ctype.repr typ).Types.desc with
        | Types.Tconstr (path, _, _) when
            Path.same async_typ (normalize_type_path env path) ->
          let loc = pstr_item.Parsetree.pstr_loc in
          { Parsetree.pstr_desc = Parsetree.Pstr_eval (async_rewrite loc e, []);
            Parsetree.pstr_loc = loc }
        | _ -> pstr_item
      end
    | _ -> pstr_item

  let rewrite_phrase =
    let is_eval = function
      | { pstr_desc = Pstr_eval _; _ } -> true
      | _ -> false
    in
    function
    | Ptop_def pstr when List.exists is_eval pstr
                      && is_persistent_value !Toploop.toplevel_env async_runner ->
      Env.reset_cache_toplevel ();
      let snap = Btype.snapshot () in
      let pstr =
        try
          let env = !Toploop.toplevel_env in
          let path = normalize_type_path env (Env.lookup_type async_typ env) in
          let tstr, _tsg, env =
            Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
          List.map2 (rewrite_item env path) pstr tstr.Typedtree.str_items
        with _ ->
          pstr
      in
      Btype.backtrack snap;
      Ptop_def pstr
    | phrase -> phrase
end

let toplevel_exec_phrase ppf p = match Phrase.result p with
  | Error exn -> raise exn
  | Ok phrase ->
    Warnings.reset_fatal ();
    let mapper = Raw.position_mapper (Phrase.start p) in
    let phrase = match phrase with
      | Ptop_def str -> Ptop_def (mapper.Ast_mapper.structure mapper str)
      | Ptop_dir _ as x -> x
    in
    let phrase = match phrase with
      | Ptop_dir _ as x -> x
      | Ptop_def s -> Ptop_def (Pparse.apply_rewriters_str ~tool_name:"expect" s)
    in
    let phrase = Async_autorun.rewrite_phrase phrase in
    if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
    if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
    Env.reset_cache_toplevel ();
    Toploop.execute_phrase !verbose ppf phrase
;;

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter (fun (V (r, v)) -> r := v) l in
  fun vars ~f ->
    let backup = List.map (fun (V (r, _)) -> V (r, !r)) vars in
    set_vars vars;
    Misc.try_finally f (fun () -> set_vars backup)
;;

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Location.formatter_for_warnings , ppf) ]
    ~f
;;

let redirect ~f =
  let lazy () = disable_outputs in
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stdout in
  let filename = Filename.temp_file "expect-test" "stdout" in
  let fd_out = Unix.openfile filename Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  let ic = open_in filename in
  let read_up_to = ref 0 in
  let capture buf =
    flush stdout;
    flush stderr;
    let pos = Unix.lseek fd_out 0 Unix.SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Buffer.add_channel buf ic len
  in
  Misc.try_finally (fun () -> f ~capture)
    (fun () ->
       close_in_noerr ic;
       Unix.close fd_out;
       Unix.dup2 stdout_backup Unix.stdout;
       Unix.dup2 stderr_backup Unix.stderr;
       Unix.close stdout_backup;
       Unix.close stderr_backup;
       Sys.remove filename)
;;

let cleanup_chunk (kind, str) =
  let len = String.length str in
  if len = 0 then (kind, str) else
    let trim_from = if str.[0] = '\n' then 1 else 0 in
    let trim_to = if str.[len - 1] = '\n' then len - 1 else len in
    (kind, String.sub str trim_from (trim_to - trim_from))

let cleanup_lines lines =
  let lines = List.map cleanup_chunk lines in
  let rec join = function
    | (Chunk.Raw, str1) :: (Chunk.Raw, str2) :: rest ->
      join ((Chunk.Raw, str1 ^ "\n" ^ str2) :: rest)
    | (Chunk.OCaml, str1) :: (Chunk.OCaml, str2) :: rest ->
      join ((Chunk.OCaml, str1 ^ "\n" ^ str2) :: rest)
    | x :: xs -> x :: join xs
    | [] -> []
  in
  join lines

let eval_phrases ~run_nondeterministic ~dry_run doc =
  let open Phrase in
  (* 4.03: Warnings.reset_fatal (); *)
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_code ~capture phrase =
    let lines = ref [] in
    let capture kind =
      capture buf;
      match Buffer.contents buf with
      | "" -> ()
      | s -> Buffer.clear buf; lines := (kind, s) :: !lines
    in
    let out_phrase' = !Oprint.out_phrase in
    let out_phrase ppf phr = match phr with
      | Outcometree.Ophr_exception _ -> out_phrase' ppf phr
      | _ ->
        capture Chunk.Raw;
        out_phrase' ppf phr;
        capture Chunk.OCaml;
    in
    Oprint.out_phrase := out_phrase;
    let restore () = Oprint.out_phrase := out_phrase' in
    begin match toplevel_exec_phrase ppf phrase with
      | (_ : bool) -> restore ()
      | exception exn ->
        restore ();
        Location.report_exception ppf exn
    end;
    Format.pp_print_flush ppf ();
    capture Chunk.Raw;
    if !silent || (not !verbose_findlib && is_findlib_directive phrase) then
      Code []
    else
      Code (cleanup_lines (List.rev !lines))
  in
  if dry_run then  (
    let rec aux phrases = match Phrase.read doc with
      | None        ->  List.rev phrases
      | Some phrase -> aux ((phrase, Phrase.role phrase) :: phrases)
    in
    Phrase.dry_exec (aux [])
  ) else (
    redirect ~f:(fun ~capture ->
        capture_compiler_stuff ppf ~f:(fun () ->
            let rec process_phrase chunks phrase =
              match Phrase.role phrase with
              | Expect x ->
                next_phrase ((phrase, Expect {x with responses = cleanup_lines x.responses}) :: chunks)
              | Part _ as x ->
                next_phrase ((phrase, x) :: chunks)
              | Code () ->
                match Phrase.read doc with
                | None         ->
                  List.rev ((phrase, exec_code ~capture phrase) :: chunks)
                | Some phrase' ->
                  let role = match Phrase.role phrase' with
                    | Expect { nondeterministic = true; responses; _ }
                      when not run_nondeterministic -> Code responses
                    | _ -> exec_code ~capture phrase
                  in
                  process_phrase ((phrase, role) :: chunks) phrase'
            and next_phrase chunks = match Phrase.read doc with
              | None        -> List.rev chunks
              | Some phrase -> process_phrase chunks phrase
            in
            next_phrase []
          )
      )
  )
;;

let is_whitespace = function
  | ' ' | '\n' -> true
  | _ -> false

let is_all_whitespace str =
  try
    for i = 0 to String.length str - 1 do
      if not (is_whitespace str.[i]) then raise Exit
    done;
    true
  with Exit -> false

let is_ellision_line str =
  let i = ref 0 in
  let j = String.length str in
  while !i < j && is_whitespace str.[!i] do incr i done;
  !i <= j - 3 && str.[!i] = '.' && str.[!i+1] = '.' && str.[!i+2] = '.' && (
    i := !i + 3;
    while !i < j && is_whitespace str.[!i] do incr i done;
    !i = j
  )

let string_subequal subject str i =
  let len = String.length subject in
  String.length str >= len + i &&
  try
    for j = 0 to len - 1 do
      if subject.[j] <> str.[i+j] then
        raise Exit;
    done;
    true
  with Exit ->
    false

let match_outcome_chunk (k1,outcome) (k2,expected) =
  k1 = k2 &&
  let rec split_chunks acc str i0 i =
    match String.index_from str i '\n' with
    | exception Not_found ->
      let acc =
        if is_ellision_line (String.sub str i (String.length str - i))
        then "" :: String.sub str i0 i :: acc
        else String.sub str i0 (String.length str - i0) :: acc
      in
      List.rev acc
    | j ->
      if is_ellision_line (String.sub str i (j - i)) then
        split_chunks (String.sub str i0 (i - i0) :: acc) str (j + 1) (j + 1)
      else
        split_chunks acc str i0 (j + 1)
  in
  match split_chunks [] expected 0 0 with
  | [] -> assert false
  | x :: xs ->
    string_subequal x outcome 0 &&
    let rec match_chunks i = function
      | [] -> i = String.length outcome
      | [x] ->
        let i' = String.length outcome - String.length x in
        i' >= i && string_subequal x outcome i'
      | x :: xs ->
        let bound = String.length outcome - String.length x in
        let i = ref i in
        while !i <= bound && not (string_subequal x outcome !i)
        do incr i done;
        if !i > bound then false
        else match_chunks (!i + String.length x) xs
    in
    match_chunks (String.length x) xs

let match_outcome xs ys =
  List.length xs = List.length ys &&
  List.for_all2 match_outcome_chunk xs ys

(* Check if output matches expectations and keep ellisions when possible *)
let validate_phrases run_nondeterministic =
  let open Phrase in
  let rec aux success acc = function
    | [] -> (success, List.rev acc)
    | (_, Part _ as entry) :: rest ->
      aux success (entry :: acc) rest
    | (p0, Code outcome) :: (p1, Expect outcome') :: rest ->
      let success' =
        if outcome'.nondeterministic && not run_nondeterministic then
          true
        else
          match_outcome outcome outcome'.responses
      in
      let acc =
        if success' then
          (p1, Expect outcome') :: (p0, Code outcome'.responses) :: acc
        else
          (p1, Expect outcome') :: (p0, Code outcome) :: acc
      in
      aux (success && success') acc rest
    | (_, Code outcome as x) :: rest ->
      let success =
        success && List.for_all (fun (_,s) -> is_all_whitespace s) outcome
      in
      aux success (x :: acc) rest
    | (_, Expect _ as x) :: rest ->
      aux false (x :: acc) rest
  in
  fun phrases -> aux true [] phrases


let find_delim s =
  let len = String.length s in
  let delims = ref [] in
  let beginning = ref (-1) in
  for i = 0 to len - 1 do
    match s.[i] with
    | '{' -> beginning := i
    | '|' when !beginning = -1 || s.[!beginning] <> '{' -> beginning := i
    | ('|' | '}' as c) when !beginning <> -1 && (c = '|' || s.[!beginning] = '|') ->
      let delim = String.sub s (!beginning + 1) (i - !beginning - 1) in
      begin match !delims with
        | delim' :: _ when delim' = delim -> ()
        | delims' -> delims := delim :: delims'
      end;
      beginning := -1
    | 'a'..'z' | '_' -> ()
    | _ -> beginning := -1
  done;
  let delims = !delims in
  let candidates = [""; "escape"; "x"; "y"; "z"] in
  match List.find (fun delim -> not (List.mem delim delims)) candidates with
  | candidate -> candidate
  | exception Not_found ->
    (* Generate a string that is not in the list of delimiters *)
    let next b =
      try
        for i = Bytes.length b - 1 downto 0 do
          match Bytes.get b i with
          | 'z' -> Bytes.set b i 'a'
          | c ->
            Bytes.set b i (Char.chr (Char.code c + 1));
            raise Exit
        done;
        Bytes.cat b (Bytes.unsafe_of_string "a")
      with Exit -> b
    in
    let rec exhaust b =
      if not (List.mem (Bytes.unsafe_to_string b) delims)
      then Bytes.unsafe_to_string b
      else exhaust (next b)
    in
    exhaust (Bytes.of_string "")

let output_phrases oc d =
  let open Phrase in
  let rec aux = function
    | [] -> ()
    | (phrase, Part {name; location}) :: rest ->
      Printf.fprintf oc "%s[@@@part %S];;\n"
        (Phrase.contents d phrase ~stop:location.loc_start.pos_cnum) name;
      aux rest
    | (phrase, Code expect_code) :: rest ->
      let phrase_post, expect_pre, expect_post, nondeterministic, rest =
        match rest with
        | (phrase_expect, Expect x) :: rest' ->
          (Phrase.whitespace d phrase rest,
           Phrase.contents d phrase_expect ~stop:x.location.loc_start.pos_cnum,
           Phrase.whitespace d phrase_expect rest',
           x.nondeterministic,
           rest')
        | _ ->
          ("\n", "", Phrase.whitespace d phrase rest, false, rest)
      in
      let phrase_code = Phrase.contents d phrase in
      if List.for_all (fun (_,s) -> is_all_whitespace s) expect_code &&
         not nondeterministic then
        Printf.fprintf oc "%s%s%s" phrase_code expect_pre expect_post
      else (
        let string_of_kind = function
          | Chunk.Raw -> ""
          | Chunk.OCaml -> "ocaml "
        in
        let output_expect oc = function
          | [] -> ()
          | [(kind, str)] when not (String.contains str '\n') ->
            let k = string_of_kind kind in
            let delim = find_delim str in
            Printf.fprintf oc "%s%s{%s|%s|%s}" (if k <> "" then " " else "") k delim str delim
          | xs ->
            let rec aux first = function
              | [] -> ()
              | (k,s) :: xs ->
                let k = string_of_kind k in
                let pre = if first then (if k <> "" then " " else "") else "\n" in
                let post = if xs = [] then "" else ";" in
                let delim = find_delim s in
                if not (String.contains s '\n') then
                  Printf.fprintf oc "%s%s{%s|%s|%s}%s" pre k delim s delim post
                else
                  Printf.fprintf oc "%s%s{%s|\n%s\n|%s}%s" pre k delim s delim post;
                aux false xs
            in
            aux true xs
        in
        Printf.fprintf oc "%s%s%s[%%%%expect%s%a];;%s"
          phrase_code phrase_post
          expect_pre
          (if nondeterministic then ".nondeterministic" else "")
          output_expect expect_code expect_post;
      );
      aux rest
    | (phrase, Expect {location; _}) :: rest ->
      Printf.fprintf oc "%s"
        (Phrase.contents d phrase ~stop:location.loc_start.pos_cnum);
      aux rest
  in aux


let process_expect_file ~run_nondeterministic ~fname ~dry_run ~use_color:_ ~sexp_output =
  let file_contents =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    result
  in
  let doc = Raw.v ~fname file_contents in
  let phrases = eval_phrases ~run_nondeterministic ~dry_run doc in
  let success, phrases = validate_phrases run_nondeterministic phrases in
  let oname = fname ^ ".corrected" in
  if success && Sys.file_exists oname then Sys.remove oname;
  let phrases =
    if success then phrases
    else (
      (* Otherwise, generate corrected file and keep toplevel output. *)
      let oc = open_out_bin (oname ^ ".tmp") in
      output_phrases oc doc phrases;
      flush oc;
      close_out oc;
      Sys.rename (oname ^ ".tmp") oname;
      phrases
    )
  in
  if sexp_output then (
    Phrase.document_of_phrases doc success phrases
    |> Document.rwo
    |> Document.sexp_of_rwo
    |> Sexplib.Sexp.output stdout_backup
  );
  success
;;

let override_sys_argv args =
  let len = Array.length args in
  assert (len <= Array.length Sys.argv);
  Array.blit args 0 Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
;;

let use_color   = ref true
let sexp_output = ref false
let dry_run     = ref false
let run_nondeterministic = ref false

let process_file fname =
  let cmd_line =
    Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current)
  in
  override_sys_argv cmd_line;
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  let _success =
    process_expect_file ~fname
      ~run_nondeterministic:!run_nondeterministic
      ~dry_run:!dry_run ~use_color:!use_color
      ~sexp_output:!sexp_output
  in
  exit 0
;;

let args =
  Arg.align
    [ "-sexp"    , Arg.Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-verbose" , Arg.Set verbose, " Include outcome of phrase evaluation (like ocaml toplevel)"
    ; "-dry-run" , Arg.Set dry_run, " Don't execute code, only return expected outcome"
    ; "-run-nondeterministic" , Arg.Set run_nondeterministic, " Run non-deterministic tests"
    ; "-verbose-findlib", Arg.Set verbose_findlib, " Include outcome of findlib directives (#require, #use, ...)"
    ]

let print_version () =
  Printf.fprintf stdout_backup "topexect, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.fprintf stdout_backup "%s\n" Sys.ocaml_version;
  exit 0;
;;

module Options = Main_args.Make_bytetop_options (struct
    open Clflags
    open Compenv

    let set r () = r := true
    let clear r () = r := false

    let _absname = set Location.absname
    let _I dir =
      let dir = Misc.expand_directory Config.standard_library dir in
      include_dirs := dir :: !include_dirs
    let _init s = init_file := Some s
    let _noinit = set noinit
    let _labels = clear classic
    let _alias_deps = clear transparent_modules
    let _no_alias_deps = set transparent_modules
    let _app_funct = set applicative_functors
    let _no_app_funct = clear applicative_functors
    let _noassert = set noassert
    let _nolabels = set classic
    let _noprompt = set noprompt
    let _nopromptcont = set nopromptcont
    let _nostdlib = set no_std_include
    let _open s = open_modules := s :: !open_modules
    let _plugin p = Compplugin.load p
    let _ppx s = first_ppx := s :: !first_ppx
    let _principal = set principal
    let _no_principal = clear principal
    let _rectypes = set recursive_types
    let _no_rectypes = clear recursive_types
    let _safe_string = clear unsafe_string
    let _short_paths = clear real_paths
    let _stdin () = raise (Arg.Bad "-stdin not supported")
    let _strict_sequence = set strict_sequence
    let _no_strict_sequence = clear strict_sequence
    let _strict_formats = set strict_formats
    let _no_strict_formats = clear strict_formats
    let _unboxed_types = set unboxed_types
    let _no_unboxed_types = clear unboxed_types
    let _unsafe = set fast
    let _unsafe_string = set unsafe_string
    let _version () = print_version ()
    let _vnum () = print_version_num ()
    let _no_version = set noversion
    let _w s = Warnings.parse_options false s
    let _warn_error s = Warnings.parse_options true s
    let _warn_help = Warnings.help_warnings
    let _dparsetree = set dump_parsetree
    let _dtypedtree = set dump_typedtree
    let _dsource = set dump_source
    let _drawlambda = set dump_rawlambda
    let _dlambda = set dump_lambda
    let _dflambda = set dump_flambda
    (* let _dtimings = set print_timings *)
    let _dinstr = set dump_instr

    let anonymous s = process_file s
    let _args _ = failwith "Arg.read_arg not implemented"
    let _args0 _ = failwith "Arg.read_arg0 not implemented"
    (*let _args = Arg.read_arg
    let _args0 = Arg.read_arg0*)
  end);;

(* BLACK MAGIC: patch field of a module at runtime *)
let monkey_patch (type a) (type b) (m: a) (prj: unit -> b) (v : b) =
  let m = Obj.repr m in
  let v = Obj.repr v in
  let v' = Obj.repr (prj ()) in
  if v' == v then () else (
    try
      for i = 0 to Obj.size m - 1 do
        if Obj.field m i == v' then (
          Obj.set_field m i v;
          if Obj.repr (prj ()) == v then raise Exit;
          Obj.set_field m i v';
        )
      done;
      invalid_arg "monkey_patch: field not found"
    with Exit -> ()
  )

let main () =
  let module M = struct
    module type T = module type of Env
    let field () = Env.without_cmis
    let replacement f x = f x
    let () = monkey_patch (module Env : T) field replacement
  end in
  Topfind.don't_load_deeply ["unix"; "findlib.top"; "findlib.internal"; "compiler-libs.toplevel"; "ppx_sexp_conv"];

  let usage =
    Printf.sprintf "Usage: %s [OPTIONS] FILE [ARGS]\n"
      (Filename.basename Sys.argv.(0))
  in
  try
    let args = Arg.align (args @ Options.list) in
    Arg.parse args process_file (usage ^ "\nOptions are:");
    Printf.fprintf stderr_backup "%s\n%!" usage;
    exit 2
  with exn ->
    ignore (Format.flush_str_formatter ());
    Location.report_exception Format.str_formatter exn;
    Printf.fprintf stderr_backup "%s\n%!" (Format.flush_str_formatter ());
    exit 2
;;
let () = main ();;
