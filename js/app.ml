open Lwt.Infix
open Tyxml_js
open Html5

let debug fmt = Fmt.kstrf print_endline ("LOG: "^^fmt)

let run t =
  Lwt_js_events.onload () >>= fun _ -> t ()

module Store = Irmin_mem.KV(Irmin.Contents.String)
module Hash = Store.Contents.Hash

(** Irmin stuff *)

let info msg () = Irmin.Info.v ~date:0L ~author:"Thomas" msg

let connect () =
  Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
  Store.master repo

let (/) = Store.Key.rcons

module Comment = struct

  let string_of_id d = Fmt.strf "%02d" d

  let root_path = []
  let comments_path id = root_path / id / "comments"
  let body_path id i = comments_path id / i / "body"

  let size t id =
    debug "Comment.length %s" id;
    Store.list t (comments_path id) >|= fun l ->
    List.length l

  let find t id =
    debug "Comment.find %s" id;
    Store.list t (comments_path id) >>= fun l ->
    let comments =
      List.fold_left (fun acc -> function
          | s, `Node -> s :: acc
          | _ -> acc
        ) [] l
    in
    debug "found %d comments" (List.length comments);
    Lwt_list.map_p (fun i ->
        Store.get t (body_path id i) >|= fun s ->
        int_of_string i, s
      ) comments
    >|= fun l ->
    List.sort (fun (x, _) (y, _) -> compare x y) l

  let add t id body =
    debug "Comment.add %s" id;
    Store.list t (comments_path id) >>= fun comments ->
    let max =
      List.fold_left (fun acc -> function
          | s, `Node ->
            let n = match int_of_string (String.trim s) with
              | i -> i
              | exception n -> debug "??? %s %a" s Fmt.exn n; failwith "err"
            in max n acc
          | _ -> acc
        ) 0 comments
    in
    let curr = string_of_id (max + 1) in
    let path = body_path id curr in
    debug "add: path=%a" Store.Key.pp path;
    Store.set ~info:(info "adding a new comment.") t path body >|= fun () ->
    max+1

  let init t =
    add t "11c699b8c160ab4deca1fee084ed37052a7d7b81" "One comment!"
    >>= fun _ ->
    add t "11c699b8c160ab4deca1fee084ed37052a7d7b81" "Two comments!"
    >|= fun _ ->
    ()

end

(*
let user = "samoht"
let avatar =
  "https://avatars1.githubusercontent.com/u/103693?s=44&v=4"
*)

(** Web stuff *)
let async ~name (fn:unit -> unit Lwt.t) =
  Lwt_js_events.async (fun () ->
      Lwt.catch fn (fun ex ->
          Printf.printf "Async error in '%s'" name;
          Lwt.fail ex
        )
    )

(* Cross-browser form submit buttons *)
let submit_button label =
  try
    input ~a:[a_input_type `Submit; a_value label] ()
  with _ ->
    (* Hello, MSIE!
     * http://reference.sitepoint.com/javascript/Element/setAttributeNode *)
    let s = span [] in
    let elem = Tyxml_js.To_dom.of_span s in
    elem##.innerHTML :=
      Js.string (Printf.sprintf "<input type='submit' value='%s'>" label);
    s

(* Form submission MUST always return false, or the page will refresh. *)
let a_onsubmit fn =
  a_onsubmit (fun ev ->
    try fn ev; false
    with ex ->
      !Lwt.async_exception_hook ex;
      false
  )

let auto_focus input =
  async ~name:"focus" (fun () ->
    let elem = Tyxml_js.To_dom.of_input input in
    elem##select;
    Lwt.return ()
  )

(** Rendering *)

let id_of_p (p: Dom_html.element Js.t) =
  let str = Js.to_string p##.innerHTML in
  Fmt.kstrf print_endline "XXX %s" str;
  Fmt.to_to_string Hash.pp @@ Hash.digest Irmin.Type.string str

let show_comment id body =
  li ~a:[a_id (Comment.string_of_id id)] [pcdata body]

let show_add_comment t id incr_n set_comments =
  let comment_input =
    input ~a:[a_name "comment"; a_placeholder "Your comment."; a_size 40] ()
  in
  auto_focus comment_input;
  let add _ =
    let elt = To_dom.of_input comment_input in
    let comment = Js.to_string elt##.value in
    if String.trim comment <> "" then (
      async ~name:"add" (fun () ->
          Comment.add t id comment >|= fun id ->
          let n = incr_n () in
          ReactiveData.RList.(patch set_comments [
              I (n-1, show_comment id comment);
            ]);
          elt##.value := Js.string ""
        ));
  in
  form ~a:[a_onsubmit add] [
    comment_input;
    submit_button "+"
  ]

let show_comments t id set_n set_comments =
  Comment.find t id >|= fun comments ->
  let n = List.length comments in
  set_n n;
  let incr_n =
    let r = ref n in
    fun () -> incr r; set_n !r; !r
  in
  let comments =
    List.map (fun (id, body) -> show_comment id body) comments
    @ [li [show_add_comment t id incr_n set_comments]]
  in
  ReactiveData.RList.set set_comments comments

let show_comments t id p =
  let comments, set_comments = ReactiveData.RList.create [] in
  Comment.size t id >|= fun n ->
  let n, set_n = React.S.create n in
  let txt =
    React.S.map (function
      | 0 | 1 as i -> Fmt.strf "(%d comment)" i
      | i -> Fmt.strf "(%d comments)" i
    ) n
  in
  let toggle =
    let visible = ref false in
    fun _ ->
      if not !visible then (
        visible := true;
        async ~name:"show" (fun () -> show_comments t id set_n set_comments)
      ) else (
        ReactiveData.RList.set set_comments [];
        visible := false
      );
      false
  in
  let nodes = [
    pcdata " ";
    span [a ~a:[a_href "#"; a_onclick toggle] [R.Html5.pcdata txt]];
    div ~a:[a_class ["comments"]]  [R.Html5.ul comments];
  ] in
  Register.id ~keep:true id nodes

let ps () =
  connect () >>= fun t ->
  Comment.init t >>= fun () ->
  let ps =
    Dom.list_of_nodeList @@
    Dom_html.document##getElementsByTagName (Js.string "p")
  in
  Lwt_list.iter_s (fun p ->
      let id = id_of_p p in
      p##.id := Js.string id;
      show_comments t id p
    ) ps

let _ = run ps
