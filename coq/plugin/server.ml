
module Make(PA: Pa.ProofAssistant) = struct
  module St = Store.Make(PA.M)
  type 'a m = ('a,PA.t) St.t
  open St.Combinators

  let rec whileM (cond : unit -> bool m) (body : unit -> unit m) : unit m =
    let* c = cond () in
    if c then
      let* _ = body () in whileM cond body
    else ret ()

  type remote = 
    { stdin: out_channel
    ; stdout: in_channel
    }

  (* TODO find a better way to discover it, instead of hardcoding the path *)
  let engine_path = "/home/luc/repos/coq-commutative-diagrams/engine/target/debug/commutative-diagrams-engine"

  let start_remote (_ : unit) : remote =
    let (stdout,stdin) = Unix.open_process_args engine_path [| "--embed" |] in
    { stdin = stdin; stdout = stdout }

  let is_none opt : bool =
    match opt with
    | Some _ -> true
    | None -> false

  let invalid_message (_ : unit) : unit m =
    (* TODO fail the tactic, instead of crashing *)
    Feedback.msg_warning (Pp.str "Incoming message is not well formed");
    assert false

  type [@warning "-37"] handler_ret =
    | HError of string
    | HRet of Msgpack.t
    | HNil
  type handler = Msgpack.t list -> handler_ret m

  let handle_goal (args : Msgpack.t list) : handler_ret m =
    assert false

  let run_handler (rm : remote) (msgid : int) (args : Msgpack.t list) (h : handler) : unit m =
    let* rt = h args in
    let (err,rt) =
      match rt with
      | HError err -> (Msgpack.String err, Msgpack.Nil)
      | HRet rt -> (Msgpack.Nil, rt)
      | HNil -> (Msgpack.Nil, Msgpack.Nil) in
    let response = Msgpack.Array [ Msgpack.Integer 1; Msgpack.Integer msgid; err; rt ] in
    let response = Msgpack.string_of_t_exn response in
    Stdio.Out_channel.output_string rm.stdin response;
    ret ()

  let handle_message (rm : remote) (msg : Msgpack.t) : unit m =
    match msg with
    | Msgpack.Array [ Msgpack.Integer 0; Msgpack.Integer msgid; Msgpack.String mtd; Msgpack.Array params ] -> begin
      match mtd with
      | "goal" -> run_handler rm msgid params handle_goal
      | _ -> invalid_message ()
    end
    | _ -> invalid_message ()

  let run (left : PA.t Data.morphism) (right : PA.t Data.morphism) : PA.t Data.eq m =
    let rm = start_remote () in
    let parser = ref (Angstrom.Buffered.parse ~initial_buffer_size:4096 Msgpack.Internal.Parser.msg) in
    let buffer = Bytes.create 1024 in
    let result : PA.t Data.eq option ref = ref None in
    let* _ = whileM (fun () -> ret (is_none !result)) (fun () -> begin
      match !parser with
      | Angstrom.Buffered.Partial _ -> begin
        (* Needs more input *)
        let read = Stdio.In_channel.input rm.stdout ~buf:buffer ~pos:0 ~len:1024 in
        let read_string = Bytes.sub_string buffer 0 read in
        parser := Angstrom.Buffered.feed !parser (`String read_string);
        ret ()
      end
      | Angstrom.Buffered.Fail _ -> invalid_message ()
      | Angstrom.Buffered.Done (ub, msg) -> begin
        (* Make sure to keep parsing *)
        parser := Angstrom.Buffered.parse ~initial_buffer_size:4096 Msgpack.Internal.Parser.msg;
        parser := Angstrom.Buffered.feed !parser (`Bigstring (Bigarray.Array1.sub ub.buf ub.off ub.len));
        (* Handle the message *)
        handle_message rm msg
      end
    end) in
    match !result with
    | Some eq -> ret eq
    | None -> assert false (* Shouldn't happen *)
end
