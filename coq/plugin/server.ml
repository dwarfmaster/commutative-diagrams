
module Make(PA: Pa.ProofAssistant) = struct
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

  let invalid_message (_ : unit) =
    (* TODO fail the tactic, instead of crashing *)
    Feedback.msg_warning (Pp.str "Incoming message is not well formed");
    assert false

  type [@warning "-37"] handler_ret =
    | HError of string
    | HRet of Msgpack.t
    | HNil
  type handler = Msgpack.t list -> handler_ret

  let handle_goal (args : Msgpack.t list) : handler_ret =
    assert false

  let run_handler (rm : remote) (msgid : int) (args : Msgpack.t list) (h : handler) =
    let ret = h args in
    let (err,ret) =
      match ret with
      | HError err -> (Msgpack.String err, Msgpack.Nil)
      | HRet ret -> (Msgpack.Nil, ret)
      | HNil -> (Msgpack.Nil, Msgpack.Nil) in
    let response = Msgpack.Array [ Msgpack.Integer 1; Msgpack.Integer msgid; err; ret ] in
    let response = Msgpack.string_of_t_exn response in
    Stdio.Out_channel.output_string rm.stdin response

  let handle_message (rm : remote) (msg : Msgpack.t) =
    match msg with
    | Msgpack.Array [ Msgpack.Integer 0; Msgpack.Integer msgid; Msgpack.String mtd; Msgpack.Array params ] -> begin
      match mtd with
      | "goal" -> run_handler rm msgid params handle_goal
      | _ -> invalid_message ()
    end
    | _ -> invalid_message ()

  let run (left : PA.t Data.morphism) (right : PA.t Data.morphism) : PA.t Data.eq Proofview.tactic =
    let rm = start_remote () in
    let parser = ref (Angstrom.Buffered.parse ~initial_buffer_size:4096 Msgpack.Internal.Parser.msg) in
    let buffer = Bytes.create 1024 in
    let result : PA.t Data.eq option ref = ref None in
    while is_none !result do
      match !parser with
      | Angstrom.Buffered.Partial _ -> begin
        (* Needs more input *)
        let read = Stdio.In_channel.input rm.stdout ~buf:buffer ~pos:0 ~len:1024 in
        let read_string = Bytes.sub_string buffer 0 read in
        parser := Angstrom.Buffered.feed !parser (`String read_string);
      end
      | Angstrom.Buffered.Fail _ -> invalid_message ()
      | Angstrom.Buffered.Done (ub, msg) -> begin
        (* Make sure to keep parsing *)
        parser := Angstrom.Buffered.parse ~initial_buffer_size:4096 Msgpack.Internal.Parser.msg;
        parser := Angstrom.Buffered.feed !parser (`Bigstring (Bigarray.Array1.sub ub.buf ub.off ub.len));
        (* Handle the message *)
        handle_message rm msg
      end
    done;
    match !result with
    | Some eq -> assert false (* TODO return eq *)
    | None -> assert false (* Shouldn't happen *)
end
