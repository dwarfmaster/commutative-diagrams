
open Hyps.Combinators

let rec whileM (cond : unit -> bool Hyps.t) (body : unit -> unit Hyps.t) : unit Hyps.t =
  let* c = cond () in
  if c then
    let* _ = body () in whileM cond body
  else ret ()

type goal =
  { file: string option
  ; force: bool
  ; graph: Graph.graph
  ; lemmas: Lemmas.t list
  }
type remote = 
  { stdin: out_channel
  ; stdout: in_channel
  ; stderr: in_channel
  ; state: Protocol.state
  }

let start_remote (gl : goal) : remote Hyps.t =
  let args = List.concat [
    [ "embed" ];
    (match gl.file with
    | Some file -> [ "--state"; file ]
    | None -> [ ]);
    (if gl.force then [ "--edit" ] else []);
  ] in
  let engine_path = Sys.getenv_opt "COMDIAG_ENGINE" in
  match engine_path with
  | Some engine_path ->
      let (stdout,stdin,stderr) =
        Unix.open_process_args_full
          engine_path
          (Array.of_list ("commutative-diagrams-engine" :: args))
          (Unix.environment ()) in
      let state = Protocol.({
        goal = gl.graph;
        lemmas = Array.of_list gl.lemmas;
      }) in
      ret { stdin = stdin; stdout = stdout; stderr = stderr; state = state; }
  | None -> fail "$COMDIAG_ENGINE not found in environment"

let invalid_message (_ : unit) : 'a Hyps.t =
  fail "Incoming message ill formed"

type handler = Protocol.state -> Msgpack.t list -> Protocol.result Hyps.t

let run_handler (rm : remote) 
                (msgid : int) 
                (args : Msgpack.t list) 
                (handler : handler)
              : (bool * bool) Hyps.t =
  let open Protocol in
  let* rt = handler rm.state args in
  let finish, fail = match rt with
  | Success _ -> false, false
  | Failure _ -> true, true
  | Terminate (true,_) -> true, false
  | Terminate (false,_) -> true, true in
  let (err,rt) =
    match rt with
    | Success msg -> (Msgpack.Nil, msg)
    | Failure str -> (Msgpack.String str, Msgpack.Nil)
    | Terminate (_,msg) -> (Msgpack.Nil, msg) in
  let response = Msgpack.Array [ Msgpack.Integer 1; Msgpack.Integer msgid; err; rt ] in
  let response = Msgpack.serialize response in
  Out_channel.output_bytes rm.stdin response;
  Out_channel.flush rm.stdin;
  ret (finish, fail)

let count = ref 0

let handle_message (rm : remote) (msg : Msgpack.t) : (bool * bool) Hyps.t =
  count := !count + 1;
  match msg with
  | Msgpack.Array [ Msgpack.Integer 0
                  ; Msgpack.Integer msgid
                  ; Msgpack.String mtd
                  ; params ] ->
    let* params = match params with
      | Msgpack.Array params -> ret params
      | Msgpack.Nil -> ret []
      | _ -> fail "Params is not a vector" in
    let run = run_handler rm msgid params in
    begin match mtd with
    | "goal" -> run Protocol.goal
    | "info" -> run Protocol.info
    | "repr" -> run Protocol.repr
    | "unify" -> run Protocol.unify
    | "equalify" -> run Protocol.equalify
    | "lemmas" -> run Protocol.lemmas
    | "instantiate" -> run Protocol.instantiate
    | "pattern" -> run Protocol.pattern
    | "query" -> run Protocol.query
    | "build" -> run Protocol.build
    | "parse" -> run Protocol.parse
    | "save_state" -> run Protocol.saveState
    | "restore_state" -> run Protocol.restoreState
    | "finish" -> run Protocol.finish
    | _ -> fail ("Unknown method: " ^ mtd)
    end
  | _ -> fail "Ill formed rpc message"

let run ?(file = None) ?(force = false) gr lms : unit Hyps.t =
  let goal = 
    { file = file
    ; force = force
    ; graph = gr
    ; lemmas = lms
    } in
  let* rm = start_remote goal in
  let parser = Msgpack.make_parser rm.stdout in
  let finish = ref false in
  let failed = ref false in
  let* _ = whileM (fun () -> ret (not !finish)) (fun () ->
    match Msgpack.parse parser with
    | Msgpack.Eof ->
        (* Failed to finish *)
        finish := true;
        let lines = Seq.unfold (fun () -> In_channel.input_line rm.stderr
                                       |> Option.map (fun x -> (x,()))) () in
        let* _ = Seq.fold_left (fun m line -> m >>= (fun () -> warning line)) (ret ()) lines in
        fail "Connection interrupted"
    | Msgpack.Error err ->
        let* _ = warning err in
        fail "Error in engine"
    | Msgpack.Msg msg -> 
        let* (f,fl) = handle_message rm msg in
        finish := f;
        failed := fl;
        ret ()
    ) in
  if !failed then fail "Unsolvable goal" else ret ()
