
let (++) = Pp.(++)

let print_hyp : Evd.evar_map -> Environ.env -> Constr.named_declaration -> Pp.t = fun sigma env dec ->
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  Names.Id.print name ++ Pp.str " : " ++ Printer.pr_constr_env env sigma tp

let extract_goal : out_channel -> Evd.evar_map -> Environ.env -> Evar.t -> Pp.t = fun oc sigma env goal ->
  let info = Evd.find sigma goal in
  let pp = Pp.str "Conclusion: " ++ Printer.pr_econstr_env env sigma info.evar_concl ++ Pp.fnl () in
  let context = Environ.named_context_of_val info.evar_hyps in
  let pp = pp ++ Pp.pr_vertical_list (print_hyp sigma env) context in
  pp

let extract : Proof.t -> string -> unit = fun state path ->
  let oc = open_out path in
  let data = Proof.data state in
  let sigma, env = Proof.get_proof_context state in
  let goal_id = ref 0 in
  let pp = Pp.str "Goal:" ++ Pp.spc () ++ Pp.int (List.length data.goals) ++ Pp.fnl () in
  let pp = pp ++ Pp.pr_vertical_list (fun goal -> begin
        let pp = Pp.str "Focusing goal:" ++ Pp.spc () ++ Pp.int !goal_id ++ Pp.fnl () in
        goal_id := !goal_id + 1;
        pp ++ extract_goal oc sigma env goal
    end) data.goals in
  (* TODO: doesn't work *)
  (* Pp.pp_with (Stdlib.Format.formatter_of_out_channel oc) (Pp.strbrk "test"); *)
  Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds pp);
  flush oc;
  close_out oc
