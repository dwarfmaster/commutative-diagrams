
let (++) = Pp.(++)

type ckind = (Constr.t,Constr.types,Sorts.t,Univ.Instance.t) Constr.kind_of_term

exception Inductive_not_found
let locate_inductive : string -> Names.inductive = fun name ->
  match Nametab.global (Libnames.qualid_of_string name) with
  | IndRef i -> i
  | _ -> raise Inductive_not_found
(* exception Constructor_not_found *)
(* let locate_constructor : string -> Names.constructor = fun name -> *)
(*   match Nametab.global (Libnames.qualid_of_string name) with *)
(*   | ConstructRef c -> c *)
(*   | _ -> raise Constructor_not_found *)
(* exception Constant_not_found *)
(* let locate_constant : string -> Names.Constant.t = fun name -> *)
(*   match Nametab.global (Libnames.qualid_of_string name) with *)
(*   | ConstRef c -> c *)
(*   | _ -> raise Constant_not_found *)


let is_category : Evd.evar_map -> Environ.env -> ckind -> bool =
  fun sigma env c ->
  let coq_cat = locate_inductive "HoTT.Categories.Category.Core.PreCategory" in
  match c with
  | Ind (name,_) -> Names.Ind.UserOrd.equal name coq_cat
  | _ -> false

type c_object = { category : Constr.t }

let is_object : Evd.evar_map -> Environ.env -> ckind -> c_object option =
  fun sigma env o ->
  let coq_cat = locate_inductive "HoTT.Categories.Category.Core.PreCategory" in
  let lbl = Names.Label.make "object" in
  match o with
  | Proj (p,arg) ->
    let r = Names.Projection.repr p in
    let ind = Names.Projection.Repr.inductive r in
    let p_lbl = Names.Projection.Repr.label r in
    if Names.Ind.UserOrd.equal ind coq_cat && Names.Label.equal lbl p_lbl
    then Some { category = arg }
    else None
  | _ -> None

(* type c_morphism = *)
(*   { category : Constr.t *)
(*   ; src      : Constr.t *)
(*   ; dst      : Constr.t *)
(*   } *)

(* let is_morphism : Evd.evar_map -> Environ.env -> ckind -> c_morphism option = *)
(*   fun sigma env m -> None *)

(* type c_face = *)
(*   { category : Constr.t *)
(*   ; src      : Constr.t *)
(*   ; dst      : Constr.t *)
(*   ; side1    : Constr.t *)
(*   ; side2    : Constr.t *)
(*   } *)

(* let is_face : Evd.evar_map -> Environ.env -> ckind -> c_face option = *)
(*   fun sigma env f -> None *)

let print_hyp : Evd.evar_map -> Environ.env -> Constr.named_declaration -> Pp.t = fun sigma env dec ->
  let name,tp = match dec with
    | Context.Named.Declaration.LocalAssum (name,tp) -> (name.binder_name, tp)
    | Context.Named.Declaration.LocalDef (name,_,tp) -> (name.binder_name, tp) in
  let ck : ckind = Constr.kind tp in
  let is_cat = if is_category sigma env ck then Pp.str "category " else Pp.str "" in
  let is_obj = match is_object sigma env ck with
    | None -> Pp.str ""
    | Some obj -> Pp.str "object(" ++ Printer.pr_constr_env env sigma obj.category ++ Pp.str ") " in
  Names.Id.print name ++ Pp.str " : " ++ Printer.pr_constr_env env sigma tp
    ++ Pp.str " [ " ++ is_cat ++ is_obj ++ Pp.str "]"

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
  let pp = Pp.str "Goal: " ++ Pp.int (List.length data.goals) ++ Pp.fnl () in
  let pp = pp ++ Pp.pr_vertical_list (fun goal -> begin
        let pp = Pp.str "Focusing goal: " ++ Pp.int !goal_id ++ Pp.fnl () in
        goal_id := !goal_id + 1;
        pp ++ extract_goal oc sigma env goal
    end) data.goals in
  (* TODO: doesn't work *)
  (* Pp.pp_with (Stdlib.Format.formatter_of_out_channel oc) (Pp.strbrk "test"); *)
  Printf.fprintf oc "%s\n" (Pp.string_of_ppcmds pp);
  flush oc;
  close_out oc
