
type t = EConstr.t

let (let*) = Proofview.tclBIND
let ret = Proofview.tclUNIT



(*   ____      _                        _ *)
(*  / ___|__ _| |_ ___  __ _  ___  _ __(_) ___  ___ *)
(* | |   / _` | __/ _ \/ _` |/ _ \| '__| |/ _ \/ __| *)
(* | |__| (_| | ||  __/ (_| | (_) | |  | |  __/\__ \ *)
(*  \____\__,_|\__\___|\__, |\___/|_|  |_|\___||___/ *)
(*                     |___/ *)
(* Categories *)

let is_cat : Environ.env -> t -> bool Proofview.tactic = fun env cat ->
  let* sigma = Proofview.tclEVARMAP in
  ret (match EConstr.kind sigma cat with
      | Ind (ind,_) -> Env.is_cat ind
      | _ -> false)

let is_object : Environ.env -> t -> t option Proofview.tactic = fun env obj ->
  let* sigma = Proofview.tclEVARMAP in
  ret (match EConstr.kind sigma obj with
      | Proj (p,obj) when Env.is_projection p Env.is_cat "object" -> Some obj
      | _ -> None)

let morphism : Environ.env -> t -> t -> t -> t Proofview.tactic = fun env cat src dst ->
  Env.app (Env.mk_mphT ()) [| cat; src; dst |]

let is_morphism : Environ.env -> t -> (t*t*t) option Proofview.tactic = fun env mph ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mph with
  | App (p, [| src; dst |]) ->
    begin match EConstr.kind sigma p with
      | Proj (p,cat) when Env.is_projection p Env.is_cat "morphism" -> ret (Some (cat,src,dst))
      | _ -> ret None
    end
  | _ -> ret None

let compose : Environ.env -> t -> t -> t -> t -> t -> t -> t Proofview.tactic =
  fun env cat src mid dst m1 m2->
  Env.app (Env.mk_comp ())
    [| cat; src; mid; dst; m1; m2 |]

let parse_compose : Environ.env -> t -> (t*t*t*t*t*t) option Proofview.tactic = fun env mph ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mph with
  | App (cmp, [| src; int; dst; mid; msi |]) ->
    begin match EConstr.kind sigma cmp with
      | Proj (cmp,cat) when Env.is_projection cmp Env.is_cat "compose" ->
        ret (Some (cat,src,int,dst,msi,mid))
      | _ -> ret None
    end
  | _ -> ret None

let identity : Environ.env -> t -> t -> t Proofview.tactic = fun env cat x ->
  Env.app (Env.mk_id ()) [| cat; x |]

let parse_identity : Environ.env -> t -> (t*t) option Proofview.tactic = fun env id ->
  let* sigma = Proofview.tclEVARMAP in
  ret (match EConstr.kind sigma id with
      | App (id, [| cat; elem |]) ->
        begin match EConstr.kind sigma id with
          | Const (name,_) when Env.is_id name -> Some (cat,elem)
          | _ -> None
        end
      | App (id, [| elem |]) ->
        begin match EConstr.kind sigma id with
          | Proj (id,cat) when Env.is_projection id Env.is_cat "identity" ->
            Some (cat,elem)
          | _ -> None
        end
      | _ -> None)


let is_mono : Environ.env -> t -> (t*t*t*t) option Proofview.tactic = fun env mono ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma mono with
  | App (mono, [| cat; src; dst; mph |]) ->
    begin match EConstr.kind sigma mono with
      | Const (mono,_) when Env.is_mono mono -> ret (Some (cat,src,dst,mph))
      | _ -> ret None
    end
  | _ -> ret None

let is_epi  : Environ.env -> t -> (t*t*t*t) option Proofview.tactic = fun env epi ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma epi with
  | App (epi, [| cat; src; dst; mph |]) ->
    begin match EConstr.kind sigma epi with
      | Const (epi,_) when Env.is_epi epi -> ret (Some (cat,src,dst,mph))
      | _ -> ret None
    end
  | _ -> ret None

let is_iso  : Environ.env -> t -> (t*t*t*t) option Proofview.tactic = fun env iso ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma iso with
  | App (iso, [| cat; src; dst; mph |]) ->
    begin match EConstr.kind sigma iso with
      | Ind (iso,_) when Env.is_iso iso -> ret (Some (cat,src,dst,mph))
      | _ -> ret None
    end
  | _ -> ret None

let inverse : Environ.env -> t -> t -> t -> t -> t -> t Proofview.tactic = fun env cat src dst mph iso ->
  Env.app (Env.mk_inv_mph ()) [| cat; src; dst; mph; iso |]


(*  ____       _   _          *)
(* |  _ \ __ _| |_| |__  ___  *)
(* | |_) / _` | __| '_ \/ __| *)
(* |  __/ (_| | |_| | | \__ \ *)
(* |_|   \__,_|\__|_| |_|___/ *)
(* Paths *)

let eq : Environ.env -> t -> t -> t -> t Proofview.tactic = fun env tp a b ->
  Env.app (Env.mk_eq ()) [| tp; a; b |]

let is_eq : Environ.env -> t -> (t*t*t) option Proofview.tactic = fun env eq ->
  let* sigma = Proofview.tclEVARMAP in
  match EConstr.kind sigma eq with
  | App (eq, [| tp; a; b |]) ->
    begin match EConstr.kind sigma eq with
      | Ind (eq,_) when Env.is_eq eq -> ret (Some (tp,a,b))
      | _ -> ret None
    end
  | _ -> ret None
