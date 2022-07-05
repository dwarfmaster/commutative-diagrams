
module UF = UnionFind
let (let*) = Proofview.tclBIND
let ret = Proofview.tclUNIT
let (<$>) = fun f x -> Proofview.tclBIND x (fun x -> ret (f x))
let (>>=) = Proofview.tclBIND
let (++) = Pp.(++)

type face =
  { side1 : UF.path
  ; side2 : UF.path
  ; eq    : Data.eq
  }

(*  ___                                      _     _                    *)
(* |_ _|___  ___  _ __ ___   ___  _ __ _ __ | |__ (_)___ _ __ ___  ___  *)
(*  | |/ __|/ _ \| '_ ` _ \ / _ \| '__| '_ \| '_ \| / __| '_ ` _ \/ __| *)
(*  | |\__ \ (_) | | | | | | (_) | |  | |_) | | | | \__ \ | | | | \__ \ *)
(* |___|___/\___/|_| |_| |_|\___/|_|  | .__/|_| |_|_|___/_| |_| |_|___/ *)
(*                                    |_|                               *)
(* Isomorphisms *)

(* m1 -> m2 -> m3 -> m2 o m1 = 1 -> (m3 o m2) o m1 = m3 *)
let simplify_iso : Data.morphismData -> Data.morphismData -> Data.morphismData
  -> Data.eq -> Data.eq Proofview.tactic =
  fun m1 m2 m3 p ->
  (* m3 o (m2 o m1) = m3 o 1 *)
  let* p_m3 = Hyps.refl m3 in
  let* p_iso = Hyps.composeP p p_m3 in
  (* (m3 o m2) o m1 = m3 o 1 *)
  let* p_assoc = Hyps.assoc m1 m2 m3 in
  let* p_assoc = Hyps.inv p_assoc in
  let* p_iso = Hyps.concat p_assoc p_iso in
  (* (m3 o m2) o m1 = m3 *)
  let* p_id = Hyps.right_id m3 in
  Hyps.concat p_iso p_id

let rec normalize_iso : Data.morphism list -> (Data.morphism list * Data.eq) option Proofview.tactic =
  fun lst ->
  match lst with
  | [] -> ret None
  | m :: [] -> ret None
  | m1 :: m2 :: [] ->
    begin match m1.shape, m2.shape with 
    | Base m1', Base m2' ->
      begin match m1'.iso with
        | Some iso when iso.mph.id = m2'.id && iso.inv.id = m1'.id ->
          let* p_iso = Hyps.left_inv iso in
          ret (Some ([],p_iso))
        | Some iso when iso.mph.id = m1'.id && iso.inv.id = m2'.id ->
          let* p_iso = Hyps.right_inv iso in
          ret (Some ([],p_iso))
        | _ -> ret None
      end
    end
  | m1 :: m2 :: lst ->
    begin match m1.shape, m2.shape with 
    | Base m1', Base m2' ->
      begin match m1'.iso with
        | Some iso when iso.mph.id = m2'.id && iso.inv.id = m1'.id ->
          let* m3 = Hyps.realize m1.data.tp.src (Hyps.extract lst) in
          let* p_iso = Hyps.left_inv iso in
          let* p_iso = simplify_iso m1.data m2.data m3 p_iso in
          let* norm = normalize_iso lst in
          (match norm with
          | None -> ret (Some (lst,p_iso))
          | Some (lst,p) -> let* p = Hyps.concat p_iso p in ret (Some (lst,p)))
        | Some iso when iso.mph.id = m1'.id && iso.inv.id = m2'.id ->
          let* m3 = Hyps.realize m1.data.tp.src (Hyps.extract lst) in
          let* p_iso = Hyps.right_inv iso in
          let* p_iso = simplify_iso m1.data m2.data m3 p_iso in
          let* norm = normalize_iso lst in
          (match norm with
          | None -> ret (Some (lst,p_iso))
          | Some (lst,p) -> let* p = Hyps.concat p_iso p in ret (Some (lst,p)))
        | _ ->
          let* norm = normalize_iso (m2 :: lst) in
          match norm with
          | None -> ret None
          | Some ([],eq) ->
            let* p_m1 = Hyps.refl m1.data in
            let* p = Hyps.composeP p_m1 eq in
            let* p_id = Hyps.left_id m1.data in
            let* p = Hyps.concat p p_id in
            ret (Some ([m1], p))
          | Some (lst,eq) ->
            let* p_m1 = Hyps.refl m1.data in
            let* p = Hyps.composeP p_m1 eq in
            ret (Some (m1 :: lst, p))
      end
    end

let normalize_iso_in_path : Data.path -> Data.path Proofview.tactic = fun pth ->
  let* norm = normalize_iso pth.path in
  match norm with
  | None -> ret pth
  | Some (lst,p) ->
    let* p = Hyps.concat pth.eq p in
    ret { Data.mph = pth.mph
        ; eq       = p
        ; path     = lst
        }

let normalize_iso_in_face : Data.face -> Data.face Proofview.tactic = fun fce ->
  let* side1 = normalize_iso_in_path fce.side1 in
  let* side2 = normalize_iso_in_path fce.side2 in
  ret { Data.tp = fce.tp
      ; side1   = side1
      ; side2   = side2
      ; obj     = fce.obj
      ; id      = fce.id
      }

let normalize_iso_for_hook : face -> face Proofview.tactic =
  fun face ->
  let* norm1 = normalize_iso (snd face.side1) in
  let* norm2 = normalize_iso (snd face.side2) in
  let* (side1,p) =
    match norm1 with
    | None -> ret (snd face.side1, face.eq)
    | Some (side,eq) ->
      let* eq = Hyps.inv eq in
      let* p = Hyps.concat eq face.eq in
      ret (side,p) in
  let* (side2,p) =
    match norm2 with
    | None -> ret (snd face.side2, p)
    | Some (side,eq) ->
      let* p = Hyps.concat p eq in
      ret (side,p) in
  ret { side1 = (fst face.side1,side1)
      ; side2 = (fst face.side2,side2)
      ; eq = p
      }



(*  _   _             _         *)
(* | | | | ___   ___ | | _____  *)
(* | |_| |/ _ \ / _ \| |/ / __| *)
(* |  _  | (_) | (_) |   <\__ \ *)
(* |_| |_|\___/ \___/|_|\_\___/ *)
(* Hooks *)

type hook = face -> face option Proofview.tactic

(* Precompose hook *)
let precompose_hook : Data.morphismBase -> hook = fun mph fce ->
  if fce.eq.tp.src.id = mph.data.tp.dst.id
  then
    let* r = Hyps.refl mph.data in
    let* eq = Hyps.composeP r fce.eq in
    let* eq =
      match snd fce.side1, snd fce.side2 with
      | _ :: _, _ :: _ -> ret eq
      | [], _ :: _ ->
        let* p = Hyps.left_id mph.data in
        let* p = Hyps.inv p in
        Hyps.concat p eq
      | _ :: _, [] ->
        let* p = Hyps.left_id mph.data in
        Hyps.concat eq p
      | [], [] -> ret r in
    ret (Some { side1 = (mph.data.tp.src, Data.fromBase mph :: snd fce.side1)
              ; side2 = (mph.data.tp.src, Data.fromBase mph :: snd fce.side2)
              ; eq    = eq
              })
  else ret None

(* Postcompose hook *)
let rec push_equality : Data.morphism list -> Data.morphism -> Data.eq Proofview.tactic = fun ms m ->
  match ms with
  | [ ] -> Hyps.refl m.data
  | [ m1 ] -> Hyps.compose m1.data m.data >>= Hyps.refl
  | m1 :: ms ->
    let* r = Hyps.refl m1.data in
    let* pe = push_equality ms m in
    let* pe = Hyps.composeP r pe in
    let* ms = Hyps.realize m1.data.tp.dst (Hyps.extract ms) in
    let* p  = Hyps.assoc m1.data ms m.data in
    Hyps.concat p pe

let postcompose_hook : Data.morphismBase -> hook = fun mph fce ->
  let mph = Data.fromBase mph in
  if fce.eq.tp.dst.id = mph.data.tp.src.id
  then
    let* r = Hyps.refl mph.data in
    let* eq = Hyps.composeP fce.eq r in
    let* rnorm_l = push_equality (snd fce.side1) mph in
    let* rnorm_l = Hyps.inv rnorm_l in
    let* rnorm_r = push_equality (snd fce.side2) mph in
    let* eq = Hyps.concat rnorm_l eq in
    let* eq = Hyps.concat eq rnorm_r in
    let* eq =
      match snd fce.side1, snd fce.side2 with
      | _ :: _, _ :: _ -> ret eq
      | [], _ :: _ ->
        let* p = Hyps.right_id mph.data in
        let* p = Hyps.inv p in
        Hyps.concat p eq
      | _ :: _, [] ->
        let* p = Hyps.right_id mph.data in
        Hyps.concat eq p
      | [], [] -> ret r in
    ret (Some { side1 = (fst fce.side1, List.append (snd fce.side1) [ mph ])
              ; side2 = (fst fce.side2, List.append (snd fce.side2) [ mph ])
              ; eq    = eq
              })
  else ret None

(* Monomorphism hook *)
(* If the last element matches the predicate, return the list without it *)
let rec lastrmP : ('a -> bool) -> 'a list -> 'a list option = fun pred -> function
  | [] -> None
  | [ x ] -> if pred x then Some [] else None
  | x :: l -> match lastrmP pred l with
    | Some l -> Some (x :: l)
    | None -> None

let monomorphism_hook : Data.morphismBase -> hook = fun mono fce ->
  match mono.mono with
  | None -> ret None
  | Some h when fce.eq.tp.dst.id = mono.data.tp.dst.id
             && (fst fce.side1).id = (fst fce.side2).id ->
    let pred = fun (m : Data.morphism) -> 
      match m.shape with 
      | Base m -> m.id = mono.id
    in
    begin match lastrmP pred (snd fce.side1), lastrmP pred (snd fce.side2) with
      | Some pth1, Some pth2 ->
        let src = fst fce.side1 in
        let* m1 = Hyps.realize src (Hyps.extract pth1) in
        let* m2 = Hyps.realize src (Hyps.extract pth2) in
        let eq  = Hyps.mono_eq h m1 m2 fce.eq in
        ret (Some { side1 = (fst fce.side1, pth1)
                  ; side2 = (fst fce.side2, pth2)
                  ; eq    =
                      { eq = eq
                      ; src = m1
                      ; dst = m2
                      ; tp = m1.tp
                      }
                  })
      | _, _ -> ret None
    end
  | _ -> ret None

(* Epimorphism hook *)
let rec path_dst_id : Data.elem * (Data.morphism list) -> Data.elem = fun (x,l) ->
  match l with
  | [] -> x
  | [ x ] -> x.data.tp.dst
  | _ :: xs -> path_dst_id (x,xs)

let epimorphism_hook : Data.morphismBase -> hook = fun epi fce ->
  match epi.epi with
  | None -> ret None
  | Some h when fce.eq.tp.src.id = epi.data.tp.src.id
             && path_dst_id fce.side1 = path_dst_id fce.side2 ->
    begin match snd fce.side1, snd fce.side2 with
      | epi1 :: pth1, epi2 :: pth2 ->
        begin match epi1.shape, epi2.shape with 
          | Base epi1, Base epi2 when epi1.id = epi.id && epi2.id = epi.id ->
            let src = epi.data.tp.dst in
            let* m1 = Hyps.realize src (Hyps.extract pth1) in
            let* m2 = Hyps.realize src (Hyps.extract pth2) in
            let eq = Hyps.epi_eq h m1 m2 fce.eq in
            ret (Some { side1 = (src, pth1)
                      ; side2 = (src, pth2)
                      ; eq    =
                          { eq = eq
                          ; src = m1
                          ; dst = m2
                          ; tp = m1.tp
                          }
                      })
          | _, _ -> ret None
        end
      | _, _ -> ret None
    end
  | _ -> ret None

(*   ____                                _        _   _              *)
(*  / ___|___  _ __ ___  _ __ ___  _   _| |_ __ _| |_(_) ___  _ __   *)
(* | |   / _ \| '_ ` _ \| '_ ` _ \| | | | __/ _` | __| |/ _ \| '_ \  *)
(* | |__| (_) | | | | | | | | | | | |_| | || (_| | |_| | (_) | | | | *)
(*  \____\___/|_| |_| |_|_| |_| |_|\__,_|\__\__,_|\__|_|\___/|_| |_| *)

type t =
  { union : UF.t
  ; paths : Data.path list
  ; hyps  : Hyps.t
  }

type buildData =
  { union : UF.t
  ; hooks : hook list
  ; level : int
  }

let query = fun p1 p2 (store : t) ->
  let* p1 = normalize_iso_in_path p1 in
  let* p2 = normalize_iso_in_path p2 in
  let* r = UF.query_conn (UF.extract p1) (UF.extract p2) store.union in
  match r with
  | None -> ret None
  | Some eq ->
    let* p = Hyps.concat p1.eq eq in
    let* p2 = Hyps.inv p2.eq in
    let* p = Hyps.concat p p2 in
    ret (Some p)

let singlePath : Data.morphismBase -> Data.path Proofview.tactic = fun m ->
  let* r = Hyps.refl m.data in
  ret { Data.mph = m.data
      ; eq       = r
      ; path     = [ Data.fromBase m ]
      }
let precomposePath : Data.morphismBase -> Data.path -> Data.path option Proofview.tactic = fun mph path ->
  match path.path with
  | [] -> let* s = singlePath mph in ret (Some s)
  | m :: _ ->
    match m.shape with 
    | Base m ->
      match mph.iso with
      | Some iso when (iso.mph.id = mph.id && iso.inv.id = m.id)
                   || (iso.mph.id = m.id   && iso.inv.id = mph.id) -> ret None
      | _ ->
        let* c  = Hyps.compose mph.data path.mph in
        let* r  = Hyps.refl mph.data in
        let* eq = Hyps.composeP r path.eq in
        ret (Some { Data.mph = c; eq = eq; path = Data.fromBase mph :: path.path })

module Array = struct
  include Array
  let rec forM' : 'a array -> int -> ('a -> unit Proofview.tactic) -> unit Proofview.tactic = fun arr i body ->
    if i >= length arr then ret ()
    else
      let* _ = body arr.(i) in
      forM' arr (i + 1) body
  let forM : 'a array -> ('a -> unit Proofview.tactic) -> unit Proofview.tactic = fun arr body -> forM' arr 0 body
end

module List = struct
  include List
  let rec forM : 'a list -> ('a -> unit Proofview.tactic) -> unit Proofview.tactic = fun lst body ->
    match lst with
    | [] -> ret ()
    | x :: lst ->
      let* _ = body x in
      forM lst body
end

(* last is included *)
let forEnum : int -> int -> (int -> unit Proofview.tactic) -> unit Proofview.tactic = fun first last body ->
  List.forM (List.init (last - first + 1) (fun n -> n + first)) body

(* All paths, sorted by the index of their starting element and size *)
type pathEnumeration = Data.path list array
let idPath : Data.elem -> Data.path Proofview.tactic = fun e ->
  let* mph = Hyps.realize e [] in
  let* r = Hyps.refl mph in
  ret { Data.mph = mph
      ; eq       = r
      ; path     = [ ]
      }

let enumerateAllPaths : Hyps.t -> int -> pathEnumeration Proofview.tactic = fun store level ->
  let lvlC = Array.length store.elems in
  let res = Array.make (lvlC * (level + 1)) [] in
  let* _ = Array.forM store.elems begin fun elem ->
      let* p = idPath elem in
      res.(elem.id) <- [ p ];
      ret ()
    end in
  let* sigma = Proofview.tclEVARMAP in
  let* env = Proofview.tclENV in
  let* _ =
    forEnum 0 (level-1) begin fun lvl ->
      Array.forM store.morphisms begin fun mph ->
        let s = mph.data.tp.src.id in
        let d = mph.data.tp.dst.id in
        List.forM res.(d + lvl*lvlC) begin fun pth ->
          let* pth = precomposePath mph pth in
          (match pth with
           | Some pth ->
             res.(s + (lvl+1)*lvlC) <- pth :: res.(s + (lvl+1)*lvlC);
           | None -> ());
          ret ()
        end
      end
    end in
  ret res
let mergePaths : pathEnumeration -> Data.path list = fun enum ->
  Array.fold_right (fun lst paths -> List.append lst paths) enum []

let rec processHooks : buildData -> face -> unit Proofview.tactic = fun data face ->
  List.forM data.hooks begin fun hook ->
    let* res = hook face in
    match res with
    | None -> ret ()
    | Some fce -> addFace data fce
  end
and addFace : buildData -> face -> unit Proofview.tactic = fun data face ->
  let* face = normalize_iso_for_hook face in
  if List.length (snd face.side1) >= data.level || List.length (snd face.side2) >= data.level
  then ret ()
  else
    let* added = UF.connect face.side1 face.side2 face.eq data.union in
    if added then processHooks data face else ret ()

let addEq : buildData -> Data.face -> unit Proofview.tactic = fun data face ->
  let* p1 = Hyps.inv face.side1.eq in
  let* p  = Hyps.concat p1 face.obj in
  let* p  = Hyps.concat p  face.side2.eq in
  addFace data
    { side1 = UF.extract face.side1; side2 = UF.extract face.side2; eq = p }

let build = fun hyps level ->
  let* paths = mergePaths <$> enumerateAllPaths hyps level in
  let* union = UF.init (List.map UF.extract paths) in
  let hooks = List.concat
    [ List.map precompose_hook   (Array.to_list hyps.morphisms)
    ; List.map postcompose_hook  (Array.to_list hyps.morphisms)
    ; List.map monomorphism_hook (Array.to_list hyps.morphisms)
    ; List.map epimorphism_hook  (Array.to_list hyps.morphisms)
    ] in
  let data = { union = union; hooks = hooks; level = level } in
  Proofview.tclTHEN
    (Array.forM hyps.faces (addEq data))
    (ret { union = union
         ; paths = paths
         ; hyps  = hyps
         })
