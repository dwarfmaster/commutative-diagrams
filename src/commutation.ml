
module UF = UnionFind
type t =
  { union : UF.t
  ; paths : Hyps.path list
  ; hyps  : Hyps.t
  }

let (let*) = Proofview.tclBIND
let ret = Proofview.tclUNIT
let (<$>) = fun f x -> Proofview.tclBIND x (fun x -> ret (f x))

let query = fun p1 p2 store ->
  let* r = UF.query_conn (UF.extract p1) (UF.extract p2) store.union in
  match r with
  | None -> ret None
  | Some eq ->
    let* p = Hyps.concat p1.eq eq in
    let* p2 = Hyps.inv p2.eq in
    let* p = Hyps.concat p p2 in
    ret (Some p)

let precomposePath : Hyps.morphism -> Hyps.path -> Hyps.path Proofview.tactic = fun mph path ->
  let* c  = Hyps.compose mph.data path.mph in
  let* r  = Hyps.refl mph.data in
  let* eq = Hyps.composeP r path.eq in
  ret { Hyps.mph = c; eq = eq; path = mph :: path.path }

let forM : 'a array -> ('a -> unit Proofview.tactic) -> unit Proofview.tactic = fun arr body ->
  Array.fold_left (fun m x -> Proofview.tclTHEN m (body x)) (Proofview.tclUNIT ()) arr
let forM' : 'a list -> ('a -> unit Proofview.tactic) -> unit Proofview.tactic = fun lst body ->
  List.fold_left (fun m x -> Proofview.tclTHEN m (body x)) (Proofview.tclUNIT ()) lst

(* All paths, sorted by the index of their starting element *)
type pathEnumeration = Hyps.path list array
let singlePath : Hyps.morphism -> Hyps.path Proofview.tactic = fun m ->
  let* r = Hyps.refl m.data in
  ret { Hyps.mph = m.data
      ; eq       = r
      ; path     = [ m ]
      }
let rec enumerateAllPaths : Hyps.t -> int -> pathEnumeration Proofview.tactic = fun store level ->
  if level <= 1 then begin
    let res = Array.make (Array.length store.elems) [] in
    Proofview.tclTHEN
      (forM store.morphisms begin fun mph ->
          let* p = singlePath mph in
          res.(mph.data.tp.src.id) <- p :: res.(mph.data.tp.src.id);
          ret ()
        end)
      (ret res)
  end else begin
    let* sub = enumerateAllPaths store (level - 1) in
    let res = Array.init (Array.length sub) (fun i -> sub.(i)) in
    Proofview.tclTHEN
      (forM store.morphisms begin fun mph ->
          forM' sub.(mph.data.tp.dst.id) begin fun pth ->
            let* pth = precomposePath mph pth in
            res.(mph.data.tp.src.id) <- pth :: res.(mph.data.tp.src.id);
            ret ()
          end
        end)
      (ret res)
  end
let mergePaths : pathEnumeration -> Hyps.path list = fun enum ->
  Array.fold_right (fun lst paths -> List.append lst paths) enum []

let addEq : UF.t -> Hyps.face -> unit Proofview.tactic = fun union face ->
  let* p1 = Hyps.inv face.side1.eq in
  let* p  = Hyps.concat p1 face.obj in
  let* p  = Hyps.concat p  face.side2.eq in
  UF.connect (UF.extract face.side1) (UF.extract face.side2) p union

let build = fun hyps level ->
  let* paths = mergePaths <$> enumerateAllPaths hyps level in
  let* union = UF.init (List.map UF.extract paths) in
  Proofview.tclTHEN
    (forM hyps.faces (addEq union))
    (ret { union = union
         ; paths = paths
         ; hyps  = hyps
         })
