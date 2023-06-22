
module Make(O: Map.OrderedType) = struct
  module Registerer(Ord : Map.OrderedType) = struct
    module M = Map.Make(Ord)
    type t =
      { values: Ord.t list
      ; length: int
      ; reverse: int M.t
      }
  
    let empty () =
      { values = []
      ; length = 0
      ; reverse = M.empty
      }
  
    let to_list (reg: t) : Ord.t list =
      List.rev reg.values
  
    let insert (x : Ord.t) (reg : t) : int * t =
      let nreg =
        { values = x :: reg.values
        ; length = reg.length + 1
        ; reverse = M.add x reg.length reg.reverse
        } in
      reg.length, nreg
  
    let get (x : Ord.t) (reg: t) : int option =
      M.find_opt x reg.reverse
  
    let find (x : Ord.t) (reg: t) : int =
      M.find x reg.reverse
  
    let may_insert (x : Ord.t) (reg : t) : int * t =
      match get x reg with
      | Some i -> i, reg
      | None -> insert x reg
  
    let debug_print (fmt: Ord.t -> Pp.t) (reg: t) : Pp.t =
      let rec pv = function
        | [] -> Pp.str ""
        | [x] -> fmt x
        | x :: xs -> Pp.(fmt x ++ str "::" ++ pv xs) in
      pv reg.values
  end
  
  let cmp2 x y = if x = 0 then y else x
  module EqObj = struct
    type t = O.t * O.t
    let compare (o1,c1) (o2,c2) = cmp2 (O.compare o1 o2) (O.compare c1 c2)
  end
  module RegNode = Registerer(EqObj)
  module EqEdge = struct
    type t = int * int * O.t * O.t
    let compare (src1,dst1,cat1,mph1) (src2,dst2,cat2,mph2) =
      cmp2 (cmp2 (O.compare mph1 mph2) (O.compare cat1 cat2))
           (List.compare Int.compare [src1;dst1] [src2;dst2])
  end
  module RegEdge = Registerer(EqEdge)
  module EqEq = struct
    type t = int * int * int list * int list * O.t * O.t * bool
    let compare (src1,dst1,left1,right1,cat1,eq1,_) (src2,dst2,left2,right2,cat2,eq2,_) =
      cmp2 (cmp2 (O.compare eq1 eq2) (O.compare cat1 cat2))
           (List.compare
             (List.compare Int.compare)
             [ [src1;dst1]; left1; right1 ]
             [ [src2;dst2]; left2; right2 ])
  end
  module RegEq = Registerer(EqEq)
  type t =
    { elems : RegNode.t
    ; mphs : RegEdge.t
    ; eqs : RegEq.t
    }
  
  let debug_print (env: Environ.env) (sigma: Evd.evar_map) printer (bld: t) : Pp.t =
    let elems = RegNode.debug_print (fun (_,o) -> printer o) bld.elems in
    let mphs = RegEdge.debug_print (fun (_,_,_,mph) -> printer mph) bld.mphs in
    let eqs = RegEq.debug_print (fun (_,_,_,_,_,eq,_) -> printer eq) bld.eqs in
    Pp.(str "elems: " ++ elems ++ str "," ++ cut ()
      ++ str "mphs: " ++ mphs ++ str "," ++ cut ()
      ++ str "eqs: " ++ eqs)
  
  let empty () =
    { elems = RegNode.empty ()
    ; mphs = RegEdge.empty ()
    ; eqs = RegEq.empty ()
    }
  
  let add_node (cat: O.t) (obj: O.t) (builder: t) : int * t =
    let i, nelems = RegNode.may_insert (cat,obj) builder.elems in
    i, { builder with elems = nelems }
  let add_edge (cat: O.t) (src: O.t) (dst: O.t) (mph: O.t) (builder: t) : int * t =
    let src, builder = add_node cat src builder in
    let dst, builder = add_node cat dst builder in
    let mph, nmphs = RegEdge.may_insert (src,dst,cat,mph) builder.mphs in
    mph, { builder with mphs = nmphs }
  let add_face ?(important = false) cat src dst left right eq (builder: t) =
    let flp (a,b) = (b,a) in
    let (src, builder) = add_node cat src builder in
    let (dst, builder) = add_node cat dst builder in
    let (builder,left) = 
      List.fold_left_map
        (fun bld (src,dst,mph) -> add_edge cat src dst mph bld |> flp) 
        builder left in
    let (builder,right) = 
      List.fold_left_map
        (fun bld (src,dst,mph) -> add_edge cat src dst mph bld |> flp)
        builder right in
    let _, neqs = RegEq.may_insert (src,dst,left,right,cat,eq,important) builder.eqs in
    { builder with eqs = neqs }
  
  let rec find_in_list (lst: (int * 'a) list) (x : 'a) (n: int) : int * int =
    match lst with
    | [] -> assert false
    | (i,y) :: l when x = y -> (i,n)
    | _ :: l -> find_in_list l x (n+1)
  
  exception ImportantFailed
  let build_face mphs (src,dst,left,right,_,eq,important) : O.t Graph.face option =
    let open Graph in
    let rec last_is pred = function
    | [] -> true
    | [x] -> pred x
    | _ :: tl -> last_is pred tl in
    let check_left = match left with
    | [] -> true
    | mph :: _ -> (List.nth mphs mph).mph_src = src
               && last_is (fun mph -> (List.nth mphs mph).mph_dst = dst) left in
    let check_right = match right with
    | [] -> true
    | mph :: _ -> (List.nth mphs mph).mph_src = src 
               && last_is (fun mph -> (List.nth mphs mph).mph_dst = dst) right in
    if check_left && check_right
    then Some { face_src = src
              ; face_dst = dst
              ; face_left = left
              ; face_right = right
              ; face_eq = eq
              }
    else if important then raise ImportantFailed else None

  let import obj tp mk builder =
    let open Hyps.Combinators in
    let* mtdt = Hyps.getObjMtdt tp in
    let* builder = match mtdt.is_elem with
    | Some cat -> 
        let* cat = mk cat in
        add_node cat obj builder |> snd |> ret
    | None -> ret builder in
    let* builder = match mtdt.is_mph with
    | Some (cat,src,dst) ->
        let* cat = mk cat in
        let* src = mk src in
        let* dst = mk dst in
        add_edge cat src dst obj builder |> snd |> ret
    | None -> ret builder in
    let* builder = match mtdt.is_eq with
    | Some (cat,src,dst,left,right) ->
        let* cat = mk cat in
        let* src = mk src in
        let* dst = mk dst in
        let* left = mk left in
        let* right = mk right in
        (* TODO split left and right *)
        add_face cat src dst [(src,dst,left)] [(src,dst,right)] obj builder |> ret
    | None -> ret builder in
    ret builder
  
  let build_morphism (src,dst,_,mph) : O.t Graph.morphism =
    let open Graph in
    { mph_src = src
    ; mph_dst = dst
    ; mph_mph = mph
    }

  let build_object (cat,obj) : O.t Graph.obj =
    let open Graph in
    { obj_obj = obj
    ; obj_cat = cat
    }
  
  let build (builder: t) =
    try
      let nodes = RegNode.to_list builder.elems |> List.map build_object in
      let mphs = RegEdge.to_list builder.mphs |> List.map build_morphism in
      let faces = List.filter_map (build_face mphs) builder.eqs.values in
      let open Graph in
      Some { gr_nodes = nodes
           ; gr_edges = mphs
           ; gr_faces = faces
           }
    with
      _ -> None
  let build_unsafe (builder : t) =
    match build builder with
    | Some gr -> gr
    | None -> assert false
end

module MInt = Make(struct type t = int let compare = Int.compare end)
type t = MInt.t
let empty = MInt.empty
let add_node = MInt.add_node
let add_edge = MInt.add_edge
let add_face = MInt.add_face
let import obj tp bld = 
  MInt.import
    obj 
    Hyps.({ id = tp; namespace = 0; })
    (fun obj -> Hyps.Combinators.ret obj.Hyps.id) 
    bld
let build = MInt.build
let build_unsafe = MInt.build_unsafe
let debug_print env sigma bld = MInt.debug_print env sigma Pp.int bld
