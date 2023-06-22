
open Hyps.Combinators
type 't m = 't Hyps.t

module M = Map.Make(String)

type 'a obj =
  { obj_obj: 'a
  ; obj_cat: 'a
  }
type 'a face =
  { face_src: int
  ; face_dst: int
  ; face_left: int list
  ; face_right: int list
  ; face_eq: 'a
  }
type 'a morphism =
  { mph_src: int
  ; mph_dst: int
  ; mph_mph: 'a
  }
type 'a graph_impl =
  { gr_nodes: 'a obj list
  ; gr_edges: 'a morphism list
  ; gr_faces: 'a face list
  }
type graph = int graph_impl

let rec mapOptM (f : 'a -> 'b option m) (l : 'a list) : 'b list option m =
  match l with
  | [] -> ret (Some [])
  | x :: l ->
      let* x = f x in
      let* l = mapOptM f l in begin
        match x, l with
        | Some x, Some l -> ret (Some (x :: l))
        | _ -> ret None
      end

let pack_obj obj =
  ret (Msgpack.Array [
    Msgpack.Integer obj.obj_obj;
    Msgpack.Integer obj.obj_cat;
  ])

let unpack_obj msg =
  let open Msgpack in
  match msg with
  | Array [ Integer obj; Integer cat ] ->
      some { obj_obj = obj; obj_cat = cat; }
  | _ -> none ()

let pack_edge mph =
  ret (Msgpack.Array [
    Msgpack.Integer mph.mph_src;
    Msgpack.Integer mph.mph_dst;
    Msgpack.Integer mph.mph_mph;
  ])

let unpack_edge msg =
  let open Msgpack in
  match msg with
  | Array [ Integer src; Integer dst; Integer mph ] ->
      some { mph_src = src; mph_dst = dst; mph_mph = mph; }
  | _ -> none ()

let pack_face fce =
  let open Msgpack in
  ret (Array [
    Integer fce.face_src;
    Integer fce.face_dst;
    Array (List.map (fun i -> Integer i) fce.face_left);
    Array (List.map (fun i -> Integer i) fce.face_right);
    Integer fce.face_eq;
  ])

let unpack_int msg =
  match msg with
  | Msgpack.Integer i -> ret (Some i)
  | _ -> ret None

let unpack_face msg =
  let open Msgpack in
  match msg with
  | Array [ Integer src; Integer dst; Array left; Array right; Integer eq ] -> begin
    let* left = mapOptM unpack_int left in
    let* right = mapOptM unpack_int right in
    match left, right with
    | Some left, Some right -> some { 
        face_src = src; 
        face_dst = dst; 
        face_left = left; 
        face_right = right; 
        face_eq = eq;
      }
    | _ -> none ()
  end
  | _ -> none ()

module Serde = struct
  type t = graph

  let pack gr =
    let* nodes = mapM pack_obj gr.gr_nodes in
    let* edges = mapM pack_edge gr.gr_edges in
    let* faces = mapM pack_face gr.gr_faces in
    ret (Msgpack.Array [
      Msgpack.Array nodes;
      Msgpack.Array edges;
      Msgpack.Array faces;
    ])

  let unpack msg = 
    let open Msgpack in
    match msg with
    | Array [ Array nodes; Array edges; Array faces ] -> begin
      let* nodes = mapOptM unpack_obj nodes in
      let* edges = mapOptM unpack_edge edges in
      let* faces = mapOptM unpack_face faces in
      match nodes, edges, faces with
      | Some nodes, Some edges, Some faces -> some {
        gr_nodes = nodes;
        gr_edges = edges;
        gr_faces = faces;
      }
      | _ -> none ()
    end
    | _ -> none ()
end

let mapM f gr =
  let open Hyps.Combinators in
  let fobj obj =
    let* o = f obj.obj_obj in
    let* cat = f obj.obj_cat in
    ret { obj_obj = o; obj_cat = cat; } in
  let* nodes = mapM fobj gr.gr_nodes in
  let fmph mph =
    let* m = f mph.mph_mph in
    ret { mph with mph_mph = m } in
  let* edges = mapM fmph gr.gr_edges in
  let fface fce =
    let* eq = f fce.face_eq in
    ret { fce with face_eq = eq } in
  let* faces = mapM fface gr.gr_faces in
  ret {
    gr_nodes = nodes;
    gr_edges = edges;
    gr_faces = faces;
  }
