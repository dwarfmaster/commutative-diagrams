
module Make(PA : Pa.ProofAssistant) = struct
  module St = Store.Make(PA.M)
  open St.Combinators
  type 't m = ('t,PA.t) St.t

  module M = Map.Make(String)

  type faces =
    { face_src: int
    ; face_dst: int
    ; face_left: int list
    ; face_right: int list
    ; face_eq: PA.t Data.eq
    }
  type graph =
    { gr_nodes: PA.t Data.elem array
    ; gr_edges: (int * (PA.t Data.morphism)) list array
    ; gr_faces: faces list
    }

  let rec concat (l : 'a m list) : 'a list m =
    match l with
    | [] -> ret []
    | x :: l ->
        let* x = x in
        let* l = concat l in
        ret (x :: l)
  let mapM (f : 'a -> 'b m) (l : 'a list) : 'b list m =
    concat (List.map f l)
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

  module SD = Serde.Make(PA)
  
  let pack_edge src dst mph =
    let* mph = SD.Mph.pack mph in
    ret (Msgpack.Map [
      (Msgpack.String "src", Msgpack.Integer src);
      (Msgpack.String "dst", Msgpack.Integer dst);
      (Msgpack.String "mph", mph);
    ])

  let unpack_edge msg =
    match msg with
    | Msgpack.Map [ (Msgpack.String s1, v1); (Msgpack.String s2, v2); (Msgpack.String s3, v3) ] -> begin
        let items = List.sort (fun (a,_) (b,_) -> String.compare a b) [ (s1,v1); (s2,v2); (s3,v3) ] in
        match items with
        | [ ("dst", Msgpack.Integer dst); ("mph", mph); ("src", Msgpack.Integer src) ] -> begin
          let* mph = SD.Mph.unpack mph in
          match mph with
          | Some mph -> ret (Some (src,dst,mph))
          | _ -> ret None
        end
        | _ -> ret None
    end
    | _ -> ret None

  let pack_face fce =
    let* eq = SD.Eq.pack fce.face_eq in
    ret (Msgpack.Map [
      (Msgpack.String "src", Msgpack.Integer fce.face_src);
      (Msgpack.String "dst", Msgpack.Integer fce.face_dst);
      (Msgpack.String "left", Msgpack.Array (List.map (fun i -> Msgpack.Integer i) fce.face_left));
      (Msgpack.String "right", Msgpack.Array (List.map (fun i -> Msgpack.Integer i) fce.face_right));
      (Msgpack.String "eq", eq)
    ])

  let unpack_int msg =
    match msg with
    | Msgpack.Integer i -> ret (Some i)
    | _ -> ret None

  let unpack_face msg =
    match msg with
    | Msgpack.Map [ (Msgpack.String s1, v1); (Msgpack.String s2, v2); (Msgpack.String s3, v3); (Msgpack.String s4, v4); (Msgpack.String s5, v5) ] -> begin
      let items = List.sort (fun (a,_) (b,_) -> String.compare a b) [ (s1,v1); (s2,v2); (s3,v3); (s4,v4); (s5,v5) ] in
      match items with
      | [ ("dst", Msgpack.Integer dst); ("eq", eq); ("left", Msgpack.Array left); ("right", Msgpack.Array right); ("src", Msgpack.Integer src) ] -> begin
        let* eq = SD.Eq.unpack eq in
        let* left = mapOptM unpack_int left in
        let* right = mapOptM unpack_int right in
        match eq, left, right with
        | Some eq, Some left, Some right -> ret (Some { face_src = src; face_dst = dst; face_left = left; face_right = right; face_eq = eq })
        | _ -> ret None
      end
      | _ -> ret None
    end
    | _ -> ret None

  module Serde = struct
    type t = graph

    let pack gr =
      let* nodes = mapM (fun e -> SD.Elem.pack e) (Array.to_list gr.gr_nodes) in
      let* edges = concat (List.flatten (Array.to_list
          (Array.mapi (fun s l -> List.map (fun (d,m) -> pack_edge s d m) l) gr.gr_edges))) in
      let* faces = mapM pack_face gr.gr_faces in
      ret (Msgpack.Map [
        (Msgpack.String "nodes", Msgpack.Array nodes);
        (Msgpack.String "edges", Msgpack.Array edges);
        (Msgpack.String "faces", Msgpack.Array faces);
      ])

    let unpack msg = 
      match msg with
      | Msgpack.Map [ (Msgpack.String s1, v1); (Msgpack.String s2, v2); (Msgpack.String s3, v3) ] -> begin
        let items = List.sort (fun (a,_) (b,_) -> String.compare a b) [ (s1,v1); (s2,v2); (s3,v3) ] in
        match items with
        | [ ("edges", Msgpack.Array edges); ("faces", Msgpack.Array faces); ("nodes", Msgpack.Array nodes) ] -> begin
          let* nodes = mapOptM SD.Elem.unpack nodes in
          let* edges = mapOptM unpack_edge edges in
          let* faces = mapOptM unpack_face faces in
          match nodes, edges, faces with
          | Some nodes, Some edges, Some faces -> begin
            let size = List.length nodes in
            let gr = {
              gr_nodes = Array.of_list nodes; 
              gr_edges = Array.make size [];
              gr_faces = faces;
            } in
            let edges = ref edges in
            while !edges != [] do
              let (s,d,m) = List.hd !edges in
              edges := List.tl !edges;
              gr.gr_edges.(s) <- (d,m) :: gr.gr_edges.(s)
            done;
            ret (Some gr)
          end
          | _ -> ret None
        end
        | _ -> ret None
      end
      | _ -> ret None
  end
end
