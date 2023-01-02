
type t =
  | Nil
  | Integer of int
  | Int64 of int64
  | Boolean of bool
  | Floating of float
  | Array of t list
  | Map of (t * t) list
  | String of string
  | Binary of Bytes.t
  | Extension of (int * Bytes.t)

type result =
  | Error of string
  | Msg of t
  | Eof

type parser =
  { channel: In_channel.t
  }

let make_parser (input : In_channel.t) : parser = { channel = input }

module Monad = struct
  type 'a err =
    | Error of string
    | Val of 'a
  type 'a t = In_channel.t -> 'a err
  let r x = fun _ -> Val x
  let bind x f = fun input ->
    match x input with
    | Val x -> f x input
    | Error err -> Error err
  let get () : int t = fun input -> 
    let bt = In_channel.input_byte input in
    match bt with
    | Some bt -> Val bt
    | None -> Error "eof"
  let gets (n : int) : bytes t = fun input ->
    let bts = Bytes.create n in
    match In_channel.really_input input bts 0 n with
    | Some _ -> Val bts
    | None -> Error "eof"
  let error (err : string) : 'a t =
    fun _ -> Error err
end
let (let*) = Monad.bind
let ret = Monad.r
let error = Monad.error
let get = Monad.get
let gets = Monad.gets
type 'a m = 'a Monad.t

let rec do_parse () : t m =
  let* bt = get () in
  match bt with
  | 0xc0 -> ret Nil
  | 0xc2 -> ret (Boolean false)
  | 0xc3 -> ret (Boolean true)
  (* Short integers *)
  | _ when bt land 0x80 == 0 -> ret (Integer (bt land 0x7f))
  | _ when bt land 0xe0 == 0xe0 -> ret (Integer (-(bt land 0x1f)))
  (* Unsigned integers *)
  | 0xcc -> let* bts = gets 1 in ret (Integer (Bytes.get_uint8 bts 0))
  | 0xcd -> let* bts = gets 2 in ret (Integer (Bytes.get_uint16_be bts 0))
  | 0xce -> let* bts = gets 4 in ret (Int64 (Bytes.get_int64_be bts 0))
  | 0xcf -> let* bts = gets 8 in
      if Char.code (Bytes.get bts 0) land 0x70 <> 0
      then error "Integer too big"
      else ret (Int64 (Bytes.get_int64_be bts 0))
  (* Signed integers *)
  | 0xd0 -> let* bts = gets 1 in ret (Integer (Bytes.get_int8 bts 0))
  | 0xd1 -> let* bts = gets 2 in ret (Integer (Bytes.get_int16_be bts 0))
  | 0xd2 -> let* bts = gets 4 in ret (Int64 (Bytes.get_int64_be bts 0))
  | 0xd3 -> let* bts = gets 8 in ret (Int64 (Bytes.get_int64_be bts 0))
  (* Floating points *)
  | 0xca -> let* bts = gets 4 in ret (Floating (Int64.float_of_bits (Bytes.get_int64_ne bts 0)))
  | 0xcb -> let* bts = gets 8 in ret (Floating (Int64.float_of_bits (Bytes.get_int64_ne bts 0)))
  (* Strings *)
  | _ when bt land 0xe0 == 0xa0 ->
      let len = bt land 0x1f in
      let* bts = gets len in
      ret (String (Bytes.to_string bts))
  | 0xd9 ->
      let* lenbt = gets 1 in
      let len = Bytes.get_uint8 lenbt 0 in
      let* bts = gets len in
      ret (String (Bytes.to_string bts))
  | 0xda ->
      let* lenbt = gets 2 in
      let len = Bytes.get_uint16_be lenbt 0 in
      let* bts = gets len in
      ret (String (Bytes.to_string bts))
  | 0xdb ->
      let* lenbt = gets 4 in
      let len = Int64.to_int (Bytes.get_int64_be lenbt 0) in
      let* bts = gets len in
      ret (String (Bytes.to_string bts))
  (* Bytes *)
  | 0xc4 ->
      let* lenbt = gets 1 in
      let len = Bytes.get_uint8 lenbt 0 in
      let* bts = gets len in
      ret (Binary bts)
  | 0xc5 ->
      let* lenbt = gets 2 in
      let len = Bytes.get_uint16_be lenbt 0 in
      let* bts = gets len in
      ret (Binary bts)
  | 0xc6 ->
      let* lenbt = gets 4 in
      let len = Int64.to_int (Bytes.get_int64_be lenbt 0) in
      let* bts = gets len in
      ret (Binary bts)
  (* Array *)
  | _ when bt land 0xf0 == 0x90 ->
      let len = bt land 0x0f in
      let* lst = do_parse_list len in
      ret (Array lst)
  | 0xdc ->
      let* bts = gets 2 in
      let len = Bytes.get_uint16_be bts 0 in
      let* lst = do_parse_list len in
      ret (Array lst)
  | 0xdd ->
      let* bts = gets 4 in
      let len = Int64.to_int (Bytes.get_int64_be bts 0) in
      let* lst = do_parse_list len in
      ret (Array lst)
  (* Map *)
  | _ when bt land 0xf0 == 0x80 ->
      let len = bt land 0x0f in
      let* mp = do_parse_map len in
      ret (Map mp)
  | 0xde ->
      let* bts = gets 2 in
      let len = Bytes.get_uint16_be bts 0 in
      let* mp = do_parse_map len in
      ret (Map mp)
  | 0xdf ->
      let* bts = gets 4 in
      let len = Int64.to_int (Bytes.get_int64_be bts 0) in
      let* mp = do_parse_map len in
      ret (Map mp)
  (* Extensions *)
  | 0xd4 ->
      let* tp = gets 1 in
      let* data = gets 1 in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xd5 ->
      let* tp = gets 1 in
      let* data = gets 2 in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xd6 ->
      let* tp = gets 1 in
      let* data = gets 4 in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xd7 ->
      let* tp = gets 1 in
      let* data = gets 8 in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xd8 ->
      let* tp = gets 1 in
      let* data = gets 16 in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xc7 ->
      let* lenbt = gets 1 in
      let len = Bytes.get_uint8 lenbt 0 in
      let* tp = gets 1 in
      let* data = gets len in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xc8 ->
      let* lenbt = gets 2 in
      let len = Bytes.get_uint16_be lenbt 0 in
      let* tp = gets 1 in
      let* data = gets len in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | 0xc9 ->
      let* lenbt = gets 4 in
      let len = Int64.to_int (Bytes.get_int64_be lenbt 0) in
      let* tp = gets 1 in
      let* data = gets len in
      ret (Extension (Bytes.get_int8 tp 0,data))
  | _ -> error "Invalid byte"
and do_parse_list (n : int) : t list m =
  if n = 0 then ret []
  else
    let* elem = do_parse () in
    let* tail = do_parse_list (n-1) in
    ret (elem :: tail)
and do_parse_map (n : int) : (t*t) list m =
  if n = 0 then ret []
  else
    let* key = do_parse () in
    let* value = do_parse () in
    let* tail = do_parse_map (n-1) in
    ret ((key,value) :: tail)

let parse (p : parser) : result =
  match do_parse () p.channel with
  | Monad.Error "eof" -> Eof
  | Monad.Error err -> Error err
  | Monad.Val msg -> Msg msg

let rec byte_size (msg : t) : int =
  match msg with
  | Nil -> 1
  | Boolean _ -> 1
  | Integer _ -> 9
  | Int64 _ -> 9
  | Floating _ -> 9
  | String str ->
      let len = String.length str in
      if len < 32 then len + 1
      else if len < 256 then len + 2
      else len + 5
  | Binary bts ->
      let len = Bytes.length bts in
      if len < 256 then len + 2 else len + 5
  | Array lst ->
      let len = List.length lst in
      let off = if len < 16 then 1 else 5 in
      List.fold_left (fun a b -> a + byte_size b) off lst
  | Map mp ->
      let len = List.length mp in
      let off = if len < 16 then 1 else 5 in
      List.fold_left (fun a (k,v) -> a + byte_size k + byte_size v) off mp 
  | Extension (tp, bts) ->
      let len = Bytes.length bts in
      match len with
      | 1 -> 3
      | 2 -> 4
      | 4 -> 6
      | 8 -> 10
      | 16 -> 18
      | _ -> len + 6

let bset bts off v = Bytes.set bts off (Char.chr v)

let rec serialize_at (bts : bytes) (off : int) (msg : t) : int =
  match msg with
  | Nil -> bset bts off 0xc0; off + 1
  | Boolean false -> bset bts off 0xc2; off + 1
  | Boolean true -> bset bts off 0xc3; off + 1
  | Integer i -> bset bts off 0xd3; Bytes.set_int64_be bts (off+1) (Int64.of_int i); off + 9
  | Int64 i -> bset bts off 0xd3; Bytes.set_int64_be bts (off+1) i; off + 9
  | Floating f -> bset bts off 0xcb; Bytes.set_int64_be bts (off+1) (Int64.bits_of_float f); off + 9
  | String str ->
      let len = String.length str in
      let off = if len < 32 then begin
        bset bts off (0xa0 lor len);
        off + 1
      end else if len < 256 then begin
        bset bts off 0xd9;
        Bytes.set_uint8 bts (off+1) len;
        off + 2
      end else begin
        bset bts off 0xdb;
        Bytes.set_int32_be bts (off+1) (Int32.of_int len);
        off + 5
      end in
      Bytes.blit_string str 0 bts off len;
      off + len
  | Binary bs ->
      let len = Bytes.length bs in
      let off = if len < 256 then begin
        bset bts off 0xc4;
        Bytes.set_uint8 bts (off+1) len;
        off + 2
      end else begin
        bset bts off 0xc6;
        Bytes.set_int32_be bts (off+1) (Int32.of_int len);
        off + 5
      end in
      Bytes.blit bs 0 bts off len;
      off + len
  | Array lst ->
      let len = List.length lst in
      let off = if len < 16 then begin
        bset bts off (0x90 lor len);
        off + 1
      end else begin
        bset bts off 0xdd;
        Bytes.set_int32_be bts (off+1) (Int32.of_int len);
        off + 5
      end in
      List.fold_left (serialize_at bts) off lst
  | Map mp ->
      let len = List.length mp in
      let off = if len < 16 then begin
        bset bts off (0x80 lor len);
        off + 1
      end else begin
        bset bts off 0xdf;
        Bytes.set_int32_be bts (off+1) (Int32.of_int len);
        off + 5
      end in
      List.fold_left (fun off (k,v) -> let off = serialize_at bts off k in serialize_at bts off v) off mp
  | Extension (tp,bs) ->
      let len = Bytes.length bs in
      let off =
        if len = 1 then (bset bts off 0xd4; off + 1)
        else if len = 2 then (bset bts off 0xd5; off + 1)
        else if len = 4 then (bset bts off 0xd6; off + 1)
        else if len = 8 then (bset bts off 0xd7; off + 1)
        else if len = 16 then (bset bts off 0xd8; off + 1)
        else begin
          bset bts off 0xc9;
          Bytes.set_int32_be bts (off+1) (Int32.of_int len);
          off + 5
        end in
      bset bts off tp;
      Bytes.blit bs 0 bts (off + 1) len;
      off + 1 + len

let serialize (msg : t) : bytes =
  let len = byte_size msg in
  let bts = Bytes.make len (Char.chr 0) in
  let _ = serialize_at bts 0 msg in bts

