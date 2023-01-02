
type t =
  | Nil
  | Integer of int
  | Int64 of Int64.t
  | Boolean of bool
  | Floating of float
  | Array of t list
  | Map of (t * t) list
  | String of string
  | Binary of Bytes.t
  | Extension of (int * Bytes.t)

type parser
type result =
  | Error of string
  | Msg of t
  | Eof

val make_parser : In_channel.t -> parser
val parse : parser -> result
val serialize : t -> bytes

