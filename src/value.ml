open Ast
open Token

type value =
  | Primitive of literal
  | NativeFunc of int * string * (value list -> value)
  | UserFunc of int * token_info * string * token_info list * statement list
