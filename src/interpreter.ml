open Ast
open Token

let interpret_literal_to_str = function
  | LBool b -> string_of_bool b
  | LNil -> "nil"
  | LString s -> s
  | LNumber n ->
      if Float.is_integer n then Printf.sprintf "%d" (int_of_float n)
      else Printf.sprintf "%.15g" n

let interpreter (expr : expr) =
  match expr with
  | Literal lit -> Ok (interpret_literal_to_str lit)
  | _ -> Error "Not implemented yet"
