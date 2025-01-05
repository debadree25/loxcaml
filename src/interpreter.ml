open Ast
open Token

let interpreter (expr: expr) =
  match expr with
  | Literal lit -> Ok (literal_to_str lit)
  | _ -> Error "Not implemented yet"
