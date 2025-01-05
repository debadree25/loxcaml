open Ast
open Token

let interpret_literal_to_str = function
  | LBool b -> string_of_bool b
  | LNil -> "nil"
  | LString s -> s
  | LNumber n ->
      if Float.is_integer n then Printf.sprintf "%d" (int_of_float n)
      else Printf.sprintf "%.15g" n

let is_truthy = function
  | LBool b -> b
  | LNil -> false
  | LNumber n -> n <> 0.0
  | LString s -> String.length s > 0

let evaluate_unary op right =
  match op with
  | MINUS -> (
      match right with
      | LNumber n -> Ok (LNumber (-.n))
      | _ -> Error "Unary minus can only be applied to numbers")
  | BANG -> Ok (LBool (not (is_truthy right)))
  | _ -> Error "Invalid unary operator"

let rec evaluate_expr expr =
  match expr with
  | Literal lit -> Ok lit
  | Grouping expr -> evaluate_expr expr
  | Unary (op, right) -> (
      match evaluate_expr right with
      | Ok right_lit -> evaluate_unary op right_lit
      | Error _ as e -> e)
  | _ -> Error "Not implemented"

let interpreter expr =
  match evaluate_expr expr with
  | Ok lit -> Ok (interpret_literal_to_str lit)
  | Error e -> Error e
