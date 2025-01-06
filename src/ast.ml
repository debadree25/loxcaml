open Token

type expr =
  | Binary of expr * token * expr * token_info
  | Grouping of expr
  | Literal of literal
  | Unary of token * expr * token_info

let rec ast_printer = function
  | Binary (left, op, right, _) ->
      Printf.sprintf "(%s %s %s)" (token_type_to_str op) (ast_printer left)
        (ast_printer right)
  | Grouping expr -> Printf.sprintf "(group %s)" (ast_printer expr)
  | Literal lit -> literal_to_str lit
  | Unary (op, expr, _) ->
      Printf.sprintf "(%s %s)" (token_type_to_str op) (ast_printer expr)
