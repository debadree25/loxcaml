open Token

type expr =
  | Binary of expr * token * expr
  | Grouping of expr
  | Literal of token
  | Unary of token * expr

let rec ast_printer = function
  | Binary (left, op, right) ->
      Printf.sprintf "(%s %s %s)" (token_type_to_str op) (ast_printer left)
        (ast_printer right)
  | Grouping expr -> Printf.sprintf "(group %s)" (ast_printer expr)
  | Literal token -> token_type_to_str token
  | Unary (op, expr) ->
      Printf.sprintf "(%s %s)" (token_type_to_str op) (ast_printer expr)
