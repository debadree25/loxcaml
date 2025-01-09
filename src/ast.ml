open Token

type expr =
  | Binary of expr * token * expr * token_info
  | Grouping of expr
  | Literal of literal
  | Unary of token * expr * token_info

type statement = Expression of expr | Print of expr

let rec expression_ast_printer = function
  | Binary (left, op, right, _) ->
      Printf.sprintf "(%s %s %s)" (token_type_to_str op)
        (expression_ast_printer left)
        (expression_ast_printer right)
  | Grouping expr -> Printf.sprintf "(group %s)" (expression_ast_printer expr)
  | Literal lit -> literal_to_str lit
  | Unary (op, expr, _) ->
      Printf.sprintf "(%s %s)" (token_type_to_str op)
        (expression_ast_printer expr)

let statement_ast_printer = function
  | Expression expr -> expression_ast_printer expr
  | Print expr -> Printf.sprintf "(print %s)" (expression_ast_printer expr)

let ast_printer stmts =
  let rec print_stmts = function
    | [] -> ()
    | stmt :: rest ->
        print_endline (statement_ast_printer stmt);
        print_stmts rest
  in
  print_stmts stmts
