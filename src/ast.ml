open Token

type expr =
  | Binary of expr * token * expr * token_info
  | Grouping of expr
  | Literal of literal
  | Unary of token * expr * token_info
  | Variable of token * token_info

type statement =
  | Expression of expr
  | Print of expr
  | Evaluation of statement
  | Var of token * token_info * expr option

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
  | Variable (name, _) -> token_type_to_str name

let rec statement_ast_printer = function
  | Expression expr -> expression_ast_printer expr
  | Print expr -> Printf.sprintf "(print %s)" (expression_ast_printer expr)
  | Evaluation stmt -> Printf.sprintf "(eval %s)" (statement_ast_printer stmt)
  | Var (name, _, Some expr) ->
      Printf.sprintf "(var %s = %s)" (token_type_to_str name)
        (expression_ast_printer expr)
  | Var (name, _, None) -> Printf.sprintf "(var %s)" (token_type_to_str name)

let ast_printer stmts =
  let rec print_stmts = function
    | [] -> ()
    | stmt :: rest ->
        print_endline (statement_ast_printer stmt);
        print_stmts rest
  in
  print_stmts stmts
