open Token

type expr =
  | Binary of expr * token * expr * token_info
  | Grouping of expr
  | Literal of literal
  | Unary of token * expr * token_info
  | Variable of token * token_info
  | Assign of token * token_info * expr
  | Logical of expr * token * expr * token_info
  | Call of expr * expr list * token_info

type statement =
  | Expression of expr
  | Print of expr
  | Evaluation of statement
  | Var of token * token_info * expr option
  | Block of statement list
  | If of expr * statement * statement option
  | While of expr * statement
  | Function of token * token_info * token_info list * statement

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
  | Variable (name, _) ->
      Printf.sprintf "(identifier %s)" (token_type_to_str name)
  | Assign (name, _, expr) ->
      Printf.sprintf "(%s = %s)" (token_type_to_str name)
        (expression_ast_printer expr)
  | Logical (left, op, right, _) ->
      Printf.sprintf "(%s %s %s)" (token_type_to_str op)
        (expression_ast_printer left)
        (expression_ast_printer right)
  | Call (callee, args, _) ->
      let rec print_args = function
        | [] -> ""
        | arg :: rest ->
            Printf.sprintf "%s %s" (print_args rest)
              (expression_ast_printer arg)
      in
      Printf.sprintf "(call %s %s)"
        (expression_ast_printer callee)
        (print_args args)

let rec statement_ast_printer = function
  | Expression expr -> expression_ast_printer expr
  | Print expr -> Printf.sprintf "(print %s)" (expression_ast_printer expr)
  | Evaluation stmt -> Printf.sprintf "(eval %s)" (statement_ast_printer stmt)
  | Var (name, _, Some expr) ->
      Printf.sprintf "(var %s = %s)" (token_type_to_str name)
        (expression_ast_printer expr)
  | Var (name, _, None) -> Printf.sprintf "(var %s)" (token_type_to_str name)
  | Block stmts ->
      let rec print_stmts = function
        | [] -> ""
        | stmt :: rest ->
            Printf.sprintf "%s\n%s"
              (statement_ast_printer stmt)
              (print_stmts rest)
      in
      Printf.sprintf "{\n%s}" (print_stmts stmts)
  | If (expr, then_stmt, else_stmt) ->
      let else_str =
        match else_stmt with
        | Some stmt -> Printf.sprintf "else %s" (statement_ast_printer stmt)
        | None -> ""
      in
      Printf.sprintf "if %s then %s %s"
        (expression_ast_printer expr)
        (statement_ast_printer then_stmt)
        else_str
  | While (expr, stmt) ->
      Printf.sprintf "while %s do %s"
        (expression_ast_printer expr)
        (statement_ast_printer stmt)
  | Function (name, _, params, body) ->
      let rec print_params = function
        | [] -> ""
        | param :: rest ->
            Printf.sprintf "%s %s" (print_params rest)
              (token_type_to_str param.ttype)
      in
      Printf.sprintf "function %s (%s) %s"
        (token_type_to_str name)
        (print_params params)
        (statement_ast_printer body)

let ast_printer stmts =
  let rec print_stmts = function
    | [] -> ()
    | stmt :: rest ->
        print_endline (statement_ast_printer stmt);
        print_stmts rest
  in
  print_stmts stmts
