open Ast
open Token

type interpreter_state = {
  bindings : (string, literal) Hashtbl.t;
  mutable had_error : bool;
}

let make_interpreter_state () =
  { bindings = Hashtbl.create 10; had_error = false }

let report_runtime_error message token_info =
  Printf.eprintf "%s\n[line %d]\n" message token_info.line;
  Error message

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

let evaluate_unary op right op_info =
  match (op, right) with
  | BANG, (_ as right_lit) -> Ok (LBool (not (is_truthy right_lit)))
  | MINUS, LNumber n -> Ok (LNumber (-.n))
  | MINUS, _ -> report_runtime_error "Operand must be a number." op_info
  | _ -> report_runtime_error "Invalid unary operator." op_info

let is_pure_binary_numeric_op = function
  | STAR | SLASH | MINUS | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL -> true
  | _ -> false

let is_equality_op = function BANG_EQUAL | EQUAL_EQUAL -> true | _ -> false

let pure_numeric_binary_op left op right op_info =
  match (left, op, right) with
  | LNumber l, STAR, LNumber r -> Ok (LNumber (l *. r))
  | LNumber l, SLASH, LNumber r -> Ok (LNumber (l /. r))
  | LNumber l, MINUS, LNumber r -> Ok (LNumber (l -. r))
  | LNumber l, GREATER, LNumber r -> Ok (LBool (l > r))
  | LNumber l, GREATER_EQUAL, LNumber r -> Ok (LBool (l >= r))
  | LNumber l, LESS, LNumber r -> Ok (LBool (l < r))
  | LNumber l, LESS_EQUAL, LNumber r -> Ok (LBool (l <= r))
  | _ -> report_runtime_error "Operands must be numbers." op_info

let equality_op left op right op_info =
  match (left, op, right) with
  | LNumber l, BANG_EQUAL, LNumber r -> Ok (LBool (l <> r))
  | LNumber l, EQUAL_EQUAL, LNumber r -> Ok (LBool (l = r))
  | LBool l, BANG_EQUAL, LBool r -> Ok (LBool (l <> r))
  | LBool l, EQUAL_EQUAL, LBool r -> Ok (LBool (l = r))
  | LString l, BANG_EQUAL, LString r -> Ok (LBool (l <> r))
  | LString l, EQUAL_EQUAL, LString r -> Ok (LBool (l = r))
  | LNil, BANG_EQUAL, LNil -> Ok (LBool false)
  | LNil, EQUAL_EQUAL, LNil -> Ok (LBool true)
  | _, BANG_EQUAL, _ -> Ok (LBool true)
  | _, EQUAL_EQUAL, _ -> Ok (LBool false)
  | _ -> report_runtime_error "Unknown equality op" op_info

let evaluate_binary left op right op_info =
  if is_pure_binary_numeric_op op then
    pure_numeric_binary_op left op right op_info
  else if is_equality_op op then equality_op left op right op_info
  else
    match (left, op, right) with
    | LString l, PLUS, LString r -> Ok (LString (l ^ r))
    | LNumber l, PLUS, LNumber r -> Ok (LNumber (l +. r))
    | _ ->
        report_runtime_error "Operands must be two numbers or two strings."
          op_info

let rec evaluate_expr interpreter_state expr =
  match expr with
  | Literal lit -> Ok lit
  | Grouping expr -> evaluate_expr interpreter_state expr
  | Unary (op, right, op_info) -> (
      match evaluate_expr interpreter_state right with
      | Ok right_lit -> evaluate_unary op right_lit op_info
      | Error _ as e -> e)
  | Binary (left, op, right, op_info) -> (
      match
        ( evaluate_expr interpreter_state left,
          evaluate_expr interpreter_state right )
      with
      | Ok left_lit, Ok right_lit ->
          evaluate_binary left_lit op right_lit op_info
      | Error err, _ -> Error err
      | _, Error err -> Error err)
  | Variable (_, name_info) -> (
      match Hashtbl.find_opt interpreter_state.bindings name_info.lexeme with
      | Some lit -> Ok lit
      | None ->
          report_runtime_error
            (Printf.sprintf "Undefined variable %s." name_info.lexeme)
            name_info)
  | Assign (_, name_info, expr) -> (
      match evaluate_expr interpreter_state expr with
      | Ok lit ->
          let exists =
            Hashtbl.mem interpreter_state.bindings name_info.lexeme
          in
          if exists then (
            Hashtbl.replace interpreter_state.bindings name_info.lexeme lit;
            Ok lit)
          else
            report_runtime_error
              (Printf.sprintf "Undefined variable %s." name_info.lexeme)
              name_info
      | Error err -> Error err)

let evaluate_print interpreter_state expr =
  match evaluate_expr interpreter_state expr with
  | Ok lit ->
      Printf.printf "%s\n" (interpret_literal_to_str lit);
      Ok LNil
  | Error err -> Error err

let rec evaluate_statement interpreter_state stmt =
  match stmt with
  | Expression expr -> evaluate_expr interpreter_state expr
  | Print expr -> evaluate_print interpreter_state expr
  | Evaluation stmt -> (
      match evaluate_statement interpreter_state stmt with
      | Ok lit ->
          Printf.printf "%s\n" (interpret_literal_to_str lit);
          Ok LNil
      | Error err -> Error err)
  | Var (_, name_token_info, Some expr) -> (
      match evaluate_expr interpreter_state expr with
      | Ok lit ->
          Hashtbl.add interpreter_state.bindings name_token_info.lexeme lit;
          Ok LNil
      | Error err -> Error err)
  | Var (_, name_token_info, None) ->
      Hashtbl.add interpreter_state.bindings name_token_info.lexeme LNil;
      Ok LNil

let interpreter stmts =
  let interpreter_state = make_interpreter_state () in
  let rec eval_stmts stmts =
    match stmts with
    | [] -> Ok ()
    | stmt :: rest -> (
        match evaluate_statement interpreter_state stmt with
        | Ok _ -> eval_stmts rest
        | Error err -> Error err)
  in
  eval_stmts stmts
