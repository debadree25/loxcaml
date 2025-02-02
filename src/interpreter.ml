open Ast
open Token
open Utils
open Runtime

type ('a, 'b) execution_result = Ok of 'a | Error of 'b | Return of value

let execution_bind r f =
  match r with Ok v -> f v | Error _ as e -> e | Return v -> Return v

let ( let* ) = execution_bind

let rec execution_result_list_map (f : 'a -> ('b, 'c) execution_result) lst =
  match lst with
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = execution_result_list_map f xs in
      Ok (y :: ys)

type interpreter_state = { mutable environment : environment }

let make_interpreter_state () =
  let environment = make_enviroment None in
  make_globals environment;
  { environment }

let report_runtime_error message token_info =
  Printf.eprintf "%s\n[line %d]\n" message token_info.line;
  Error message

let push_environment interpreter_state =
  interpreter_state.environment <-
    make_enviroment (Some interpreter_state.environment)

let pop_environment interpreter_state =
  interpreter_state.environment <-
    Option.get interpreter_state.environment.enclosing

let push_closure interpreter_state closure =
  let existing_env = interpreter_state.environment in
  interpreter_state.environment <- make_enviroment (Some closure);
  existing_env

let pop_closure interpreter_state restore_env =
  interpreter_state.environment <- restore_env

let add_binding interpreter_state name_info value =
  add_binding interpreter_state.environment name_info.lexeme value

let get_binding_and_env interpreter_state name_info =
  let rec get_binding_and_env' environment =
    match Hashtbl.find_opt environment.bindings name_info.lexeme with
    | Some value -> Some (environment, name_info.lexeme, value)
    | None ->
        let** enclosing = environment.enclosing in
        get_binding_and_env' enclosing
  in
  get_binding_and_env' interpreter_state.environment

let get_binding_value interpreter_state name_info =
  match get_binding_and_env interpreter_state name_info with
  | Some (_, _, value) -> Ok value
  | None ->
      report_runtime_error
        (Printf.sprintf "Undefined variable %s." name_info.lexeme)
        name_info

let binding_exists interpreter_state name =
  match get_binding_and_env interpreter_state name with
  | Some _ -> true
  | None -> false

let reassign_binding interpreter_state name_info value =
  match get_binding_and_env interpreter_state name_info with
  | Some (environment, name, _) -> Ok (reassign_binding environment name value)
  | None ->
      report_runtime_error
        (Printf.sprintf "Undefined variable %s." name_info.lexeme)
        name_info

let is_truthy = function
  | Primitive lit -> (
      match lit with
      | LBool b -> b
      | LNil -> false
      | LNumber n -> n <> 0.0
      | LString _ -> true)
  | NativeFunc _ | UserFunc _ -> true

let evaluate_unary op right op_info =
  match (op, right) with
  | BANG, (_ as right_lit) -> Ok (LBool (not (is_truthy right_lit)))
  | MINUS, Primitive (LNumber n) -> Ok (LNumber (-.n))
  | MINUS, _ -> report_runtime_error "Operand must be a number." op_info
  | _ -> report_runtime_error "Invalid unary operator." op_info

let is_pure_binary_numeric_op = function
  | STAR | SLASH | MINUS | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL -> true
  | _ -> false

let is_equality_op = function BANG_EQUAL | EQUAL_EQUAL -> true | _ -> false

let extract_number_from_value = function
  | Primitive (LNumber n) -> Some n
  | _ -> None

let pure_numeric_binary_op left op right op_info =
  match
    (extract_number_from_value left, op, extract_number_from_value right)
  with
  | Some l, STAR, Some r -> Ok (Primitive (LNumber (l *. r)))
  | Some l, SLASH, Some r -> Ok (Primitive (LNumber (l /. r)))
  | Some l, MINUS, Some r -> Ok (Primitive (LNumber (l -. r)))
  | Some l, GREATER, Some r -> Ok (Primitive (LBool (l > r)))
  | Some l, GREATER_EQUAL, Some r -> Ok (Primitive (LBool (l >= r)))
  | Some l, LESS, Some r -> Ok (Primitive (LBool (l < r)))
  | Some l, LESS_EQUAL, Some r -> Ok (Primitive (LBool (l <= r)))
  | _ -> report_runtime_error "Operands must be numbers." op_info

let equality_op left op right op_info =
  match (left, op, right) with
  | Primitive (LNumber l), BANG_EQUAL, Primitive (LNumber r) ->
      Ok (Primitive (LBool (l <> r)))
  | Primitive (LNumber l), EQUAL_EQUAL, Primitive (LNumber r) ->
      Ok (Primitive (LBool (l = r)))
  | Primitive (LBool l), BANG_EQUAL, Primitive (LBool r) ->
      Ok (Primitive (LBool (l <> r)))
  | Primitive (LBool l), EQUAL_EQUAL, Primitive (LBool r) ->
      Ok (Primitive (LBool (l = r)))
  | Primitive (LString l), BANG_EQUAL, Primitive (LString r) ->
      Ok (Primitive (LBool (l <> r)))
  | Primitive (LString l), EQUAL_EQUAL, Primitive (LString r) ->
      Ok (Primitive (LBool (l = r)))
  | Primitive LNil, BANG_EQUAL, Primitive LNil -> Ok (Primitive (LBool false))
  | Primitive LNil, EQUAL_EQUAL, Primitive LNil -> Ok (Primitive (LBool true))
  | _, BANG_EQUAL, _ -> Ok (Primitive (LBool true))
  | _, EQUAL_EQUAL, _ -> Ok (Primitive (LBool false))
  | _ -> report_runtime_error "Unknown equality op" op_info

let evaluate_binary left op right op_info =
  if is_pure_binary_numeric_op op then
    pure_numeric_binary_op left op right op_info
  else if is_equality_op op then equality_op left op right op_info
  else
    match (left, op, right) with
    | Primitive (LString l), PLUS, Primitive (LString r) ->
        Ok (Primitive (LString (l ^ r)))
    | Primitive (LNumber l), PLUS, Primitive (LNumber r) ->
        Ok (Primitive (LNumber (l +. r)))
    | _ ->
        report_runtime_error "Operands must be two numbers or two strings."
          op_info

let rec evaluate_expr interpreter_state expr =
  match expr with
  | Literal lit -> Ok (Primitive lit)
  | Grouping expr -> evaluate_expr interpreter_state expr
  | Unary (op, right, op_info) ->
      let* right_lit = evaluate_expr interpreter_state right in
      let* lit_val = evaluate_unary op right_lit op_info in
      Ok (Primitive lit_val)
  | Binary (left, op, right, op_info) ->
      let* left_lit = evaluate_expr interpreter_state left in
      let* right_lit = evaluate_expr interpreter_state right in
      evaluate_binary left_lit op right_lit op_info
  | Variable (_, name_info) ->
      let* lit = get_binding_value interpreter_state name_info in
      Ok lit
  | Assign (_, name_info, expr) ->
      let* lit = evaluate_expr interpreter_state expr in
      let* _ = reassign_binding interpreter_state name_info lit in
      Ok lit
  | Logical (left, op, right, _) ->
      evaluate_logical interpreter_state left op right
  | Call (callee, args, paren) ->
      evaluate_call interpreter_state callee args paren

and evaluate_call interpreter_state callee args paren =
  let* callee_val = evaluate_expr interpreter_state callee in
  let check_arity arity =
    if List.length args <> arity then
      report_runtime_error
        (Printf.sprintf "Expected %d arguments but got %d." arity
           (List.length args))
        paren
    else Ok ()
  in
  let evaluate_args =
    execution_result_list_map (evaluate_expr interpreter_state)
  in
  let rec bind_params param arg =
    match (param, arg) with
    | [], [] -> Ok ()
    | param :: rest_params, arg :: rest_args ->
        add_binding interpreter_state param arg;
        bind_params rest_params rest_args
    | _ -> report_runtime_error "Mismatched number of arguments." paren
  in
  let* args = evaluate_args args in
  match callee_val with
  | NativeFunc (arity, _, func) ->
      let* _ = check_arity arity in
      Ok (func args)
  | UserFunc (arity, _, _, params, body, closure) -> (
      let* _ = check_arity arity in
      let present_env = push_closure interpreter_state closure in
      let* _ = bind_params params args in
      let block_val = evaluate_statement interpreter_state body in
      pop_closure interpreter_state present_env;
      match block_val with
      | Ok _ -> Ok (Primitive LNil)
      | Error _ as e -> e
      | Return v -> Ok v)
  | _ -> report_runtime_error "Can only call functions and classes." paren

and evaluate_logical interpreter_state left op right =
  let* left_lit = evaluate_expr interpreter_state left in
  if op = OR then
    if is_truthy left_lit then Ok left_lit
    else evaluate_expr interpreter_state right
  else if not (is_truthy left_lit) then Ok left_lit
  else evaluate_expr interpreter_state right

and evaluate_print interpreter_state expr =
  let* v = evaluate_expr interpreter_state expr in
  Printf.printf "%s\n" (value_to_str v);
  Ok (Primitive LNil)

and evaluate_statement interpreter_state stmt =
  match stmt with
  | Expression expr -> evaluate_expr interpreter_state expr
  | Print expr -> evaluate_print interpreter_state expr
  | Evaluation stmt ->
      let* v = evaluate_statement interpreter_state stmt in
      Printf.printf "%s\n" (value_to_str v);
      Ok (Primitive LNil)
  | Var (_, name_token_info, Some expr) ->
      let* lit = evaluate_expr interpreter_state expr in
      add_binding interpreter_state name_token_info lit;
      Ok (Primitive LNil)
  | Var (_, name_token_info, None) ->
      add_binding interpreter_state name_token_info (Primitive LNil);
      Ok (Primitive LNil)
  | Block stmts -> evaluate_block interpreter_state stmts
  | If (condition, then_branch, else_branch) ->
      evaluate_if interpreter_state condition then_branch else_branch
  | While (condition, body) -> evaluate_while interpreter_state condition body
  | Function (_, name_info, params, body) ->
      evaluate_function interpreter_state name_info params body
  | Return (_, Some expr) ->
      let* v = evaluate_expr interpreter_state expr in
      Return v
  | Return (_, None) -> Return (Primitive LNil)

and evaluate_function interpreter_state name_info params body =
  let arity = List.length params in
  let name = name_info.lexeme in
  let func =
    UserFunc
      (arity, name_info, name, params, body, interpreter_state.environment)
  in
  add_binding interpreter_state name_info func;
  Ok (Primitive LNil)

and evaluate_if interpreter_state condition then_branch else_branch =
  let* condition_lit = evaluate_expr interpreter_state condition in
  if is_truthy condition_lit then
    evaluate_statement interpreter_state then_branch
  else
    match else_branch with
    | Some stmt -> evaluate_statement interpreter_state stmt
    | None -> Ok (Primitive LNil)

and evaluate_while interpreter_state condition body =
  let rec loop () =
    let* condition_lit = evaluate_expr interpreter_state condition in
    if is_truthy condition_lit then
      let* _ = evaluate_statement interpreter_state body in
      loop ()
    else Ok (Primitive LNil)
  in
  loop ()

and evaluate_block interpreter_state stmts =
  push_environment interpreter_state;
  let rec eval_stmts stmts =
    match stmts with
    | [] -> Ok (Primitive LNil)
    | stmt :: rest ->
        let* _ = evaluate_statement interpreter_state stmt in
        eval_stmts rest
  in
  let evaluated = eval_stmts stmts in
  pop_environment interpreter_state;
  evaluated

let interpreter stmts =
  let interpreter_state = make_interpreter_state () in
  let rec eval_stmts stmts =
    match stmts with
    | [] -> Ok ()
    | stmt :: rest ->
        let* _ = evaluate_statement interpreter_state stmt in
        eval_stmts rest
  in
  eval_stmts stmts
