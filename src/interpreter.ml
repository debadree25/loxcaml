open Ast
open Token

type environment = {
  enclosing : environment option;
  bindings : (string, literal) Hashtbl.t;
}

let ( let* ) = Result.bind
let ( let** ) = Option.bind
let make_enviroment enclosing = { enclosing; bindings = Hashtbl.create 10 }

let add_binding environment name value =
  Hashtbl.add environment.bindings name value

let reassign_binding environment name value =
  Hashtbl.replace environment.bindings name value

type interpreter_state = { mutable environment : environment }

let make_interpreter_state () = { environment = make_enviroment None }

let report_runtime_error message token_info =
  Printf.eprintf "%s\n[line %d]\n" message token_info.line;
  Error message

let push_environment interpreter_state =
  interpreter_state.environment <-
    make_enviroment (Some interpreter_state.environment)

let pop_environment interpreter_state =
  interpreter_state.environment <-
    Option.get interpreter_state.environment.enclosing

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
  | Unary (op, right, op_info) ->
      let* right_lit = evaluate_expr interpreter_state right in
      evaluate_unary op right_lit op_info
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

let evaluate_print interpreter_state expr =
  let* lit = evaluate_expr interpreter_state expr in
  Printf.printf "%s\n" (interpret_literal_to_str lit);
  Ok LNil

let rec evaluate_statement interpreter_state stmt =
  match stmt with
  | Expression expr -> evaluate_expr interpreter_state expr
  | Print expr -> evaluate_print interpreter_state expr
  | Evaluation stmt ->
      let* lit = evaluate_statement interpreter_state stmt in
      Printf.printf "%s\n" (interpret_literal_to_str lit);
      Ok LNil
  | Var (_, name_token_info, Some expr) ->
      let* lit = evaluate_expr interpreter_state expr in
      add_binding interpreter_state name_token_info lit;
      Ok LNil
  | Var (_, name_token_info, None) ->
      add_binding interpreter_state name_token_info LNil;
      Ok LNil
  | Block stmts -> evaluate_block interpreter_state stmts
  | If (condition, then_branch, else_branch) ->
      evaluate_if interpreter_state condition then_branch else_branch
  | While (condition, body) -> evaluate_while interpreter_state condition body

and evaluate_if interpreter_state condition then_branch else_branch =
  let* condition_lit = evaluate_expr interpreter_state condition in
  if is_truthy condition_lit then
    evaluate_statement interpreter_state then_branch
  else
    match else_branch with
    | Some stmt -> evaluate_statement interpreter_state stmt
    | None -> Ok LNil

and evaluate_while interpreter_state condition body =
  let rec loop () =
    let* condition_lit = evaluate_expr interpreter_state condition in
    if is_truthy condition_lit then
      let* _ = evaluate_statement interpreter_state body in
      loop ()
    else Ok LNil
  in
  loop ()

and evaluate_block interpreter_state stmts =
  push_environment interpreter_state;
  let rec eval_stmts stmts =
    match stmts with
    | [] -> Ok LNil
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
