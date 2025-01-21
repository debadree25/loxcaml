open Token
open Ast
open Utils

type parser = {
  mutable tokens : token_info array;
  mutable current : int;
  mutable had_error : bool;
}

let make_parser (tokens : token_info list) =
  { tokens = Array.of_list tokens; current = 0; had_error = false }

let peek parser = parser.tokens.(parser.current).ttype
let peek_with_token_info parser = parser.tokens.(parser.current)
let is_at_end parser = match peek parser with EOF -> true | _ -> false
let previous parser = parser.tokens.(parser.current - 1).ttype
let previous_with_token_info parser = parser.tokens.(parser.current - 1)

let advance parser =
  if not (is_at_end parser) then parser.current <- parser.current + 1;
  previous parser

let retreat parser =
  if parser.current > 0 then parser.current <- parser.current - 1;
  peek parser

let check parser expected =
  if is_at_end parser then false else peek parser = expected

let check_pattern parser expected =
  if is_at_end parser then false
  else token_to_token_pattern (peek parser) = expected

let rec match_tokens_by_matcher matcher parser tokens =
  match tokens with
  | [] -> false
  | token :: remaining ->
      if matcher parser token then (
        ignore (advance parser);
        true)
      else match_tokens_by_matcher matcher parser remaining

let match_tokens = match_tokens_by_matcher check
let match_tokens_by_pattern = match_tokens_by_matcher check_pattern

type parse_error = string * token_info

let consume parser expected (message : string) =
  if check parser expected then Ok (advance parser)
  else Error (message, peek_with_token_info parser)

let consume_by_pattern parser pattern (message : string) =
  if check_pattern parser pattern then Ok (advance parser)
  else Error (message, peek_with_token_info parser)

let report_error parser (parse_error : parse_error) =
  let message, token = parse_error in
  Printf.eprintf "[line %d] Error: %s %s\n" token.line
    (match token.ttype with
    | EOF -> "at end"
    | _ -> Printf.sprintf "at '%s'" token.lexeme)
    message;
  parser.had_error <- true

let literal_from_token_info token_info =
  match token_info.ttype with
  | NUMBER n -> Some (LNumber (float_of_string n))
  | STRING s -> Some (LString s)
  | TRUE -> Some (LBool true)
  | FALSE -> Some (LBool false)
  | NIL -> Some LNil
  | _ -> None

let rec expression parser = assignment parser

and make_binaryish_continuation tokens_to_match continuation_fn mapper_fn parser
    left =
  if match_tokens parser tokens_to_match then
    let operator_info = previous_with_token_info parser in
    let operator = operator_info.ttype in
    let* right = continuation_fn parser in
    make_binaryish_continuation tokens_to_match continuation_fn mapper_fn parser
      (mapper_fn (left, operator, right, operator_info))
  else Ok left

and make_binary_continuation tokens_to_match continuation_fn parser left =
  make_binaryish_continuation tokens_to_match continuation_fn
    (fun (left, op, right, op_info) -> Binary (left, op, right, op_info))
    parser left

and make_logical_continuation tokens_to_match continuation_fn parser left =
  make_binaryish_continuation tokens_to_match continuation_fn
    (fun (left, op, right, op_info) -> Logical (left, op, right, op_info))
    parser left

and assignment parser =
  let* exp = logical_or parser in
  if match_tokens parser [ EQUAL ] then
    let equals_tok = previous_with_token_info parser in
    match exp with
    | Variable (name, name_info) ->
        let* val_expr = assignment parser in
        Ok (Assign (name, name_info, val_expr))
    | _ -> Error ("Invalid assignment target", equals_tok)
  else Ok exp

and logical_or parser =
  let logical_or_continuation = make_logical_continuation [ OR ] logical_and in
  let* expr = logical_and parser in
  logical_or_continuation parser expr

and logical_and parser =
  let logical_and_continuation = make_logical_continuation [ AND ] equality in
  let* expr = equality parser in
  logical_and_continuation parser expr

and equality parser =
  let equality_continuation =
    make_binary_continuation [ BANG_EQUAL; EQUAL_EQUAL ] comparison
  in
  let* expr = comparison parser in
  equality_continuation parser expr

and comparison parser =
  let comparison_continuation =
    make_binary_continuation [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] term
  in
  let* expr = term parser in
  comparison_continuation parser expr

and term parser =
  let term_continuation = make_binary_continuation [ MINUS; PLUS ] factor in
  let* expr = factor parser in
  term_continuation parser expr

and factor parser =
  let factor_continuation = make_binary_continuation [ SLASH; STAR ] unary in
  let* expr = unary parser in
  factor_continuation parser expr

and unary parser =
  if match_tokens parser [ BANG; MINUS ] then
    let operator_info = previous_with_token_info parser in
    let operator = operator_info.ttype in
    let* right = unary parser in
    Ok (Unary (operator, right, operator_info))
  else call parser

and call parser =
  let rec call_helper callee =
    if match_tokens parser [ LEFT_PAREN ] then
      let* arguments = arguments parser in
      let* _ = consume parser RIGHT_PAREN "Expect ')' after arguments" in
      call_helper (Call (callee, arguments, previous_with_token_info parser))
    else Ok callee
  in
  let* callee = primary parser in
  call_helper callee

and arguments parser =
  let rec arguments_helper args argCount =
    if argCount > 255 then
      Error ("Cannot have more than 255 arguments", peek_with_token_info parser)
    else if not (check parser RIGHT_PAREN) then
      let* arg = expression parser in
      let args = arg :: args in
      if match_tokens parser [ COMMA ] then arguments_helper args (argCount + 1)
      else Ok args
    else Ok []
  in
  arguments_helper [] 0

and primary parser =
  if
    match_tokens_by_pattern parser
      [ T_FALSE; T_TRUE; T_NIL; T_NUMBER; T_STRING ]
  then
    let token_info = previous_with_token_info parser in
    let literal = literal_from_token_info token_info in
    match literal with
    | Some lit -> Ok (Literal lit)
    | None -> Error ("Expect literal", token_info)
  else if match_tokens_by_pattern parser [ T_IDENTIFIER ] then
    let token_info = previous_with_token_info parser in
    Ok (Variable (token_info.ttype, token_info))
  else if match_tokens parser [ LEFT_PAREN ] then
    let* expr = expression parser in
    let* _ = consume parser RIGHT_PAREN "Expect ')' after expression" in
    Ok (Grouping expr)
  else Error ("Expect expression", peek_with_token_info parser)

let rec synchronize parser =
  let prev = advance parser in
  if is_at_end parser then ()
  else
    match prev with
    | SEMICOLON -> ()
    | CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN ->
        ignore (retreat parser)
    | _ -> synchronize parser

let rec declaration parser =
  if match_tokens parser [ VAR ] then (
    match var_declaration parser with
    | Ok stmt -> Ok stmt
    | Error e ->
        synchronize parser;
        Error e)
  else statement parser

and var_declaration parser =
  let* name_token =
    consume_by_pattern parser T_IDENTIFIER "Expect variable name"
  in
  let name_token_info = previous_with_token_info parser in
  let* expr =
    if match_tokens parser [ EQUAL ] then expression parser
    else Ok (Literal LNil)
  in
  let* _ = consume parser SEMICOLON "Expect ';' after variable declaration" in
  Ok (Var (name_token, name_token_info, Some expr))

and statement parser =
  if match_tokens parser [ PRINT ] then print_statement parser
  else if match_tokens parser [ FUN ] then function_declaration parser
  else if match_tokens parser [ FOR ] then for_statement parser
  else if match_tokens parser [ LEFT_BRACE ] then block parser
  else if match_tokens parser [ IF ] then if_statement parser
  else if match_tokens parser [ WHILE ] then while_statement parser
  else expression_statement parser

and function_declaration parser =
  let* name_token =
    consume_by_pattern parser T_IDENTIFIER "Expect function name" in
  let name_token_info = previous_with_token_info parser in
  let* _ = consume parser LEFT_PAREN "Expect '(' after function name" in
  let* params = function_parameters parser in
  let* _ = consume parser RIGHT_PAREN "Expect ')' after function parameters" in
  let* _ = consume parser LEFT_BRACE "Expect '{' before function body" in
  let* body = block parser in
  Ok (Function (name_token, name_token_info, params, body))

and function_parameters parser =
  let rec function_parameters_helper params =
    if not (check parser RIGHT_PAREN) then
      let* _ =
        consume_by_pattern parser T_IDENTIFIER "Expect parameter name"
      in
      let param_token_info = previous_with_token_info parser in
      let params = param_token_info :: params in
      if match_tokens parser [ COMMA ] then
        function_parameters_helper params
      else Ok params
    else Ok []
  in
  function_parameters_helper []

and if_statement parser =
  let* _ = consume parser LEFT_PAREN "Expect '(' after 'if'." in
  let* condition = expression parser in
  let* _ = consume parser RIGHT_PAREN "Expect ')' after if condition." in
  let* then_branch = statement parser in
  if match_tokens parser [ ELSE ] then
    let* else_branch = statement parser in
    Ok (If (condition, then_branch, Some else_branch))
  else Ok (If (condition, then_branch, None))

and for_statement parser =
  let* _ = consume parser LEFT_PAREN "Expect '(' after 'for'." in
  let* initializer_stmt =
    if match_tokens parser [ SEMICOLON ] then Ok None
    else if match_tokens parser [ VAR ] then
      let* var_decl = var_declaration parser in
      Ok (Some var_decl)
    else
      let* expr = expression_statement parser in
      Ok (Some expr)
  in
  let* condition =
    if not (check parser SEMICOLON) then expression parser
    else Ok (Literal (LBool true))
  in
  let* _ = consume parser SEMICOLON "Expect ';' after loop condition." in
  let* increment =
    if not (check parser RIGHT_PAREN) then
      let* expr = expression parser in
      Ok (Some expr)
    else Ok None
  in
  let* _ = consume parser RIGHT_PAREN "Expect ')' after for clauses." in
  let* body = statement parser in
  let body =
    match increment with
    | Some expr -> Block [ body; Expression expr ]
    | None -> body
  in
  let body =
    match initializer_stmt with
    | Some stmt -> Block [ stmt; While (condition, body) ]
    | None -> While (condition, body)
  in
  Ok body

and while_statement parser =
  let* _ = consume parser LEFT_PAREN "Expect '(' after 'while'." in
  let* condition = expression parser in
  let* _ = consume parser RIGHT_PAREN "Expect ')' after while condition." in
  let* body = statement parser in
  Ok (While (condition, body))

and print_statement parser =
  let* expr = expression parser in
  let* _ = consume parser SEMICOLON "Expect ';' after value" in
  Ok (Print expr)

and expression_statement parser =
  let* expr = expression parser in
  let _ = consume parser SEMICOLON "Expect ';' after expression" in
  Ok (Expression expr)

and block parser =
  let block_helper =
    parser_builder (fun parser ->
        not (check parser RIGHT_BRACE || is_at_end parser))
  in
  let statements = block_helper parser in
  let* _ = consume parser RIGHT_BRACE "Expect '}' after block" in
  Ok (Block statements)

and parser_builder parsing_condition_fn parser =
  let rec parser_builder_helper parser =
    if parsing_condition_fn parser then (
      match declaration parser with
      | Ok stmt -> stmt :: parser_builder_helper parser
      | Error e ->
          report_error parser e;
          synchronize parser;
          parser_builder_helper parser)
    else []
  in
  parser_builder_helper parser

and parse parser = parser_builder (fun parser -> not (is_at_end parser)) parser

let parse_tokens tokens =
  let parser = make_parser tokens in
  let statements = parse parser in
  if parser.had_error then Error 65 else Ok statements
