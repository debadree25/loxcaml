open Token
open Ast

type parser = {
  mutable tokens : token_info array;
  mutable current : int;
  mutable had_error : bool;
}

let ( let* ) = Result.bind

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

and make_binary_continuation tokens_to_match continuation_fn parser left =
  if match_tokens parser tokens_to_match then
    let operator_info = previous_with_token_info parser in
    let operator = operator_info.ttype in
    let* right = continuation_fn parser in
    make_binary_continuation tokens_to_match continuation_fn parser
      (Binary (left, operator, right, operator_info))
  else Ok left

and assignment parser =
  let* exp = equality parser in
  if match_tokens parser [ EQUAL ] then
    let equals_tok = previous_with_token_info parser in
    match exp with
    | Variable (name, name_info) ->
        let* val_expr = assignment parser in
        Ok (Assign (name, name_info, val_expr))
    | _ -> Error ("Invalid assignment target", equals_tok)
  else Ok exp

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
  else primary parser

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
  else if match_tokens parser [ LEFT_BRACE ] then block parser
  else if match_tokens parser [ IF ] then if_statement parser
  else expression_statement parser

and if_statement parser =
  let* _ = consume parser LEFT_PAREN "Expect '(' after 'if'." in
  let* condition = expression parser in
  let* _ = consume parser RIGHT_PAREN "Expect ')' after if condition." in
  let* then_branch = statement parser in
  if match_tokens parser [ ELSE ] then
    let* else_branch = statement parser in
    Ok (If (condition, then_branch, Some else_branch))
  else Ok (If (condition, then_branch, None))

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
