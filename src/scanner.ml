open Token

type scanner = {
  source : string;
  mutable tokens : token_info list;
  mutable start_lexeme : int;
  mutable current_lexeme : int;
  mutable line : int;
  mutable had_error : bool;
}

let make_scanner source =
  {
    source;
    tokens = [];
    start_lexeme = 0;
    current_lexeme = 0;
    line = 1;
    had_error = false;
  }

let is_at_end scanner = scanner.current_lexeme >= String.length scanner.source

let add_token scanner token literal line =
  scanner.tokens <- make_token_info token literal line :: scanner.tokens

let advance scanner =
  scanner.current_lexeme <- scanner.current_lexeme + 1;
  scanner.source.[scanner.current_lexeme - 1]

let advance_line scanner = scanner.line <- scanner.line + 1

let peek scanner =
  if is_at_end scanner then '\000' else scanner.source.[scanner.current_lexeme]

let peek_next scanner =
  if scanner.current_lexeme + 1 >= String.length scanner.source then '\000'
  else scanner.source.[scanner.current_lexeme + 1]

let match_next (scanner : scanner) expected =
  if peek scanner <> expected then false
  else (
    ignore (advance scanner);
    true)

let report_error scanner line message =
  Printf.eprintf "[line %d] Error: %s\n" line message;
  scanner.had_error <- true

let consume_comment scanner =
  while peek scanner <> '\n' && not (is_at_end scanner) do
    ignore (advance scanner)
  done

let consume_string scanner =
  while peek scanner <> '"' && not (is_at_end scanner) do
    if peek scanner = '\n' then advance_line scanner;
    ignore (advance scanner)
  done;
  if is_at_end scanner then
    report_error scanner scanner.line "Unterminated string."
  else
    let _ = advance scanner in
    let start_str = scanner.start_lexeme + 1 in
    let end_str = scanner.current_lexeme - 1 in
    let length = end_str - start_str in
    let literal = String.sub scanner.source start_str length in
    add_token scanner (STRING literal) (Some (LString literal)) scanner.line

let consume_number scanner =
  while peek scanner >= '0' && peek scanner <= '9' do
    ignore (advance scanner)
  done;
  if peek scanner = '.' && peek_next scanner >= '0' && peek_next scanner <= '9'
  then (
    ignore (advance scanner);
    while peek scanner >= '0' && peek scanner <= '9' do
      ignore (advance scanner)
    done);
  let start = scanner.start_lexeme in
  let length = scanner.current_lexeme - start in
  let literal = String.sub scanner.source start length in
  let number = float_of_string literal in
  add_token scanner (NUMBER literal) (Some (LNumber number)) scanner.line

let consume_identifier scanner =
  while
    match peek scanner with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
    | _ -> false
  do
    ignore (advance scanner)
  done;
  let start = scanner.start_lexeme in
  let length = scanner.current_lexeme - start in
  let literal = String.sub scanner.source start length in
  let reserved = string_to_reseved_word literal in
  let token = match reserved with Some t -> t | None -> IDENTIFIER literal in
  add_token scanner token None scanner.line

let scan_token scanner =
  let c = advance scanner in
  match c with
  | '(' -> add_token scanner LEFT_PAREN None scanner.line
  | ')' -> add_token scanner RIGHT_PAREN None scanner.line
  | '{' -> add_token scanner LEFT_BRACE None scanner.line
  | '}' -> add_token scanner RIGHT_BRACE None scanner.line
  | ',' -> add_token scanner COMMA None scanner.line
  | '.' -> add_token scanner DOT None scanner.line
  | '-' -> add_token scanner MINUS None scanner.line
  | '+' -> add_token scanner PLUS None scanner.line
  | ';' -> add_token scanner SEMICOLON None scanner.line
  | '*' -> add_token scanner STAR None scanner.line
  | '/' ->
      if match_next scanner '/' then consume_comment scanner
      else add_token scanner SLASH None scanner.line
  | '!' ->
      if match_next scanner '=' then
        add_token scanner BANG_EQUAL None scanner.line
      else add_token scanner BANG None scanner.line
  | '=' ->
      if match_next scanner '=' then
        add_token scanner EQUAL_EQUAL None scanner.line
      else add_token scanner EQUAL None scanner.line
  | '<' ->
      if match_next scanner '=' then
        add_token scanner LESS_EQUAL None scanner.line
      else add_token scanner LESS None scanner.line
  | '>' ->
      if match_next scanner '=' then
        add_token scanner GREATER_EQUAL None scanner.line
      else add_token scanner GREATER None scanner.line
  | '\n' -> advance_line scanner
  | ' ' | '\r' | '\t' -> ()
  | '"' -> consume_string scanner
  | '0' .. '9' -> consume_number scanner
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> consume_identifier scanner
  | _ ->
      report_error scanner scanner.line
        (Printf.sprintf "Unexpected character: %c" c)

let scan_tokens scanner =
  while not (is_at_end scanner) do
    scanner.start_lexeme <- scanner.current_lexeme;
    scan_token scanner
  done;
  add_token scanner EOF None scanner.line;
  List.rev scanner.tokens

let tokenize source =
  let scanner = make_scanner source in
  let tokens = scan_tokens scanner in
  if scanner.had_error then Error (tokens, "Tokenization error") else Ok tokens

let token_printer tokens =
  List.fold_left
    (fun acc token -> acc ^ token_info_to_str token ^ "\n")
    "" tokens
