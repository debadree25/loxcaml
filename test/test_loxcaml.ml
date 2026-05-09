open Token

(* ============================================================ *)
(*                          helpers                              *)
(* ============================================================ *)

let token_types tokens = List.map (fun t -> t.ttype) tokens
let pp_token ppf t = Format.fprintf ppf "%s" (token_type_to_name_str t)
let token_eq = Alcotest.testable pp_token ( = )
let tokens_t = Alcotest.list token_eq

let with_silenced_stderr f =
  let saved = Unix.dup Unix.stderr in
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o600 in
  Unix.dup2 dev_null Unix.stderr;
  Unix.close dev_null;
  let finally () =
    flush stderr;
    Unix.dup2 saved Unix.stderr;
    Unix.close saved
  in
  Fun.protect ~finally f

let with_captured_stdout f =
  let tmp = Filename.temp_file "loxcaml-test" ".out" in
  let saved = Unix.dup Unix.stdout in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  Unix.dup2 fd Unix.stdout;
  Unix.close fd;
  let finally () =
    flush stdout;
    Unix.dup2 saved Unix.stdout;
    Unix.close saved
  in
  Fun.protect ~finally f;
  let ic = open_in tmp in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Sys.remove tmp;
  Bytes.to_string buf

let ok_tokens source =
  match Scanner.tokenize source with
  | Ok ts -> ts
  | Error _ -> Alcotest.failf "tokenize failed: %S" source

let parse_ok source =
  match Scanner.tokenize source with
  | Error _ -> Alcotest.failf "tokenize failed: %S" source
  | Ok ts -> (
      match Parser.parse_tokens ts with
      | Ok stmts -> stmts
      | Error _ -> Alcotest.failf "parse failed: %S" source)

let parse_fails source =
  match Scanner.tokenize source with
  | Error _ -> ()
  | Ok ts -> (
      match with_silenced_stderr (fun () -> Parser.parse_tokens ts) with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "expected parse error: %S" source)

let run_program source =
  let stmts = parse_ok source in
  with_captured_stdout (fun () ->
      match Interpreter.interpreter stmts with
      | Interpreter.Ok _ -> ()
      | Interpreter.Error _ -> Alcotest.fail "runtime error"
      | Interpreter.Return _ -> Alcotest.fail "unexpected top-level return")

let run_program_expect_runtime_error source =
  let stmts = parse_ok source in
  let outcome = ref `Unset in
  let _ : string =
    with_captured_stdout (fun () ->
        with_silenced_stderr (fun () ->
            outcome :=
              match Interpreter.interpreter stmts with
              | Interpreter.Error _ -> `Error
              | Interpreter.Ok _ -> `Ok
              | Interpreter.Return _ -> `Return))
  in
  match !outcome with
  | `Error -> ()
  | _ -> Alcotest.failf "expected runtime error: %S" source

(* ============================================================ *)
(*                          scanner                              *)
(* ============================================================ *)

let test_scanner_empty () =
  Alcotest.check tokens_t "just EOF" [ EOF ] (token_types (ok_tokens ""))

let test_scanner_only_whitespace () =
  Alcotest.check tokens_t "spaces, tabs, newlines collapse" [ EOF ]
    (token_types (ok_tokens "   \t\t\n\n  "))

let test_scanner_punctuation () =
  Alcotest.check tokens_t "single-char punctuation"
    [ LEFT_PAREN; RIGHT_PAREN; LEFT_BRACE; RIGHT_BRACE; COMMA; DOT; SEMICOLON; EOF ]
    (token_types (ok_tokens "(){},.;"))

let test_scanner_arithmetic_ops () =
  Alcotest.check tokens_t "arithmetic operators"
    [ MINUS; PLUS; STAR; SLASH; EOF ]
    (token_types (ok_tokens "- + * /"))

let test_scanner_one_vs_two_char_eq () =
  Alcotest.check tokens_t "= alone vs ==" [ EQUAL; EQUAL_EQUAL; EOF ]
    (token_types (ok_tokens "= =="))

let test_scanner_one_vs_two_char_bang () =
  Alcotest.check tokens_t "! alone vs !=" [ BANG; BANG_EQUAL; EOF ]
    (token_types (ok_tokens "! !="))

let test_scanner_one_vs_two_char_less () =
  Alcotest.check tokens_t "< alone vs <=" [ LESS; LESS_EQUAL; EOF ]
    (token_types (ok_tokens "< <="))

let test_scanner_one_vs_two_char_greater () =
  Alcotest.check tokens_t "> alone vs >=" [ GREATER; GREATER_EQUAL; EOF ]
    (token_types (ok_tokens "> >="))

let test_scanner_two_char_operators_grouped () =
  Alcotest.check tokens_t "all two-char comparison operators"
    [ BANG_EQUAL; EQUAL_EQUAL; LESS_EQUAL; GREATER_EQUAL; EOF ]
    (token_types (ok_tokens "!= == <= >="))

let test_scanner_slash_vs_comment () =
  Alcotest.check tokens_t "/ is division but // is comment"
    [ NUMBER "1"; SLASH; NUMBER "2"; EOF ]
    (token_types (ok_tokens "1 / 2 // a comment"))

let test_scanner_string_literal () =
  let tokens = ok_tokens "\"hello\"" in
  match List.map (fun t -> t.ttype) tokens with
  | [ STRING s; EOF ] -> Alcotest.(check string) "string content" "hello" s
  | _ -> Alcotest.fail "expected STRING then EOF"

let test_scanner_empty_string () =
  let tokens = ok_tokens "\"\"" in
  match List.map (fun t -> t.ttype) tokens with
  | [ STRING ""; EOF ] -> ()
  | _ -> Alcotest.fail "expected empty STRING then EOF"

let test_scanner_string_spans_lines () =
  let tokens = ok_tokens "\"line1\nline2\"" in
  match tokens with
  | [ { ttype = STRING s; line = ln; _ }; { ttype = EOF; line = eof_line; _ } ] ->
      Alcotest.(check string) "string content" "line1\nline2" s;
      Alcotest.(check int) "line on which string ends" 2 ln;
      Alcotest.(check int) "EOF line" 2 eof_line
  | _ -> Alcotest.fail "expected STRING then EOF"

let test_scanner_unterminated_string () =
  match Scanner.tokenize "\"oops" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected unterminated string error"

let test_scanner_integer_number () =
  match ok_tokens "1234" with
  | [ { ttype = NUMBER raw; literal = Some (LNumber n); _ }; { ttype = EOF; _ } ] ->
      Alcotest.(check string) "raw" "1234" raw;
      Alcotest.(check (float 1e-9)) "value" 1234.0 n
  | _ -> Alcotest.fail "expected NUMBER then EOF"

let test_scanner_decimal_number () =
  match ok_tokens "42.5" with
  | [ { ttype = NUMBER raw; literal = Some (LNumber n); _ }; _ ] ->
      Alcotest.(check string) "raw" "42.5" raw;
      Alcotest.(check (float 1e-9)) "value" 42.5 n
  | _ -> Alcotest.fail "expected NUMBER then EOF"

let test_scanner_number_dot_no_fraction () =
  (* "1." should parse as NUMBER(1) then DOT — consume_number requires digit after . *)
  Alcotest.check tokens_t "1. = NUMBER + DOT"
    [ NUMBER "1"; DOT; EOF ]
    (token_types (ok_tokens "1."))

let test_scanner_keywords_individually () =
  let pairs =
    [ ("and", AND); ("class", CLASS); ("else", ELSE); ("false", FALSE);
      ("fun", FUN); ("for", FOR); ("if", IF); ("nil", NIL); ("or", OR);
      ("print", PRINT); ("return", RETURN); ("super", SUPER); ("this", THIS);
      ("true", TRUE); ("var", VAR); ("while", WHILE) ]
  in
  List.iter
    (fun (src, expected) ->
      Alcotest.check tokens_t src [ expected; EOF ] (token_types (ok_tokens src)))
    pairs

let test_scanner_identifier_with_underscore () =
  match ok_tokens "_foo_bar2" with
  | [ { ttype = IDENTIFIER s; _ }; _ ] ->
      Alcotest.(check string) "identifier name" "_foo_bar2" s
  | _ -> Alcotest.fail "expected IDENTIFIER then EOF"

let test_scanner_keyword_prefix_is_identifier () =
  (* "varX" must be a single identifier, not VAR + X *)
  match ok_tokens "varX" with
  | [ { ttype = IDENTIFIER "varX"; _ }; _ ] -> ()
  | toks ->
      Alcotest.failf "expected IDENTIFIER varX but got %s"
        (String.concat "," (List.map (fun t -> token_type_to_name_str t.ttype) toks))

let test_scanner_uppercase_keyword_is_identifier () =
  (* lox keywords are lowercase; "VAR" is an identifier *)
  match ok_tokens "VAR" with
  | [ { ttype = IDENTIFIER "VAR"; _ }; _ ] -> ()
  | _ -> Alcotest.fail "expected IDENTIFIER VAR"

let test_scanner_skips_line_comment () =
  Alcotest.check tokens_t "comments are skipped"
    [ NUMBER "1"; PLUS; NUMBER "2"; EOF ]
    (token_types (ok_tokens "1 + 2 // trailing comment"))

let test_scanner_unexpected_char_errors () =
  match Scanner.tokenize "@" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected scanner error on '@'"

let test_scanner_line_tracking () =
  let tokens = ok_tokens "1\n+\n2" in
  let lines = List.map (fun t -> t.line) tokens in
  Alcotest.(check (list int)) "line per token" [ 1; 2; 3; 3 ] lines

(* ============================================================ *)
(*                          parser                               *)
(* ============================================================ *)

let test_parser_empty_program () =
  Alcotest.(check int) "no statements" 0 (List.length (parse_ok ""))

let test_parser_expression_statement () =
  match parse_ok "1 + 2;" with
  | [ Ast.Expression (Ast.Binary (Ast.Literal (LNumber 1.), PLUS, Ast.Literal (LNumber 2.), _)) ]
    ->
      ()
  | _ -> Alcotest.fail "expected (1 + 2) expression statement"

let test_parser_grouping () =
  match parse_ok "(1 + 2) * 3;" with
  | [ Ast.Expression
        (Ast.Binary
           (Ast.Grouping (Ast.Binary _), STAR, Ast.Literal (LNumber 3.), _)) ] ->
      ()
  | _ -> Alcotest.fail "expected grouped binary"

let test_parser_unary_minus () =
  match parse_ok "-5;" with
  | [ Ast.Expression (Ast.Unary (MINUS, Ast.Literal (LNumber 5.), _)) ] -> ()
  | _ -> Alcotest.fail "expected unary minus"

let test_parser_unary_bang () =
  match parse_ok "!true;" with
  | [ Ast.Expression (Ast.Unary (BANG, Ast.Literal (LBool true), _)) ] -> ()
  | _ -> Alcotest.fail "expected unary bang"

let test_parser_literal_kinds () =
  match parse_ok "true; false; nil;" with
  | [ Ast.Expression (Ast.Literal (LBool true));
      Ast.Expression (Ast.Literal (LBool false));
      Ast.Expression (Ast.Literal LNil) ] ->
      ()
  | _ -> Alcotest.fail "expected three literal statements"

let test_parser_string_literal () =
  match parse_ok "\"hi\";" with
  | [ Ast.Expression (Ast.Literal (LString "hi")) ] -> ()
  | _ -> Alcotest.fail "expected string literal expression"

let test_parser_var_decl () =
  match parse_ok "var x = 3;" with
  | [ Ast.Var (IDENTIFIER "x", _, Some (Ast.Literal (LNumber 3.))) ] -> ()
  | _ -> Alcotest.fail "expected var x = 3"

let test_parser_var_decl_no_initializer () =
  (* Parser desugars to `Var (..., Some (Literal LNil))` *)
  match parse_ok "var x;" with
  | [ Ast.Var (IDENTIFIER "x", _, Some (Ast.Literal LNil)) ] -> ()
  | _ -> Alcotest.fail "expected var x; with implicit nil"

let test_parser_assignment () =
  match parse_ok "x = 5;" with
  | [ Ast.Expression (Ast.Assign (IDENTIFIER "x", _, Ast.Literal (LNumber 5.))) ]
    ->
      ()
  | _ -> Alcotest.fail "expected assignment"

let test_parser_assignment_right_assoc () =
  (* a = b = 1 should parse as a = (b = 1) *)
  match parse_ok "a = b = 1;" with
  | [ Ast.Expression
        (Ast.Assign
           (IDENTIFIER "a", _, Ast.Assign (IDENTIFIER "b", _, Ast.Literal _))) ]
    ->
      ()
  | _ -> Alcotest.fail "expected right-associative assignment"

let test_parser_invalid_assignment_target () =
  parse_fails "1 = 2;"

let test_parser_precedence () =
  match parse_ok "1 + 2 * 3;" with
  | [ Ast.Expression
        (Ast.Binary
           ( Ast.Literal (LNumber 1.),
             PLUS,
             Ast.Binary (Ast.Literal (LNumber 2.), STAR, Ast.Literal (LNumber 3.), _),
             _ )) ] ->
      ()
  | _ -> Alcotest.fail "operator precedence is wrong"

let test_parser_comparison_chain () =
  (* a < b == c parses as (a < b) == c — equality is lower precedence *)
  match parse_ok "a < b == c;" with
  | [ Ast.Expression
        (Ast.Binary (Ast.Binary (_, LESS, _, _), EQUAL_EQUAL, _, _)) ] ->
      ()
  | _ -> Alcotest.fail "expected (a < b) == c"

let test_parser_logical_or_and () =
  (* a or b and c parses as a or (b and c) *)
  match parse_ok "a or b and c;" with
  | [ Ast.Expression (Ast.Logical (_, OR, Ast.Logical (_, AND, _, _), _)) ] -> ()
  | _ -> Alcotest.fail "expected a or (b and c)"

let test_parser_block () =
  match parse_ok "{ var x = 1; print x; }" with
  | [ Ast.Block [ Ast.Var (IDENTIFIER "x", _, _); Ast.Print _ ] ] -> ()
  | _ -> Alcotest.fail "expected a block with two statements"

let test_parser_empty_block () =
  match parse_ok "{}" with
  | [ Ast.Block [] ] -> ()
  | _ -> Alcotest.fail "expected empty block"

let test_parser_if_no_else () =
  match parse_ok "if (true) print 1;" with
  | [ Ast.If (Ast.Literal (LBool true), Ast.Print _, None) ] -> ()
  | _ -> Alcotest.fail "expected if without else"

let test_parser_if_else () =
  match parse_ok "if (a) print 1; else print 2;" with
  | [ Ast.If (Ast.Variable _, Ast.Print _, Some (Ast.Print _)) ] -> ()
  | _ -> Alcotest.fail "expected if/else"

let test_parser_while () =
  match parse_ok "while (a) print 1;" with
  | [ Ast.While (Ast.Variable _, Ast.Print _) ] -> ()
  | _ -> Alcotest.fail "expected while"

let test_parser_for_desugars_to_while () =
  (* for (var i = 0; i < 3; i = i + 1) print i;
     desugars to: Block [Var i; While (cond, Block [Print i; Expression incr])] *)
  match parse_ok "for (var i = 0; i < 3; i = i + 1) print i;" with
  | [ Ast.Block
        [ Ast.Var (IDENTIFIER "i", _, _);
          Ast.While (_, Ast.Block [ Ast.Print _; Ast.Expression _ ]) ] ] ->
      ()
  | _ -> Alcotest.fail "for did not desugar as expected"

let test_parser_for_no_clauses () =
  (* for (;;) body — initializer None, condition true, increment None *)
  match parse_ok "for (;;) print 1;" with
  | [ Ast.While (Ast.Literal (LBool true), Ast.Print _) ] -> ()
  | _ -> Alcotest.fail "for(;;) did not desugar to While(true, body)"

let test_parser_function_decl () =
  match parse_ok "fun f(a, b) { return a + b; }" with
  | [ Ast.Function (IDENTIFIER "f", _, [ _; _ ], Ast.Block _) ] -> ()
  | _ -> Alcotest.fail "expected function declaration"

let test_parser_function_no_params () =
  match parse_ok "fun f() {}" with
  | [ Ast.Function (IDENTIFIER "f", _, [], Ast.Block []) ] -> ()
  | _ -> Alcotest.fail "expected zero-arg function"

let test_parser_call_no_args () =
  match parse_ok "f();" with
  | [ Ast.Expression (Ast.Call (Ast.Variable _, [], _)) ] -> ()
  | _ -> Alcotest.fail "expected zero-arg call"

let test_parser_call_with_args () =
  match parse_ok "f(1, 2, 3);" with
  | [ Ast.Expression (Ast.Call (Ast.Variable _, args, _)) ] ->
      Alcotest.(check int) "arity" 3 (List.length args)
  | _ -> Alcotest.fail "expected three-arg call"

let test_parser_chained_call () =
  match parse_ok "f()();" with
  | [ Ast.Expression (Ast.Call (Ast.Call (Ast.Variable _, [], _), [], _)) ] -> ()
  | _ -> Alcotest.fail "expected chained call f()()"

let test_parser_return_with_value () =
  match parse_ok "fun f() { return 5; }" with
  | [ Ast.Function (_, _, _, Ast.Block [ Ast.Return (_, Some (Ast.Literal _)) ]) ]
    ->
      ()
  | _ -> Alcotest.fail "expected return with value"

let test_parser_return_no_value () =
  (* `return;` desugars to Return (_, Some (Literal LNil)) *)
  match parse_ok "fun f() { return; }" with
  | [ Ast.Function
        ( _, _, _,
          Ast.Block [ Ast.Return (_, Some (Ast.Literal LNil)) ] ) ] ->
      ()
  | _ -> Alcotest.fail "expected bare return desugared to return nil"

let test_parser_print () =
  match parse_ok "print 42;" with
  | [ Ast.Print (Ast.Literal (LNumber 42.)) ] -> ()
  | _ -> Alcotest.fail "expected print 42"

let test_parser_missing_semicolon_var_decl () = parse_fails "var x = 1"
let test_parser_unclosed_paren () = parse_fails "(1 + 2;"
let test_parser_unclosed_block () = parse_fails "{ var x = 1;"
let test_parser_missing_expression () = parse_fails "1 + ;"

(* ============================================================ *)
(*                        interpreter                            *)
(* ============================================================ *)

let test_interp_print_number_integer () =
  Alcotest.(check string) "integer prints without .0" "5\n"
    (run_program "print 5;")

let test_interp_print_number_float () =
  Alcotest.(check string) "non-integer prints with fraction" "1.5\n"
    (run_program "print 1.5;")

let test_interp_print_bool () =
  Alcotest.(check string) "true / false" "true\nfalse\n"
    (run_program "print true; print false;")

let test_interp_print_nil () =
  Alcotest.(check string) "nil literal" "nil\n" (run_program "print nil;")

let test_interp_print_string () =
  Alcotest.(check string) "strings print without quotes" "hi\n"
    (run_program "print \"hi\";")

let test_interp_arithmetic_add () =
  Alcotest.(check string) "+" "7\n" (run_program "print 3 + 4;")

let test_interp_arithmetic_sub () =
  Alcotest.(check string) "-" "1\n" (run_program "print 5 - 4;")

let test_interp_arithmetic_mul () =
  Alcotest.(check string) "*" "12\n" (run_program "print 3 * 4;")

let test_interp_arithmetic_div () =
  Alcotest.(check string) "/" "2.5\n" (run_program "print 5 / 2;")

let test_interp_precedence () =
  Alcotest.(check string) "1 + 2 * 3" "7\n" (run_program "print 1 + 2 * 3;")

let test_interp_grouping () =
  Alcotest.(check string) "(1 + 2) * 3" "9\n" (run_program "print (1 + 2) * 3;")

let test_interp_unary_negate () =
  Alcotest.(check string) "negation" "-5\n" (run_program "print -5;")

let test_interp_unary_negate_double () =
  Alcotest.(check string) "double negation" "5\n" (run_program "print --5;")

let test_interp_unary_bang () =
  Alcotest.(check string) "!true / !false / !nil" "false\ntrue\ntrue\n"
    (run_program "print !true; print !false; print !nil;")

let test_interp_string_concat () =
  Alcotest.(check string) "concat" "foobar\n"
    (run_program "print \"foo\" + \"bar\";")

let test_interp_comparisons () =
  Alcotest.(check string) "all six comparisons"
    "true\nfalse\ntrue\nfalse\ntrue\nfalse\n"
    (run_program
       "print 1 < 2; print 2 < 1; print 1 <= 1; print 2 <= 1; print 2 > 1; \
        print 1 > 2;")

let test_interp_equality_same_types () =
  Alcotest.(check string) "==/!= within types"
    "true\nfalse\ntrue\nfalse\ntrue\nfalse\n"
    (run_program
       "print 1 == 1; print 1 != 1; print \"a\" == \"a\"; print \"a\" != \
        \"a\"; print true == true; print true != true;")

let test_interp_equality_mixed_types () =
  (* mismatched types compare unequal *)
  Alcotest.(check string) "1 == \"1\" is false" "false\ntrue\n"
    (run_program "print 1 == \"1\"; print 1 != \"1\";")

let test_interp_nil_equality () =
  Alcotest.(check string) "nil == nil" "true\nfalse\n"
    (run_program "print nil == nil; print nil != nil;")

let test_interp_logical_or_short_circuit () =
  (* `or` returns left if truthy, otherwise right (returns the value, not bool) *)
  Alcotest.(check string) "or returns first truthy" "1\nhi\nnil\n"
    (run_program "print 1 or 2; print nil or \"hi\"; print nil or nil;")

let test_interp_logical_and_short_circuit () =
  Alcotest.(check string) "and returns first falsy or last" "nil\nhi\nfalse\n"
    (run_program "print nil and 1; print 1 and \"hi\"; print false and 1;")

let test_interp_truthiness_zero_is_falsy () =
  (* NOTE: This deviates from the Lox spec (where only nil/false are falsy).
     The current `is_truthy` treats `LNumber 0.0` as falsy. *)
  Alcotest.(check string) "0 is falsy in this implementation" "no\n"
    (run_program "if (0) print \"yes\"; else print \"no\";")

let test_interp_truthiness_nonzero_is_truthy () =
  Alcotest.(check string) "non-zero number is truthy" "yes\n"
    (run_program "if (1) print \"yes\"; else print \"no\";")

let test_interp_truthiness_empty_string () =
  (* `LString _` is always truthy — empty string included *)
  Alcotest.(check string) "empty string is truthy" "yes\n"
    (run_program "if (\"\") print \"yes\"; else print \"no\";")

let test_interp_truthiness_nil_falsy () =
  Alcotest.(check string) "nil is falsy" "no\n"
    (run_program "if (nil) print \"yes\"; else print \"no\";")

let test_interp_var_default_nil () =
  Alcotest.(check string) "uninitialized var is nil" "nil\n"
    (run_program "var x; print x;")

let test_interp_assignment_value () =
  (* assignment is an expression that returns the assigned value *)
  Alcotest.(check string) "x = 5 returns 5" "5\n5\n"
    (run_program "var x; print x = 5; print x;")

let test_interp_block_scoping () =
  Alcotest.(check string) "inner var doesn't leak" "inner\nouter\n"
    (run_program
       "var x = \"outer\"; { var x = \"inner\"; print x; } print x;")

let test_interp_assignment_in_inner_scope_modifies_outer () =
  Alcotest.(check string) "inner assignment hits outer var" "modified\n"
    (run_program "var x = \"orig\"; { x = \"modified\"; } print x;")

let test_interp_if_else () =
  Alcotest.(check string) "else branch" "no\n"
    (run_program "if (false) print \"yes\"; else print \"no\";")

let test_interp_if_no_else_skipped () =
  Alcotest.(check string) "if without else, false condition prints nothing" ""
    (run_program "if (false) print \"x\";")

let test_interp_while_loop () =
  Alcotest.(check string) "while counts down" "3\n2\n1\n"
    (run_program "var i = 3; while (i > 0) { print i; i = i - 1; }")

let test_interp_for_loop () =
  Alcotest.(check string) "for sums 1..5" "15\n"
    (run_program
       "var s = 0; for (var i = 1; i <= 5; i = i + 1) s = s + i; print s;")

let test_interp_function_call () =
  Alcotest.(check string) "function returns value" "5\n"
    (run_program "fun add(a, b) { return a + b; } print add(2, 3);")

let test_interp_function_no_explicit_return_is_nil () =
  Alcotest.(check string) "fall-through return" "nil\n"
    (run_program "fun f() {} print f();")

let test_interp_recursion_factorial () =
  Alcotest.(check string) "factorial(5) = 120" "120\n"
    (run_program
       "fun fact(n) { if (n <= 1) return 1; return n * fact(n - 1); } print \
        fact(5);")

let test_interp_higher_order_pass_function () =
  Alcotest.(check string) "function passed as arg" "7\n"
    (run_program
       "fun apply(f, x) { return f(x); } fun inc(n) { return n + 1; } print \
        apply(inc, 6);")

let test_interp_higher_order_return_function () =
  Alcotest.(check string) "function returned" "5\n"
    (run_program
       "fun adder(a) { fun add(b) { return a + b; } return add; } print \
        adder(2)(3);")

let test_interp_closure_captures_environment () =
  Alcotest.(check string) "counter captures i" "1\n2\n3\n"
    (run_program
       "fun makeCounter() { var i = 0; fun count() { i = i + 1; print i; } \
        return count; } var c = makeCounter(); c(); c(); c();")

let test_interp_closure_two_independent_counters () =
  Alcotest.(check string) "two counters do not share state" "1\n1\n2\n"
    (run_program
       "fun make() { var i = 0; fun f() { i = i + 1; print i; } return f; } \
        var a = make(); var b = make(); a(); b(); a();")

let test_interp_print_function_value () =
  Alcotest.(check string) "user function prints as <fn name>" "<fn f>\n"
    (run_program "fun f() {} print f;")

let test_interp_print_native_function_value () =
  Alcotest.(check string) "native function prints as <native fn>"
    "<native fn>\n" (run_program "print clock;")

let test_interp_clock_returns_number () =
  (* don't pin the value, just confirm it produces output that ends with \n
     and parses back to a number-ish string *)
  let out = run_program "print clock();" in
  let trimmed = String.trim out in
  match float_of_string_opt trimmed with
  | Some _ -> ()
  | None -> Alcotest.failf "clock() should print a number, got %S" out

(* ---- runtime errors ---- *)

let test_interp_undefined_variable () =
  run_program_expect_runtime_error "print x;"

let test_interp_assign_to_undefined () =
  run_program_expect_runtime_error "x = 1;"

let test_interp_unary_minus_on_string () =
  run_program_expect_runtime_error "print -\"hi\";"

let test_interp_add_number_and_string () =
  run_program_expect_runtime_error "print 1 + \"hi\";"

let test_interp_subtract_strings () =
  run_program_expect_runtime_error "print \"a\" - \"b\";"

let test_interp_compare_strings () =
  run_program_expect_runtime_error "print \"a\" < \"b\";"

let test_interp_call_non_function () =
  run_program_expect_runtime_error "var x = 5; x();"

let test_interp_arity_mismatch_too_few () =
  run_program_expect_runtime_error "fun f(a, b) {} f(1);"

let test_interp_arity_mismatch_too_many () =
  run_program_expect_runtime_error "fun f(a) {} f(1, 2);"

(* ============================================================ *)
(*    resolver / variable resolution                             *)
(*                                                               *)
(*  These tests document behaviors that require a static         *)
(*  variable-resolution pass (Crafting Interpreters ch. 11),     *)
(*  which is NOT yet implemented. They are expected to FAIL      *)
(*  until the resolver lands; once it does, they should pass     *)
(*  without modification.                                        *)
(*                                                               *)
(*  `compile_and_run` is the single entry point through which    *)
(*  resolver-related tests probe behaviour. Today it's just      *)
(*  Scanner -> Parser -> Interpreter; once the resolver exists,  *)
(*  insert it between parse and interpret and convert resolver   *)
(*  errors to `Static_error`.                                    *)
(* ============================================================ *)

type run_outcome =
  | Static_error
  | Runtime_error of string  (* captured stdout up to the error *)
  | Ok_output of string

let compile_and_run source =
  match Scanner.tokenize source with
  | Error _ -> Static_error
  | Ok ts -> (
      match with_silenced_stderr (fun () -> Parser.parse_tokens ts) with
      | Error _ -> Static_error
      | Ok stmts ->
          (* TODO(resolver): plug in `Resolver.resolve stmts` here and convert
             its errors to `Static_error`. *)
          let outcome = ref `Pending in
          let captured =
            with_captured_stdout (fun () ->
                with_silenced_stderr (fun () ->
                    outcome :=
                      match Interpreter.interpreter stmts with
                      | Interpreter.Ok _ -> `Ok
                      | Interpreter.Error _ -> `Runtime
                      | Interpreter.Return _ -> `Runtime))
          in
          match !outcome with
          | `Ok -> Ok_output captured
          | `Runtime -> Runtime_error captured
          | `Pending -> Static_error)

let test_resolver_closure_late_binding () =
  (* Crafting Interpreters ch. 11 example: closures capture variables
     lexically. The `var a = "block"` declared *after* showA must not
     change what showA prints. *)
  let source =
    "var a = \"global\";\n\
     {\n\
    \  fun showA() { print a; }\n\
    \  showA();\n\
    \  var a = \"block\";\n\
    \  showA();\n\
     }"
  in
  match compile_and_run source with
  | Ok_output out ->
      Alcotest.(check string)
        "showA captures the global a, not the later-declared block-local a"
        "global\nglobal\n" out
  | Static_error -> Alcotest.fail "unexpected static error"
  | Runtime_error _ -> Alcotest.fail "unexpected runtime error"

let test_resolver_var_in_own_initializer () =
  (* `var a = a;` inside a local scope must be a static error.
     Today, the inner `a` resolves to the outer `a` and the inner
     binding shadows it — the program runs and prints "outer". *)
  match compile_and_run "var a = \"outer\"; { var a = a; print a; }" with
  | Static_error -> ()
  | Ok_output out ->
      Alcotest.failf
        "expected static error for `var a = a` in local scope, but program \
         ran and printed %S"
        out
  | Runtime_error _ ->
      Alcotest.fail
        "expected static error for `var a = a` in local scope, got runtime \
         error instead"

let test_resolver_duplicate_local_var () =
  (* Redeclaring a variable in the same local scope must be a static error.
     Today, `Hashtbl.add` silently shadows and `print a` prints "2". *)
  match compile_and_run "{ var a = 1; var a = 2; print a; }" with
  | Static_error -> ()
  | Ok_output out ->
      Alcotest.failf
        "expected static error for duplicate local var, but program ran and \
         printed %S"
        out
  | Runtime_error _ ->
      Alcotest.fail
        "expected static error for duplicate local var, got runtime error"

let test_resolver_top_level_return () =
  (* `return` outside any function must be a static error.
     Today, the parser accepts it and the interpreter raises it as a
     `Return` value, which main.ml maps to runtime exit 70. *)
  match compile_and_run "return \"no\";" with
  | Static_error -> ()
  | Ok_output _ -> Alcotest.fail "expected static error for top-level return"
  | Runtime_error _ ->
      Alcotest.fail
        "expected static error for top-level return, got runtime error \
         instead (resolver should reject this before interpretation)"

let test_resolver_duplicate_parameter () =
  (* Duplicate parameter names must be a static error. Today the parser
     accepts them and the interpreter binds them via `Hashtbl.add`, which
     silently shadows. *)
  match compile_and_run "fun f(a, a) { print a; } f(1, 2);" with
  | Static_error -> ()
  | Ok_output out ->
      Alcotest.failf
        "expected static error for duplicate parameter, but program ran and \
         printed %S"
        out
  | Runtime_error _ ->
      Alcotest.fail
        "expected static error for duplicate parameter, got runtime error"

(* ============================================================ *)
(*                          suite                                *)
(* ============================================================ *)

let () =
  Alcotest.run "loxcaml"
    [
      ( "scanner",
        [
          Alcotest.test_case "empty input" `Quick test_scanner_empty;
          Alcotest.test_case "only whitespace" `Quick test_scanner_only_whitespace;
          Alcotest.test_case "punctuation" `Quick test_scanner_punctuation;
          Alcotest.test_case "arithmetic ops" `Quick test_scanner_arithmetic_ops;
          Alcotest.test_case "= vs ==" `Quick test_scanner_one_vs_two_char_eq;
          Alcotest.test_case "! vs !=" `Quick test_scanner_one_vs_two_char_bang;
          Alcotest.test_case "< vs <=" `Quick test_scanner_one_vs_two_char_less;
          Alcotest.test_case "> vs >=" `Quick test_scanner_one_vs_two_char_greater;
          Alcotest.test_case "two-char operators" `Quick
            test_scanner_two_char_operators_grouped;
          Alcotest.test_case "/ vs //" `Quick test_scanner_slash_vs_comment;
          Alcotest.test_case "string literal" `Quick test_scanner_string_literal;
          Alcotest.test_case "empty string" `Quick test_scanner_empty_string;
          Alcotest.test_case "string spanning lines" `Quick
            test_scanner_string_spans_lines;
          Alcotest.test_case "unterminated string" `Quick
            test_scanner_unterminated_string;
          Alcotest.test_case "integer number" `Quick test_scanner_integer_number;
          Alcotest.test_case "decimal number" `Quick test_scanner_decimal_number;
          Alcotest.test_case "1. is NUMBER + DOT" `Quick
            test_scanner_number_dot_no_fraction;
          Alcotest.test_case "all keywords" `Quick
            test_scanner_keywords_individually;
          Alcotest.test_case "identifier with underscores" `Quick
            test_scanner_identifier_with_underscore;
          Alcotest.test_case "varX is identifier, not VAR + X" `Quick
            test_scanner_keyword_prefix_is_identifier;
          Alcotest.test_case "uppercase keyword is identifier" `Quick
            test_scanner_uppercase_keyword_is_identifier;
          Alcotest.test_case "line comment" `Quick test_scanner_skips_line_comment;
          Alcotest.test_case "unexpected char errors" `Quick
            test_scanner_unexpected_char_errors;
          Alcotest.test_case "line tracking" `Quick test_scanner_line_tracking;
        ] );
      ( "parser",
        [
          Alcotest.test_case "empty program" `Quick test_parser_empty_program;
          Alcotest.test_case "expression statement" `Quick
            test_parser_expression_statement;
          Alcotest.test_case "grouping" `Quick test_parser_grouping;
          Alcotest.test_case "unary minus" `Quick test_parser_unary_minus;
          Alcotest.test_case "unary bang" `Quick test_parser_unary_bang;
          Alcotest.test_case "literal kinds" `Quick test_parser_literal_kinds;
          Alcotest.test_case "string literal" `Quick test_parser_string_literal;
          Alcotest.test_case "var decl" `Quick test_parser_var_decl;
          Alcotest.test_case "var decl no initializer" `Quick
            test_parser_var_decl_no_initializer;
          Alcotest.test_case "assignment" `Quick test_parser_assignment;
          Alcotest.test_case "assignment is right-assoc" `Quick
            test_parser_assignment_right_assoc;
          Alcotest.test_case "invalid assignment target" `Quick
            test_parser_invalid_assignment_target;
          Alcotest.test_case "operator precedence" `Quick test_parser_precedence;
          Alcotest.test_case "comparison vs equality precedence" `Quick
            test_parser_comparison_chain;
          Alcotest.test_case "logical or/and precedence" `Quick
            test_parser_logical_or_and;
          Alcotest.test_case "block" `Quick test_parser_block;
          Alcotest.test_case "empty block" `Quick test_parser_empty_block;
          Alcotest.test_case "if without else" `Quick test_parser_if_no_else;
          Alcotest.test_case "if/else" `Quick test_parser_if_else;
          Alcotest.test_case "while" `Quick test_parser_while;
          Alcotest.test_case "for desugars to while" `Quick
            test_parser_for_desugars_to_while;
          Alcotest.test_case "for(;;) is while(true)" `Quick
            test_parser_for_no_clauses;
          Alcotest.test_case "function declaration" `Quick test_parser_function_decl;
          Alcotest.test_case "function with no params" `Quick
            test_parser_function_no_params;
          Alcotest.test_case "call with no args" `Quick test_parser_call_no_args;
          Alcotest.test_case "call with multiple args" `Quick
            test_parser_call_with_args;
          Alcotest.test_case "chained call" `Quick test_parser_chained_call;
          Alcotest.test_case "return with value" `Quick test_parser_return_with_value;
          Alcotest.test_case "bare return" `Quick test_parser_return_no_value;
          Alcotest.test_case "print" `Quick test_parser_print;
          Alcotest.test_case "missing semicolon (var)" `Quick
            test_parser_missing_semicolon_var_decl;
          Alcotest.test_case "unclosed paren" `Quick test_parser_unclosed_paren;
          Alcotest.test_case "unclosed block" `Quick test_parser_unclosed_block;
          Alcotest.test_case "missing expression after +" `Quick
            test_parser_missing_expression;
        ] );
      ( "interpreter",
        [
          Alcotest.test_case "print integer number" `Quick
            test_interp_print_number_integer;
          Alcotest.test_case "print float number" `Quick
            test_interp_print_number_float;
          Alcotest.test_case "print booleans" `Quick test_interp_print_bool;
          Alcotest.test_case "print nil" `Quick test_interp_print_nil;
          Alcotest.test_case "print string (no quotes)" `Quick
            test_interp_print_string;
          Alcotest.test_case "+" `Quick test_interp_arithmetic_add;
          Alcotest.test_case "-" `Quick test_interp_arithmetic_sub;
          Alcotest.test_case "*" `Quick test_interp_arithmetic_mul;
          Alcotest.test_case "/" `Quick test_interp_arithmetic_div;
          Alcotest.test_case "operator precedence at runtime" `Quick
            test_interp_precedence;
          Alcotest.test_case "grouping at runtime" `Quick test_interp_grouping;
          Alcotest.test_case "unary negate" `Quick test_interp_unary_negate;
          Alcotest.test_case "double unary negate" `Quick
            test_interp_unary_negate_double;
          Alcotest.test_case "unary !" `Quick test_interp_unary_bang;
          Alcotest.test_case "string concat" `Quick test_interp_string_concat;
          Alcotest.test_case "all comparisons" `Quick test_interp_comparisons;
          Alcotest.test_case "equality same types" `Quick
            test_interp_equality_same_types;
          Alcotest.test_case "equality mixed types" `Quick
            test_interp_equality_mixed_types;
          Alcotest.test_case "nil equality" `Quick test_interp_nil_equality;
          Alcotest.test_case "or short-circuit" `Quick
            test_interp_logical_or_short_circuit;
          Alcotest.test_case "and short-circuit" `Quick
            test_interp_logical_and_short_circuit;
          Alcotest.test_case "0 is falsy (impl quirk)" `Quick
            test_interp_truthiness_zero_is_falsy;
          Alcotest.test_case "non-zero is truthy" `Quick
            test_interp_truthiness_nonzero_is_truthy;
          Alcotest.test_case "empty string is truthy" `Quick
            test_interp_truthiness_empty_string;
          Alcotest.test_case "nil is falsy" `Quick test_interp_truthiness_nil_falsy;
          Alcotest.test_case "uninitialized var is nil" `Quick
            test_interp_var_default_nil;
          Alcotest.test_case "assignment expression value" `Quick
            test_interp_assignment_value;
          Alcotest.test_case "block scoping" `Quick test_interp_block_scoping;
          Alcotest.test_case "inner scope assigns to outer" `Quick
            test_interp_assignment_in_inner_scope_modifies_outer;
          Alcotest.test_case "if/else" `Quick test_interp_if_else;
          Alcotest.test_case "if without else, false" `Quick
            test_interp_if_no_else_skipped;
          Alcotest.test_case "while loop" `Quick test_interp_while_loop;
          Alcotest.test_case "for loop" `Quick test_interp_for_loop;
          Alcotest.test_case "function call" `Quick test_interp_function_call;
          Alcotest.test_case "function fall-through is nil" `Quick
            test_interp_function_no_explicit_return_is_nil;
          Alcotest.test_case "recursion (factorial)" `Quick
            test_interp_recursion_factorial;
          Alcotest.test_case "function as argument" `Quick
            test_interp_higher_order_pass_function;
          Alcotest.test_case "function return" `Quick
            test_interp_higher_order_return_function;
          Alcotest.test_case "closure" `Quick
            test_interp_closure_captures_environment;
          Alcotest.test_case "two independent closures" `Quick
            test_interp_closure_two_independent_counters;
          Alcotest.test_case "print user function" `Quick
            test_interp_print_function_value;
          Alcotest.test_case "print native function" `Quick
            test_interp_print_native_function_value;
          Alcotest.test_case "clock() returns a number" `Quick
            test_interp_clock_returns_number;
          Alcotest.test_case "undefined variable" `Quick
            test_interp_undefined_variable;
          Alcotest.test_case "assign to undefined" `Quick
            test_interp_assign_to_undefined;
          Alcotest.test_case "unary - on string" `Quick
            test_interp_unary_minus_on_string;
          Alcotest.test_case "number + string" `Quick
            test_interp_add_number_and_string;
          Alcotest.test_case "string - string" `Quick test_interp_subtract_strings;
          Alcotest.test_case "string < string" `Quick test_interp_compare_strings;
          Alcotest.test_case "calling non-function" `Quick
            test_interp_call_non_function;
          Alcotest.test_case "arity too few" `Quick
            test_interp_arity_mismatch_too_few;
          Alcotest.test_case "arity too many" `Quick
            test_interp_arity_mismatch_too_many;
        ] );
      ( "resolver (not yet implemented)",
        [
          Alcotest.test_case "closure captures lexical a (not late binding)"
            `Quick test_resolver_closure_late_binding;
          Alcotest.test_case "var a = a in local scope is a static error"
            `Quick test_resolver_var_in_own_initializer;
          Alcotest.test_case "duplicate local var is a static error" `Quick
            test_resolver_duplicate_local_var;
          Alcotest.test_case "top-level return is a static error" `Quick
            test_resolver_top_level_return;
          Alcotest.test_case "duplicate parameter is a static error" `Quick
            test_resolver_duplicate_parameter;
        ] );
    ]
