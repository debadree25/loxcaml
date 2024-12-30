open Scanner
open Token

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  if String.length file_contents > 0 then (
    let tokenize_result = tokenize file_contents in
    List.iter
      (fun t -> print_endline (token_info_to_str t))
      tokenize_result.tokens;
    if tokenize_result.had_error then exit 65 else ())
  else print_endline (token_info_to_str (make_token_info EOF None 1))
