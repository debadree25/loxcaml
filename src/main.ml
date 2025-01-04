open Scanner
open Token
open Parser
open Ast

let tokenize_handler file_contents =
  match tokenize file_contents with
  | Ok tokens ->
      print_endline (token_printer tokens);
      Ok ()
  | Error (tokens, _) -> (
      print_endline (token_printer tokens);
      Error 65)

let parse_handler file_contents =
  match tokenize file_contents with
  | Ok tokens -> (
      match parse_tokens tokens with
      | Ok expr ->
          print_endline (ast_printer expr);
          Ok ()
      | Error _ -> Error 65)
  | Error _ -> Error 65

let command_handler command file_contents =
  match command with
  | "tokenize" -> tokenize_handler file_contents
  | "parse" -> parse_handler file_contents
  | _ ->
      Printf.eprintf "Unknown command: %s\n" command;
      Error 1

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let file_contents = In_channel.with_open_text filename In_channel.input_all in

  if String.length file_contents > 0 then
    match command_handler command file_contents with
    | Ok () -> ()
    | Error code -> exit code
  else print_endline (token_info_to_str (make_token_info EOF None 1))
