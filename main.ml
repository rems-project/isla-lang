(****************************************************************************)
(*  BSD 2-Clause License                                                    *)
(*                                                                          *)
(*  Copyright (c) 2020-2021 Alasdair Armstrong                              *)
(*  Copyright (c) 2020-2021 Brian Campbell                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                  *)
(*  Copyright (c) 2020-2021 Peter Sewell                                    *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                   *)
(*  Copyright (c) 2021 Michael Sammler                                      *)
(*  Copyright (c) 2021 Rodolphe Lepigre                                     *)
(*                                                                          *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  This software was developed by the University of Cambridge Computer     *)
(*  Laboratory (Department of Computer Science and Technology) and          *)
(*  contributors, in part under DARPA/AFRL contract FA8650-18-C-7809        *)
(*  ("CIFV"), in part funded by EPSRC Programme Grant EP/K008528/1 "REMS:   *)
(*  Rigorous Engineering for Mainstream Systems", in part funded from the   *)
(*  European Research Council (ERC) under the European Union’s Horizon      *)
(*  2020 research and innovation programme (grant agreement No 789108,      *)
(*  "ELVER"), in part supported by the UK Government Industrial Strategy    *)
(*  Challenge Fund (ISCF) under the Digital Security by Design (DSbD)       *)
(*  Programme, to deliver a DSbDtech enabled digital platform (grant        *)
(*  105694), and in part funded by Google.                                  *)
(*                                                                          *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions are  *)
(*  met:                                                                    *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR   *)
(*  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT    *)
(*  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,  *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   *)
(*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   *)
(*  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     *)
(*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   *)
(*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    *)
(*                                                                          *)
(****************************************************************************)

open Isla_lang

type ast = AST.lrng AST.trcs

let pp_pretty : out_channel -> ast -> unit = fun oc ast ->
  PPrintEngine.ToChannel.compact oc (PP.pp_trcs ast)

let pp_raw : out_channel -> ast -> unit = fun oc ast ->
  PPrintEngine.ToChannel.compact oc (PP.pp_raw_trcs ast)

let parse : Lexing.lexbuf -> ast = fun lexbuf ->
  Parser.trcs_start Lexer.token lexbuf

let run_batch : in_channel -> unit = fun ic ->
  let lexbuf = Lexing.from_channel ic in
  try
    let ast = parse lexbuf in
    Printf.printf "Raw version:\n    %a\n\n" pp_raw ast;
    Printf.printf "Pretty version:\n    %a\n" pp_pretty ast;
  with
  | Lexer.Error(s) -> Printf.printf "%s" s; exit 1
  | Parser.Error   -> Printf.printf "Parse error.\n"; exit 1

let run_interactive : unit -> unit = fun _ ->
  let prompt = "isla-lang> " in
  let process_line line =
    if String.length (String.trim line) = 0 then () else
    let lexbuf = Lexing.from_string line in
    try
      let ast = parse lexbuf in
      Printf.printf "Raw:    %a\n" pp_raw ast;
      Printf.printf "Pretty: %a\n" pp_pretty ast;
    with
    | Lexer.Error(s) -> Printf.printf "%s" s; exit 1
    | Parser.Error   ->
        let padding = String.length prompt + Lexing.lexeme_start lexbuf in
        Printf.printf "%s^\nAt offset %d: syntax error.\n%!"
          (String.make padding ' ') (Lexing.lexeme_start lexbuf); exit 1
  in
  try while true do
    Printf.printf "%s%!" prompt;
    process_line (read_line ())
  done with End_of_file -> ()

let with_file : string -> (in_channel -> 'a) -> 'a = fun file fn ->
  let ic = open_in file in
  let res = fn ic in
  close_in ic; res

let print_help : string -> unit = fun prog ->
  Printf.printf "Usage: %s [-i|-h|-|FILE]\n" prog;
  Printf.printf "Options:\n";
  Printf.printf "\t-i\tRun in interactive mode.\n";
  Printf.printf "\t-h\tDisplay this help message.\n";
  Printf.printf "\t-\tProcess standard input as an isla trace file.\n";
  Printf.printf "\tFILE\tProcess FILE as an isla trace file.\n"

let _ =
  let not_arg s = String.length s = 0 || s.[0] <> '-' in
  match Sys.argv with
  | [| _    ; "-i" |]                -> run_interactive ()
  | [| prog ; "-h" |]                -> print_help prog
  | [| _    ; "-"  |]                -> run_batch stdin
  | [| _    ; f    |] when not_arg f -> with_file f run_batch
  | _                                -> print_help Sys.argv.(0); exit 1
