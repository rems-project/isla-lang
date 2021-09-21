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

let input_file = ref (None : string option)

let opts =
  [("-i", Arg.String (fun s -> input_file := Some s), Printf.sprintf "<string> input file")]

let usage =
  "Usage: just main (for interactive) or main -i <filename> (for batch)\n"
  ^ "       main -help   to show options"

let usage' = usage ^ "\n" (*^ "Options:"*)

let help outchan msg = Printf.fprintf outchan "%s\n\n" msg

let collect_file s =
  help stderr "illegal argument";
  exit 1

let _ =
  try Arg.parse_argv Sys.argv (Arg.align opts) collect_file usage' with
  | Arg.Bad msg ->
      help stderr msg;
      exit 1
  | Arg.Help msg ->
      help stdout msg;
      exit 0

let process linebuf =
  (* Run the generated lexer and parser on this input *)
  try
    let t = Parser.trcs_start Lexer.token linebuf in
    (* Show the generated raw and "pretty" pp of the result *)
    Printf.printf "   ";
    PPrintEngine.ToChannel.compact stdout (PP.pp_raw_trcs t);
    Printf.printf "\n";
    Printf.printf "   ";
    PPrintEngine.ToChannel.compact stdout (PP.pp_trcs t);
    Printf.printf "\n"
  with
  | Lexer.Error msg -> Printf.fprintf stdout "%s" msg
  | Parser.Error ->
      Printf.fprintf stdout "%s^\nAt offset %d: syntax error.\n"
        (String.make (Lexing.lexeme_start linebuf) ' ')
        (Lexing.lexeme_start linebuf)

let _ =
  match !input_file with
  | None ->
      Printf.printf "enter isla traces\n";
      let foo () =
        let line = read_line () in
        let linebuf = Lexing.from_string line in
        process linebuf;
        flush stdout
      in
      foo ()
  | Some f ->
      let c = open_in f in
      let linebuf = Lexing.from_channel c in
      process linebuf
