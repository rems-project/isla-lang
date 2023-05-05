(****************************************************************************)
(* BSD 2-Clause License                                                     *)
(*                                                                          *)
(* Copyright (c) 2020-2021 Alasdair Armstrong                               *)
(* Copyright (c) 2020-2021 Brian Campbell                                   *)
(* Copyright (c) 2020-2023 Thibaut Pérami                                   *)
(* Copyright (c) 2020-2021 Peter Sewell                                     *)
(* Copyright (c) 2020-2021 Dhruv Makwana                                    *)
(* Copyright (c) 2021 Michael Sammler                                       *)
(* Copyright (c) 2021 Rodolphe Lepigre                                      *)
(*                                                                          *)
(* All rights reserved.                                                     *)
(*                                                                          *)
(* This software was developed by the University of Cambridge Computer      *)
(* Laboratory (Department of Computer Science and Technology) and           *)
(* contributors, in part under DARPA/AFRL contract FA8650-18-C-7809         *)
(* ("CIFV"), in part funded by EPSRC Programme Grant EP/K008528/1 "REMS":   *)
(* "Rigorous Engineering for Mainstream Systems", in part funded from the   *)
(* European Research Council (ERC) under the European Union’s Horizon       *)
(* 2020 research and innovation programme (grant agreement No 789108,       *)
(* "ELVER"), in part supported by the UK Government Industrial Strategy     *)
(* Challenge Fund (ISCF) under the Digital Security by Design (DSbD)        *)
(* Programme, to deliver a DSbDtech enabled digital platform (grant         *)
(* 105694), and in part funded by Google.                                   *)
(*                                                                          *)
(*                                                                          *)
(* Redistribution and use in source and binary forms, with or without       *)
(* modification, are permitted provided that the following conditions are   *)
(* met:                                                                     *)
(*                                                                          *)
(* 1. Redistributions of source code must retain the above copyright        *)
(* notice, this list of conditions and the following disclaimer.            *)
(*                                                                          *)
(* 2. Redistributions in binary form must reproduce the above copyright     *)
(* notice, this list of conditions and the following disclaimer in the      *)
(* documentation and/or other materials provided with the distribution.     *)
(*                                                                          *)
(* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      *)
(* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        *)
(* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    *)
(* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     *)
(* HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   *)
(* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT         *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,    *)
(* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY    *)
(* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE    *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.     *)
(****************************************************************************)

From stdpp Require Import prelude.
From stdpp Require Import gmap.
Require Import lang traces.


Module Litmus.

Module Assert.

  Inductive loc :=
  | Register (name : string) (tid : N): loc
  | LastWriteTo (address : bv 64) (bytes : N) : loc.

  Inductive exp :=
  | EqLoc : loc -> exp -> exp
  | Loc : bv 64 -> exp
  | Label : string -> exp
  | Bool : bool -> exp
  | Bits : bvn -> exp
  | Nat : N -> exp
  | And : list exp -> exp
  | Or : list exp -> exp
  | Not : exp -> exp
  | Imp : exp -> exp -> exp.

End Assert.

Module Memory.

  Definition address := bv 64.

  Record range := mrng {
      rstart : address;
      rend : address;
    }.

  Inductive region :=
  | Constrained (rng : range)
  | Symbolic (rng : range)
  | SymbolicCode (rng : range)
  | Concrete (rng : range) (map : address -> option (bv 8))
  | Custom (rng : range).

  Definition t := list region.
End Memory.


Module Footprint.

  Record register_field := {
      register : string;
      fields : list accessor;
    }.

  (** Represents the dependencies into something,
      if the memory bool is true, then all memory read affect this. *)
  Record taints := {
      regs : list register_field;
      mem : bool;
    }.

  Record t := make {
      (** Dependencies feeding into the data of writes *)
      writes_data : taints;
      (** Dependencies feeding into the address of memory accesses *)
      memory_address: taints;
      (** Dependencies feeding into a branch address *)
      branch_dep: taints;
      (** List of register read (dependencies for any register write) *)
      register_reads : list register_field;
      (** List of written registers that only use register reads *)
      register_writes : list register_field;
      (** List of written registers that also use memory reads *)
      register_writes_tainted : list register_field;
      (** All register read-write pairs to the following registers are
          ignored for tracking dependencies within an instruction. If
          the first element of the tuple is None then all writes are
          ignored *)
      register_write_ignored : list ((option string) * string);
      is_store : bool;
      is_load : bool;
      is_branch : bool;
      is_exclusive : bool;
    }.

End Footprint.

Module Test.

  Record t := make {
      threads: list isla_trace; (* Those are multi instruction traces *)
      final_assertion: Assert.exp;
      memory : Memory.t;
      footprints : (* opcode *) bv 64 -> Footprint.t;
    }.

End Test.

End Litmus.
