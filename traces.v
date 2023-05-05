(****************************************************************************)
(* BSD 2-Clause License                                                     *)
(*                                                                          *)
(* Copyright (c) 2020-2021 Alasdair Armstrong                               *)
(* Copyright (c) 2020-2021 Brian Campbell                                   *)
(* Copyright (c) 2020-2021 Thibaut Pérami                                   *)
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
Require Import lang.

Notation EVal_Sym b := (Val (Val_Symbolic b) Mk_annot).
Notation EVal_Bool b := (Val (Val_Bool b) Mk_annot).
Notation EVal_Bits b := (Val (Val_Bits b) Mk_annot).
Notation EVal_Enum b := (Val (Val_Enum b) Mk_annot).

(* TODO Code in the file is copy-pasted straight from islaris, at some point,
   islaris should be made to depend on it as a library *)

Inductive isla_trace : Set :=
| tnil
| tcons (e : event) (t : isla_trace)
| tcases (ts : list isla_trace).
Notation "e :t: t" := (tcons e t) (at level 60, right associativity,
 format "'[v' e  :t: '/' t ']'" ) : stdpp_scope.
Global Instance isla_trace_inhabited : Inhabited isla_trace := populate tnil.

Fixpoint subst_trace (v : base_val) (x : var_name) (t : isla_trace) :=
  match t with
  | tnil => tnil
  | tcons e t' => tcons (subst_val_event v x e) (subst_trace v x t')
  | tcases ts => tcases (subst_trace v x <$> ts)
  end.

Fixpoint isla_trace_length (t : isla_trace) : nat :=
  match t with
  | tnil => 0
  | tcons _ t' => S (isla_trace_length t')
  | tcases ts => S (sum_list (isla_trace_length <$> ts))
  end.

Definition false_trace : isla_trace := Assume (AExp_Val (AVal_Bool false) Mk_annot) Mk_annot:t:tnil.
