open Mips_ast
open Mips_assem
open Int
open Byte

exception TODO
exception FatalError

(* Take a look at the definition of the Mips AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the Mips machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
*)
let load_next_instruction pc memory = rev_endianess (read_word memory pc)

let decode_instruction (instruction : int32) : instformat =
  (* Implement decoding of a word into a MIPS instruction *)
  (*Shift word*)
  let open Int32 in
  let opcode = logand (shift_right instruction 26) 0x3fl in
  (* Match opcode with R, j L*)
  (*Shift word*)
  match opcode with
  (* | 0l -> *)
  (* let r_rs = shift_right_logical instruction 21 land 0x1Fl in *)
  (* let r_rt = shift_right_logical instruction 16 land 0x1Fl in *)
  (* let r_rd = shift_right_logical instruction 11 land 0x1Fl in *)
  (* let r_shamt = shift_right_logical instruction 6 land 0x1Fl in *)
  (* let r_fun = instruction land 0x3Fl in *)
  (* R {r_opcode=opcode; r_rs; r_rt; r_rd; r_shamt; r_fun} *)
  | 0x2l | 0x3l ->
      let j_addr = logand instruction 0x03FFFFFFl in
      J { j_opcode = opcode; j_addr }
  | _ ->
      let i_rs = logand (shift_right instruction 21) 0x1Fl in
      let i_rt = logand (shift_right instruction 16) 0x1Fl in
      let i_imm = logand instruction 0xFFFFl in
      I { i_opcode = opcode; i_rs; i_rt; i_imm }
(* type iformat_args = {i_opcode : int32; i_rs: int32; i_rt: int32; i_imm: int32} *)

(* Define separate functions for executing each instruction, e.g., *)
let step_add state rd rs rt =
  (* Implement the semantics of the ADD instruction *)
  raise TODO

let rec interp (init_state : state) : state =
  let instruction_word = load_next_instruction init_state.pc init_state.m in
  if instruction_word = Int32.zero then init_state
    (* Halt condition: instruction is all zeros *)
  else
    let instruction = decode_instruction instruction_word in
    let updated_state =
      match instruction with
      (* Add (r1,r2,r3) -> step_add init_state r1 r2 r3 *)
      (* | Beq (r1,r2,imm) -> step_beq init_state r1 r2 imm *)
      (* | Jr (r) -> step_jr init_state r *)
      (* | Jal (imm) -> step_jal init_state imm *)
      (* | Li (r,imm) -> step_li init_state r imm *)
      (* | Lui (r,imm) -> step_lui init_state r imm *)
      (* | Ori (r1,r2,imm) -> step_ori init_state r1 r2 imm *)
      (* | Lw (r1,r2,imm) -> step_lw init_state r1 r2 imm *)
      (* | Sw (r1,r2,imm) -> step_sw init_state r1 r2 imm *)
      | _ -> raise FatalError
    in
    interp updated_state
(* Other step_* functions for different instructions *)

(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Big Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Stack grows downward starting at 0x7ffffffc
  * > GP points to 30000000
  * > The assembler uses register 1 as temp storage for encoding Li
  * > We don't implement delay slots in either assembly or bitcode semantics
  * > As stated in lecture, we shift jump and break immediates left by 2
  * > The PC is always incremented before executing an instruction
  * > Beq subtracts 4 from the PC before adding its offset
  * > We preserve the top 4 bits of the PC when executing a jal
*)
