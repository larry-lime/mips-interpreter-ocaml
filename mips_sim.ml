open Mips_ast
open Mips_assem
open Int
open Byte
open Printf

exception TODO
exception FatalError

(* Take a look at the definition of the Mips AST and machine state in mips_ast.ml *)

(* Given a starting state, simulate the Mips machine code to get a final state;
   a final state is reached if the the next instruction pointed to by the PC is
   all 0s.
*)
let load_next_instruction pc memory = rev_endianess (read_word memory pc)

let decode_instruction (instruction : int32) =
  let open Int32 in
  let opcode = logand (shift_right instruction 26) 0x3fl in
  instformat2ins
    (match opcode with
    | 0l ->
        let r_rs = logand (shift_right instruction 21) 0x1Fl in
        let r_rt = logand (shift_right instruction 16) 0x1Fl in
        let r_rd = logand (shift_right instruction 11) 0x1Fl in
        let r_shamt = logand (shift_right instruction 6) 0x1Fl in
        let r_fun = logand instruction 0x3Fl in
        R { r_opcode = opcode; r_rs; r_rt; r_rd; r_shamt; r_fun }
    | 0x2l | 0x3l ->
        let j_addr = logand instruction 0x03FFFFFFl in
        J { j_opcode = opcode; j_addr }
    | _ ->
        let i_rs = logand (shift_right instruction 21) 0x1Fl in
        let i_rt = logand (shift_right instruction 16) 0x1Fl in
        let i_imm = logand instruction 0xFFFFl in
        I { i_opcode = opcode; i_rs; i_rt; i_imm })

let step_add state r1 r2 r3 =
  let sum =
    Int32.add (rf_lookup (reg2ind r2) state.r) (rf_lookup (reg2ind r3) state.r)
  in
  let updated_rf = rf_update (reg2ind r1) sum state.r in
  { state with r = updated_rf; pc = Int32.add state.pc 4l }

let step_ori (state : state) (r1 : reg) (r2 : reg) (imm : int32) =
  let imm_or = Int32.logor (rf_lookup (reg2ind r2) state.r) imm in
  let updated_rf = rf_update (reg2ind r1) imm_or state.r in
  { state with r = updated_rf; pc = Int32.add state.pc 4l }

let step_lui (state : state) (r : reg) (imm : int32) =
  let imm_shifted = Int32.shift_left imm 16 in
  let updated_rf = rf_update (reg2ind r) imm_shifted state.r in
  { state with r = updated_rf; pc = Int32.add state.pc 4l }

let step_li (state : state) (r : reg) (imm : int32) =
  let updated_rf = rf_update (reg2ind r) imm state.r in
  { state with r = updated_rf; pc = Int32.add state.pc 4l }

let step_lw (state : state) (r1 : reg) (r2 : reg) (imm : int32) =
  let curPos = Int32.add (rf_lookup (reg2ind r2) state.r) imm in
  let word = read_word state.m curPos in
  let updated_rf = rf_update (reg2ind r1) word state.r in
  { state with r = updated_rf; pc = Int32.add state.pc 4l }

let step_sw (state : state) (r1 : reg) (r2 : reg) (imm : int32) =
  let curPos = Int32.add (rf_lookup (reg2ind r2) state.r) imm in
  let curWord = rf_lookup (reg2ind r1) state.r in
  let updated_mem =
    mem_update curPos (getByte curWord 0)
      (mem_update (Int32.add curPos 1l) (getByte curWord 1)
         (mem_update (Int32.add curPos 2l) (getByte curWord 2)
            (mem_update (Int32.add curPos 3l) (getByte curWord 3) state.m)))
  in
  { state with m = updated_mem; pc = Int32.add state.pc 4l }

let step_beq (state : state) (r1 : reg) (r2 : reg) (imm : int32) =
  let r1_val = rf_lookup (reg2ind r1) state.r in
  let r2_val = rf_lookup (reg2ind r2) state.r in
  if Int32.equal r1_val r2_val then
    let updated_pc = Int32.add (Int32.add state.pc 8l) imm in
    { state with pc = Int32.add updated_pc 4l }
  else { state with pc = Int32.add state.pc 4l }

let step_jr (state : state) (r : reg) =
  let r_val = rf_lookup (reg2ind r) state.r in
  { state with pc = r_val }

let step_jal (state : state) (imm : int32) =
  let return_address = Int32.add state.pc 8l in
  let updated_rf = rf_update 31 return_address state.r in
  let upper_bits = Int32.shift_left (Int32.shift_right state.pc 28) 28 in
  let updated_pc = Int32.add upper_bits (Int32.shift_left imm 2) in
  { state with pc = updated_pc; r = updated_rf }

let rec interp (state : state) : state =
  let open Int32 in
  let instruction_word = load_next_instruction state.pc state.m in
  if instruction_word = zero then state
  else
    let instruction = decode_instruction instruction_word in
    let updated_state =
      match instruction with
      | Add (r1, r2, r3) -> step_add state r1 r2 r3
      | Beq (r1, r2, imm) -> step_beq state r1 r2 imm
      | Jr r -> step_jr state r
      | Jal imm -> step_jal state imm
      | Li (r, imm) -> step_li state r imm
      | Lui (r, imm) -> step_lui state r imm
      | Ori (r1, r2, imm) -> step_ori state r1 r2 imm
      | Lw (r1, r2, imm) -> step_lw state r1 r2 imm
      | Sw (r1, r2, imm) -> step_sw state r1 r2 imm
    in
    interp updated_state

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
