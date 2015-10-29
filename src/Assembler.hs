module Assembler(assemble) where

import Datatypes
import Opcodes

import Text.Printf

assemble :: [IR] -> [String]
assemble instructions =
  map (printf "0x%08x") hex
  where hex = map assembleInstruction instructions

assembleInstruction :: IR -> Int

assembleInstruction LoadIR = (hexOpcode Lw)

assembleInstruction StoreIR = (hexOpcode Sw)

assembleInstruction (TwoIR (R reg) (I immediate)  masked) =
  (hexMask masked) + (hexOpcode Addi) + (hexRt reg) + (immediate `mod` 2 ^ 16)

assembleInstruction (TwoIR (R reg) (C address) masked) =
  (hexMask masked) + (hexOpcode Ldc) + (hexRt reg) + (address `mod` 2 ^ 16)

assembleInstruction (ThreeIR operator (R rd) (R rs) (R rt) masked) =
  (hexMask masked) + (hexOpcode opcode) + (hexRd rd) + (hexRs rs) + (hexRt rt) + (hexAlufunction opcode)
    where opcode = case operator of
                BitwiseAnd -> And
                BitwiseOr -> Or
                BitwiseXor -> Xor
                Plus -> Add
                Minus -> Sub
                Multiply -> Mul
                LessThan -> Slt
                EqualTo -> Seq

assembleInstruction (ThreeIR op rd (I rs) (R rt) masked)
  | op == Plus = assembleInstruction (ThreeIR op rd (R rt) (I rs) masked)

assembleInstruction (ThreeIR operator (R rd) (R rs) (I sh) masked) =
  (hexMask masked) + (hexOpcode opcode) + (hexRd rd) + (hexRs rs) + sh
  where opcode = case operator of
               Plus -> Addi
               ShiftLeft -> Sll
               ShiftRight -> Srl
               ShiftRightArithmetic -> Sra
