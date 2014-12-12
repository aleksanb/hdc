module Serializer(serialize) where
import Datatypes
import qualified Opcodes as Op

serialize :: [IR] -> [String]
serialize instructions = (map serializeInstruction instructions)


serializeInstruction :: IR -> String
serializeInstruction (TwoIR (R reg) (I immediate)  masked) =
  "ldi $" ++ (show reg) ++ ", " ++ (show immediate)

serializeInstruction (TwoIR (R reg) (C address) masked) =
  "ldc $" ++ (show reg) ++ ", " ++ (show address)

serializeInstruction (ThreeIR operator rd (I rs) rt masked)
  | rs == 1 = serializeInstruction $ ThreeIR operator rd (R 0) rt masked

serializeInstruction (ThreeIR operator (R rd) (R rs) (R rt) masked) =
  (Op.mnemonic opcode) ++ " $" ++ (show rd) ++ ", $" ++ (show rs) ++ ", $" ++ (show rt)
  where opcode = case operator of
               BitwiseAnd -> Op.And
               BitwiseOr -> Op.Or
               BitwiseXor -> Op.Xor
               Plus -> Op.Add
               Minus -> Op.Sub
               Multiply -> Op.Mul
               LessThan -> Op.Slt
               EqualTo -> Op.Seq

serializeInstruction (ThreeIR operator (R rd) (R rs) (I sh) masked) =
  (Op.mnemonic opcode) ++ " $" ++ (show rd) ++ ", $" ++ (show rs) ++ ", " ++ (show sh)
  where opcode = case operator of
               Plus -> Op.Addi
               ShiftLeft -> Op.Sll
               ShiftRight -> Op.Srl
               ShiftRightArithmetic -> Op.Sra

serializeInstruction (ThreeIR op rd (I rs) (R rt) masked)
 | op == Plus = serializeInstruction (ThreeIR op rd (R rt) (I rs) masked)

serializeInstruction LoadIR = "lw"

serializeInstruction StoreIR = "sw"
