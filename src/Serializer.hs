module Serializer(serialize) where
import Datatypes
import qualified Opcodes as Op

serialize :: [IR] -> [String]
serialize instructions = (map serializeInstruction instructions)


serializeInstruction :: IR -> String
serializeInstruction (TwoIR (R reg) (I immediate)  masked) =
  prefix ++ "ldi $" ++ (show reg) ++ ", " ++ (show immediate)
  where prefix = if masked then "? " else ""

serializeInstruction (TwoIR (R reg) (C address) masked) =
  "ldc $" ++ (show reg) ++ ", " ++ (show address)

serializeInstruction (ThreeIR operator (R rd) (R rs) (R rt) masked) =
  prefix ++ (Op.mnemonic opcode) ++ " $" ++ (show rd) ++ ", $" ++ (show rs) ++ ", $" ++ (show rt)
  where opcode = case operator of
               BitwiseAnd -> Op.And
               BitwiseOr -> Op.Or
               BitwiseXor -> Op.Xor
               Plus -> Op.Add
               Minus -> Op.Sub
               Multiply -> Op.Mul
               LessThan -> Op.Slt
               EqualTo -> Op.Seq
        prefix = if masked then "? " else ""

serializeInstruction (ThreeIR op rd (I rs) (R rt) masked)
 | op == Plus = serializeInstruction (ThreeIR op rd (R rt) (I rs) masked)

serializeInstruction (ThreeIR operator (R rd) (R rs) (I sh) masked) =
  prefix ++ (Op.mnemonic opcode) ++ " $" ++ (show rd) ++ ", $" ++ (show rs) ++ ", " ++ (show sh)
  where opcode = case operator of
               Plus -> Op.Addi
               ShiftLeft -> Op.Sll
               ShiftRight -> Op.Srl
               ShiftRightArithmetic -> Op.Sra
        prefix = if masked then "? " else ""

serializeInstruction LoadIR = "lw"

serializeInstruction StoreIR = "sw"
