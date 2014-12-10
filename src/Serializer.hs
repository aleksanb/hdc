module Serializer(serialize) where
import Datatypes
import qualified Opcodes as Op

serialize :: [IR] -> [String]
serialize instructions = (map serializeInstruction instructions)


serializeInstruction :: IR -> String
serializeInstruction (LoadImmediateIR reg immediate masked) =
  "ldi $" ++ (show reg) ++ ", " ++ (show immediate)

serializeInstruction (RRR operator rd rs rt masked) =
  (Op.mnemonic opcode) ++ " $" ++ (show rd) ++ ", $" ++ (show rs) ++ ", $" ++ (show rt)
  where opcode = case operator of
               And -> Op.And
               Or -> Op.Or
               BitwiseAnd -> Op.And
               BitwiseOr -> Op.Or
               BitwiseXor -> Op.Xor
               Plus -> Op.Add
               Minus -> Op.Sub
               Multiply -> Op.Mul
               LessThan -> Op.Slt
               EqualTo -> Op.Seq

serializeInstruction (RRI operator rd rs sh masked) =
  (Op.mnemonic opcode) ++ " $" ++ (show rd) ++ ", $" ++ (show rs) ++ ", " ++ (show sh)
  where opcode = case operator of
               Plus -> Op.Addi
               ShiftLeft -> Op.Sll
               ShiftRight -> Op.Srl
               ShiftRightArithmetic -> Op.Sra

serializeInstruction (LoadConstantIR reg address masked) =
  "ldc $" ++ (show reg) ++ ", " ++ (show address)

serializeInstruction LoadIR = "lw"

serializeInstruction StoreIR = "sw"
