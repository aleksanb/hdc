module Opcodes where

import Data.Char (toLower)
import Data.Bits

data Opcode = And
            | Or
            | Xor
            | Add
            | Sub
            | Mul
            | Slt
            | Seq
            | Addi
            | Sll
            | Srl
            | Sra
            | Ldc
            | Lw
            | Sw
            | ThreadFinished
    deriving (Show, Eq)

mnemonic :: Opcode -> String
mnemonic = map toLower . show

hexOpcode :: Opcode -> Int
hexOpcode opcode =
  code `shiftL` 26
  where code = opcode of
     Ldc            -> 0x2
     Addi           -> 0x1
     Lw             -> 0x8
     Sw             -> 0x4
     ThreadFinished -> 0x10
     _              -> 0x0

hexAlufunction :: Opcode -> Int
hexAlufunction opcode =
  case opcode of
    Add ->  0x4
    Sub ->  0x4
    Mul ->  0x9
    And ->  0x6
    Or  ->  0x7
    Xor ->  0x8
    Slt ->  0x3
    Seq ->  0xA
    Sll ->  0x0
    Srl ->  0x1
    Sra ->  0x2
    _   ->  0x0

hexRs :: Int -> Int
hexRs rs =
  rs `shiftL` 21

hexRt :: Int -> Int
hexRt rt =
  rt `shiftL` 16

hexRd :: Int -> Int
hexRd rd =
  rd `shiftL` 11

hexMask :: Bool -> Int
hexMask mask
  | mask      = 1 `shiftL` 31
  | otherwise = 0
