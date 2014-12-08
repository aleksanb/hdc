module Opcodes where

import Data.Char (toLower)

data Opcode = And
            | Or
            | Xor
            | Add
            | Sub
            | Mul
            | Slt
            | Seq
            | Sll
            | Srl
            | Sra
    deriving (Show, Eq)

mnemonic :: Opcode -> String
mnemonic = map toLower . show
