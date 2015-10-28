import qualified Assembler
import qualified Serializer

import Datatypes

import Test.QuickCheck
import Text.Printf

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

-- Check the hex output
prop_hex :: Int -> Bool
prop_hex s =
  hex == [00008000]
  where hex = Assembler.assemble [LoadIR]

-- Check the assembly output
prop_assembly :: Int -> Bool
prop_assembly s =
  assembly == ["lw"]
  where assembly = Serializer.serialize [LoadIR]

tests = [("Check hex",      quickCheck prop_hex)
        ,("Check assembly", quickCheck prop_assembly)]
