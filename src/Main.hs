import qualified Tokenizer
import qualified Parser

main :: IO ()
main = getContents >>= print . Parser.parse . Tokenizer.tokenize
