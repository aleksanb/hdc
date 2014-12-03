import qualified Tokenizer
import qualified Parser

runEvalWith parser input = do
  let ast = Parser.parse input
  putStrLn $ "Ast: " ++ (show ast)

main :: IO ()
main = do
  putStrLn "HDC: Haskell Demolicous Compiler\n"

  program <- getContents
  runEvalWith Parser.parse program

  --input <- getLine
  --runEvalWith Parser.parse input
