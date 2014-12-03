import qualified Tokenizer
import qualified Parser

runEvalWith parser input = do
  let ast = Parser.parse input
  putStrLn $ "Ast: " ++ (show ast)

main :: IO ()
main = do
  putStrLn "Input"
  runEvalWith Parser.parse "a = 20"
  --input <- getContents
  --runEvalWith Parser.parse input
