import qualified Parser
import qualified Beautifier
import qualified GeneratorM as Generator
import qualified Serializer
import qualified Optimizer
import qualified Text.Show.Pretty as Pr

import Control.Monad.Writer

runEvalWith input = do
  let syntax_tree = Parser.parse input
  putStrLn $ "Syntax tree:\n" ++ Pr.ppShow syntax_tree

  let ast = Beautifier.beautify syntax_tree
  putStrLn $ "Ast:\n" ++ Pr.ppShow ast

  let ir = Generator.generate ast
  putStrLn $ "IR:\n" ++ Pr.ppShow ir

  let (optimized, stats) = runWriter $ Optimizer.optimize ir
  putStrLn $ "Optimized:\n" ++ Pr.ppShow optimized

  let assembly = Serializer.serialize optimized

  putStrLn "; Generated by HDC"
  putStrLn $ unlines $ map ("; "++) stats

  putStrLn $ unlines assembly

main :: IO ()
main = do

  program <- getContents
  runEvalWith program
