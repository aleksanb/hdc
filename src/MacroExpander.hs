module MacroExpander(expand) where

import System.IO
import Data.String.Utils
import Control.Monad
import Text.Regex.Posix

expand :: String -> IO String
expand program = do
  let capturedMacros = program =~ "(.*) = @(.*)\\((.*)\\)" :: [[String]]

  fst $ foldl
    (\(acc, id:ids) (pattern:macroReturn:macroFile:macroParameters:_) ->
      ((liftM3
        replace
          (return pattern)
          (buildMacro macroFile macroReturn (split ", " macroParameters) id)
          acc),
      ids))
    (return program, [0..])
    (capturedMacros)


buildMacro :: String -> String -> [String] -> Int -> IO String
buildMacro macroFile macroReturn macroParameters macroID = do
  sourceMacro <- readFile $ macroFile ++ ".d"
  let prefix = "_" ++ macroFile ++ "_" ++ show macroID ++ "_"
      bindings =
        ("__return", macroReturn) :
        zip
          (map (("__param" ++) . show) [0..])
          macroParameters
      withBoundParameters = foldl
        (\acc binding -> uncurry replace binding acc)
        sourceMacro
        bindings
      withUniquePrefixes = replace "__" prefix withBoundParameters

  return withUniquePrefixes
