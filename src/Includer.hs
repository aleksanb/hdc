module Includer(include) where

import System.IO
import Data.String.Utils
import Control.Monad
import Text.Regex.Posix

include :: String -> IO String
include input = do
  let parameters = (input =~ "@macro\\((.*)\\)" :: [[String]])
      templatedCode =
        foldl
          (\acc (pattern:capture:_) ->
            liftM3 replace (return pattern) (parseMacro capture) acc)
          (return input)
          parameters

  putStrLn $ show parameters

  templatedCode


parseMacro :: String -> IO String
parseMacro parameters = do
  let macroFile:macroParameters = split "," parameters
      macroReturn = last macroParameters
  buildMacro macroFile macroParameters macroReturn


buildMacro :: String -> [String] -> String -> IO String
buildMacro macroFile macroParameters macroReturn = do
  sourceMacro <- readFile macroFile
  let bindings =
        ("__return", macroReturn) :
        zip
          (map (\s -> "__param" ++ show s) [0..])
          macroParameters
      boundMacro = foldl
        (\acc binding -> uncurry replace binding acc)
        sourceMacro
        bindings
  return boundMacro
