module DemoliciousParser where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { caseSensitive = False
           , Token.commentLine = "//"
           , Token.reservedNames = [ "and"
                                   , "or"
                                   ]
           , Token.reservedOpNames = [ "="
                                     , "&="
                                     , "|="
                                     , "^="
                                     , "+="
                                     , "-="
                                     , "*="

                                     , ":"
                                     , "*="
                                     ]
           }

lexer = Token.makeTokenParser languageDef
