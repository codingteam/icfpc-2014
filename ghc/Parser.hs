module Parser (
  -- auxiliary functions
  iParse

  -- parsers
, pFunc
) where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

import AST

-- copied from: https://gist.github.com/sw17ch/2048516

type IParser a = ParsecT String () (State SourcePos) a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndent source_name $ runParserT aParser () source_name input

pFunc :: IParser Function
pFunc = do
  body <- withBlock
            (\(name, args) body -> Function name args body)
            pFuncHeader
            pFuncBody
  spaces
  return body

pFuncHeader :: IParser (FuncName, FuncArgs)
pFuncHeader = do
  string "function"
  spaces
  name_start <- letter
  name_end <- many alphaNum

  spaces

  args <- pFuncArg `sepBy` spaces
  char ':'

  return (name_start:name_end, args)

pFuncArg :: IParser Var
pFuncArg = do
  name_start <- letter
  name_end <- many alphaNum

  return $ Var $ name_start : name_end

pFuncBody :: IParser Statement
pFuncBody = do -- TODO: implement
  letter
  return Halt
