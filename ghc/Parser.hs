module Parser (
  -- auxiliary functions
  iParse

  -- parsers
, pProgram
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

pProgram :: IParser Program
pProgram = do
  functions <- pFunc `sepBy1` many1 newline
  eof
  return functions

pFunc :: IParser Function
pFunc = do
  body <- withBlock
            (\(name, args) body -> Function name args body)
            pFuncHeader
            pStatement
  return body

pFuncHeader :: IParser (FuncName, FuncArgs)
pFuncHeader = do
  string "function"
  skipMany1 space
  name <- pVarName

  args <- option [] $ do
    skipMany1 space
    pFuncArg `sepBy` spaces

  spaces
  char ':'
  spaces

  return (name, args)

pFuncArg :: IParser Var
pFuncArg = pVar

pVar :: IParser Var
pVar = do
  name <- pVarName

  return $ Var name

pStatement :: IParser Statement
pStatement = choice [ pDeclare
                    ]

pDeclare :: IParser Statement
pDeclare = do
  string "declare"
  skipMany1 space
  name <- pVarName
  newline
  return $ Declare name

pVarName :: IParser VarName
pVarName = do
  name_start <- letter
  name_end <- many alphaNum
  return $ name_start : name_end
