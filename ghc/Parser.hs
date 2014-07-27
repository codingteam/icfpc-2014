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
  functions <- many1 pFunc
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

pFuncArg :: IParser VarName
pFuncArg = pVarName

pVarName :: IParser VarName
pVarName = do
  name_start <- letter
  name_end <- many alphaNum
  return $ name_start : name_end

pExpr :: IParser Expr
pExpr = choice [ pConst, pVar ]
  where
  pConst = do
    number <- many1 digit
    return $ Const $ read number

  pVar = do
    name <- pVarName
    return $ Var name

pStatement :: IParser Statement
pStatement = choice [ pDeclare
                    , pAssign
                    , pInc
                    , pDec
                    , pAdd
                    , pSub
                    , pMul
                    , pDiv
                    , pAnd
                    , pOr
                    , pXor
                    ]

pDeclare :: IParser Statement
pDeclare = try $ do
  string "declare"
  skipMany1 space
  name <- pVarName
  spaces
  return $ Declare name

genBinOpParser :: String -> (VarName -> Expr -> Statement) -> IParser Statement
genBinOpParser op constructor = try $ do
  dest <- pVarName
  spaces
  string op
  spaces
  src <- pExpr
  spaces
  return $ constructor dest src

pAssign :: IParser Statement
pAssign = genBinOpParser "=" Assign

pAdd :: IParser Statement
pAdd = genBinOpParser "+=" Add

pSub :: IParser Statement
pSub = genBinOpParser "-=" Sub

pMul :: IParser Statement
pMul = genBinOpParser "*=" Mul

pDiv :: IParser Statement
pDiv = genBinOpParser "/=" Div

pAnd :: IParser Statement
pAnd = genBinOpParser "&=" And

pOr :: IParser Statement
pOr = genBinOpParser "|=" Or

pXor :: IParser Statement
pXor = genBinOpParser "^=" Xor

genPostfixOfParser :: String -> (VarName -> Statement) -> IParser Statement
genPostfixOfParser op constructor = try $ do
  var <- pVarName
  string op
  spaces
  return $ constructor var

pInc :: IParser Statement
pInc = genPostfixOfParser "++" Inc

pDec :: IParser Statement
pDec = genPostfixOfParser "--" Dec
