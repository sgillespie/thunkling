module Language.Thunkling.Parser
  ( ParseError,
    parseErrorPretty,
    parseProgram,
  ) where

import Language.Thunkling.Syntax

import Control.Monad.Combinators as C
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Thunkling.Config (InputFile (..))
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char qualified as Char
import Text.Megaparsec.Char.Lexer qualified as Lex
import Prelude hiding (many, some)

type Parser = Parsec Void Text
type ParseError = ParseErrorBundle Text Void

parseErrorPretty :: ParseError -> String
parseErrorPretty = Parsec.errorBundlePretty

parseProgram
  :: InputFile
  -> Text
  -> Either ParseError (Program 'Parsed)
parseProgram (InputFile file) =
  Parsec.runParser program file

program :: Parser (Program 'Parsed)
program = Program <$> (space *> topLevelBinds <* Parsec.eof)

topLevelBinds :: Parser [TopLevelBind 'Parsed]
topLevelBinds = sepEndBy topLevelBind (Char.eol *> vspace)

topLevelBind :: Parser (TopLevelBind 'Parsed)
topLevelBind = do
  -- Parse the signature
  sig <- optional $ Parsec.try signature <* eol
  -- Parse the function name
  (name, params, expr') <- bind sig

  let
    ann = ParsedAnn (snd <$> sig)

  pure $ TopLevelBind (name{vType = ann}) params expr'

signature :: Parser (Name, ExprTy)
signature = do
  (,) <$> (identifier <* symbol ":") <*> exprTy

exprTy :: Parser ExprTy
exprTy = tyAbs <|> tyTerm

tyAbs :: Parser ExprTy
tyAbs =
  fmap TyAbs $
    Forall
      <$> (symbol "forall" *> many tyVar <* symbol ".")
      <*> tyTerm
  where
    tyVar = Tv <$> identifier

tyTerm :: Parser ExprTy
tyTerm = Parsec.try arrowTy <|> simpleTy

arrowTy :: Parser ExprTy
arrowTy =
  TyArrow
    <$> (simpleTy <* symbol "->")
    <*> exprTy

simpleTy :: Parser ExprTy
simpleTy =
  (symbol "()" $> TyUnit)
    <|> (symbol "Int" $> TyInt)
    <|> (symbol "Bool" $> TyBool)
    <|> (TyVar <$> identifier)

bind
  :: Maybe (Name, ExprTy)
  -> Parser (Var (Ann Parsed), [Param], Expr 'Parsed)
bind ty = do
  (,,)
    <$> bindName
    <*> many bindParam
    <*> (symbol "=" *> expr)
  where
    bindName :: Parser (Var (Ann Parsed))
    bindName = do
      -- Set a marker for the error, if necessary
      offset <- Parsec.getOffset
      -- Parse the function name
      name <- identifier

      case ty of
        Just (expectedName, _)
          | name /= expectedName -> mkTrivialError offset name expectedName
        _ -> pure $ V name parsedAnnEmpty

    bindParam = Param <$> identifier

mkTrivialError :: Int -> Name -> Name -> Parser a
mkTrivialError offset unexpected expected =
  Parsec.parseError $
    Parsec.TrivialError
      offset
      (tokens unexpected)
      (maybeToSet $ tokens expected)
  where
    tokens = fmap Parsec.Tokens . nonEmpty . Text.unpack
    maybeToSet (Just a) = Set.singleton a
    maybeToSet Nothing = Set.empty

expr :: Parser (Expr 'Parsed)
expr = makeExprParser term table
  where
    table =
      [ [ appOp App
        ],
        [ binOp "*",
          binOp "/",
          binOp "+",
          binOp "-"
        ]
      ]

    appOp f = InfixL (pure f)
    binOp name = InfixL (binApp name <$ symbol name)
    binApp fname = App . App (Var $ V fname parsedAnnEmpty)

term :: Parser (Expr 'Parsed)
term =
  var
    <|> abstraction
    <|> literal
    <|> betweenParens expr

var :: Parser (Expr 'Parsed)
var = Var . flip V parsedAnnEmpty <$> identifier

abstraction :: Parser (Expr 'Parsed)
abstraction = do
  Abs
    <$> (symbol "\\" *> (flip V parsedAnnEmpty <$> identifier))
    <*> (symbol "." *> expr)

literal :: Parser (Expr 'Parsed)
literal =
  int
    <|> boolean
    <|> string
    <|> unit

int :: Parser (Expr 'Parsed)
int = LitInt <$> intLiteral

boolean :: Parser (Expr 'Parsed)
boolean =
  (symbol "True" $> litBool True)
    <|> (symbol "False" $> litBool False)
  where
    litBool = LitBool

string :: Parser (Expr 'Parsed)
string = LitString <$> stringLiteral

unit :: Parser (Expr 'Parsed)
unit = symbol "()" $> LitUnit

identifier :: Parser Name
identifier =
  lexeme $ mkName <$> Char.letterChar <*> many Char.alphaNumChar
  where
    mkName c cs = c `Text.cons` Text.pack cs

intLiteral :: Parser Int
intLiteral = lexeme Lex.decimal

stringLiteral :: Parser Text
stringLiteral = lexeme $ quote *> manyUntilText Lex.charLiteral quote
  where
    manyUntilText :: Parser Char -> Parser end -> Parser Text
    manyUntilText p end = Text.pack <$> manyTill p end

betweenParens :: Parser a -> Parser a
betweenParens = lexeme . between "(" ")"

quote :: Parser Char
quote = Char.char '\"'

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space

eol :: Parser ()
eol = Char.eol *> vspace

space :: Parser ()
space = space' Char.hspace1

vspace :: Parser ()
vspace = space' Char.space1

space' :: Parser () -> Parser ()
space' p = Lex.space p (Lex.skipLineComment "--") empty
