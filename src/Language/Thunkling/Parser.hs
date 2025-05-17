module Language.Thunkling.Parser
  ( parseProgram,
  ) where

import Language.Thunkling.Syntax

import Control.Monad.Combinators as C
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

parseProgram
  :: InputFile
  -> Text
  -> Either ParseError (Program 'Parsed)
parseProgram (InputFile file) =
  Parsec.runParser program file

program :: Parser (Program 'Parsed)
program = Program <$> (space *> topLevelBinds <* space)

topLevelBinds :: Parser [TopLevelBind 'Parsed]
topLevelBinds = many topLevelBind

topLevelBind :: Parser (TopLevelBind 'Parsed)
topLevelBind = do
  -- Parse the signature
  sig <- optional (Parsec.try signature)
  -- Parse the function name
  (name, expr') <- bind sig

  let
    ann = ParsedAnn (snd <$> sig)

  pure $ TopLevelBind name ann expr'

signature :: Parser (Name, ExprTy)
signature = do
  (,) <$> (identifier <* symbol ":") <*> exprTy

exprTy :: Parser ExprTy
exprTy =
  (symbol "()" $> TyUnit)
    <|> (symbol "Int" $> TyInt)
    <|> (symbol "Bool" $> TyBool)

bind
  :: Maybe (Name, ExprTy)
  -> Parser (Name, Expr 'Parsed)
bind ty =
  (,)
    <$> bindName
    <*> (symbol "=" *> expr)
  where
    bindName :: Parser Name
    bindName = do
      -- Set a marker for the error, if necessary
      offset <- Parsec.getOffset
      -- Parse the function name
      name <- identifier

      case ty of
        Just (expectedName, _)
          | name /= expectedName -> mkTrivialError offset name expectedName
        _ -> pure name

mkTrivialError :: Int -> Name -> Name -> Parser a
mkTrivialError offset unexpected expected =
  Parsec.parseError $
    Parsec.TrivialError
      offset
      (tokens unexpected)
      (maybeToSet $ tokens expected)
  where
    tokens = fmap Parsec.Tokens . nonEmpty . Text.unpack . unName
    maybeToSet (Just a) = Set.singleton a
    maybeToSet Nothing = Set.empty

expr :: Parser (Expr 'Parsed)
expr = app <|> var <|> literal

app :: Parser (Expr 'Parsed)
app = Parsec.try $ do
  name <- identifier
  args <- some expr

  pure (App (ParsedAnn Nothing) name args)

var :: Parser (Expr 'Parsed)
var = Var (ParsedAnn Nothing) <$> identifier

literal :: Parser (Expr 'Parsed)
literal =
  int
    <|> boolean
    <|> unit

int :: Parser (Expr 'Parsed)
int = LitInt (ParsedAnn Nothing) <$> Lex.decimal

boolean :: Parser (Expr 'Parsed)
boolean =
  (symbol "True" $> litBool True)
    <|> (symbol "False" $> litBool False)
  where
    litBool = LitBool (ParsedAnn Nothing)

unit :: Parser (Expr 'Parsed)
unit = symbol "()" $> LitUnit (ParsedAnn Nothing)

space :: Parser ()
space = Lex.space Char.space1 (Lex.skipLineComment "--") empty

identifier :: Parser Name
identifier =
  lexeme $ mkName <$> Char.letterChar <*> many Char.alphaNumChar
  where
    mkName c cs = Name $ c `Text.cons` Text.pack cs

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space
