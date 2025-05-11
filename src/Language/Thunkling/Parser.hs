module Language.Thunkling.Parser
  ( parseProgram,
  ) where

import Language.Thunkling.Syntax

import Control.Monad.Combinators as C
import Data.ByteString (pack)
import Data.ByteString qualified as ByteString
import Data.Set (singleton)
import Language.Thunkling.Config (InputFile (..))
import Relude.Unsafe (read)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Byte qualified as Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex
import Prelude hiding (many, some)

type Parser = Parsec Void ByteString
type ParseError = ParseErrorBundle ByteString Void

parseProgram
  :: InputFile
  -> ByteString
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
      (Just $ Parsec.Tokens (asNonEmptyBytes unexpected))
      (singleton $ Parsec.Tokens (asNonEmptyBytes expected))
  where
    asNonEmptyBytes = fromList . ByteString.unpack . unName

identifier :: Parser Name
identifier =
  lexeme $ mkName <$> Byte.letterChar <*> many Byte.alphaNumChar
  where
    mkName c cs = Name $ pack (c : cs)

expr :: Parser (Expr 'Parsed)
expr =
  int

int :: Parser (Expr 'Parsed)
int = LitInt (ParsedAnn Nothing) . read . decodeUtf8 . pack <$> some Byte.digitChar

space :: Parser ()
space = Lex.space Byte.space1 (Lex.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: ByteString -> Parser ByteString
symbol = Lex.symbol space
