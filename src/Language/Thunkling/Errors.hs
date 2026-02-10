module Language.Thunkling.Errors
  ( AppError (..),
  ) where

import Language.Thunkling.Config (InputFile (..), OutputFile (..))
import Language.Thunkling.Parser (ParseError, parseErrorPretty)
import Language.Thunkling.Typecheck (TypeError)

import Text.Show (Show (..))
import Text.Show qualified as Show

-- | Application-wide exception type. Used for reporting errors from the
-- application, not necessarily "compiler errors".
data AppError
  = NoInputFile InputFile
  | NoOutputFile OutputFile
  | ParseError ParseError
  | TypeError TypeError
  | UnknownError SomeException
  deriving stock Typeable

instance Exception AppError

instance Show AppError where
  show (NoInputFile (InputFile file)) = "Unable to open input file " <> file <> "!"
  show (NoOutputFile (OutputFile file)) = "Unable to write output file " <> file <> "!"
  show (ParseError err) = "Parse error at " <> parseErrorPretty err
  show (TypeError err) = Show.show err
  show (UnknownError err) = Show.show err
