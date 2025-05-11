module Language.Thunkling.Errors
  ( AppError (..),
  ) where

import Language.Thunkling.Config (InputFile (..), OutputFile (..))

import Text.Show (Show (..))

-- | Application-wide exception type. Used for reporting errors from the
-- application, not necessarily "compiler errors".
data AppError
  = NoInputFile InputFile
  | NoOutputFile OutputFile
  deriving stock (Eq, Ord, Typeable)

instance Exception AppError

instance Show AppError where
  show (NoInputFile (InputFile file)) = "Unable to open input file " <> file <> "!"
  show (NoOutputFile (OutputFile file)) = "Unable to write output file " <> file <> "!"

