module Language.Thunkling.Config
  ( InputFile (..),
    OutputFile (..),
  ) where

newtype InputFile = InputFile {unInputFile :: FilePath}
  deriving stock (Eq, Ord, Show)

newtype OutputFile = OutputFile {unOutputFile :: FilePath}
  deriving stock (Eq, Ord, Show)
