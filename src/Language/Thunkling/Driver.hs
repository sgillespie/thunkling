module Language.Thunkling.Driver
  ( InputFile (..),
    OutputFile (..),
    compileProgram,
  ) where

newtype InputFile = InputFile {unInputFile :: FilePath}
  deriving stock (Eq, Show)

newtype OutputFile = OutputFile {unOutputFile :: FilePath}
  deriving stock (Eq, Show)

compileProgram :: InputFile -> OutputFile -> IO ()
compileProgram _ _ = pure ()
