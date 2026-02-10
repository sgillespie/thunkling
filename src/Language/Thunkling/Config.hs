module Language.Thunkling.Config
  ( InputFile (..),
    OutputFile (..),
    DumpProgramParsed (..),
    DumpProgramTypechecked (..),
    Opts (..),
  ) where

newtype InputFile = InputFile {unInputFile :: FilePath}
  deriving stock (Eq, Ord, Show)

newtype OutputFile = OutputFile {unOutputFile :: FilePath}
  deriving stock (Eq, Ord, Show)

data Opts = Opts
  { dumpProgramParsed :: DumpProgramParsed,
    dumpProgramTypechecked :: DumpProgramTypechecked
  }

newtype DumpProgramParsed = DumpProgramParsed {unDumpProgramParsed :: Bool}
  deriving stock (Eq, Ord, Show)

newtype DumpProgramTypechecked = DumpProgramTypechecked {unDumpProgramTypechecked :: Bool}
  deriving stock (Eq, Ord, Show)
