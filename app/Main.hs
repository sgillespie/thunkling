module Main where

import Options.Applicative (Parser)
import Options.Applicative qualified as Opts

data Options = Options
  { optInputFile :: InputFile,
    optOutputFile :: OutputFile
  }

newtype InputFile = InputFile {unInputFile :: FilePath}
  deriving stock (Eq, Show)

newtype OutputFile = OutputFile {unOutputFile :: FilePath}
  deriving stock (Eq, Show)

main :: IO ()
main = run =<< Opts.execParser parserInfo
  where
    parserInfo = Opts.info (parseOpts <**> Opts.helper) desc
    desc = 
      Opts.fullDesc
        <> Opts.progDesc "Compile untitled programs"

run :: Options -> IO ()
run _ = pure ()

parseOpts :: Parser Options
parseOpts =
  Options
    <$> parseInputFile
    <*> parseOutputFile

parseInputFile :: Parser InputFile
parseInputFile = InputFile <$> Opts.strArgument optMods
  where
    optMods =
      Opts.metavar "<file>"
        <> Opts.help "The input file to compile."

parseOutputFile :: Parser OutputFile
parseOutputFile = OutputFile <$> Opts.strOption optMods
  where
    optMods =
      Opts.long "output"
        <> Opts.short 'o'
        <> Opts.metavar "<file>"
        <> Opts.value "main"
        <> Opts.showDefault
        <> Opts.help "Specify the output file."

