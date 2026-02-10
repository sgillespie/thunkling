module Main where

import Language.Thunkling
  ( DumpProgramParsed (..),
    DumpProgramTypechecked (..),
    InputFile (..),
    Opts (..),
    OutputFile (..),
    compileProgram,
  )

import Options.Applicative (Parser)
import Options.Applicative qualified as Opts

data Options = Options
  { optInputFile :: InputFile,
    optOutputFile :: OutputFile,
    optDumpProgramParsed :: DumpProgramParsed,
    optDumpProgramTypechecked :: DumpProgramTypechecked
  }

main :: IO ()
main = run =<< Opts.execParser parserInfo
  where
    parserInfo = Opts.info (parseOpts <**> Opts.helper) desc
    desc =
      Opts.fullDesc
        <> Opts.progDesc "Compile thunkling programs"

run :: Options -> IO ()
run Options{..} =
  compileProgram optInputFile optOutputFile extraOpts
  where
    extraOpts =
      Opts
        { dumpProgramParsed = optDumpProgramParsed,
          dumpProgramTypechecked = optDumpProgramTypechecked
        }

parseOpts :: Parser Options
parseOpts =
  Options
    <$> parseInputFile
    <*> parseOutputFile
    <*> parseDumpProgramParsed
    <*> parseDumpProgramTypechecked

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

parseDumpProgramParsed :: Parser DumpProgramParsed
parseDumpProgramParsed = DumpProgramParsed <$> Opts.switch optMods
  where
    optMods =
      Opts.long "dump-parsed"
        <> Opts.short 'p'
        <> Opts.showDefault
        <> Opts.help "Dump results of parsing pass"

parseDumpProgramTypechecked :: Parser DumpProgramTypechecked
parseDumpProgramTypechecked = DumpProgramTypechecked <$> Opts.switch optMods
  where
    optMods =
      Opts.long "dump-typechecked"
        <> Opts.short 't'
        <> Opts.showDefault
        <> Opts.help "Dump results of typechecking pass"
