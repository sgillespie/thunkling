module Language.Thunkling.Driver
  ( compileProgram,
  ) where

import Language.Thunkling.Config
  ( DumpProgramParsed (..),
    DumpProgramTypechecked (..),
    InputFile (..),
    Opts (..),
    OutputFile (..),
  )
import Language.Thunkling.Errors (AppError (..))

import Control.Exception (catch, throwIO, try)
import Data.Text.IO (hPutStrLn)
import Language.Thunkling.Parser (parseProgram)
import Language.Thunkling.Pretty (showProgramParsed, showProgramTypechecked)
import Language.Thunkling.Typecheck (typecheck)
import System.Exit (ExitCode (..))
import System.IO.Error (IOError)

compileProgram :: InputFile -> OutputFile -> Opts -> IO ()
compileProgram inFile outFile opts = do
  res <- try @SomeException (compileProgram' inFile outFile opts)
  case res of
    Right _ -> pure ()
    Left err -> do
      hPutStrLn stderr $ toText (displayException err)
      exitWith (ExitFailure 1)

compileProgram' :: InputFile -> OutputFile -> Opts -> IO ()
compileProgram' inFile outFile Opts{..} = do
  -- Read inFile
  text <- readInputFile inFile

  res <- runExceptT $ do
    parsed <- hoistEither $ first ParseError (parseProgram inFile text)
    when (unDumpProgramParsed dumpProgramParsed) $ do
      putTextLn "Parsed program:\n"
      putTextLn (showProgramParsed parsed)

    typechecked <- hoistEither $ first TypeError (typecheck parsed)
    when (unDumpProgramTypechecked dumpProgramTypechecked) $ do
      putTextLn "Typechecked program:\n"
      putTextLn (showProgramTypechecked typechecked)

    -- TODO[sgillespie]:
    --
    --  3. Desugar to SystemF
    --  4. Add explicit Thunk/Forces
    --  5. Generate LLVM

    -- Dump results
    hoistEither $ Right (showProgramTypechecked typechecked)

  either
    throwIO
    (writeOutputFile outFile . encodeUtf8)
    res

readInputFile :: InputFile -> IO Text
readInputFile inFile =
  catch @IOError (decodeUtf8 <$> readFileBS') handleErr
  where
    readFileBS' = readFileBS $ unInputFile inFile
    handleErr _ = throwIO $ NoInputFile inFile

writeOutputFile :: OutputFile -> ByteString -> IO ()
writeOutputFile outFile text =
  catch @IOError writeFileBS' handleErr
  where
    writeFileBS' = writeFileBS (unOutputFile outFile) text
    handleErr _ = throwIO $ NoOutputFile outFile
