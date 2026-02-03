module Language.Thunkling.Driver
  ( compileProgram,
  ) where

import Language.Thunkling.Config (InputFile (..), OutputFile (..))
import Language.Thunkling.Errors (AppError (..))

import Control.Exception (catch, throwIO, try)
import Data.Text.IO (hPutStrLn)
import Language.Thunkling.Parser (parseProgram)
import System.Exit (ExitCode (..))
import System.IO.Error (IOError)
import Language.Thunkling.Typecheck (typecheck)
import Language.Thunkling.Pretty (showProgramParsed, showProgramTypechecked)

compileProgram :: InputFile -> OutputFile -> IO ()
compileProgram inFile outFile = do
  res <- try @SomeException (compileProgram' inFile outFile)
  case res of
    Right _ -> pure ()
    Left err -> do
      hPutStrLn stderr $ toText (displayException err)
      exitWith (ExitFailure 1)

compileProgram' :: InputFile -> OutputFile -> IO ()
compileProgram' inFile outFile = do
  -- Read inFile
  text <- readInputFile inFile

  let res = do
        parsed <- first ParseError (parseProgram inFile text)
        typechecked <- first TypeError (typecheck parsed)

        -- TODO[sgillespie]:
        --
        --  3. Desugar to SystemF
        --  4. Add explicit Thunk/Forces
        --  5. Generate LLVM

        -- Dump results
        Right (showProgramTypechecked typechecked)

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
