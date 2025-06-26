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

  let syn = parseProgram inFile text
  case syn of
    Left err -> throwIO err
    Right syn' -> do
      -- TODO[sgillespie]:
      --
      --  2. Typecheck
      --  3. Desugar to SystemF
      --  4. Add explicit Thunk/Forces
      --  5. Generate LLVM

      -- Write outFile
      writeOutputFile outFile (show syn')

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
