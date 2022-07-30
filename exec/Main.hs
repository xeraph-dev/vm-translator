{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either                (rights)
import           System.Console.ANSI        (Color (Red),
                                             ColorIntensity (Vivid),
                                             ConsoleLayer (Foreground),
                                             SGR (Reset, SetColor), clearScreen,
                                             setCursorPosition, setSGR)
import           System.Console.CmdArgs     (Data, Default (def), Typeable,
                                             cmdArgs, details, explicit, help,
                                             helpArg, name, program, summary,
                                             typFile, (&=))
import           System.Console.Haskeline   (InputT, Settings (Settings),
                                             autoAddHistory, complete,
                                             getInputLine, historyFile,
                                             noCompletion, outputStrLn,
                                             runInputT)
import           System.Directory           (doesDirectoryExist,
                                             getDirectoryContents, makeAbsolute)
import           System.FilePath            (isExtensionOf, takeBaseName,
                                             (-<.>), (<.>), (</>))
import           VMTranslator.Translator    (translate)

data Args
  = Args
  { argsInput  :: String
  , argsOutput :: String
  , argsRepl   :: Bool
  }
  deriving (Show, Data, Typeable)

args :: Args
args
  = Args
  {  argsInput = def &= explicit &= name "i" &= name "input" &= help "Path of VM file" &= typFile
  , argsOutput = def &= explicit &= name "o" &= name "output" &= help "Path for Hack Assembly output" &= typFile
  ,   argsRepl = def &= explicit &= name "r" &= name "repl" &= help "Start a repl shell"
  }
  &= program "vmt"
  &= summary "VM Translator"
  &= details ["An VM Translator for Hack VM.", "Produce hack assembly <file_name>.asm"]
  &= helpArg [explicit, name "h", name "help"]


process :: (ByteString -> Either String ByteString) -> String -> IO ()
process _ "" = return ()
process f str = do
  case f $ BS.pack str of
    Left e -> liftIO (setSGR [SetColor Foreground Vivid Red] >> putStrLn e >> setSGR [Reset])
    Right h -> BS.putStr h

repl :: (ByteString -> Either String ByteString) -> InputT IO ()
repl f = do
  minput <- getInputLine "\ESC[35m>>>\ESC[0m " -- Magenta, ansi-terminal methods does not work with getInputLine


  case minput of
    Nothing       -> outputStrLn "\ESC[33mLeaving\ESC[0m" -- Yellow, ansi-terminal methods does not work with outputStrLn
    Just "clear!" -> liftIO (clearScreen >> setCursorPosition 0 0) >> repl f
    Just input    -> liftIO (process f input) >> repl f

replLoop :: (ByteString -> Either String ByteString) -> IO ()
replLoop f
  =  clearScreen
  >> setCursorPosition 0 0
  >> runInputT Settings
    { historyFile = Nothing
    , autoAddHistory = True
    , complete = noCompletion
    } (repl f)

translateVM :: (ByteString -> Either String ByteString) -> String -> IO (Either String ByteString)
translateVM f path
  = do
    file <- readFile path
    case f $ BS.pack file of
      Left e  -> printError e >> return (Left e)
      Right r -> return (Right r)

printError :: String -> IO ()
printError e = liftIO (setSGR [SetColor Foreground Vivid Red] >> putStrLn e >> setSGR [Reset])

main :: IO ()
main = do
  progArgs@Args {argsInput=input, argsOutput=output'} <- cmdArgs args

  isDir <- doesDirectoryExist input
  absoluteInput <- makeAbsolute input

  let output
        | not $ null output' = output'
        | isDir = absoluteInput </> takeBaseName input <.> "asm"
        | otherwise = input -<.> "asm"

  let translate' = translate . BS.pack $ takeBaseName input

  if null input || argsRepl progArgs
    then replLoop translate'
    else if isDir
      then do
        dirs <- getDirectoryContents input
        let vms = map (input </>) $ filter (isExtensionOf "vm") dirs
        let t = (\s -> translateVM (translate . BS.pack $ takeBaseName s) s) <$> vms
        res <- sequence t >>= \c -> return (BS.concat $ rights c)

        case translate "" "call Sys.init 0" of
          Left e -> printError e
          Right bootstrap -> BS.writeFile output ("@256\nD=A\n@SP\nM=D\n" <> bootstrap <> res)

      else do
      file <- readFile input
      case translate' $ BS.pack file of
        Left e  -> printError e
        Right h -> BS.writeFile output h
