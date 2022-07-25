{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
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


replaceExt :: String -> String -> String
replaceExt x = ((reverse . dropWhile (/= '.') $ reverse x) <>)

main :: IO ()
main = do
  progArgs@Args {argsInput=input, argsOutput=output'} <- cmdArgs args
  let output = if not $ null output' then output' else replaceExt input "hack"

  if null input || argsRepl progArgs
    then replLoop translate
    else do
      file <- readFile input
      case translate $ BS.pack file of
        Left e -> liftIO (setSGR [SetColor Foreground Vivid Red] >> putStrLn e >> setSGR [Reset])
        Right h -> BS.writeFile output h

