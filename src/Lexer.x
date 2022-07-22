{
module Lexer
  ( Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  
  , Range (..)
  , RangedToken (..)
  , Token (..)
  , scanMany
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

tokens :-

<0> $white+		;

<0> "//" .*   ;

-- Stack operators
<0> push      { tok Push }
<0> pop       { tok Pop }

-- Arithmetic / Logical commands
<0> add       { tok Add }
<0> sub       { tok Sub }
<0> neg       { tok Neg }
<0> eq        { tok Eq }
<0> gt        { tok Gt }
<0> lt        { tok Lt }
<0> and       { tok And }
<0> or        { tok Or }
<0> not       { tok Not }

-- Memory segments
<0> local     { tok Local }
<0> argument  { tok Argument }
<0> this      { tok This }
<0> that      { tok That }
<0> constant  { tok Constant }
<0> static    { tok Static }
<0> pointer   { tok Pointer }
<0> temp      { tok Temp }


-- Constants
<0> $digit+    { tokInteger }

{
data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  {
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  _ <- alexGetStartCode
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  -- Stack operators
  = Push
  | Pop
  -- Arithmetic / Logical commands
  | Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  -- Memory segments
  | Local
  | Argument
  | This
  | That
  | Constant
  | Static
  | Pointer
  | Temp
  -- Constants
  | Integer Integer
  -- EOF
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start', _, str, _) len = Range{start = start', stop = stop'}
  where
    stop' = BS.foldl' alexMove start' $ BS.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer . read . BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}