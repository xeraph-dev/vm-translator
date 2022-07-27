{
module VMTranslator.Lexer 
  ( Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range(..)
  , RangedToken(..)
  , Token(..)
  , scan
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]
$symbol = [\(\)\+\-\!\|\&\@\=\;]

@id_start = ($alpha | \_ | \. | \: | \$)
@id = @id_start (@id_start | $digit)*

tokens :-

<0> $white+ ;

<0> "//".* ;

<0> $symbol   { tokSymbol }
<0> $digit+   { tokInteger }
<0> @id       { tokId }

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
  = Symbol ByteString
  | Integer Integer
  | Identifier ByteString
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start', _, str, _) len = Range {start = start', stop = stop'}
  where
    stop' = BS.foldl' alexMove start' $ BS.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len = tok (Identifier $ BS.take len str) inp len

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len = tok (Integer . read . BS.unpack $ BS.take len str) inp len

tokSymbol :: AlexAction RangedToken
tokSymbol inp@(_, _, str, _) len = tok (Symbol $ BS.take len str) inp len

scan :: ByteString -> Either String [RangedToken]
scan input = runAlex input go
  where 
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}