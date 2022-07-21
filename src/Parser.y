{
module Parser (parseHasm) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First(getFirst))

import qualified Lexer as L
import qualified AST as A
}

%name parseHasm exp
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
  -- Stack operators
  push      { L.RangedToken L.Push _ }
  pop       { L.RangedToken L.Pop _ }

  -- Arithmetic / Logical commands
  add       { L.RangedToken L.Add _ }
  sub       { L.RangedToken L.Sub _ }
  neg       { L.RangedToken L.Neg _ }
  eq        { L.RangedToken L.Eq _ }
  gt        { L.RangedToken L.Gt _ }
  lt        { L.RangedToken L.Lt _ }
  and       { L.RangedToken L.And _ }
  or        { L.RangedToken L.Or _ }
  not       { L.RangedToken L.Not _ }

  -- Memory segments
  local     { L.RangedToken L.Local _ }
  argument  { L.RangedToken L.Argument _ }
  this      { L.RangedToken L.This _ }
  that      { L.RangedToken L.That _ }
  constant  { L.RangedToken L.Constant _ }
  static    { L.RangedToken L.Static _ }
  pointer   { L.RangedToken L.Pointer _ }
  temp      { L.RangedToken L.Temp _ }

  -- Constants
  integer   { L.RangedToken (L.Integer _) _ }


%%

exp :: { A.Exp L.Range }
  : push local integer { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                          (A.Push (L.rtRange $1))
                                          (A.Local (L.rtRange $2)) 
                                          (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2
}