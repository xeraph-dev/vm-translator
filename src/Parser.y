{
module Parser (parseHasm) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First(getFirst))

import qualified Lexer as L
import qualified AST as A
}

%name parseHasm exps
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

accessCommand :: { A.Exp L.Range }
  : push local integer   { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.Local (L.rtRange $2)) 
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push argument integer { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.Argument (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push this integer     { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.This (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push that integer     { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.That (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push constant integer { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.Constant (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push static integer   { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.Static (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push pointer integer  { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.Pointer (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | push temp integer     { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Push (L.rtRange $1))
                                             (A.Temp (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }

  | pop local integer     { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.Local (L.rtRange $2)) 
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | pop argument integer  { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.Argument (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | pop this integer      { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.This (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | pop that integer      { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.That (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | pop static integer    { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.Static (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | pop pointer integer   { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.Pointer (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }
  | pop temp integer      { A.EAccessCommand (L.rtRange $1 <-> L.rtRange $3)
                                             (A.Pop (L.rtRange $1))
                                             (A.Temp (L.rtRange $2))
                                             (unTok $3 (\range (L.Integer int) -> A.EInteger range int)) }

alCommands :: { A.Exp L.Range }
  : add { A.EALCommand (L.rtRange $1) (A.Add (L.rtRange $1)) }
  | sub { A.EALCommand (L.rtRange $1) (A.Sub (L.rtRange $1)) }
  | neg { A.EALCommand (L.rtRange $1) (A.Neg (L.rtRange $1)) }
  | eq  { A.EALCommand (L.rtRange $1) (A.Eq (L.rtRange $1)) }
  | gt  { A.EALCommand (L.rtRange $1) (A.Gt (L.rtRange $1)) }
  | lt  { A.EALCommand (L.rtRange $1) (A.Lt (L.rtRange $1)) }
  | and { A.EALCommand (L.rtRange $1) (A.And (L.rtRange $1)) }
  | or  { A.EALCommand (L.rtRange $1) (A.Or (L.rtRange $1)) }
  | not { A.EALCommand (L.rtRange $1) (A.Not (L.rtRange $1)) }

exp :: { A.Exp L.Range }
  : alCommands { $1 }
  | accessCommand { $1 }

exps :: { [A.Exp L.Range] }
  : many(exp) { $1 }

many(p)
  :           { [] }
  | p many(p) { $1 <-:-> $2 }

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

(<>:<>) :: (A.Exp L.Range, L.Range) -> ([A.Exp L.Range], L.Range) -> [A.Exp L.Range]
(e1, r1) <>:<> (e2, r2) = do
  let (L.AlexPn _ line1 _) = L.stop r1
  let (L.AlexPn _ line2 column) = L.start r2
  if line2 > line1 
    then e1 : e2
    else error $ "Parser error: Missing line break at line " <> show line2 <> ", column " <> show column

(<-:->) :: A.Exp L.Range -> [A.Exp L.Range] -> [A.Exp L.Range]
e1 <-:-> [] = [e1]
e1@(A.EAccessCommand r1 _ _ _) <-:-> e2@((A.EAccessCommand r2 _ _ _):_) = (e1, r1) <>:<> (e2, r2)
e1@(A.EAccessCommand r1 _ _ _) <-:-> e2@((A.EALCommand r2 _):_) = (e1, r1) <>:<> (e2, r2)
e1@(A.EALCommand r1 _) <-:-> e2@((A.EAccessCommand r2 _ _ _):_) = (e1, r1) <>:<> (e2, r2)
e1@(A.EALCommand r1 _) <-:-> e2@((A.EALCommand r2 _):_) = (e1, r1) <>:<> (e2, r2)
}