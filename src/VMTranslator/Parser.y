{
module VMTranslator.Parser (parse) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified VMTranslator.Lexer as L
import qualified VMTranslator.AST as A
import VMTranslator.Lexer (runAlex)
}

%name parseVMT exps
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
  push        { L.RangedToken (L.Identifier "push") _ }
  pop         { L.RangedToken (L.Identifier "pop") _ }

  local       { L.RangedToken (L.Identifier "local") _ }
  argument    { L.RangedToken (L.Identifier "argument") _ }
  this        { L.RangedToken (L.Identifier "this") _ }
  that        { L.RangedToken (L.Identifier "that") _ }
  constant    { L.RangedToken (L.Identifier "constant") _ }
  static      { L.RangedToken (L.Identifier "static") _ }
  pointer     { L.RangedToken (L.Identifier "pointer") _ }
  temp        { L.RangedToken (L.Identifier "temp") _ }

  add         { L.RangedToken (L.Identifier "add") _ }
  sub         { L.RangedToken (L.Identifier "sub") _ }
  neg         { L.RangedToken (L.Identifier "neg") _ }
  eq          { L.RangedToken (L.Identifier "eq") _ }
  gt          { L.RangedToken (L.Identifier "gt") _ }
  lt          { L.RangedToken (L.Identifier "lt") _ }
  and         { L.RangedToken (L.Identifier "and") _ }
  or          { L.RangedToken (L.Identifier "or") _ }
  not         { L.RangedToken (L.Identifier "not") _ }

  if          { L.RangedToken (L.Identifier "if") _ }
  goto        { L.RangedToken (L.Identifier "goto") _ }
  label       { L.RangedToken (L.Identifier "label") _ }

  call        { L.RangedToken (L.Identifier "call") _ }
  function    { L.RangedToken (L.Identifier "function") _ }
  return      { L.RangedToken (L.Identifier "return") _ }

  '-'         { L.RangedToken (L.Symbol "-") _ }

  identifier  { L.RangedToken (L.Identifier _) _ }
  integer     { L.RangedToken (L.Integer _) _ }

%%

maCommand :: { A.Exp L.Range }
  : push local integer    { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.Local (unTok $3 (\s (L.Integer int) -> int))) }
  | push argument integer { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.Argument (unTok $3 (\s (L.Integer int) -> int))) }
  | push this integer     { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.This (unTok $3 (\s (L.Integer int) -> int))) }
  | push that integer     { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.That (unTok $3 (\s (L.Integer int) -> int))) }
  | push constant integer { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.Constant (unTok $3 (\s (L.Integer int) -> int))) }
  | push static integer   { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.Static (unTok $3 (\s (L.Integer int) -> int))) }
  | push pointer integer  { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.Pointer (unTok $3 (\s (L.Integer int) -> int))) }
  | push temp integer     { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Push A.Temp (unTok $3 (\s (L.Integer int) -> int))) }

  | pop local integer    { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.Local (unTok $3 (\s (L.Integer int) -> int))) }
  | pop argument integer { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.Argument (unTok $3 (\s (L.Integer int) -> int))) }
  | pop this integer     { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.This (unTok $3 (\s (L.Integer int) -> int))) }
  | pop that integer     { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.That (unTok $3 (\s (L.Integer int) -> int))) }
  | pop static integer   { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.Static (unTok $3 (\s (L.Integer int) -> int))) }
  | pop pointer integer  { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.Pointer (unTok $3 (\s (L.Integer int) -> int))) }
  | pop temp integer     { A.EMACommand (L.rtRange $1 <-> L.rtRange $3) (A.Pop A.Temp (unTok $3 (\s (L.Integer int) -> int))) }


alCommand :: { A.Exp L.Range }
  : add { A.EALCommand (L.rtRange $1) A.Add }
  | sub { A.EALCommand (L.rtRange $1) A.Sub }
  | neg { A.EALCommand (L.rtRange $1) A.Neg }
  | eq  { A.EALCommand (L.rtRange $1) A.Eq }
  | gt  { A.EALCommand (L.rtRange $1) A.Gt }
  | lt  { A.EALCommand (L.rtRange $1) A.Lt }
  | and { A.EALCommand (L.rtRange $1) A.And }
  | or  { A.EALCommand (L.rtRange $1) A.Or }
  | not { A.EALCommand (L.rtRange $1) A.Not }

bCommand :: { A.Exp L.Range }
  : goto identifier         { A.EBCommand (L.rtRange $1 <-> L.rtRange $2) (A.Goto (unTok $2 (\s (L.Identifier id) -> id))) }
  | if '-' goto identifier  { A.EBCommand (L.rtRange $1 <-> L.rtRange $4) (A.IfGoto (unTok $4 (\s (L.Identifier id) -> id))) }
  | label identifier        { A.EBCommand (L.rtRange $1 <-> L.rtRange $2) (A.Label (unTok $2 (\s (L.Identifier id) -> id))) }

fCommand :: { A.Exp L.Range }
  : call identifier integer     { A.EFCommand (L.rtRange $1 <-> L.rtRange $3) (A.Call (unTok $2 (\s (L.Identifier id) -> id)) (unTok $3 (\s (L.Integer int) -> int))) }
  | function identifier integer { A.EFCommand (L.rtRange $1 <-> L.rtRange $3) (A.Function (unTok $2 (\s (L.Identifier id) -> id)) (unTok $3 (\s (L.Integer int) -> int))) }
  | return                      { A.EFCommand (L.rtRange $1) A.Return }

exp :: { A.Exp L.Range }
  : alCommand { $1 }
  | maCommand { $1 }
  | bCommand  { $1 }
  | fCommand  { $1 }


exps
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


extractValue :: L.Range -> L.Token -> ByteString
extractValue _ (L.Symbol sym) = sym
extractValue _ (L.Identifier id) = id
extractValue _ (L.Integer int) = BS.pack $ show int

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
e1@(A.EMACommand r1 _) <-:-> e2@((A.EMACommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EMACommand r1 _) <-:-> e2@((A.EALCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EMACommand r1 _) <-:-> e2@((A.EBCommand r2 _):_) = (e1, r1)   <>:<> (e2, r2)
e1@(A.EMACommand r1 _) <-:-> e2@((A.EFCommand r2 _):_) = (e1, r1)   <>:<> (e2, r2)

e1@(A.EALCommand r1 _) <-:-> e2@((A.EALCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EALCommand r1 _) <-:-> e2@((A.EMACommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EALCommand r1 _) <-:-> e2@((A.EBCommand r2 _):_) = (e1, r1)   <>:<> (e2, r2)
e1@(A.EALCommand r1 _) <-:-> e2@((A.EFCommand r2 _):_) = (e1, r1)   <>:<> (e2, r2)

e1@(A.EBCommand r1 _)  <-:-> e2@((A.EBCommand r2 _):_) = (e1, r1)   <>:<> (e2, r2)
e1@(A.EBCommand r1 _)  <-:-> e2@((A.EMACommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EBCommand r1 _)  <-:-> e2@((A.EALCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EBCommand r1 _)  <-:-> e2@((A.EFCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)

e1@(A.EFCommand r1 _)  <-:-> e2@((A.EFCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EFCommand r1 _)  <-:-> e2@((A.EMACommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EFCommand r1 _)  <-:-> e2@((A.EALCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)
e1@(A.EFCommand r1 _)  <-:-> e2@((A.EBCommand r2 _):_) = (e1, r1)  <>:<> (e2, r2)

parse :: ByteString -> Either String [A.Exp L.Range]
parse input = runAlex input parseVMT
}