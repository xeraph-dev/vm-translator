module VMTranslator.Translator (translate) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map                   as M
import qualified Data.Maybe
import           VMTranslator.AST           (ALCommand (..), BCommand (..),
                                             Exp (..), FCommand (..),
                                             MACommand (..), Segment (..))
import           VMTranslator.Lexer         (Range (..))
import           VMTranslator.Parser        (parse)

data State = State
  { label   :: Int
  , fun     :: ByteString
  , funCall :: M.Map ByteString Int
  }

initState :: State
initState = State
  { label = 0
  , fun = ""
  , funCall = M.empty
  }



translate' :: ByteString -> (State, [ByteString]) -> Exp Range -> (State, [ByteString])
translate' _ (state, acc) (EMACommand _ (Push Constant int))
  = (state,
    ("@" <> BS.pack (show int) <> "\n" {- *SP = i , SP++ -}
  <> "D=A\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1"):acc)
translate' fileName (state, acc) (EMACommand _ (Push Static int))
  = (state,
    ("@" <> (fileName <> "." <> BS.pack (show int)) <> "\n" {- *SP = FileName.int, SP++ -}
  <> "D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1"):acc)
translate' fileName (state, acc) (EMACommand _ (Pop Static int))
  = (state,
    ("@SP\n\
     \M=M-1\n\
     \A=M\n\
     \D=M\n\
     \@" <> (fileName <> "." <> BS.pack (show int)) <> "\n"
  <> "M=D"):acc)
translate' _ (state, acc) (EMACommand _ (Push Pointer int))
  = let p = if int == 0 then "THIS" else "THAT"
    in (state,
       ("@" <> p <> "\n" {- *SP = THIS/THAT, SP++ -}
     <> "D=M\n\
        \@SP\n\
        \A=M\n\
        \M=D\n\
        \@SP\n\
        \M=M+1"):acc)
translate' _ (state, acc) (EMACommand _ (Pop Pointer int))
  = let p = if int == 0 then "THIS" else "THAT"
    in (state,
       ("@SP\n\
        \M=M-1\n\
        \A=M\n\
        \D=M\n\
        \@" <> p <> "\n"
     <> "M=D"):acc)
translate' _ (state, acc) (EMACommand _ (Push Temp int))
  = (state,
    ("@" <> BS.pack (show $ int + 5) <> "\n"  {- addr = 5 + i, *SP = *addr, SP++ -}
  <> "D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1"):acc)
translate' _ (state, acc) (EMACommand _ (Pop Temp int))
  = (state,
    ("@SP\n\
     \M=M-1\n\
     \A=M\n\
     \D=M\n\
     \@" <> BS.pack (show $ int + 5) <> "\n"
  <> "M=D"):acc)
translate' _ (state, acc) (EMACommand _ (Push seg int))
  = let seg' = case seg of
                  Local    -> "LCL"
                  Argument -> "ARG"
                  This     -> "THIS"
                  That     -> "THAT"
  in (state,
    ("@" <> BS.pack (show int) <> "\n"  {- addr = seg' + i, *SP = *addr, SP++ -}
  <> "D=A\n\
     \@" <> seg' <> "\n"
  <> "A=D+M\n\
     \D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1"):acc)
translate' _ (state, acc) (EMACommand _ (Pop seg int))
  = let seg' = case seg of
                  Local    -> "LCL"
                  Argument -> "ARG"
                  This     -> "THIS"
                  That     -> "THAT"
                  _        -> error "Can't pop a constant command"
  in (state,
    ("@" <> BS.pack (show int) <> "\n"  {- addr = seg' + i, SP--, *addr = *SP -}
  <> "D=A\n\
     \@" <> seg' <> "\n"
  <> "D=D+M\n\
     \@R13\n\
     \M=D\n\
     \@SP\n\
     \M=M-1\n\
     \A=M\n\
     \D=M\n\
     \@R13\n\
     \A=M\n\
     \M=D"):acc)

translate' _ (state, acc) (EALCommand _ Add)
  = (state,
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \M=D+M\n\
    \@SP\n\
    \M=M+1":acc)
translate' _ (state, acc) (EALCommand _ Sub)
  = (state,
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \M=M-D\n\
    \@SP\n\
    \M=M+1":acc)
translate' _ (state, acc) (EALCommand _ Neg)
  = (state, "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \M=-M\n\
    \@SP\n\
    \M=M+1":acc)
translate' _ (state, acc) (EALCommand _ Eq)
  = (state {label=label state + 1},
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M-D\n\
    \@LABEL." <> l <> "\n"
 <> "D;JEQ\n\
    \@SP\n\
    \A=M\n\
    \M=0\n\
    \@LABEL." <> l <> ".end" <> "\n"
 <> "0;JMP\n\
    \(LABEL." <> l <> ")" <> "\n"
 <> "@SP\n\
    \A=M\n\
    \M=-1\n\
    \(LABEL." <> l <> ".end" <> ")" <> "\n"
 <> "@SP\n\
    \M=M+1":acc)
    where
      l = BS.pack (show $ label state)
translate' _ (state, acc) (EALCommand _ Gt)
  = (state {label=label state + 1},
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M-D\n\
    \@LABEL." <> l <> "\n"
 <> "D;JGT\n\
    \@SP\n\
    \A=M\n\
    \M=0\n\
    \@LABEL." <> l <> ".end" <> "\n"
 <> "0;JMP\n\
    \(LABEL." <> l <> ")" <> "\n"
 <> "@SP\n\
    \A=M\n\
    \M=-1\n\
    \(LABEL." <> l <> ".end" <> ")" <> "\n"
 <> "@SP\n\
    \M=M+1":acc)
    where
      l = BS.pack (show $ label state)
translate' _ (state, acc) (EALCommand _ Lt)
  = (state {label=label state + 1},
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M-D\n\
    \@LABEL." <> l <> "\n"
 <> "D;JLT\n\
    \@SP\n\
    \A=M\n\
    \M=0\n\
    \@LABEL." <> l <> ".end" <> "\n"
 <> "0;JMP\n\
    \(LABEL." <> l <> ")" <> "\n"
 <> "@SP\n\
    \A=M\n\
    \M=-1\n\
    \(LABEL." <> l <> ".end" <> ")" <> "\n"
 <> "@SP\n\
    \M=M+1":acc)
    where
      l = BS.pack (show $ label state)
translate' _ (state, acc) (EALCommand _ And)
  = (state,
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \M=D&M\n\
    \@SP\n\
    \M=M+1":acc)
translate' _ (state, acc) (EALCommand _ Or)
  = (state,
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \M=D|M\n\
    \@SP\n\
    \M=M+1":acc)
translate' _ (state, acc) (EALCommand _ Not)
  = (state, "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \M=!M\n\
    \@SP\n\
    \M=M+1":acc)

translate' fileName (state, acc) (EBCommand _ (Label l))
  = (state, "(" <> l' <> ")":acc)
  where
    l' = (if not . BS.null $ fun state then fun state else fileName) <> "$" <> l
translate' fileName (state, acc) (EBCommand _ (Goto l))
  = (state,
    "@" <> l' <> "\n"
 <> "0;JMP":acc)
  where
    l' = (if not . BS.null $ fun state then fun state else fileName) <> "$" <> l
translate' fileName (state, acc) (EBCommand _ (IfGoto l))
  = (state,
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@" <> l' <> "\n"
 <> "D;JNE":acc)
  where
    l' = (if not . BS.null $ fun state then fun state else fileName) <> "$" <> l
translate' fileName (state, acc) (EFCommand _ (Call l i))
  = (state {funCall=M.insert l' (count + 1) (funCall state)} {- *SP=<function end>, SP++, *SP=LCL, SP++, *SP=ARG, SP++, *SP=THIS, SP++, *SP=THAT, D=SP++, ARG=D-(i+4), JUMP to <function> -}
    ,"@" <> ret <> "\n\
     \D=A\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1\n\

     \@LCL\n\
     \D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1\n\

     \@ARG\n\
     \D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1\n\

     \@THIS\n\
     \D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \M=M+1\n\

     \@THAT\n\
     \D=M\n\
     \@SP\n\
     \A=M\n\
     \M=D\n\
     \@SP\n\
     \MD=M+1\n\

     \@" <> BS.pack (show $ i + 5) <> "\n\
     \D=A\n\
     \@SP\n\
     \D=M-D\n\
     \@ARG\n\
     \M=D\n\

     \@SP\n\
     \D=M\n\
     \@LCL\n\
     \M=D\n\

     \@" <> l <> "\n\
     \0;JMP\n\
     \(" <> ret <> ")":acc)
     where
      l' = fileName <> "." <> l
      count = Data.Maybe.fromMaybe 0 (M.lookup l' (funCall state))
      ret = l' <> "$ret." <> BS.pack (show count)
translate' fn (state, acc) (EFCommand r (Function l i))
  = (state {fun=l}, {- LCL=SP, SP+=i -}
    "(" <> l <> ")" <>
    (if i == 0
      then ""
      else "\n"
      <> BS.pack (init $ unlines (replicate  (fromInteger i)
          (BS.unpack . BS.init . BS.unlines . snd $ translate' fn (initState, []) (EMACommand r (Push Constant 0)))
         ))):acc)
translate' _ (state, acc) (EFCommand _ Return)
  = (state, {-  -}
    "@LCL\n\
    \D=M\n\
    \@R13\n\
    \M=D\n\

    \@5\n\
    \D=A\n\
    \@R13\n\
    \A=M-D\n\
    \D=M\n\
    \@R14\n\
    \M=D\n\

    \@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@ARG\n\
    \A=M\n\
    \M=D\n\

    \@ARG\n\
    \D=M+1\n\
    \@SP\n\
    \M=D\n\

    \@1\n\
    \D=A\n\
    \@R13\n\
    \A=M-D\n\
    \D=M\n\
    \@THAT\n\
    \M=D\n\

    \@2\n\
    \D=A\n\
    \@R13\n\
    \A=M-D\n\
    \D=M\n\
    \@THIS\n\
    \M=D\n\

    \@3\n\
    \D=A\n\
    \@R13\n\
    \A=M-D\n\
    \D=M\n\
    \@ARG\n\
    \M=D\n\

    \@4\n\
    \D=A\n\
    \@R13\n\
    \A=M-D\n\
    \D=M\n\
    \@LCL\n\
    \M=D\n\

    \@R14\n\
    \A=M\n\
    \0;JMP":acc)


translate :: ByteString -> ByteString -> Either String ByteString
translate fileName code = case parse code of
                  Left e  -> Left e
                  Right p -> Right . BS.unlines . reverse . snd $ foldl (translate' fileName) (initState,[]) p
