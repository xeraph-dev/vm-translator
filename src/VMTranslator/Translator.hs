module VMTranslator.Translator (translate) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           VMTranslator.AST           (ALCommand (..), BCommand (..),
                                             Exp (..), FCommand (..),
                                             MACommand (..), Segment (..))
import           VMTranslator.Lexer         (Range (..))
import           VMTranslator.Parser        (parse)

newtype State = State
  { label  :: Int
  }

initState :: State
initState = State
  { label = 0
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

translate' _ (state, acc) (EBCommand _ (Label l))
  = (state, "(" <> l <> ")":acc)
translate' _ (state, acc) (EBCommand _ (Goto l))
  = (state,
    "@" <> l <> "\n"
 <> "0;JMP":acc)
translate' _ (state, acc) (EBCommand _ (IfGoto l))
  = (state,
    "@SP\n\
    \M=M-1\n\
    \A=M\n\
    \D=M\n\
    \@" <> l <> "\n"
 <> "D;JNE":acc)
translate' _ (state, acc) (EFCommand _ (Call l i))
  = (state {label=label state + 1} {- *SP=<function end>, SP++, *SP=LCL, SP++, *SP=ARG, SP++, *SP=THIS, SP++, *SP=THAT, D=SP++, ARG=D-(i+4), JUMP to <function> -}
    ,"@" <> l <> ".end\n\
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
     \D=M\n\
     \M=M+1\n\
     \@" <> BS.pack (show $ i + 4) <> "\n\
     \D=D-A\n\
     \@ARG\n\
     \M=D\n\
     \@SP\n\
     \D=M\n\
     \@LCL\n\
     \M=D\n\
     \@" <> l <> "\n\
     \0;JMP\n\
     \(" <>  l <> ".end)" <> "\n":acc)
translate' _ (state, acc) (EFCommand _ (Function l i))
  = (state, {- LCL=SP, SP+=i -}
    "(" <> l <> ")\n" <>
    BS.pack (init $ unlines (replicate (fromInteger i) (BS.unpack
    "@SP\n\
    \A=M\n\
    \M=0\n\
    \@SP\n\
    \M=M+1"))):acc)
translate' fn (state, acc) (EFCommand r Return)
  = (state, {-  -}
    (BS.unlines . snd $ translate' fn (initState, []) (EMACommand r (Pop Argument 0)))
 <> "@ARG\n\
    \D=M+1\n\
    \@SP\n\
    \M=D\n\
    \@LCL\n\
    \D=M\n\
    \@R13\n\
    \AM=D-1\n\
    \D=M\n\
    \@THAT\n\
    \M=D\n\
    \@R13\n\
    \AM=M-1\n\
    \D=M\n\
    \@THIS\n\
    \M=D\n\
    \@R13\n\
    \AM=M-1\n\
    \D=M\n\
    \@ARG\n\
    \M=D\n\
    \@R13\n\
    \AM=M-1\n\
    \D=M\n\
    \@LCL\n\
    \M=D\n\
    \@R13\n\
    \AM=M-1\n\
    \A=M\n\
    \0;JMP":acc)


translate :: ByteString -> ByteString -> Either String ByteString
translate fileName code = case parse code of
                  Left e  -> Left e
                  Right p -> Right . BS.unlines . reverse . snd $ foldl (translate' fileName) (initState,[]) p
