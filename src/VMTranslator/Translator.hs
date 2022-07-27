module VMTranslator.Translator (translate) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           VMTranslator.AST           (ALCommand (..), Exp (..),
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
                  _        -> error "This case can't happen"
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
                  _        -> error "This case can't happen"
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

translate :: ByteString -> ByteString -> Either String ByteString
translate fileName code = case parse code of
                  Left e  -> Left e
                  Right p -> Right . BS.unlines . reverse . snd $ foldl (translate' fileName) (initState,[]) p
