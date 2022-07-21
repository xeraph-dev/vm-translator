module LexerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Lexer (Token (..), scanMany, RangedToken (rtToken))
import Data.ByteString.Lazy.Char8 (ByteString)

spec :: Spec
spec = do
  describe "Stack operators" $ do
    it "push" $ scanTokens "push" `shouldBe` Right [Push, EOF]
    it "pop"  $ scanTokens "pop"  `shouldBe` Right [Pop, EOF] 

  describe "Arithmetic / Logical commands" $ do
    it "add" $ scanTokens "add"  `shouldBe` Right [Add, EOF]
    it "sub" $ scanTokens "sub"  `shouldBe` Right [Sub, EOF]
    it "neg" $ scanTokens "neg"  `shouldBe` Right [Neg, EOF]
    it "eq"  $ scanTokens "eq"   `shouldBe` Right [Eq, EOF]
    it "gt"  $ scanTokens "gt"   `shouldBe` Right [Gt, EOF]
    it "lt"  $ scanTokens "lt"   `shouldBe` Right [Lt, EOF]
    it "and" $ scanTokens "and"  `shouldBe` Right [And, EOF]
    it "or"  $ scanTokens "or"   `shouldBe` Right [Or, EOF]
    it "not" $ scanTokens "not"  `shouldBe` Right [Not, EOF]

  describe "Memory segments" $ do
    it "local"    $ scanTokens "local"    `shouldBe` Right [Local, EOF]
    it "argument" $ scanTokens "argument" `shouldBe` Right [Argument, EOF]
    it "this"     $ scanTokens "this"     `shouldBe` Right [This, EOF]
    it "that"     $ scanTokens "that"     `shouldBe` Right [That, EOF]
    it "constant" $ scanTokens "constant" `shouldBe` Right [Constant, EOF]
    it "static"   $ scanTokens "static"   `shouldBe` Right [Static, EOF]
    it "pointer"  $ scanTokens "pointer"  `shouldBe` Right [Pointer, EOF]
    it "temp"     $ scanTokens "temp"     `shouldBe` Right [Temp, EOF]

  describe "Constants" $ do
    it "Integer" $ scanTokens "2" `shouldBe` Right [Integer 2, EOF]


scanTokens :: ByteString -> Either String [Token]
scanTokens s = case scanMany s of
                (Left e) -> Left e
                (Right rts) -> Right $ map rtToken rts
