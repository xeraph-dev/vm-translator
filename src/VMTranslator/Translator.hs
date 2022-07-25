module VMTranslator.Translator (translate) where

import           Data.ByteString.Lazy.Char8 (ByteString)

translate :: ByteString -> Either String ByteString
translate code = Right $ code <> "\n"
