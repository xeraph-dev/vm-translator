{-# LANGUAGE DeriveFoldable #-}

module VMTranslator.AST
  ( Segment(..)
  , MACommand(..)
  , ALCommand(..)
  , BCommand(..)
  , FCommand(..)
  , Exp(..)
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)

data Segment
  = Local     | Argument
  | This      | That
  | Constant  | Static
  | Pointer   | Temp
  deriving (Eq, Show)

data MACommand
  = Push Segment Integer
  | Pop Segment Integer
  deriving (Eq, Show)

data ALCommand
  = Add | Sub | Neg
  | Eq  | Gt  | Lt
  | And | Or  | Not
  deriving (Eq, Show)

data BCommand
  = Goto ByteString
  | IfGoto ByteString
  | Label ByteString
  deriving (Eq, Show)

data FCommand
  = Call ByteString Integer
  | Function ByteString Integer
  | Return
  deriving (Eq, Show)

data Exp a
  = EMACommand a MACommand
  | EALCommand a ALCommand
  | EBCommand a BCommand
  | EFCommand a FCommand
  deriving (Foldable, Eq, Show)
