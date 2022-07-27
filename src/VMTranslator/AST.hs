{-# LANGUAGE DeriveFoldable #-}

module VMTranslator.AST
  ( Segment(..)
  , MACommand(..)
  , ALCommand(..)
  , Exp(..)
  ) where

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

data Exp a
  = EMACommand a MACommand
  | EALCommand a ALCommand
  deriving (Foldable, Eq, Show)
