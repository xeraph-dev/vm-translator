{-# LANGUAGE DeriveFoldable #-}

module AST 
  ( EInteger(..)
  , Operator(..)
  , ALCommand(..)
  , Segment(..)
  , Exp(..)
  ) where

data EInteger a
  = EInteger a Integer
  deriving (Foldable, Show, Eq)

data Operator a
  = Push a
  | Pop a
  deriving (Foldable, Show, Eq)

data ALCommand a
  = Add a
  | Sub a
  | Neg a
  | Eq a
  | Gt a
  | Lt a
  | And a
  | Or a
  | Not a
  deriving (Foldable, Show, Eq)

data Segment a
  = Local a
  | Argument a
  | This a
  | That a
  | Constant a
  | Static a
  | Pointer a
  | Temp a
  deriving (Foldable, Show, Eq)

data Exp a
  = EAccessCommand a (Operator a) (Segment a) (EInteger a)
  | EALCommand a (ALCommand a)
  deriving (Foldable, Show, Eq)