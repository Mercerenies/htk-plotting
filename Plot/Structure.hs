{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Plot.Structure(Function(..), ExprF(..), Expr,
                      (.+.), (.-), (.*.), (./), (.^), func,
                      evalFunc, eval,
                      Fix(..)) where

import Data.Functor.Foldable

data Function = Sqrt | Sin | Cos | Tan | Ln
                deriving (Show, Read, Eq, Ord, Enum)

data ExprF a f = f :+: f
               | f :-  f
               | f :*: f
               | f :/  f
               | f :^  f
               | Func Function f
               | Const a
               | Var
                 deriving (Show, Functor)

type Expr a = Fix (ExprF a)

(.+.) :: Expr a -> Expr a -> Expr a
a .+. b = Fix (a :+: b)

(.-) :: Expr a -> Expr a -> Expr a
a .- b = Fix (a :- b)

(.*.) :: Expr a -> Expr a -> Expr a
a .*. b = Fix (a :*: b)

(./) :: Expr a -> Expr a -> Expr a
a ./ b = Fix (a :/ b)

(.^) :: Expr a -> Expr a -> Expr a
a .^ b = Fix (a :^ b)

func :: Function -> Expr a -> Expr a
func f e = Fix (Func f e)

evalFunc :: Floating a => Function -> a -> a
evalFunc Sqrt = sqrt
evalFunc Sin  =  sin
evalFunc Cos  =  cos
evalFunc Tan  =  tan
evalFunc Ln   =  log

evalF :: Floating a => a -> ExprF a a -> a
evalF _ (x :+: y)  = x +  y
evalF _ (x :-  y)  = x -  y
evalF _ (x :*: y)  = x *  y
evalF _ (x :/  y)  = x /  y
evalF _ (x :^  y)  = x ** y
evalF _ (Func f x) = evalFunc f x
evalF _ (Const x)  = x
evalF a Var        = a

eval :: Floating a => a -> Expr a -> a
eval v = cata (evalF v)
