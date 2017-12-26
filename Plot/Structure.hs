{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Plot.Structure(Function(..), ExprF(..), Expr,
                      (.+.), (.-), (.*.), (./), (.^), func,
                      evalFunc, eval,
                      Fix(..)) where

import Data.Functor.Foldable

-- Using general recursion schemes is absolutely overkill for this
-- simple example, but it can be good practice.

-- This is a simple enumeration type, for the different function
-- names. evalFunc, defined below, translates these into actual
-- functions.
data Function = Sqrt | Sin | Cos | Tan | Ln
                deriving (Show, Read, Eq, Ord, Enum)

-- When using general recursion schemes, we do not write recursive
-- data types. We write data types whose Functor instance is
-- parameterized based on the "internal" values. For example, compare
-- the list type written normally to the list type written in this
-- recursion scheme style.
--
-- data List a = Nil | Cons a (List a)
--
-- data ListF a f = NilF | ConsF a f
data ExprF a f = f :+: f
               | f :-  f
               | f :*: f
               | f :/  f
               | f :^  f
               | Func Function f
               | Const a
               | Var
                 deriving (Show, Functor)

-- Of course, a non-recursive type is only going to be finite in its
-- depth. So we use the type Fix, which is recursive, to construct a
-- new recursive data type out of our non-recursive type defined
-- above. Defining our type in this way allows us to use recursion
-- schemes, such as catamorphisms and anamorphisms, since all of the
-- recursion is, in some sense, concentrated in this one place.
type Expr a = Fix (ExprF a)

-- These are all convenience functions. The downside of defining our
-- type using this fixed point technique is that constructing actual
-- instances of it tends to be a fairly ugly process littered with Fix
-- and unFix calls. So we'll hide all of those behind a pretty
-- interface here.

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

-- This converts the Function type into an actual function.
evalFunc :: Floating a => Function -> a -> a
evalFunc Sqrt = sqrt
evalFunc Sin  =  sin
evalFunc Cos  =  cos
evalFunc Tan  =  tan
evalFunc Ln   =  log

-- Here, we see more use of the non-recursive type defined
-- above. Where ExprF is designed to be the non-recursive part of the
-- recursive type Expr, evalF is the non-recursive part of the
-- recursive function eval. We're going to define what we do at each
-- step in evalF and then tell Haskell what "kind" of recursion we
-- want, and it will do the rest. In this case, we want a
-- catamorphism, which is a type of recursion that takes a recursive
-- structure and folds it in on itself, producing a single scalar
-- value at the end. Catamorphisms are a generalization of the
-- Foldable type in Haskell, but whereas Foldable treats every data
-- structure as being "list-like", a catamorphism allows its user to
-- operate on the actual data structure while still recursing in a
-- very well-defined manner.
evalF :: Floating a => a -> ExprF a a -> a
evalF _ (x :+: y)  = x +  y
evalF _ (x :-  y)  = x -  y
evalF _ (x :*: y)  = x *  y
evalF _ (x :/  y)  = x /  y
evalF _ (x :^  y)  = x ** y
evalF _ (Func f x) = evalFunc f x
evalF _ (Const x)  = x
evalF a Var        = a

-- Now we actually "request" the recursion in the form of a
-- catamorphism.
eval :: Floating a => a -> Expr a -> a
eval v = cata (evalF v)
