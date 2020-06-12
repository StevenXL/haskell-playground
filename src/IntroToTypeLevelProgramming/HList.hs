{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module IntroToTypeLevelProgramming.HList where

import Data.Kind (Constraint)

data HList (ts :: [*]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

intBoolList :: HList [Int, Bool]
intBoolList = 1 :# True :# HNil

hLength :: HList a -> Int
hLength HNil = 0
hLength (_ :# xs) = 1 + hLength xs

-- with a heterogenous list, we have a safe version of head
hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

{- The constraint here is hard to read we can intead use a closed type family to
 - give us a constraint
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (t :# ts) == (y :# ys) = t == y && ts == ys
-}

{- We can generalize the type family even more by passing in the constraint
type family AllEq (ts :: [*]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)
-}

{-
 - We didn't ned this type family, but the constraint is pretty ugly if we don't
 - have it. This is an optimization that shows how we can use a closed type
 - family to compute a "thing" that has kind Constraint
-}

type family All (c :: * -> Constraint) (t :: [*]) :: Constraint where
  All _ '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance (All Eq ts) => Eq (HList ts) where
  HNil == HNil = True
  (t :# ts) == (y :# ys) = t == y && ts == ys

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (t :# ts) (y :# ys) = compare t y <> compare ts ys

instance (All Show ts) => Show (HList ts) where
  show HNil = "HNil"
  show (t :# ts) = show t <> " :# " <> show ts
