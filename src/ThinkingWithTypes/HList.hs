{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ThinkingWithTypes.HList where

data HList (ts :: [*]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList (_1 ': Bool ': _2) -> String
showBool (_ :# b :# _) = show b

-- Haskell's deriving mechanism does not work with GADTs

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs

instance Ord (HList '[]) where
    HNil `compare` HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
    (a :# as) `compare` (b :# bs) = case (a `compare` b) of
                                        EQ -> as `compare` bs
                                        LT -> LT
                                        GT -> GT

instance Show (HList '[]) where
    show HNil = "[]"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
    show (t :# ts) = show t ++ ", " ++ show ts
