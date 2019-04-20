{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module ThinkingWithTypes.ExistentialTypes where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Typeable
import GHC.Exts (Constraint)

-- Why do we need the Show t constraint?
data HasShow where
  HasShow :: Show t => t -> HasShow

{-
instance Show HasShow where
  show :: HasShow -> String
  show (HasShow s) = "HasShow " ++ show s
-}
-- elimHasShow takes a polymorphic function that takes any value of type a,
-- provided that type a is an instance of show, and returns back an r
elimHasShow ::
     (forall a. Show a =>
                  a -> r)
  -> HasShow
  -> r
elimHasShow f (HasShow a) = f a

-- Show instances in terms of elimHasShow
instance Show HasShow where
  show :: HasShow -> String
  show = elimHasShow (\s -> "HasShow " ++ show s)

-- DYNAMIC TYPES
data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic ::
     (forall a. Typeable a =>
                  a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic t) = f t

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 ::
     forall a b r. (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic d1 <*> fromDynamic d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $
  asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int @Int a b (+)
    , liftD2 @String @Int a b (\str int -> str ++ show int)
    , liftD2 @Int @String a b (\int str -> show int ++ str)
    ]

-- EXAMPLE: FACTOR OUT HASDYNAMIC AND HASSHOW
-- Has is a type constructor that is parameterized over c; c has the kind `* ->
-- Constraint`; that is, by applying a concrete type to c, we end up with a
-- constraint
data Has (c :: * -> Constraint) where
  Has :: c t => t -> Has c

elimHas ::
     (forall a. c a =>
                  a -> r)
  -> Has c
  -> r
elimHas f (Has c) = f c

type HasShow' = Has Show

type Dynamic' = Has Typeable
