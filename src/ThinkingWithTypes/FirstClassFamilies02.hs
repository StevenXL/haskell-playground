{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ThinkingWithTypes.FirstClassFamilies02 where

-- NOTE: See Reddit Post for Interprtation:
-- https://www.reddit.com/r/haskellquestions/comments/b6idzc/snd_is_a_concrete_type_but_i_can_still_apply_it/

import Data.Kind (Type)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

-- EXERCISE 10.2
-- defunctionalize listToMaybe at the type level
-- listToMaybe :: [a] -> Maybe a

data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (a ': _)) = 'Just a

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

-- Very confused about the MapList example. I am wondering if the substitution
-- model still applies here. It is also very hard to know when we are talking
-- about types and when we are talking about kinds.
type instance Eval (MapList _ '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) : Eval (MapList f as)
