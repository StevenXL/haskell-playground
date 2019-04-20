{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module ThinkingWithTypes.AssociatedTypeFamilies where

import GHC.TypeLits
import Data.Proxy

-- Q1: Is there any difference between the :<< type constructor as written,
-- versus if we had left the kind annotations off, as in:
-- data a :<< b
data (a :: k1) :<< (b :: k2)
infix 5 :<<

-- INSIGHT: Just as data constructors remember the arguments they we called
-- with, type constructors also remember the arguments with which they were
-- called. This is how we can use a type constructor to store type-level
-- information (see Pg. 113). This also means we can deconstruct a type
-- constructor to get to the type that it was invoked with, just as we can
-- deconstruct a data constructor to get to the values it was invoked with.
--
--
-- INSIGHT: A closed type family and an associated type family have the same
-- power in terms of expressivity. However, an associated type family is
-- associated with a typeclass. This allows us to bundle values with computed
-- types.

-- 'a' is a type variable, which means that it must be instantiated to a
-- concrete type.
-- Printf a is a type family, which means that, given a concrete type, it will
-- produce another concrete type.
-- Finally, Printf is an associated type family, which means that it is
-- associated with a typeclass. This allows us to bundle a computed type (the
-- type of Printf a for a given a) to values (the implementation of the
-- typeclass's methods).
class HasPrintf a where
    type Printf a :: *
    format :: String -> Proxy a -> Printf a

instance KnownSymbol a => HasPrintf (a :: Symbol) where -- instance when a has the kind Symbol
    type Printf a = String
    format :: String -> Proxy a -> Printf a -- When "a" is a type that has a kind of Symbol, Printf a ~ String; anywhere we see Printf a in this context, we can also use String
    format s _ = s <> symbolVal (Proxy @a)

instance (KnownSymbol text, HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where -- instance when the first type has the kind symbol
    type Printf (text :<< a) = Printf a
    format :: String -> Proxy (text :<< a) -> Printf (text :<< a)
    format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (Show param, HasPrintf a) => HasPrintf ((param :: *) :<< a) where -- instance use when the first type has the kind Type
    type Printf (param :<< a) = param -> Printf a
    format :: String -> Proxy (param :<< a) -> Printf (param :<< a)
    format s _ = \param -> format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""
