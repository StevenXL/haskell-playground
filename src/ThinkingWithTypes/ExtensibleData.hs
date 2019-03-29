{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module ThinkingWithTypes.ExtensibleData where

import Data.Kind (Type)
import Data.Proxy
import Fcf (Eval, FindIndex, FromMaybe, Stuck, TyEq)
import Fcf.Combinators -- Only used for =<<; don't know how to add it on L13
import GHC.TypeLits (KnownNat, natVal)
import Unsafe.Coerce (unsafeCoerce)

-- UnsafeOpenSum is the raw data constructor. It is unsafe because it cannot
-- enforce our invariant that the first argument to UnsafeOpenSum is the index
-- of the type t in ts
data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

-- FindElem is a type family; given some types, it will compute another type. In
-- this case, FindElem will return the type Stuck if the type key is not in ts.
-- Otherwise it will return a type with the kind Nat - i.e., a type-level
-- natural number.
type FindElem (key :: k) (ts :: [k])
   = FromMaybe Stuck =<< FindIndex (TyEq key) ts

-- Finally, we use the KnownNat typeclass contraint to create a constraint that
-- ensures that t is in ts, because whatever type FindElem evaluates to, it must
-- be a member of KnownNat to satisfy the Member constraint, and that is only
-- possible for type-level natural numbers.
type Member t ts = KnownNat (Eval (FindElem t ts))

findElem ::
     forall t ts. Member t ts
  => Int
findElem = fromIntegral . natVal $ (Proxy :: Proxy (Eval (FindElem t ts)))

-- Now that we have FindElem, we can use it to build smart constructors and
-- other combinators that are TYPE SAFE and ENFORCE INVARIANTS.
inj ::
     forall f t ts. Member t ts
  => f t
  -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj ::
     forall f t ts. Member t ts
  => OpenSum f ts
  -> Maybe (f t)
prj (UnsafeOpenSum i ft) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce ft
    else Nothing

-- If prj fails - by giving us back the value Nothing - then we know something
-- about the type (what it is not), and we'd like to reflect that in the type:
decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 ft) = Left $ unsafeCoerce ft
decompose (UnsafeOpenSum n ft) = Right $ (UnsafeOpenSum (n - 1) ft)
