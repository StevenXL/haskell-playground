{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ToOvercome.BasicTypeLevelProgramming where

import qualified Data.Aeson as A
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=))
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as H
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

----------------GLOSSARY------------------------
-- TYPE: A type is a name for a set of values
--
-- KIND: A kind is a name for a set of types
--
-- A kind is a property of a type. A type is a property of a value.
--
-- The kind of Maybe is * -> *, which means Maybe is a type constructor which
-- accepts a concrete type and returns a concrete type.
--
-- Only types with the kind * are allowed to have values.
--
-- TYPE CONSTRUCTOR: A type constructor is a function at the type level which
-- accepts a type and returns a type
--
-- DATA CONSTRUCTOR: A data constructor is either a constant value, or a
-- function which accepts a value and returns a value of a specific type.
--
-- TYPE VARIABLE: A type variable is a placeholder for a type. In order for a
-- program to compile, all polymorphism must be striped away, and the only thing
-- left is concrete types.
--
-- DATAKINDS: DATAKINDS is an extension to GHC that allows us to promote types
-- (and type constructors) into kinds, and data constructors into types.
-- For example, given the following:
-- data Nat = Zero | Succ Nat
-- In plain haskell, we have the type Nat, the data constructor Zero, and the
-- data constructor Succ. The kind of Nat is *, the type of Zero is Nat, and the
-- type of Succ is (Nat -> Nat).
-- With DataKinds, we also get the kind 'Nat, the type 'Zero, and the type
-- 'Succ. Furthermore, 'Zero is of kind Nat, and 'Succ is of kind Nat -> Nat.
-- GADTs: GADTs allow us to provide extra type information when matching against
-- data constructors.
--
-- TYPE FAMILY: A type family is a function that operations on types - i.e., a
-- type-level function. It accepts a type and returns a type. A type family
-- comes in a couple of different flavors - closed, open, and associated.
--
-- PATTERN MATCHING: Must like we can pattern match on data constructors to
-- extract out their component values, we can pattern match on type constructors
-- to extract their component types! We can see this technique being used the
-- implementation of the closed type family, Add.
--
-- :kind vs :kind!: We use the former (without bang) version to ask GHCi for the
-- kind of a type. We can use the latter (with bang) to ask GHCi to evaluate the
-- type as far as possible.
--
----------------INSIGHT------------------------
-- Type-level programming is all about encoding information in the type system.
-- Once information is encoding in the type system, we are able to rely heavily
-- on the compiler to ensure our code keeps some guarantees, at least to the
-- information that is encoded in the type system. The compiler, of course,
-- cannot help us with information that it doesn't know - i.e., that is not in
-- the type system.
--
-- The algebra of a type also includes deconstructing it my pattern matching on
-- data constructors! This is how we implemented the "instance ToJSON (HList (a
-- ': as))"
----------------QUESTIONS------------------------
-- QUESTION 1: Let's talk about type families. What is a type family? A type
-- family is a function that operates on types. A type family accepts a type
-- and, when fully saturated, returns a type. Type family comes in three
-- different flavors, open, closed, and associated. What is the difference
-- between them?
--
-- QUESTION 2: All lowercase names in a type expression refer to a type
-- variable. This includes data type declarations (including GADTs), type family
-- declarations, any type expression.
--
-- QUESTION 3: I though I was getting confused because I did not know when we
-- were talking about a type versus when we were talking about a kind. It turns
-- out that my confusion is really about when we are talking about a data
-- constructor versus the (lifted) type constructor. For example:
-- type family Add n m where
--    Add Zero m = m
--    Add (Succ n) m = Succ (Add n m)
-- We are in a type expression. n and m are type variables, which means that
-- they can only be initialized to types. In the first indented line, we
-- initialize n to Zero, and because a type a type variable can only be
-- initialized to a type, then Zero must be a type. Because Zero is a type that
-- is a result of lifting the Zero data constructor to the type level, we are
-- really talking about 'Zero. This line of reasoning is also applicable to the
-- second indented line.
--
-- QUESTION 4: Let's talk about inductive versus recursive. What is induction?
-- What does it mean for something to be defined inductively? What is recursion?
-- I've lost the ability to verbalize the answer to these questions, which means
-- that my mental model around them is deficient.
--
-- QUESTION 5: I do not understand why the kind of '[] is [k].
--
-- QUESTION 6: In the definition for the type (>>), would it make sense to
-- define it like so:
-- newtype (s :: Symbol) >> a = Named a
-- To ensure that we can only use type-level strings as the type for s? Or am I
-- missing some nuance?
----------------GADTs Example------------------------
data IntBool a where
  Int' :: Int -> IntBool Int
  Bool' :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a -- depending on the data constructor matched, this function will return a value of type Int or a value of type Bool
extractIntBool (Int' i) = i
extractIntBool (Bool' b) = b

----------------INDEXED VECTORS EXAMPLE------------------------
-- We combine the power of DataKind and GADTs to create a lengh-indexed list.
-- This means that we can, statically, prevent out-of-bounds errors.
data Nat
  = Zero
  | Succ Nat

data Vector (n :: Nat) a where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
  show :: Vector n a -> String
  show VNil = "VNil"
  show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

-- closed type family
type family Add (n :: Nat) (m :: Nat) where
  Add Zero m = m
  Add (Succ n) m = Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs = xs
append (VCons a as) xs = VCons a (append as xs)

----------------HETEROGENOUS LIST EXAMPLE------------------------
-- We know that xs is a TYPE VARIABLE. As a result, it can only be instantiated
-- to a type.
data HList xs where
  HNil :: HList '[] -- We know that '[] is the data constructor [] lifted to the type level, both because of the tick, and because it is what xs is instantiated to
  HCons :: a -> HList as -> HList (a ': as)

instance Show (HList '[]) where
  show :: HList '[] -> String
  show _ = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ': as)) where
  show :: HList (a ': as) -> String
  show (HCons a as) = "HCons " ++ show a ++ " (" ++ show as ++ ")"

----------------EXCERCISE: WRITE AESON INSTANCE OF HLIST------------------------
instance ToJSON (HList '[]) where
  toJSON :: HList '[] -> Value
  toJSON _ = Array mempty

instance (ToJSON a, ToJSON (HList as)) => ToJSON (HList (a ': as)) where
  toJSON :: HList (a ': as) -> Value
  toJSON (HCons a as) = Array (vectorForA <> vectorForAs)
    where
      vectorForA = V.singleton (toJSON a)
      (Array vectorForAs) = toJSON as

instance FromJSON (HList '[]) where
  parseJSON :: Value -> Parser (HList '[])
  parseJSON (Array _) = return HNil
  parseJSON invalid = typeMismatch "HList '[]" invalid

instance (FromJSON a, FromJSON (HList as)) => FromJSON (HList (a ': as)) where
  parseJSON :: Value -> Parser (HList (a ': as))
  parseJSON (Array v) = do
    rest <- parseJSON (Array $ V.tail v) -- this is an example of return type polymorphism!
    first <- parseJSON (V.head v)
    return $ HCons first rest
  parseJSON invalid = typeMismatch "HList (a ': as)" invalid

main :: IO ()
main = do
  let hlist = HCons "Hello" (HCons (1 :: Int) (HCons True HNil))
  print $ "My hlist is: " ++ show hlist
  let encodedHList = A.encode hlist
  C.putStrLn $ (C.pack "My hlist encoded is: ") <> encodedHList
  let decodedHList = A.decode encodedHList
  putStrLn "My hlist decoded is:"
  print (decodedHList :: Maybe (HList '[ [Char], Int, Bool]))

----------------EXTENSIBLE RECORDS EXAMPLE------------------------
-- s is a type variable
-- a is a type variable
-- >> is a type constructor of kind (* -> * -> *)
-- Named is a data constructor of type (a -> (s >> a))
newtype (s :: Symbol) >> a =
  Named a

-- Question: If we had done newtype (s :: Symbol) >> a = Named a, what would the
-- kind of the type constructor >> be?
-- Answer: Symbol -> * -> *
data HRec xs where
  HREmpty :: HRec '[]
  HRCons :: (s >> a) -> HRec xs -> HRec (s >> a ': xs)

instance Show (HRec '[]) where
  show :: HRec '[] -> String
  show _ = "HREmpty"

instance (KnownSymbol s, Show a, Show (HRec xs)) =>
         Show (HRec ((s >> a) ': xs)) where
  show :: HRec ((s >> a) ': xs) -> String
  show (HRCons (Named a) as) = "(" ++ key ++ ": " ++ val ++ ") " ++ rest
    where
      key = (symbolVal @s) Proxy
      val = show a
      rest = (show as)

instance ToJSON (HRec '[]) where
  toJSON :: HRec '[] -> Value
  toJSON _ = A.object []

-- You have to keep in mind that any lower-case letter in a type expression is a
-- TYPE VARIABLE. Once we keep that clear in our head, everything else false
-- into place.
-- Remember that >> is a type constructor of kind Symbol -> * -> *; so (s >> a)
-- is a type
instance (ToJSON (HRec as), ToJSON (s >> a)) =>
         ToJSON (HRec (s >> a ': as)) where
  toJSON :: HRec (s >> a ': as) -> Value
  toJSON (HRCons named rest) =
    let Object named' = A.toJSON named
        Object rest' = A.toJSON rest
     in A.object (H.toList $ H.union named' rest')

instance (KnownSymbol s, ToJSON a) => ToJSON (s >> a) where
  toJSON :: (s >> a) -> Value
  toJSON (Named a) = A.object [name .= A.toJSON a]
    where
      name = T.pack $ (symbolVal @s) Proxy
