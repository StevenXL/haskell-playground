{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ToOvercome.BasicTypeLevelProgramming where
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
----------------QUESTIONS------------------------
-- QUESTION 1: Let's talk about type families. What is a type family? A type
-- family is a function that operates on types. A type family accepts a type
-- and, when fully saturated, returns a type. Type family comes in three
-- different flavors, open, closed, and associated.
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
-- QUESTION 4: Let's talk about inductive versus recursive. What is induction
-- and recursion. I can't verbalize the answer to that question, which means my
-- thinking around it is muddy.

data IntBool a where
  Int :: Int -> IntBool Int
  Bool :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a -- depending on the data constructor matched, this function will return a value of type Int or a value of type Bool
extractIntBool (Int i) = i
extractIntBool (Bool b) = b

-- We combine the power of DataKind and GADTs to create a lengh-indexed list.
-- This means that we can, statically, prevent out-of-bounds errors.
data Nat = Zero | Succ Nat

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
