module GetProgramming.Chapter04 where

import Data.List (sortBy, sort)

type LastName  = String
type FirstName = String
type Name      = (FirstName, LastName)

names :: [Name]
names = [("A", "Z"), ("C", "Y"), ("B", "Y")]

defaultSort :: [Name]
defaultSort = sort names

lastNameSort :: [Name]
lastNameSort = sortBy compareLastName names

lastThenFirstSort :: [Name]
lastThenFirstSort = sortBy compareLastThenFirst names

compareLastName :: Name -> Name -> Ordering
compareLastName n1 n2 = snd n1 `compare` snd n2

compareFirstName :: Name -> Name -> Ordering
compareFirstName n1 n2 = fst n1 `compare` fst n2

compareLastThenFirst :: Name -> Name -> Ordering
compareLastThenFirst n1 n2 = let lastNameCompare = compareLastName n1 n2
                             in if lastNameCompare == EQ
                                then compareFirstName n1 n2
                                else lastNameCompare

main :: IO ()
main = do
  print defaultSort
  print lastNameSort
  print lastThenFirstSort
