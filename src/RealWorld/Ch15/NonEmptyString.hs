module RealWorld.Ch15.NonEmptyString
  ( NonEmptyString
  , mkNonEmptyStr -- this is a smart constructor
  , toString
  ) where

-- NonEmptyString already exists as the more generalized NonEmptyList, but I
-- want to keep all the code here to avoid indirection

data NonEmptyString =
  NonEmptyString String

mkNonEmptyStr :: String -> Maybe NonEmptyString
mkNonEmptyStr s =
  if null s
    then Nothing
    else Just (NonEmptyString s)

toString :: NonEmptyString -> String
toString (NonEmptyString s) = s
